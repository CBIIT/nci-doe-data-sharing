package gov.nih.nci.doe.web.service.impl;

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.mail.internet.InternetAddress;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;
import org.apache.velocity.exception.VelocityException;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

import gov.nih.nci.doe.web.service.MailService;
import gov.nih.nci.doe.web.model.ContactUs;
import gov.nih.nci.doe.web.model.PredictionTaskNotification;
import gov.nih.nci.doe.web.model.UploadTaskNotification;
import gov.nih.nci.doe.web.model.DownloadTaskNotification;
import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.domain.MailTemplate;
import gov.nih.nci.doe.web.repository.ReleaseNotesNotificationRepository;
import gov.nih.nci.doe.web.repository.MailTemplateRepository;

@Component
public class MailServiceImpl implements MailService {

	protected Logger log = LoggerFactory.getLogger(this.getClass());

	@Autowired
	private JavaMailSender mailSender;

	@Autowired
	private VelocityEngine velocityEngine;

	@Autowired
	private MailTemplateRepository templateDAO;

	@Autowired
	ReleaseNotesNotificationRepository emailNotificationRepository;

	@Value("${mail.override}")
	private boolean override;
	@Value("${mail.override.addresses}")
	private String overrideAddresses;
	@Value("${mail.admin.address}")
	private String adminAddress;
	@Value("${mail.support.email}")
	private String supportEmail;

	@Value("${mail.notification.email}")
	private String notificationEmail;

	@Value("${gov.nih.nci.hpc.web.server}")
	String webServerName;

	/**
	 * Very simple mail sender method using Velocity templates pulled from a
	 * database. The message template is retrieved from the database using the short
	 * identifier. The subject and body are evaluated by the Velocity Engine, and
	 * all parameter substitutions are performed. The Map of parameters provided
	 * should include all the values expected by the subject and body, but no error
	 * checking is done to confirm this. No errors are thrown if the send fails,
	 * velocity evaluation fails, or the requested template is not found, but it
	 * will be logged. If no 'to' parameter is provided the send will log an error
	 * message and exit.
	 * 
	 * Potential error conditions:
	 * <ul>
	 * <li>The specified template isn't found.</li>
	 * <li>The 'to' parameter is not provided.</li>
	 * <li>Velocity evaluation fails.</li>
	 * <li>Mail sending fails.</li>
	 * <li>Attachment can't be attached.</li>
	 * </ul>
	 * Parameters (not including those required by the template):
	 * <ul>
	 * <li>to - a String[] of addresses to send the message to {required}</li>
	 * <li>cc, bcc - String[] of addresses for the cc and bcc fields, respectively
	 * {optional}</li>
	 * <li>attachments - a Map<String, File> containing names and Files of all
	 * attachments</li>
	 * </ul>
	 * 
	 * @param identifier the identifier
	 * @param params     the params
	 */
	private void send(final String identifier, final Map<String, Object> params) {
		MailTemplate template = null;
		try {
			template = templateDAO.findMailTemplateTByShortIdentifier(identifier);
		} catch (Exception ex) {
			log.error("Database exception", ex);
		}

		if (template != null) {
			final VelocityContext vc = new VelocityContext(params);
			final String[] to = (String[]) vc.get(TO);

			if (to != null) {
				final String[] cc = (String[]) vc.get(CC);
				final String[] bcc = (String[]) vc.get(BCC);
				final String fromAddress = (String) vc.get(FROM);

				final StringWriter body = new StringWriter();
				final StringWriter subjectWriter = new StringWriter();

				try {
					StringBuffer emailBody = new StringBuffer();
					emailBody.append(template.getEmailBody());
					velocityEngine.evaluate(vc, body, identifier, emailBody.toString());
					velocityEngine.evaluate(vc, subjectWriter, identifier, template.getEmailSubject());
					log.info("evaluating email template: " + body.toString());
					log.info("sending message....." + identifier + " with params..... " + params);

					final MimeMessageHelper helper = new MimeMessageHelper(mailSender.createMimeMessage(), true,
							"UTF-8");

					String subject = subjectWriter.toString();
					String toAdds = null;
					String ccAdds = null;

					final String[] overrideAddressesToList = overrideAddresses.split(",");

					if (!override) {
						helper.setTo(to);

						if (cc != null) {
							helper.setCc(cc);
						}
						if (bcc != null) {
							helper.setBcc(bcc);
						}
					} else {

						helper.setTo(overrideAddressesToList);

						if (vc.get(TO) != null) {
							toAdds = Arrays.deepToString((String[]) vc.get(TO));
						} else {
							toAdds = "===> No to: addresses found";
						}
						if (vc.get(CC) != null) {
							ccAdds = Arrays.deepToString((String[]) vc.get(CC));
						} else {
							ccAdds = "===> No cc: addresses found";
						}
						subject += " {TO: " + toAdds + "} {CC: " + ccAdds + "}";
					}

					helper.setText(body.toString(), true);
					helper.setSubject(subject);
					if (StringUtils.isEmpty(fromAddress)) {
						helper.setFrom(new InternetAddress(supportEmail));
					} else {
						helper.setFrom(new InternetAddress(fromAddress));
					}

					log.info("invoking mailSender");
					log.info("Sending email to -> " + toAdds + "; cc -> " + ccAdds);
					mailSender.send(helper.getMimeMessage());
					log.info("done invoking mailSender");

				} catch (final VelocityException e) {
					log.error("VelocityException", e);
				} catch (final Exception e) {
					log.error("================================================================================");
					log.error(e.getMessage(), e);
					log.error("=====> Failed sending message: " + body.toString());
					log.error("=====> Template: " + identifier);
					log.error("=====> Params: " + params);
					log.error("================================================================================");
				}
			} else {
				log.error("required parameter 'to' not found");
			}
		} else {
			log.error("No message with identifier '" + identifier + "' found");
		}
	}

	@Override
	@Transactional(readOnly = true)
	public void sendActivationEmail(String webServerName, String email, String uuid) {
		log.info("Sending an activation email after registration");
		final Map<String, Object> params = new HashMap<String, Object>();
		final List<String> to = new ArrayList<String>();
		to.add(email);
		params.put("confirm_email", webServerName + "/loginTab?token=" + uuid + "&email=" + email);
		params.put(TO, to.toArray(new String[0]));
		send("ACTIVATION_EMAIL", params);
	}

	@Override
	public void sendResetPasswordEmail(String password, String email) {
		log.info("Sending an email for password reset" + email);

		final Map<String, Object> params = new HashMap<String, Object>();
		final List<String> to = new ArrayList<String>();
		to.add(email);
		params.put(TO, to.toArray(new String[0]));
		params.put("TEMP_PSWD", password);
		send("RESET_PASSWORD_EMAIL", params);

	}

	@Override
	public void sendCollectionRegistationFailure(String email, String collectionPath, Exception e, String taskId) {
		log.info("Sending collection registration failure for: " + email);
		final Map<String, Object> params = new HashMap<String, Object>();
		final List<String> to = new ArrayList<String>();
		to.add(adminAddress);
		params.put(TO, to.toArray(new String[0]));
		params.put("COLLECTION_PATH", collectionPath);
		params.put("EMAIL", email);
		params.put("EXCEPTION", e != null ? e : "");
		params.put("TASK_ID", taskId != null ? taskId : "");
		send("COLLECTION_FAILURE_EMAIL", params);
	}

	@Override
	public void sendErrorEmail(Exception e, String user) {
		log.info("Sending exception email");
		final Map<String, Object> params = new HashMap<String, Object>();
		final List<String> to = new ArrayList<String>();
		to.add(adminAddress);
		params.put(TO, to.toArray(new String[0]));
		params.put("EXCEPTION", e);
		params.put("user_Id", user);
		send("EXCEPTION_EMAIL", params);

	}

	@Override
	public void sendContactUsEmail(ContactUs contactUs) {
		log.info("Sending contact us email");
		final Map<String, Object> params = new HashMap<String, Object>();
		final List<String> to = new ArrayList<String>();
		to.add(supportEmail);
		params.put(FROM, contactUs.getEmailAddress());
		params.put(TO, to.toArray(new String[0]));
		params.put("message", contactUs.getMessage());
		params.put("org", contactUs.getOrg());
		params.put("inquiry", contactUs.getInquiry());
		params.put("username", contactUs.getFirstName() + " " + contactUs.getLastName());
		send("CONTACT_US_EMAIL", params);
	}

	@Override
	public String sendReleaseNotesNotificationEmail(String webServerName, String loggedOnUser) throws DoeWebException {
		log.info("Sending notification email");
		try {

			StringBuilder mailtoUrl = new StringBuilder("mailto:");

			// get the sender list from email_updates_t table
			List<String> bccList = emailNotificationRepository.getAllEmailAddress();

			// get the mail template for the notification in mail_template table
			MailTemplate template = templateDAO.findMailTemplateTByShortIdentifier("RELEASE_NOTIFICATION_EMAIL");

			mailtoUrl.append(encodeField(loggedOnUser));

			if (bccList != null && !bccList.isEmpty()) {
				mailtoUrl.append("?bcc=");
				for (String bcc : bccList) {
					mailtoUrl.append(encodeField(bcc)).append(",");
				}

				mailtoUrl.deleteCharAt(mailtoUrl.length() - 1);
			}

			if (!template.getEmailSubject().isEmpty()) {
				mailtoUrl.append("&subject=").append(encodeField(template.getEmailSubject()));
			}

			if (!template.getEmailBody().isEmpty()) {
				String emailTemplate = template.getEmailBody();

				// Replace placeholders with dynamic values
				String personalizedTemplate = emailTemplate.replace("${modac_link}", webServerName)
						.replace("${gitHubLink}", "https://github.com/CBIIT/nci-doe-data-sharing/tree/master/doc")
						.replace("${unsubscribe_link}", webServerName + "/contactUs?typeOfInquiry=unsubscribe");

				mailtoUrl.append("&body=").append(encodeField(personalizedTemplate));

			}

			return mailtoUrl.toString();

		} catch (Exception e) {
			throw new DoeWebException("Error in drafting notification email: " + e.getMessage());
		}

	}

	@Override
	public String sendPredictionTaskNotification(PredictionTaskNotification notification) throws DoeWebException {
		log.info("Sending prediciton task notiifcation email");

		try {
			final Map<String, Object> params = new HashMap<String, Object>();
			final List<String> to = new ArrayList<String>();
			to.add(notification.getUserId());
			params.put(FROM, notificationEmail);
			params.put(TO, to.toArray(new String[0]));
			params.put("dateTime", notification.getCompletedDate());
			params.put("taskId", notification.getTaskId());
			params.put("modac_link", webServerName + "/tasksTab");
			params.put("resultPath", notification.getResultPath());
			params.put("inputDatasetPath", notification.getInputDataset());
			params.put("status", notification.getStatus());
			params.put("displayStatus", notification.getDisplayStatus());

			params.put("failureMsg",
					StringUtils.isNotEmpty(notification.getFailureMsg()) ? "Reason: " + notification.getFailureMsg()
							: "");
			send("PREDICTION_NOTIFICATION", params);
			return "SUCCESS";
		} catch (Exception e) {
			throw new DoeWebException("Error in sending prediction notification email: " + e.getMessage());
		}
	}

	public static String encodeField(String field) {
		try {

			return URLEncoder.encode(field, "UTF-8").replace("+", "%20");

		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
			return "";
		}
	}

	@Override
	public String sendUploadTaskNotification(UploadTaskNotification taskNotification) throws DoeWebException {
		log.info("Sending upload task notification email");

		try {
			final Map<String, Object> params = new HashMap<String, Object>();
			final List<String> to = new ArrayList<String>();
			to.add(taskNotification.getUserId());
			params.put(FROM, notificationEmail);
			params.put(TO, to.toArray(new String[0]));
			params.put("dateTime", taskNotification.getCompletedDate());
			params.put("taskId", taskNotification.getTaskId());
			params.put("status", taskNotification.getStatus());
			params.put("registrationItems", taskNotification.getRegistrationItems());
			params.put("modac_link", webServerName + "/tasksTab");
			params.put("displayStatus", taskNotification.getDisplayStatus());
			params.put("failureMsg",
					StringUtils.isNotEmpty(taskNotification.getErrorMsg()) ? "Reason: " + taskNotification.getErrorMsg()
							: "");
			send("UPLOAD_NOTIFICATION", params);
			return "SUCCESS";

		} catch (Exception e) {
			throw new DoeWebException("Error in sending upload notification email: " + e.getMessage());
		}

	}

	@Override
	public String sendDownloadTaskNotification(DownloadTaskNotification taskNotification) throws DoeWebException {
		log.info("Sending download task notification email");

		try {
			final Map<String, Object> params = new HashMap<String, Object>();
			final List<String> to = new ArrayList<String>();
			to.add(taskNotification.getUserId());
			params.put(FROM, notificationEmail);
			params.put(TO, to.toArray(new String[0]));
			params.put("dateTime", taskNotification.getCompletedDate());
			params.put("taskId", taskNotification.getTaskId());
			params.put("downloadType", taskNotification.getDownloadType());
			params.put("status", taskNotification.getStatus());
			params.put("targetPath", taskNotification.getTargetPath());
			params.put("sourcePath", taskNotification.getSourcePath());
			params.put("destinationType", taskNotification.getDestinationType());
			params.put("modac_link", webServerName + "/tasksTab");
			params.put("displayStatus", taskNotification.getDisplayStatus());
			params.put("failureMsg",
					StringUtils.isNotEmpty(taskNotification.getErrorMsg()) ? "Reason: " + taskNotification.getErrorMsg()
							: "");

			send("DOWNLOAD_NOTIFICATION", params);
			return "SUCCESS";

		} catch (Exception e) {
			throw new DoeWebException("Error in sending download notification email: " + e.getMessage());
		}
	}

}
