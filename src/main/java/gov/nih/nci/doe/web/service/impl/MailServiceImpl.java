package gov.nih.nci.doe.web.service.impl;

import java.io.IOException;
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
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import gov.nih.nci.doe.web.service.MailService;
import gov.nih.nci.doe.web.domain.MailTemplate;
import gov.nih.nci.doe.web.repository.MailTemplateRepository;


@Component
public class MailServiceImpl implements MailService {

	protected final Logger log = LogManager.getLogger(this.getClass());
	
	@Autowired
	private JavaMailSender mailSender;
	
	@Autowired	
	private VelocityEngine velocityEngine;
	
	@Autowired
	private MailTemplateRepository templateDAO;
	
	@Value("${email.from}")
	private String from;
	@Value("${email.from.display}")
	private String fromDisplay;
	@Value("${mail.override}")
	private boolean override;
	@Value("${mail.override.addresses}")
	private String overrideAddresses;
	



	
	/**
	 * Very simple mail sender method using Velocity templates pulled from a
	 * database. The message template is retrieved from the database using the short
	 * identifier. The subject and body are evaluated by the Velocity Engine, and all
	 * parameter substitutions are performed. The Map of parameters provided should include all the values 
	 * expected by the subject and body, but no error checking is done to confirm this.
	 * No errors are thrown if the send fails, velocity evaluation fails, or the requested template is not 
	 * found, but it will be logged. If no 'to' parameter is provided the send will log an error message and exit.	
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
	 * <li>cc, bcc - String[] of addresses for the cc and bcc fields,
	 * respectively {optional}</li>
	 * <li>attachments - a Map<String, File> containing names and Files of all
	 * attachments</li>
	 * </ul>
	 * 
	 * @param identifier
	 *            the identifier
	 * @param params
	 *            the params
	 */
	private void send(final String identifier, final Map<String, Object> params) {
		MailTemplate template = null;
		try{
			template = templateDAO.findMailTemplateTByShortIdentifier(identifier);
		} catch(Exception ex) {
			log.error("Database exception", ex);
		}	
		
		if (template != null) {
			final VelocityContext vc = new VelocityContext(params);
			final String[] to = (String[]) vc.get(TO);

			if (to != null) {
				final String[] cc = (String[]) vc.get(CC);
				final String[] bcc = (String[]) vc.get(BCC);

				final StringWriter body = new StringWriter();
				final StringWriter subjectWriter = new StringWriter();

				try {
					StringBuffer emailBody = new StringBuffer();
					emailBody.append(template.getEmailBody());
					velocityEngine.evaluate(vc, body, identifier, emailBody.toString());
					velocityEngine.evaluate(vc, subjectWriter, identifier, template.getEmailSubject());
					log.info("evaluating email template: " + body.toString());
					log.info("sending message....." + identifier + " with params..... " + params);
					
					final MimeMessageHelper helper = new MimeMessageHelper(mailSender.createMimeMessage(), true, "UTF-8");

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
					}  else {

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
					helper.setFrom(new InternetAddress(from, fromDisplay));

					log.info("invoking mailSender");
					log.info("Sending email to -> " + toAdds + "; cc -> " + ccAdds );
					mailSender.send(helper.getMimeMessage());
					log.info("done invoking mailSender");

				} catch (final VelocityException e) {
					log.error("VelocityException", e);
				} catch (final IOException e) {
					log.error("IOException", e);
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
	public void sendRegistrationEmail(String email) throws Exception {
		log.info("Sending an email for registration");
		final Map<String, Object> params = new HashMap<String, Object>();
		final List<String> to = new ArrayList<String>();
		to.add(email);
		
		params.put(TO, to.toArray(new String[0]));
		params.put(BCC, overrideAddresses);
		send("REGISTRATION_EMAIL", params);
	}

	@Override
	public void sendResetPasswordEmail(String password, String email) throws Exception {
		log.info("Sending an email for password reset" + email);
		
		final Map<String, Object> params = new HashMap<String, Object>();
		final List<String> to = new ArrayList<String>();
		to.add(email);		
		params.put(TO, to.toArray(new String[0]));
		params.put("TEMP_PSWD", password);
		send("RESET_PASSWORD_EMAIL", params);
		
	}

	@Override
	public void sendNotifyUsersForAccessGroups(String email) throws Exception {
        log.info("Sending an email for access group change");
		
		final Map<String, Object> params = new HashMap<String, Object>();
		final List<String> to = new ArrayList<String>();
		to.add(email);		
		params.put(TO, to.toArray(new String[0]));
		send("ACCESS_GROUP_EMAIL", params);
	}	
	
		
}
