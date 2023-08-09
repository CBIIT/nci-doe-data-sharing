package gov.nih.nci.doe.web.service;

import gov.nih.nci.doe.web.model.ContactUs;

public interface MailService {

	public static final String TO = "to";
	public static final String CC = "cc";
	public static final String BCC = "bcc";
	public static final String FROM = "from";

	public void sendActivationEmail(String webServerName, String email, String uuid);

	public void sendResetPasswordEmail(String password, String email);

	public void sendCollectionRegistationFailure(String email, String collectionPath, Exception e, String taskId);

	public void sendErrorEmail(Exception e, String user);

	public void sendContactUsEmail(ContactUs contactUs);

	public void sendNotificationEmail(String webServerName, String loggedOnUser);

}
