package gov.nih.nci.doe.web.service;


public interface MailService {

	public static final String TO = "to";
	public static final String CC = "cc";
	public static final String BCC = "bcc";
	public static final String FROM = "from";
	
	public void sendRegistrationEmail(String email) throws Exception;
	
	public void sendResetPasswordEmail(String password,String email) throws Exception;
}
