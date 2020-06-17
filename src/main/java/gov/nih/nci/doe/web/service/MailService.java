package gov.nih.nci.doe.web.service;

import java.util.List;

public interface MailService {

	public static final String TO = "to";
	public static final String CC = "cc";
	public static final String BCC = "bcc";
	public static final String FROM = "from";
	
	public void sendRegistrationEmail(String email) throws Exception;
	
	public void sendResetPasswordEmail(String password,String email) throws Exception;
	
	public void sendNotifyUsersForAccessGroups(List<String> email) throws Exception;
}
