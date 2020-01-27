package gov.nih.nci.doe.web.service;



import gov.nih.nci.doe.web.model.DoeRegistration;
import gov.nih.nci.doe.web.model.DoeUsersModel;
import gov.nih.nci.doe.web.util.LoginStatusCode;
import gov.nih.nci.doe.web.util.PasswordStatusCode;


public interface AuthenticateService {
	

	LoginStatusCode authenticateExternalUser(String emailAddress, String password);
	
	String getPassword(String username) throws Exception;

	void register (DoeRegistration register);
	
	PasswordStatusCode validatePassword(String rawPassword, String userId);
	
	boolean doesUsernameExist(String username) throws Exception;
	
	PasswordStatusCode saveUserPassword(String rawPassword, String userid) throws Exception;
	
	DoeUsersModel getUserInfo(String emailAddr);
	
	void saveUserInfo(DoeUsersModel doeModel);
	
	void clearLockFields(String username);
	
    LoginStatusCode lockOrUnlockUser(final String user, final boolean lock) throws Exception;

}