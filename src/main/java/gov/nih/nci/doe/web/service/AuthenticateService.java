package gov.nih.nci.doe.web.service;

import gov.nih.nci.doe.web.constants.LoginStatusCode;
import gov.nih.nci.doe.web.constants.PasswordStatusCode;
import gov.nih.nci.doe.web.domain.DoeUsers;
import gov.nih.nci.doe.web.model.DoeRegistration;
import gov.nih.nci.doe.web.model.DoeUsersModel;

public interface AuthenticateService {

	LoginStatusCode authenticateExternalUser(String emailAddress, String password);

	String getPassword(String username);

	DoeUsers register(DoeRegistration register);

	PasswordStatusCode validatePassword(String rawPassword, String userId);

	boolean doesUsernameExist(String username);

	PasswordStatusCode saveUserPassword(String rawPassword, String userid, Boolean validatePswd);

	DoeUsersModel getUserInfo(String emailAddr);

	void saveUserInfo(DoeUsersModel doeModel);

	void clearLockFields(String username);

	String confirmRegistration(String token, String email);

}