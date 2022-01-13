package gov.nih.nci.doe.web.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import gov.nih.nci.doe.web.constants.LoginStatusCode;
import gov.nih.nci.doe.web.constants.PasswordStatusCode;
import gov.nih.nci.doe.web.domain.DoeUsers;
import gov.nih.nci.doe.web.model.DoeRegistration;
import gov.nih.nci.doe.web.model.DoeUsersModel;
import gov.nih.nci.doe.web.repository.DoeUserRepository;
import gov.nih.nci.doe.web.repository.UserGroupRespository;
import gov.nih.nci.doe.web.service.AuthenticateService;

import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component
public class AuthenticateServiceImpl implements AuthenticateService {

	private static final Logger log = LoggerFactory.getLogger(AuthenticateServiceImpl.class);

	@Autowired
	@Qualifier("passwordEncoder")
	private PasswordEncoder passwordEncoder;

	@Autowired
	private DoeUserRepository doeUserRepository;

	@Autowired
	UserGroupRespository userGroupRespitory;

	private static final int PASSWORD_MIN_LENGTH = 8;
	private static final int PASSWORD_MAX_LENGTH = 14;
	private static final Integer MAXIM_LOGIN_ATTEMPTS = 3;

	@Override
	@Transactional(readOnly = false)
	public LoginStatusCode authenticateExternalUser(final String username, final String password) {
		try {
			// sanity check
			if (StringUtils.isBlank(username)) {
				log.error("email address must be provided");
				return LoginStatusCode.LOGIN_INVALID_EMAIL;
			}
			if (StringUtils.isBlank(password)) {
				log.error("password must be provided");
				return LoginStatusCode.LOGIN_INVALID_PASSWORD;
			}

			log.info("Attempt to login for username: {}", username);

			// check to see if the password user entered matches the one stored in the
			// system
			boolean passwordMatched = false;

			passwordMatched = passwordEncoder.matches(password, doeUserRepository.getPasswordById(username));

			DoeUsers user = doeUserRepository.getUserInfo(username);

			// no default value
			LoginStatusCode status = null;

			if (user.getLockoutCounter() == 3) {
				log.info("Account is locked for username: {}", username);
				status = LoginStatusCode.LOGIN_LOCKED;

			} else if (Boolean.FALSE.equals(user.getIsActivated())) {
				log.info("Account is not activated for username: {}", username);
				status = LoginStatusCode.LOGIN_INACTIVATED;

			} else {
				status = checkForPassword(username, passwordMatched);
			}

			return status;
		} catch (Exception e) {
			log.error("Unable to authenticate", e);
			return LoginStatusCode.LOGIN_FAILURE;
		}
	}

	private LoginStatusCode checkForPassword(final String username, final boolean passwordMatched) throws Exception {
		LoginStatusCode status = null;
		if (passwordMatched) {
			log.info("Correct password was entered for username: {}", username);
			lockOrUnlockUser(username, false);
			// user has at least successfully logged in
			status = LoginStatusCode.LOGIN_SUCCESS;

		} else {
			log.info("Invalid password entered for username: {}", username);
			status = lockOrUnlockUser(username, true);
			if (LoginStatusCode.LOGIN_SUCCESS == status) {
				status = LoginStatusCode.LOGIN_FAILURE;
			}

		}
		return status;
	}


	private LoginStatusCode lockOrUnlockUser(final String user, final boolean lock) throws Exception {
		// sanity check
		if (user == null) {
			throw new IllegalArgumentException("User is required");
		}

		LoginStatusCode status = LoginStatusCode.LOGIN_SUCCESS;
		DoeUsers d = doeUserRepository.getUserInfo(user);
		if (lock) {
			log.info("Attempt to lock user's account for user ID: {}", user);

			// update counter and lock the account if needed

			Integer newCounter = (d.getLockoutCounter() != null) ? d.getLockoutCounter() + 1 : 1;

			if (newCounter > MAXIM_LOGIN_ATTEMPTS) {
				log.info("Locking account for user ID: {}", d.getEmailAddrr());
				// login failed and user has tried 3 times.lock the account
				status = LoginStatusCode.LOGIN_LOCKED;
				d.setLockoutCounter(MAXIM_LOGIN_ATTEMPTS);
			} else {
				// login failed but has not reached the max number of attempts. do not lock the
				// account
				d.setLockoutCounter(newCounter);
			}
			d.setLastChangedDate(new Date());
			doeUserRepository.saveAndFlush(d);
		} else {
			clearLockFields(d.getEmailAddrr());
		}

		return status;
	}

	@Override
	@Transactional(readOnly = false)
	public void clearLockFields(String username) {
		log.info("Attempt to unlock account for user ID: {}", username);
		// reset counter
		DoeUsers user = doeUserRepository.getUserInfo(username);
		user.setLockoutDate(null);
		user.setLockoutCounter(0);
		doeUserRepository.saveAndFlush(user);
	}

	@Override
	@Transactional(readOnly = true)
	public String getPassword(String username) {
		return doeUserRepository.getPasswordById(username);
	}

	@Override
	@Transactional(readOnly = false)
	public DoeUsers register(DoeRegistration register) {
		log.info(" register user " + register.getEmailAddress());
		String token = UUID.randomUUID().toString();
		DoeUsers user = new DoeUsers();
		user.setFirstName(register.getFirstName());
		user.setPassword(register.getPassword());
		user.setLastName(register.getLastName());
		user.setInstitution(register.getInstitution());
		user.setEmailAddrr(register.getEmailAddress().toLowerCase());
		user.setLockoutCounter(0);
		user.setIsActivated(false);
		user.setUuid(token);
		user.setCreatedDate(new Date());
		doeUserRepository.saveAndFlush(user);

		String encodedPassword = passwordEncoder.encode(register.getPassword());

		user.setPassword(encodedPassword);
		doeUserRepository.saveAndFlush(user);
		return user;
	}

	@Override
	@Transactional(readOnly = false)
	public String confirmRegistration(String token, String email) {
		DoeUsers user = doeUserRepository.getUserInfoByToken(token, email);
		if (user != null) {
			user.setIsActivated(true);
			doeUserRepository.saveAndFlush(user);
			return "SUCCESS";
		}
		return "FAILURE";
	}

	@Override
	public PasswordStatusCode validatePassword(String rawPassword, String userId) {

		log.info("Validating password: [user ID = {}]", userId);
		if (StringUtils.isBlank(rawPassword)) {
			log.info("Password is empty");
			return PasswordStatusCode.INVALID_LENGTH;
		}

		if (rawPassword.length() < PASSWORD_MIN_LENGTH || rawPassword.length() > PASSWORD_MAX_LENGTH) {
			log.info("Password is too short or too long");
			return PasswordStatusCode.INVALID_LENGTH;
		}

		if (!Pattern.matches(
				"^.*((?=.*[a-z])(?=.*[A-Z])(?=.*\\d)(?=.*[@#$%^&*\\(\\)_+|~!\\-=\\\\`{}\\[\\]:\";'<>?,\\./])).*$",
				rawPassword)) {
			log.info("Invalid format");
			return PasswordStatusCode.INVALID_FORMAT;
		}

		// check if password is reused from current
		if (userId != null) {
			try {
				String encryptedPassword = doeUserRepository.getPasswordById(userId);
				if ((encryptedPassword != null && passwordEncoder.matches(rawPassword, encryptedPassword))) {
					log.info("New password is same as the current password for applicant ID: {}", userId);
					return PasswordStatusCode.NEW_PASSWD_SAME_AS_PREV_PASSWD;
				}
			} catch (Exception e) {
				log.error("Unable to check user's password for user ID: " + userId, e);
				return PasswordStatusCode.UNKNOWN_ERROR;
			}
		}

		log.info("valid password: [user ID = {}]", userId);
		return PasswordStatusCode.SUCCESS;
	}

	@Override
	@Transactional(readOnly = true)
	public boolean doesUsernameExist(String username) {
		DoeUsers user = doeUserRepository.getUserInfo(username);
		if (user != null) {
			log.info("user exists" + username);
			return true;
		}
		return false;
	}

	@Override
	@Transactional(readOnly = false)
	public PasswordStatusCode saveUserPassword(String rawPassword, String userid, Boolean validatePswd) {
		log.info("Updating password for user ID: {}", userid);
		// sanity check
		if (userid == null || rawPassword == null) {
			throw new IllegalArgumentException("User ID and password are required");
		}
		PasswordStatusCode status = null;
		if (Boolean.TRUE.equals(validatePswd)) {
			// validate password
			log.info("Validating password for user ID: {}", userid);
			status = validatePassword(rawPassword, userid);
		} else {
			status = PasswordStatusCode.SUCCESS;
		}
		if (PasswordStatusCode.SUCCESS == status) {
			DoeUsers user = doeUserRepository.getUserInfo(userid);

			String encodedPassword = passwordEncoder.encode(rawPassword);
			user.setPassword(encodedPassword);
			log.info("Attempt to unlock account for user ID: " + userid);
			// reset counter
			user.setLockoutCounter(0);
			user.setLastChangedDate(new Date());
			doeUserRepository.saveAndFlush(user);
		}

		return status;
	}

	@Override
	public DoeUsersModel getUserInfo(String emailAddr) {
		log.debug("get user for " + emailAddr);
		DoeUsers d = doeUserRepository.getUserInfo(emailAddr);
		if (d != null) {
			DoeUsersModel userModel = new DoeUsersModel();
			userModel.setEmailAddrr(d.getEmailAddrr().toLowerCase());
			userModel.setFirstName(d.getFirstName());
			userModel.setLastName(d.getLastName());
			userModel.setInstitution(d.getInstitution());
			userModel.setIsWrite(d.getIsWrite());
			List<String> progNamesList = userGroupRespitory.getProgramNames(d.getId());
			userModel.setProgramName(String.join(",", progNamesList));
			return userModel;
		}

		return null;
	}

	@Override
	@Transactional(readOnly = false)
	public void saveUserInfo(DoeUsersModel doeModel) {
		log.info("save user " + doeModel.getEmailAddrr());
		DoeUsers d = doeUserRepository.getUserInfo(doeModel.getEmailAddrr());
		if (d != null) {
			d.setFirstName(doeModel.getFirstName());
			d.setInstitution(doeModel.getInstitution());
			d.setLastName(doeModel.getLastName());
			d.setLastChangedDate(new Date());
			doeUserRepository.saveAndFlush(d);
		}

	}
}
