package gov.nih.nci.doe.web.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.constants.PasswordStatusCode;
import gov.nih.nci.doe.web.model.ChangePassword;

/**
 *
 * DOE Forgot Password Controller
 *
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/changePassword")
public class ChangePasswordController extends AbstractDoeController {

	@GetMapping
	public String getResetPassword(HttpSession session, HttpServletRequest request) {
		String user = getLoggedOnUserInfo();
		if (StringUtils.isEmpty(user)) {
			return "redirect:/loginTab";
		}
		return "changePassword/changePasswordTab";
	}

	@PostMapping
	public ResponseEntity<?> changePassword(@RequestHeader HttpHeaders headers,
			@RequestBody ChangePassword changePassword) throws Exception {
		log.info("change password");
		try {
			String user = getLoggedOnUserInfo();
			if (StringUtils.isEmpty(user)) {
				return new ResponseEntity<>("loginTab", HttpStatus.OK);
			}

			if (changePassword.getPassword() == null || StringUtils.isEmpty(changePassword.getPassword())) {
				log.info("Enter password.");
				return new ResponseEntity<>("Enter a password.", HttpStatus.OK);

			} else if (changePassword.getConfirmPassword() == null
					|| StringUtils.isEmpty(changePassword.getConfirmPassword())) {
				log.info("Repeat your password to confirm.");
				return new ResponseEntity<>("Repeat your password to confirm.", HttpStatus.OK);

			} else if (!authService.doesUsernameExist(user)) {
				log.info("Email not found to reset password");
				return new ResponseEntity<>("Enter a valid email address.", HttpStatus.OK);
			}

			log.info("About to set a new password for user ID {}", user);

			// validate the user's email address and password.
			PasswordStatusCode status = authService.saveUserPassword(changePassword.getPassword(), user, true);

			if (PasswordStatusCode.SUCCESS == status) {

				log.info("successfully reset the password...");
				return new ResponseEntity<>("SUCCESS", HttpStatus.OK);

			} else {

				log.info("failed to reset applicant's password...");
				// failed to reset the password. set the error message
				if (PasswordStatusCode.INVALID_FORMAT == status || PasswordStatusCode.INVALID_LENGTH == status) {
					return new ResponseEntity<>(
							"Enter a password with valid length and format. Refer to Password Constraints.",
							HttpStatus.OK);
				}
				if (PasswordStatusCode.NEW_PASSWD_SAME_AS_PREV_PASSWD == status) {
					return new ResponseEntity<>(
							"Your new password is the same as your current one. Enter a new password.", HttpStatus.OK);
				} else {
					return new ResponseEntity<>(status, HttpStatus.OK);
				}
			}

		} catch (Exception e) {
			throw new DoeWebException("Failed to reset password: " + e.getMessage());
		}
	}
}
