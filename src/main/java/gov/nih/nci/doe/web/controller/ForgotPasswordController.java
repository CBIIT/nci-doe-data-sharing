package gov.nih.nci.doe.web.controller;

import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.constants.PasswordStatusCode;
import gov.nih.nci.doe.web.model.ForgotPassword;

/**
 *
 * DOE Forgot Password Controller
 *
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/forgotPassword")
public class ForgotPasswordController extends AbstractDoeController {

	@PostMapping
	public ResponseEntity<?> forgotPassword(@RequestHeader HttpHeaders headers,
			@RequestBody ForgotPassword forgotPassword) throws Exception {
		log.info("forgot password");
		try {
			if (forgotPassword.getEmailAddrr() == null || StringUtils.isEmpty(forgotPassword.getEmailAddrr())) {
				log.error("User ID is required.");
				return new ResponseEntity<>("Enter an email address.", HttpStatus.OK);

			} else if (forgotPassword.getPassword() == null || StringUtils.isEmpty(forgotPassword.getPassword())) {
				log.info("Enter password.");
				return new ResponseEntity<>("Enter a password.", HttpStatus.OK);

			} else if (forgotPassword.getConfirmPassword() == null
					|| StringUtils.isEmpty(forgotPassword.getConfirmPassword())) {
				log.info("Repeat your password to confirm.");
				return new ResponseEntity<>("Repeat your password to confirm.", HttpStatus.OK);

			} else if (!authService.doesUsernameExist(forgotPassword.getEmailAddrr().trim().toLowerCase())) {
				log.info("Email not found to reset password");
				return new ResponseEntity<>("Enter a valid email address.", HttpStatus.OK);
			}

			log.info("About to set a new password for user ID {}", forgotPassword.getEmailAddrr());

			// validate the user's email address and password.
			PasswordStatusCode status = authService.saveUserPassword(forgotPassword.getPassword(),
					forgotPassword.getEmailAddrr(), true);

			if (PasswordStatusCode.SUCCESS == status) {

				log.info("successfully reset the password...");
				return new ResponseEntity<>("SUCCESS", HttpStatus.OK);

			} else {

				log.info("failed to reset applicant's password...");
				// failed to reset the password. set the error message
				if (PasswordStatusCode.INVALID_FORMAT == status || PasswordStatusCode.INVALID_LENGTH == status) {
					return new ResponseEntity<>("Password is in invalid length or format", HttpStatus.OK);
				}
				if (PasswordStatusCode.NEW_PASSWD_SAME_AS_PREV_PASSWD == status) {
					return new ResponseEntity<>(
							"Enter a password with valid length and format. Refer to Password Constraints.",
							HttpStatus.OK);
				} else {
					return new ResponseEntity<>(status, HttpStatus.OK);
				}
			}

		} catch (Exception e) {
			throw new DoeWebException("Failed to reset password: " + e.getMessage());
		}
	}
}
