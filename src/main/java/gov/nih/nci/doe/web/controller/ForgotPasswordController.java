package gov.nih.nci.doe.web.controller;

import javax.servlet.http.HttpSession;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;

import gov.nih.nci.doe.web.constants.PasswordStatusCode;
import gov.nih.nci.doe.web.model.ForgotPassword;
import gov.nih.nci.doe.web.service.AuthenticateService;

/**
 *
 * DOE root Controller
 *
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/forgotPassword")
public class ForgotPasswordController extends AbstractDoeController {

	 @Autowired
	 AuthenticateService authService;

	 @GetMapping
	public ResponseEntity<?> forgotPassword(HttpSession session,@RequestHeader HttpHeaders headers, ForgotPassword forgotPassword) throws Exception {
		log.info("forgot password");
		
		if(forgotPassword.getEmailAddrr() == null || StringUtils.isEmpty(forgotPassword.getEmailAddrr())) {
			log.error("User ID is required.");
			return new ResponseEntity<>("Email Address is required.", HttpStatus.OK);
			
		} else if(forgotPassword.getPassword() == null || StringUtils.isEmpty(forgotPassword.getPassword())) {
			log.info("Enter password.");
			return new ResponseEntity<>("Enter password.", HttpStatus.OK);
			
		} else if(forgotPassword.getConfirmPassword() == null || StringUtils.isEmpty(forgotPassword.getConfirmPassword())) {
			log.info("Enter the confirmation for password.");
			return new ResponseEntity<>("Enter the confirmation for password.", HttpStatus.OK);
			
		} else if(!authService.doesUsernameExist(forgotPassword.getEmailAddrr().trim().toLowerCase())) {
			log.info("Email not found to reset password");
			return new ResponseEntity<>("Email address not found.", HttpStatus.OK);
		} 

		log.info("About to set a new password for user ID {}", forgotPassword.getEmailAddrr());
		
		// validate the user's email address and password.
		PasswordStatusCode status = authService.saveUserPassword(forgotPassword.getPassword(), forgotPassword.getEmailAddrr(),true);
		
		if(PasswordStatusCode.SUCCESS == status) {
			
			log.info("successfully reset the password...");
			return new ResponseEntity<>("SUCCESS", HttpStatus.OK);
			
		} else {
			
			log.info("failed to reset applicant's password...");
			// failed to reset the password. set the error message
			if(PasswordStatusCode.INVALID_FORMAT == status || PasswordStatusCode.INVALID_LENGTH == status) { 
				return new ResponseEntity<>("Password is in invalid length or format", HttpStatus.OK);
			} if(PasswordStatusCode.NEW_PASSWD_SAME_AS_PREV_PASSWD == status) { 
				return new ResponseEntity<>("Password is same as current one. Enter a new password", HttpStatus.OK);
			}   else {
				return new ResponseEntity<>(status, HttpStatus.OK);
			}
		}		
	}
}
