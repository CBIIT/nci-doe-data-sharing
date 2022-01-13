package gov.nih.nci.doe.web.controller;

import java.util.Arrays;
import java.util.List;

import javax.servlet.http.HttpSession;

import org.apache.commons.lang3.StringUtils;
import org.passay.CharacterRule;
import org.passay.EnglishCharacterData;
import org.passay.PasswordGenerator;
import org.passay.CharacterData;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import gov.nih.nci.doe.web.DoeWebException;

/**
 *
 * DOE Reset Password Controller
 *
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/resetPasswordLink")
public class ResetPasswordController extends AbstractDoeController {

	@GetMapping
	public ResponseEntity<?> resetPassword(HttpSession session, @RequestHeader HttpHeaders headers,
			@RequestParam(value = "emailAddr") String emailAddr) throws DoeWebException {
		log.info("resetting the password for user " + emailAddr);

		if (emailAddr == null || StringUtils.isEmpty(emailAddr)) {
			log.error("User ID is required.");
			return new ResponseEntity<>("Enter an email address.", HttpStatus.OK);

		}
		String regex = "^[\\w-_\\.+]*[\\w-_\\.]\\@([\\w]+\\.)+[\\w]+[\\w]$";
		if (!emailAddr.matches(regex)) {
			log.error("Email address format is incorrect.");
			return new ResponseEntity<>("Enter a valid email address.", HttpStatus.OK);
		}

		try {
			if (!authService.doesUsernameExist(emailAddr.trim().toLowerCase())) {
				log.error("Email address does not exist.");
				return new ResponseEntity<>("Email address does not exist.", HttpStatus.OK);
			}

			log.info("About to send a reset link for user ID {}", emailAddr);
			// generate a random password for the user and store in db
			String password = generateTempPassword();
			authService.saveUserPassword(password, emailAddr.trim().toLowerCase(), false);

			// send reset email link
			mailService.sendResetPasswordEmail(password, emailAddr.trim().toLowerCase());

			return new ResponseEntity<>("SUCCESS", HttpStatus.OK);
		} catch (Exception e) {
			throw new DoeWebException("Failed to send new password: " + e.getMessage());
		}
	}

	private String generateTempPassword() {
		PasswordGenerator gen = new PasswordGenerator();
		EnglishCharacterData lowerCaseChars = EnglishCharacterData.LowerCase;
		CharacterRule lowerCaseRule = new CharacterRule(lowerCaseChars);
		lowerCaseRule.setNumberOfCharacters(5);

		EnglishCharacterData upperCaseChars = EnglishCharacterData.UpperCase;
		CharacterRule upperCaseRule = new CharacterRule(upperCaseChars);
		upperCaseRule.setNumberOfCharacters(1);

		EnglishCharacterData digitChars = EnglishCharacterData.Digit;
		CharacterRule digitRule = new CharacterRule(digitChars);
		digitRule.setNumberOfCharacters(1);
		char[] special = new char[] { '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', '-', '.', '/', ':', '<',
				'=', '>', '?' };

		CharacterRule splCharRule = new CharacterRule(new CharacterData() {
			@Override
			public String getErrorCode() {
				return "INSUFFICIENT_SPECIAL";
			}

			@Override
			public String getCharacters() {
				return new String(special);
			}
		});

		splCharRule.setNumberOfCharacters(1);

		List<CharacterRule> rules = Arrays.asList(splCharRule, lowerCaseRule, upperCaseRule, digitRule);
		return gen.generatePassword(8, rules);
	}
}
