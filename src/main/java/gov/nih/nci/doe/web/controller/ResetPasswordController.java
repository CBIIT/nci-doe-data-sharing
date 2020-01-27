package gov.nih.nci.doe.web.controller;

import java.util.Arrays;
import java.util.List;

import javax.servlet.http.HttpSession;

import org.apache.commons.lang3.StringUtils;
import org.passay.CharacterRule;
import org.passay.EnglishCharacterData;
import org.passay.PasswordGenerator;
import org.passay.CharacterData;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import gov.nih.nci.doe.web.service.AuthenticateService;
import gov.nih.nci.doe.web.service.MailService;


/**
 *
 * DOE root Controller
 *
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/resetPasswordLink")
public class ResetPasswordController extends AbstractHpcController {

	 @Autowired
	 AuthenticateService authService;
	 
	 @Autowired
	 MailService mailService;
	    

	@RequestMapping(method = RequestMethod.GET)
	public ResponseEntity<?> forgotPassword(HttpSession session,@RequestHeader HttpHeaders headers,
			@RequestParam(value = "emailAddr") String emailAddr) throws Exception {
		log.info("resetting the password...");
		
		if(emailAddr == null || StringUtils.isEmpty(emailAddr)) {
			log.error("User ID is required.");
			return new ResponseEntity<>("Email Address is required.", HttpStatus.OK);
			
		} 

		log.info("About to send a reset link for user ID {}", emailAddr);
		/*List<CharacterRule> rules = Arrays.asList(new CharacterRule(EnglishCharacterData.UpperCase, 1),
				new CharacterRule(EnglishCharacterData.LowerCase, 5),
				new CharacterRule(EnglishCharacterData.Digit, 1),new CharacterRule(EnglishCharacterData.Special, 1));

		PasswordGenerator generator = new PasswordGenerator();
		String password = generator.generatePassword(8, rules);*/
	   
		String password = generatePassayPassword();
		authService.saveUserPassword(password,emailAddr);
		
		//send reset email link
		mailService.sendResetPasswordEmail(password,emailAddr);
		
		return new ResponseEntity<>("SUCCESS", HttpStatus.OK);		
	}
	
	public String generatePassayPassword() {
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
	    char[] special = new char[]{'!', '"', '#', '$', '%', '&', '\'', '(', ')', '*',
	             '+', '-', '.', '/', ':', '<', '=', '>', '?'};
	    
	    CharacterRule  splCharRule = new CharacterRule(new CharacterData() {
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
	    String password = gen.generatePassword(8, rules);
	    return password;
	}
}
