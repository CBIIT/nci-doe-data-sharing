package gov.nih.nci.doe.web.controller;

import javax.servlet.http.HttpSession;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;

import gov.nih.nci.doe.web.constants.PasswordStatusCode;
import gov.nih.nci.doe.web.domain.DoeUsers;
import gov.nih.nci.doe.web.model.DoeRegistration;

import javax.servlet.http.HttpServletRequest;


/**
 *
 * DOE register Controller
 *
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/register")
public class RegistrationController extends AbstractDoeController {

	@Value("${gov.nih.nci.hpc.server.register}")
	private String registerUrl;

    @Value("${gov.nih.nci.hpc.web.server}")
	private String webServerName;

    @PostMapping
	public ResponseEntity<?> register(HttpSession session,@RequestHeader HttpHeaders headers, 
			HttpServletRequest request, @RequestBody DoeRegistration register) throws Exception {
    	
		log.info("register user");
		
		//validate the password first
		PasswordStatusCode status = authService.validatePassword(register.getPassword(), null);
		 if(authService.doesUsernameExist(register.getEmailAddress().trim().toLowerCase())) {
			log.info("Email already found in the system...");
			return new ResponseEntity<>("Email address already exists.", HttpStatus.OK);
		} 
		else if(!status.equals(PasswordStatusCode.SUCCESS)) {
			log.info("Password validation failed...");
			return new ResponseEntity<>("Enter a valid password.", HttpStatus.OK);
		}  else {
			//register the user in the system			
			DoeUsers user = authService.register(register);
            try {
                //send an activation link after registration
                mailService.sendActivationEmail(webServerName,register.getEmailAddress(), user.getUuid());
                return new ResponseEntity<>("SUCCESS", HttpStatus.OK);
             } catch (Exception e) {
            log.error(e.getMessage());
           }
		}
        log.info("Ending of the method register");
        return new ResponseEntity<>("FAILURE", HttpStatus.OK);
		
	}
}
