package gov.nih.nci.doe.web.controller;

import java.util.Collections;

import javax.servlet.http.HttpSession;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import gov.nih.nci.doe.web.AuthenticationProvider;
import gov.nih.nci.doe.web.constants.PasswordStatusCode;
import gov.nih.nci.doe.web.model.DoeRegistration;
import gov.nih.nci.doe.web.service.AuthenticateService;
import gov.nih.nci.doe.web.service.MailService;

import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.WebAuthenticationDetails;
import org.springframework.security.core.context.SecurityContextHolder;

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
	
    @Autowired
    AuthenticateService authService;
    
    @Autowired
    MailService mailService;
    
	@Autowired
	private AuthenticationProvider authProvider;

	@RequestMapping(method = RequestMethod.GET)
	public ResponseEntity<?> register(HttpSession session,@RequestHeader HttpHeaders headers, 
			HttpServletRequest request, DoeRegistration register) throws Exception {
    	
		log.info("register user");
		
		//validate the password first
		PasswordStatusCode status = authService.validatePassword(register.getPassword(), null);
		
		if(!status.equals(PasswordStatusCode.SUCCESS)) {
			log.info("Password validation failed...");
			return new ResponseEntity<>("Enter a valid password.", HttpStatus.OK);
		} else if(authService.doesUsernameExist(register.getEmailAddress().trim().toLowerCase())) {
			log.info("Email already found in the system...");
			return new ResponseEntity<>("Email address already exists.", HttpStatus.OK);
		} else {
			//register the user in the system			
			authService.register(register);
            try {
            	//after successful registration,log the user to the app
                authenticateUserAndSetSession(register.getEmailAddress(), register.getPassword(),request);
                
                //send a  confirmation email after register and successful login
                mailService.sendEmail(register.getEmailAddress());
             } catch (Exception e) {
            //e.printStackTrace();
            log.error(e.getMessage());
           }
		}
        log.info("Ending of the method register");
        return new ResponseEntity<>("SUCCESS", HttpStatus.OK);
		
	}
	
	private void authenticateUserAndSetSession(String email, String password, HttpServletRequest request) {
        UsernamePasswordAuthenticationToken usernamePasswordAuthenticationToken = new UsernamePasswordAuthenticationToken(email, password, Collections.emptyList());
        usernamePasswordAuthenticationToken.setDetails(new WebAuthenticationDetails(request));
        Authentication authenticatedUser = authProvider.authenticate(usernamePasswordAuthenticationToken);
        if (authenticatedUser != null && authenticatedUser.isAuthenticated()) {
            SecurityContextHolder.getContext().setAuthentication(authenticatedUser);
        }
        log.info("You are successfully registered");
    }
	


}
