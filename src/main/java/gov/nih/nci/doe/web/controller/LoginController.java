package gov.nih.nci.doe.web.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;



/**
 *
 * DOE root Controller
 *
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/login")
public class LoginController extends AbstractHpcController {
	
	@RequestMapping(method = RequestMethod.GET)
	public ResponseEntity<?> loginPage(HttpServletRequest request, HttpServletResponse response, 
			@RequestParam(value = "error", required = false) String error) {
		log.info("Entering login page");
		if (null != error) {
			Exception message = (Exception)request.getSession().getAttribute("SPRING_SECURITY_LAST_EXCEPTION");
			if(message == null) {
				log.debug("empty");
				return new ResponseEntity<>("ERRORR", HttpStatus.OK);
			} else if(message.getClass().isAssignableFrom(BadCredentialsException.class)) {
				log.debug(message.getMessage());
				return new ResponseEntity<>(message.getMessage(), HttpStatus.OK);
			}
	    } 
		return new ResponseEntity<>("SUCCESS", HttpStatus.OK);
	}
}
