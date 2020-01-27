package gov.nih.nci.doe.web.controller;

import javax.servlet.http.HttpSession;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import gov.nih.nci.doe.web.model.DoeUsersModel;
import gov.nih.nci.doe.web.service.AuthenticateService;




/**
 *
 * DOE root Controller
 *
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/")
public class HomeController extends AbstractHpcController {


	 @Autowired
	 AuthenticateService authService;
	 
	@RequestMapping(method = RequestMethod.GET)
	public String index() {
		return "home";
		
	}
	
	  @ModelAttribute("loggedOnUser")
	    public String getLoggedOnUserInfo() {
		  Authentication auth = SecurityContextHolder.getContext().getAuthentication();
		  Boolean isAnonymousUSer = auth.getAuthorities().stream().filter(o -> o.getAuthority().equals("ROLE_ANONYMOUS")).findFirst().isPresent();
			if(auth != null && auth.isAuthenticated() && !isAnonymousUSer) {
				return auth.getName().trim();
			}
			return null;
	    }
	  
	    /**
	     * @param headers
	     * @return
	     */
	    @GetMapping(value = "user-info")
	    public ResponseEntity<?> getActiveReferralCaAssignRules(HttpSession session,@RequestHeader HttpHeaders headers,
	    		@RequestParam(value = "emailAddr") String emailAddr) {
	        log.info("getting user info");
	        try {
	        	DoeUsersModel user = authService.getUserInfo(emailAddr);
	            return new ResponseEntity<>(user, headers, HttpStatus.OK);
	        } catch (Exception e) {
	            log.error(e.getMessage(), e);
	           
	            return new ResponseEntity<>(null, headers, HttpStatus.SERVICE_UNAVAILABLE);
	        }
	    }
	    
	    @PostMapping(value = "user-info",consumes = {MediaType.APPLICATION_JSON_UTF8_VALUE})
	    public void updateUserInfo(@RequestBody DoeUsersModel doeModel) {
	        log.debug("update user info");
	        try {
	        	authService.saveUserInfo(doeModel);
	        } catch (Exception e) {
	            log.error(e.getMessage(), e);
	        }

	    }
}
