package gov.nih.nci.doe.web.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.web.authentication.logout.CookieClearingLogoutHandler;
import org.springframework.security.web.authentication.logout.SecurityContextLogoutHandler;
import org.springframework.security.web.authentication.rememberme.AbstractRememberMeServices;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@EnableAutoConfiguration
@RequestMapping("/logOut")
public class LogOutController extends AbstractDoeController {

	@PostMapping
	public ResponseEntity<?> logOut(HttpSession session, HttpServletRequest request, HttpServletResponse response) {
		log.info("log out");
		doCommonLogout(request, response, session);
		return new ResponseEntity<>("SUCCESS", HttpStatus.OK);
	}

	public void doCommonLogout(HttpServletRequest request, HttpServletResponse response, HttpSession session) {
		CookieClearingLogoutHandler cookieClearingLogoutHandler = new CookieClearingLogoutHandler(
				AbstractRememberMeServices.SPRING_SECURITY_REMEMBER_ME_COOKIE_KEY);
		SecurityContextLogoutHandler securityContextLogoutHandler = new SecurityContextLogoutHandler();
		cookieClearingLogoutHandler.logout(request, response, null);
		securityContextLogoutHandler.logout(request, response, null);
		request.getSession().removeAttribute("writeAccessUserToken");
		// Invalidate the session to remove all attributes
		session = request.getSession(false);
		if (session != null) {
			session.invalidate();
		}
		log.info("User successfully logged out through hyperlink.");
	}
}
