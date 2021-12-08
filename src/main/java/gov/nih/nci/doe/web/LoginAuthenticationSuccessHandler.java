package gov.nih.nci.doe.web;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;
import org.springframework.stereotype.Component;

import gov.nih.nci.doe.web.util.DoeClientUtil;

@Component
public class LoginAuthenticationSuccessHandler implements AuthenticationSuccessHandler {

	@Value("${google.captcha.secretkey}")
	public String secretKey;

	protected final Logger log = LogManager.getLogger(this.getClass());

	@Override
	public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response,
			Authentication authentication) throws IOException, ServletException {

		log.info("Authentication Success. verifying captcha");
		String responseKey = request.getParameter("g-recaptcha-response");
		try {
			Boolean success = DoeClientUtil.getResponseFromGoogleCaptcha(secretKey, responseKey);
			if (!Boolean.TRUE.equals(success)) {
				log.info("captcha validation failed after login");
				throw new BadCredentialsException("Captcha validation failed.");
			}
		} catch (Exception e) {
			log.error("Error in captcha validation for login form: " + e.getMessage());
			throw new BadCredentialsException(e.getMessage());
		}

	}
}
