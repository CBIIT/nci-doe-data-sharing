package gov.nih.nci.doe.web;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.DefaultRedirectStrategy;
import org.springframework.security.web.RedirectStrategy;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;
import org.springframework.security.web.authentication.SimpleUrlAuthenticationSuccessHandler;
import org.springframework.security.web.authentication.logout.CookieClearingLogoutHandler;
import org.springframework.security.web.authentication.logout.SecurityContextLogoutHandler;
import org.springframework.security.web.authentication.rememberme.AbstractRememberMeServices;
import org.springframework.stereotype.Component;

import gov.nih.nci.doe.web.util.DoeClientUtil;

@Component
public class LoginAuthenticationSuccessHandler extends SimpleUrlAuthenticationSuccessHandler
		implements AuthenticationSuccessHandler {

	@Value("${google.captcha.secretkey}")
	public String secretKey;

	public static final String REDIRECT_URL_SESSION_ATTRIBUTE_NAME = "REDIRECT_URL";

	protected Logger log = LoggerFactory.getLogger(this.getClass());
	private RedirectStrategy redirectStrategy = new DefaultRedirectStrategy();

	public LoginAuthenticationSuccessHandler() {
		super();
		setUseReferer(true);
	}

	@Override
	public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response,
			Authentication authentication) throws IOException, ServletException {

		log.info("Authentication Success. verifying captcha");
		String responseKey = request.getParameter("g-recaptcha-response");
		String targetUrl = "";
		try {
			Boolean success = DoeClientUtil.getResponseFromGoogleCaptcha(secretKey, responseKey);
			if (!Boolean.TRUE.equals(success)) {
				log.info("captcha validation failed after login");
				targetUrl = "/loginTab?error=Captcha validation failed";

				// manually performing the log out since the captcha validation failed.
				CookieClearingLogoutHandler cookieClearingLogoutHandler = new CookieClearingLogoutHandler(
						AbstractRememberMeServices.SPRING_SECURITY_REMEMBER_ME_COOKIE_KEY);
				SecurityContextLogoutHandler securityContextLogoutHandler = new SecurityContextLogoutHandler();
				cookieClearingLogoutHandler.logout(request, response, null);
				securityContextLogoutHandler.logout(request, response, null);
				request.getSession().removeAttribute("writeAccessUserToken");

			} else {
				Object redirectURLObject = request.getSession().getAttribute(REDIRECT_URL_SESSION_ATTRIBUTE_NAME);
				targetUrl = redirectURLObject != null && !redirectURLObject.toString().contains("/loginTab")
						? redirectURLObject.toString()
						: "/";
				log.info("The previous Url is: " + targetUrl);
			}
			setDefaultTargetUrl(targetUrl);
			request.getSession().removeAttribute(REDIRECT_URL_SESSION_ATTRIBUTE_NAME);
			redirectStrategy.sendRedirect(request, response, targetUrl);

		} catch (Exception e) {
			log.error("Error in captcha validation for login form: " + e.getMessage());
			throw new BadCredentialsException(e.getMessage());
		}

	}

}
