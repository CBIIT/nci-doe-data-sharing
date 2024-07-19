package gov.nih.nci.doe.web.controller;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.LoginAuthenticationSuccessHandler;

@Controller
@EnableAutoConfiguration
@RequestMapping("/loginTab")
public class LoginController extends AbstractDoeController {

	@GetMapping
	public String getLoginTab(Model model, @RequestParam(value = "token", required = false) String token,
			@RequestParam(value = "email", required = false) String email,
			@RequestParam(value = "redirectMsg", required = false) Boolean redirectMsg,
			@RequestParam(value = "error", required = false) String error, HttpServletRequest request)
			throws DoeWebException {

		try {
			if (StringUtils.isNotEmpty(token) && StringUtils.isNotEmpty(email)) {
				String status = authenticateService.confirmRegistration(token, email);
				if ("SUCCESS".equalsIgnoreCase(status)) {
					model.addAttribute("successMsg", "Thank you for registering. You may now log in.");
				}
			} else if (redirectMsg != null) {
				model.addAttribute("redirectMsg", true);
			}

			if (null != error) {
				Exception message = (Exception) request.getSession().getAttribute("SPRING_SECURITY_LAST_EXCEPTION");
				if (StringUtils.isNotEmpty(error) && StringUtils.isAlphanumericSpace(error)) {
					/*
					 * The only use case when the error is not empty is during google captcha
					 * verification failed on login authentication success handler.
					 */
					model.addAttribute("error", error);
				} else if (message == null) {
					model.addAttribute("error",
							"Unknown error. Contact <a class='modacSupportLink' href='/contactUs'>MoDaC Support</a>.");
				} else if (message.getClass().isAssignableFrom(BadCredentialsException.class)) {
					model.addAttribute("error", message.getMessage());
				}
			}

			if (error == null) {
				// use the referer from request header to get the previous url.
				// the referer will be empty when the url is copied to the browser and the app
				// is redirected to login page. Also, do not use referer tag when there is an
				// error message on
				// login tab

				String referer = request.getHeader("referer");
				request.getSession().setAttribute(LoginAuthenticationSuccessHandler.REDIRECT_URL_SESSION_ATTRIBUTE_NAME,
						referer);
			}

		} catch (Exception e) {
			throw new DoeWebException("Failed to send registration email" + e.getMessage());
		}

		model.addAttribute("siteKey", siteKey);

		return "login/loginTab";
	}
}
