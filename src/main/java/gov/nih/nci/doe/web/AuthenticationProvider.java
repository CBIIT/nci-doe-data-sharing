package gov.nih.nci.doe.web;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.stereotype.Component;

import gov.nih.nci.doe.web.constants.LoginStatusCode;
import gov.nih.nci.doe.web.service.AuthenticateService;

@Component
public class AuthenticationProvider implements org.springframework.security.authentication.AuthenticationProvider {

	@Autowired
	private AuthenticateService authService;

	protected final Logger log = LoggerFactory.getLogger(this.getClass());

	@Override
	public Authentication authenticate(final Authentication authentication) {
		final String username = authentication.getName().trim().toLowerCase();
		final String password = authentication.getCredentials().toString();

		String error = "loginFailure";
		try {
			LoginStatusCode status = authService.authenticateExternalUser(username, password);
			if (status == LoginStatusCode.LOGIN_SUCCESS) {
				log.info(username + ": is authenticated!");
				String storedP = authService.getPassword(username);
				List<SimpleGrantedAuthority> authorities = new ArrayList<>();
				// spring security requires to grant some authority role to the logged on user.
				authorities.add(new SimpleGrantedAuthority("PUBLIC_USER"));
				return new UsernamePasswordAuthenticationToken(username, storedP, authorities);
			} else if (status == LoginStatusCode.LOGIN_INVALID_EMAIL) {
				error = "inValidEmail";
			} else if (status == LoginStatusCode.LOGIN_INVALID_PASSWORD) {
				error = "inValidPassword";
			} else if (status == LoginStatusCode.LOGIN_LOCKED) {
				error = "loginlocked";
			} else if (status == LoginStatusCode.LOGIN_INACTIVATED) {
				error = "loginInactivated";
			}
		} catch (Exception ex) {
			log.error(ex.getMessage());
		}
		log.error(username + ": not authenticated!");
		throw new BadCredentialsException(error);
	}

	@Override
	public boolean supports(final Class<?> authentication) {
		return authentication.equals(UsernamePasswordAuthenticationToken.class);
	}
}
