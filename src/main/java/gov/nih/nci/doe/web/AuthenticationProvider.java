package gov.nih.nci.doe.web;

import java.util.ArrayList;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.stereotype.Component;

import gov.nih.nci.doe.web.constants.LoginStatusCode;
import gov.nih.nci.doe.web.service.AuthenticateService;




@Component
public class AuthenticationProvider implements org.springframework.security.authentication.AuthenticationProvider {
	
	@Autowired
	private AuthenticateService authService;
	
	
	
	protected final Logger log = LogManager.getLogger(this.getClass());

	@Override
    public Authentication authenticate(final Authentication authentication) throws AuthenticationException {
        final String username = authentication.getName().trim().toLowerCase();
        final String password = authentication.getCredentials().toString();
        LoginStatusCode status = authService.authenticateExternalUser(username, password);
        String error = "loginFailure";
        try{
        	if(status == LoginStatusCode.LOGIN_SUCCESS) {
            	log.info(username  + ": is authenticated!");
            	String storedP = authService.getPassword(username);
            	List<SimpleGrantedAuthority> authorities = new ArrayList<>();
            	//spring security requires to grant some authority role to the logged on user.
            	authorities.add(new SimpleGrantedAuthority("PUBLIC_USER"));
                final Authentication auth = new UsernamePasswordAuthenticationToken(username, storedP,authorities);
                return auth;
            } else if(status == LoginStatusCode.LOGIN_INVALID_EMAIL) {
            	error = "inValidEmail";
            } else if(status == LoginStatusCode.LOGIN_INVALID_PASSWORD) {
            	error = "inValidPassword";
            } else if(status == LoginStatusCode.LOGIN_LOCKED) {
            	error = "loginlocked";
            } else if(status == LoginStatusCode.LOGIN_INACTIVATED) {
            	error = "loginInactivated";
            }
        } catch(Exception ex) {
        	ex.printStackTrace();
        }
        log.error(username + ": not authenticated!");
    	throw new BadCredentialsException(error);
    }

    @Override
    public boolean supports(final Class<?> authentication) {
        return authentication.equals(UsernamePasswordAuthenticationToken.class);
    }
}
