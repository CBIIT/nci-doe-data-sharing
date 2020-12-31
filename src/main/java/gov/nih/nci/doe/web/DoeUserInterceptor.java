/**
 * DOEUserInterceptor.java
 *
 * Copyright SVG, Inc.
 * Copyright Leidos Biomedical Research, Inc
 * 
 * Distributed under the OSI-approved BSD 3-Clause License.
 * See https://github.com/CBIIT/HPC_DME_APIs/LICENSE.txt for details.
 */
package gov.nih.nci.doe.web;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

import gov.nih.nci.doe.web.constants.LoginStatusCode;
import gov.nih.nci.doe.web.service.AuthenticateService;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

/**
 * <p>
 * DOE User Interceptor
 * </p>
 */
@Component
public class DoeUserInterceptor extends HandlerInterceptorAdapter {
	private static final Logger log = LoggerFactory.getLogger(DoeUserInterceptor.class);

	@Value("${gov.nih.nci.hpc.server.user.authenticate}")
	private String authenticateURL;
	
	@Value("${doe.readonly.password}")
	private String readOnlyUserPassword;
	
	@Value("${doe.writeaccount.password}")
	private String writeAccessUserPassword;
	
	@Value("${doe.readonlyaccount.username}")
	private String readOnlyUserName;
	
	@Value("${doe.writeaccount.username}")
	private String writeAccessUserName;
	
	@Autowired
	public AuthenticateService authService;
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.springframework.web.servlet.handler.HandlerInterceptorAdapter#preHandle(
	 * javax.servlet.http.HttpServletRequest,
	 * javax.servlet.http.HttpServletResponse, java.lang.Object)
	 */
	@Override
	public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler)
			throws Exception {

		HttpSession session = request.getSession();
		String userToken = (String) session.getAttribute("hpcUserToken");		
		String writeAccessToken = (String) session.getAttribute("writeAccessUserToken");
		String requestUri = request.getRequestURI();		
		final String authorization = request.getHeader("Authorization");
		
		if (StringUtils.isNotEmpty(requestUri) && requestUri.contains("/v2") && authorization != null && 
			authorization.toLowerCase().startsWith("basic")) {
		    // Authorization: Basic base64credentials
		    String base64Credentials = authorization.substring("Basic".length()).trim();
		    byte[] credDecoded = Base64.getDecoder().decode(base64Credentials);
		    String credentials = new String(credDecoded, StandardCharsets.UTF_8);
		    log.info("credential" + credentials);
		    // credentials = username:password
		    final String[] values = credentials.split(":", 2);
		    String userName = values[0];
		    String password = values[1];
		    LoginStatusCode status = authService.authenticateExternalUser(userName, password);
		    if(status == LoginStatusCode.LOGIN_SUCCESS) {
		    	session.setAttribute("doeLogin", "true");
		    	String authToken = DoeClientUtil.getAuthenticationToken(writeAccessUserName, writeAccessUserPassword,authenticateURL);
				session.setAttribute("writeAccessUserToken", authToken);
		    }
		}
		 
		if(StringUtils.isBlank(writeAccessToken)) {
		 try {
		   Authentication auth = SecurityContextHolder.getContext().getAuthentication();
		   
			 if(auth != null && auth.isAuthenticated()) {
				 Boolean isAnonymousUser = auth.getAuthorities().stream().filter(o -> o.getAuthority().equals("ROLE_ANONYMOUS")).findFirst().isPresent();			
				 if(Boolean.FALSE.equals(isAnonymousUser)) {
				   String authToken = DoeClientUtil.getAuthenticationToken(writeAccessUserName, writeAccessUserPassword,authenticateURL);
				   session.setAttribute("writeAccessUserToken", authToken);
			     }
			 }
			} catch(Exception e) {
				log.error(e.getMessage(), e);
			}
		}
		
		if(StringUtils.isBlank(userToken)) {
		 try {
			String authToken = DoeClientUtil.getAuthenticationToken(readOnlyUserName, readOnlyUserPassword,authenticateURL);
			session.setAttribute("hpcUserToken", authToken);
            log.debug("authentication successfull");
		 } catch (Exception e) {
			 log.error(e.getMessage(), e);
		 }
		}
		return super.preHandle(request, response, handler);
	}

}
