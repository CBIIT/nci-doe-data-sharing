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
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

import gov.nih.nci.doe.web.util.DoeClientUtil;

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
	
	@Value("${doe.ncidoesvct1.password}")
	private String readOnlyUserPassword;
	
	@Value("${doe.ncidoesvct1.password}")
	private String writeAccessUserPassword;
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
		
		if(StringUtils.isBlank(userToken)) {
		 try {
			String authToken = DoeClientUtil.getAuthenticationToken("gantam2", "#",authenticateURL);
			session.setAttribute("hpcUserToken", authToken);
            log.debug("authentication successfull");
		 } catch (Exception e) {
			e.printStackTrace();
		 }
		}
		return super.preHandle(request, response, handler);
	}

}
