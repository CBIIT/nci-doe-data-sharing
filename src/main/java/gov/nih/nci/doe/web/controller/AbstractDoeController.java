package gov.nih.nci.doe.web.controller;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.context.request.WebRequest;

import gov.nih.nci.doe.web.domain.DoeUsers;
import gov.nih.nci.doe.web.model.DoeResponse;
import gov.nih.nci.doe.web.model.DoeUsersModel;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.repository.DoeUserRepository;
import gov.nih.nci.doe.web.service.AuthenticateService;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;

public abstract class AbstractDoeController {
	
	@Value("${gov.nih.nci.hpc.ssl.cert}")
	protected String sslCertPath;
	@Value("${gov.nih.nci.hpc.ssl.cert.password}")
	protected String sslCertPassword;
	
	@Autowired
	private AuthenticateService authenticateService;

	protected Logger log = LoggerFactory.getLogger(this.getClass());

	@ExceptionHandler({ Exception.class, java.net.ConnectException.class })
	public @ResponseBody DoeResponse handleUncaughtException(Exception ex, WebRequest request,
			HttpServletResponse response) {
		log.info("Converting Uncaught exception to RestResponse : " + ex.getMessage());

		response.setHeader("Content-Type", "application/json");
		response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		return new DoeResponse("Error occurred", ex.toString());
	}

	@ExceptionHandler(IllegalArgumentException.class)
	public @ResponseBody DoeResponse handleIllegalArgumentException(IllegalArgumentException ex, WebRequest request,
			HttpServletResponse response) {
		log.info("Converting IllegalArgumentException to RestResponse : " + ex.getMessage());

		response.setHeader("Content-Type", "application/json");
		response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
		return new DoeResponse("Error occurred", ex.toString());
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
	  
	  @ModelAttribute("isUploader")
	    public Boolean getIsUploader() {
		  String emailAddr = getLoggedOnUserInfo();
		  if(!StringUtils.isEmpty(emailAddr)) {
			  DoeUsersModel user =  authenticateService.getUserInfo(emailAddr);
			  if(user.getIsWrite() != null && user.getIsWrite()) {
				return true;  
			  }
		  }
		  
	      return false;
	    }
	  
		public List<KeyValueBean> getUserMetadata(List<HpcMetadataEntry> list,String levelName, List<String> systemAttrs) {
			if (list == null)
				return null;

			List<KeyValueBean> entryList = new ArrayList<KeyValueBean>();
			
			for (HpcMetadataEntry entry : list) {
				if (systemAttrs != null && !systemAttrs.contains(entry.getAttribute()) && levelName.equalsIgnoreCase(entry.getLevelLabel())) {
					KeyValueBean k = new KeyValueBean(entry.getAttribute(), entry.getValue());			
					entryList.add(k);
				}
				
			}

			return entryList;
		}
}