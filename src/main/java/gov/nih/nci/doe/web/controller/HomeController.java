package gov.nih.nci.doe.web.controller;


import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import gov.nih.nci.doe.web.model.DoeUsersModel;




/**
 *
 * DOE root Controller
 *
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/")
public class HomeController extends AbstractDoeController {


	@GetMapping
	public String index(@RequestParam(value = "token",required = false) String token,
			@RequestParam(value = "email",required = false) String email) throws Exception {

		log.info("home page");
		if(StringUtils.isNotEmpty(token) && StringUtils.isNotEmpty(email)) {
			String status = authenticateService.confirmRegistration(token,email);
			if("SUCCESS".equalsIgnoreCase(status)) {
			  mailService.sendRegistrationEmail(email);
			}
		}
		return "home";
		
	}
		  
	    /**
	     * @param headers
	     * @return
	     */
	    @GetMapping(value = "user-info")
	    public ResponseEntity<?> getUserInfo(HttpSession session,@RequestHeader HttpHeaders headers,
	    		@RequestParam(value = "emailAddr") String emailAddr) {
	        log.info("getting user info with email address " +emailAddr);
	        try {
	        	DoeUsersModel user = authService.getUserInfo(emailAddr);
	            return new ResponseEntity<>(user, headers, HttpStatus.OK);
	        } catch (Exception e) {
	            log.error(e.getMessage(), e);	           
	            return new ResponseEntity<>(null, headers, HttpStatus.SERVICE_UNAVAILABLE);
	        }
	    }
	    

		@PostMapping(value = "user-info")
	    public ResponseEntity<?> updateUserInfo(@RequestBody DoeUsersModel doeModel,
	    		@RequestHeader HttpHeaders headers) {
	        log.info("update user info for user " + doeModel.getEmailAddrr());
	        try {
	        	if(doeModel.getEmailAddrr() != null) {
	        		authService.saveUserInfo(doeModel);
	        	}
	        	return new ResponseEntity<>("SUCCESS", HttpStatus.OK);
	        } catch (Exception e) {
	            log.error(e.getMessage(), e);
	            return new ResponseEntity<>(e.getMessage(), HttpStatus.SERVICE_UNAVAILABLE);
	        }

	    }
		
		
		 @GetMapping(value = "/searchTab")
		 public String getSearchTab(Model model,HttpSession session, HttpServletRequest request, 
				 @RequestParam(value = "dme_data_id", required = false) String dmeDataId,
				 @RequestParam(value = "doi", required = false) String doi,
				 @RequestParam(value = "returnToSearch", required = false) String returnToSearch)  { 	
			 
			 if(StringUtils.isNotEmpty(dmeDataId)) {
				 model.addAttribute("dmeDataId", dmeDataId);
			 }
			 
			 if(StringUtils.isNotEmpty(doi)) {
				 model.addAttribute("doi", doi);
			 }
			 
			 if(StringUtils.isNotEmpty(returnToSearch)) {
				 model.addAttribute("returnToSearch", "true");
			 }
			 
			return "searchTab";
		 }
		 
		 @GetMapping(value = "/tasksTab")
		 public String getTasksTab(HttpSession session, HttpServletRequest request)  { 			 
			return "tasksTab";
		 }
		 
		 
		 @GetMapping(value = "/loginTab")
		 public String getLoginTab(HttpSession session, HttpServletRequest request)  { 			 
			return "loginTab";
		 }
		 
		 @GetMapping(value = "/myaccount")
		 public String getMyAccount(HttpSession session, HttpServletRequest request)  { 			 
			return "myAccount";
		 }
		 
		 
		 @GetMapping(value = "/resetPassword")
		 public String getResetPassword(HttpSession session, HttpServletRequest request)  { 			 
			return "resetPassword";
		 }
		 
		 @GetMapping(value = "/aboutTab")
		 public String getAboutTab(HttpSession session, HttpServletRequest request)  { 			 
			return "aboutTab";
		 }
		 @GetMapping(value = "/downloadTab")
		 public String getDownload(Model model, HttpSession session, HttpServletRequest request,
				 @RequestParam(value = "selectedPaths",required=false) String selectedPaths,
				 @RequestParam(value = "code",required=false) String code,
				 @RequestParam(value = "downloadAsyncType",required=false) String downloadAsyncType,
				 @RequestParam(value = "fileName",required=false) String fileName)  { 
			 
			 model.addAttribute("selectedPathsString", selectedPaths);
			 model.addAttribute("downloadAsyncType", downloadAsyncType);
			 model.addAttribute("fileName", fileName);
			 
			 if(StringUtils.isNotEmpty(selectedPaths)) {
		      session.setAttribute("selectedPathsString", selectedPaths);
			 }
			 
			 
			 if(StringUtils.isNotEmpty(downloadAsyncType)) {
			      session.setAttribute("downloadAsyncType", downloadAsyncType);
			}
			 
			 if(StringUtils.isNotEmpty(fileName)) {
			      session.setAttribute("fileName", fileName);
			 }
			 
		        if (code != null) {
		        	log.info("return Authorization code from google for download tab" + code);
		        	 code = request.getParameter("code");
		        	 if(code != null) {
		            //Return from Google Drive Authorization
		            String downloadType = (String)session.getAttribute("downloadType");
		            selectedPaths = (String)session.getAttribute("selectedPathsString");
		            downloadAsyncType = (String)session.getAttribute("downloadAsyncType");
		            fileName = (String)session.getAttribute("fileName");
		            final String returnURL = this.webServerName + "/downloadTab";
		            try {
		              String accessToken = doeAuthorizationService.getToken(code, returnURL);
		              log.info("access token for download tab" + accessToken);
		              session.setAttribute("accessToken", accessToken);
		              model.addAttribute("accessToken", accessToken);
		            } catch (Exception e) {
		              model.addAttribute("error", "Failed to redirect to Google for authorization: " + e.getMessage());
		              e.printStackTrace();
		            }
		            model.addAttribute("asyncSearchType", "drive");
		            model.addAttribute("transferType", "drive");
		            model.addAttribute("authorized", "true");
		            model.addAttribute("selectedPathsString", selectedPaths);
		            model.addAttribute("downloadAsyncType", downloadAsyncType);
					model.addAttribute("fileName", fileName);
		          }
		        }
			return "downloadTab";
		 }
}
