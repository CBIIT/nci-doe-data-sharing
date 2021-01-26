package gov.nih.nci.doe.web.controller;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
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
import org.springframework.web.bind.annotation.ResponseBody;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.domain.MetaDataPermissions;
import gov.nih.nci.doe.web.model.AuditingModel;
import gov.nih.nci.doe.web.model.DoeUsersModel;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.model.PermissionsModel;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.doe.web.util.LambdaUtils;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionRegistrationDTO;




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

	  @Value("${gov.nih.nci.hpc.server.collection}")
		private String serviceURL;

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
				 String query = (String)session.getAttribute("searchQuery");
				 log.info("searchQuery search tab" + query);
				 model.addAttribute("searchQuery", query);
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
			 model.addAttribute("clientId", clientId);
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
		 
		 @GetMapping(value = "/metaDataPermissionsList")
		 public ResponseEntity<?>  getPermissionsList()  { 
			return getMetaDataPermissionsList();
		 }
		 
		 @PostMapping(value = "/metaDataPermissionsList")
		 @ResponseBody
		 public String savePermissionsList(HttpSession session,@RequestHeader HttpHeaders headers,
					@RequestParam(value = "collectionId") String collectionId,
					  @RequestParam(value = "selectedPermissions[]", required = false) String[] selectedPermissions)  { 
			 log.info("get meta data permissions list");
			 String loggedOnUser = getLoggedOnUserInfo();
			 
			 List<String> newSelectedPermissionList = selectedPermissions == null ? new ArrayList<String>() : Arrays.asList(selectedPermissions);
			 List<MetaDataPermissions> existingPermissionsList = metaDataPermissionService.getAllGroupMetaDataPermissionsByCollectionId(Integer.valueOf(collectionId));
		     List<String> oldPermissionsList = LambdaUtils.map(existingPermissionsList, MetaDataPermissions::getUserGroupId);
		            
		     if (CollectionUtils.isEmpty(oldPermissionsList) && !CollectionUtils.isEmpty(newSelectedPermissionList) && 
		    		 StringUtils.isNotBlank(collectionId)) {
		                // save the new set of permissions
		    	 metaDataPermissionService.savePermissionsList(loggedOnUser, String.join(",", newSelectedPermissionList), Integer.valueOf(collectionId),null);
		     } else {
		                List<String> deletedPermissions = new ArrayList<String>();
		                List<String> addedPermissions = new ArrayList<String>();

		                deletedPermissions = oldPermissionsList.stream()
		                    .filter(e -> !newSelectedPermissionList.contains(e)).filter(value -> value != null)
		                    .collect(Collectors.toList());

		                addedPermissions = newSelectedPermissionList.stream()
		                    .filter(e -> !oldPermissionsList.contains(e))
		                    .collect(Collectors.toList());  
				
				metaDataPermissionService.deletePermissionsList(loggedOnUser, deletedPermissions, Integer.valueOf(collectionId));  
		        metaDataPermissionService.savePermissionsList(loggedOnUser, String.join(",", addedPermissions), Integer.valueOf(collectionId),null);

		     }			 
			 return "SUCCESS";
		 }
		 
		 
		 @GetMapping(value = "/getPermissionByCollectionId")
		 public ResponseEntity<?>  getPermissionsByCollectionId(HttpSession session,@RequestHeader HttpHeaders headers,
					@RequestParam(value = "collectionId") String collectionId)  { 
			 log.info("get meta data permissions list by collection id");
			 List<KeyValueBean> keyValueBeanResults = new ArrayList<>();
			
			 List<MetaDataPermissions> permissionsList = metaDataPermissionService.getAllGroupMetaDataPermissionsByCollectionId(Integer.valueOf(collectionId));
				 if(CollectionUtils.isNotEmpty(permissionsList)) {
					 permissionsList.stream().forEach(e -> keyValueBeanResults.add(new KeyValueBean(e.getUserGroupId(), e.getUserGroupId()))); 
				 }
			 return new ResponseEntity<>(keyValueBeanResults, null, HttpStatus.OK);
		 }
		
		 @GetMapping(value = "/notifyUsers")
		 @ResponseBody
		 public String notifyUsersForUpdateAccessDicp(HttpSession session,@RequestHeader HttpHeaders headers, 
				 PermissionsModel permissionGroups) throws Exception  { 
			 log.info("notify users");
			 log.info("permissionGroups" + permissionGroups);
			 List<String> collectionOwnersList = new ArrayList<String>();
			 //notify users
			 MetaDataPermissions perm = null;
			 if("study".equalsIgnoreCase(permissionGroups.getSelectedCollection())) {
				  perm = metaDataPermissionService.getMetaDataPermissionsOwnerByCollectionId
						 (Integer.valueOf(permissionGroups.getProgCollectionId()));
				  collectionOwnersList.add(perm.getUserGroupId());
			 } else if("Asset".equalsIgnoreCase(permissionGroups.getSelectedCollection())) {
				 perm = metaDataPermissionService.getMetaDataPermissionsOwnerByCollectionId
						 (Integer.valueOf(permissionGroups.getStudyCollectionId()));
				 MetaDataPermissions progPermissions = metaDataPermissionService.getMetaDataPermissionsOwnerByCollectionId
						 (Integer.valueOf(permissionGroups.getProgCollectionId()));
				 collectionOwnersList.add(perm.getUserGroupId());
				 collectionOwnersList.add(progPermissions.getUserGroupId());
			 }
			 
			 
			 mailService.sendNotifyUsersForAccessGroups(collectionOwnersList);

			 return "SUCCESS";
		 }
		 
		 
		 @GetMapping(value = "/updateAccessGroupMetaData")
		 public ResponseEntity<?> saveAccessGroup(HttpSession session,@RequestHeader HttpHeaders headers,
					 PermissionsModel permissionGroups) throws DoeWebException { 
			 log.info("get meta data permissions list");
			 Boolean isUpdate = false;
			  if("program".equalsIgnoreCase(permissionGroups.getSelectedCollection())) {
				 isUpdate = true;
			 } else if("study".equalsIgnoreCase(permissionGroups.getSelectedCollection())) {
				 if("public".equalsIgnoreCase(permissionGroups.getProgramLevelAccessGroups())) {
					 isUpdate = true;
				 } else if(!"public".equalsIgnoreCase(permissionGroups.getProgramLevelAccessGroups()) && 
						 permissionGroups.getSelectedAccessGroups().contains(permissionGroups.getProgramLevelAccessGroups())) {
					 isUpdate = true;
				 }
			 } else if("Asset".equalsIgnoreCase(permissionGroups.getSelectedCollection())) {
				 if("public".equalsIgnoreCase(permissionGroups.getProgramLevelAccessGroups()) && 
					"public".equalsIgnoreCase(permissionGroups.getStudyLevelAccessGroups())) {
					 isUpdate = true;
				 } else if(permissionGroups.getSelectedAccessGroups().contains(permissionGroups.getStudyLevelAccessGroups())) {
					 isUpdate = true;
				 }
			 }
			  
			 if(Boolean.TRUE.equals(isUpdate)) {
			 String loggedOnUser = getLoggedOnUserInfo();
			 String authToken = (String) session.getAttribute("writeAccessUserToken");
			 HpcCollectionRegistrationDTO dto = new HpcCollectionRegistrationDTO();
			 List<HpcMetadataEntry> metadataEntries = new ArrayList<>();
			 HpcMetadataEntry entry = new HpcMetadataEntry();
				entry.setAttribute("access_group");
				if(permissionGroups.getSelectedAccessGroups().isEmpty()) {
					entry.setValue("public");
				} else {
					entry.setValue(permissionGroups.getSelectedAccessGroups());
				}
			
				metadataEntries.add(entry);
			 dto.getMetadataEntries().addAll(metadataEntries);
				boolean updated = DoeClientUtil.updateCollection(authToken, serviceURL, dto,
						permissionGroups.getPath(), sslCertPath, sslCertPassword);
				if (updated) {
					 //store the auditing info
	                  AuditingModel audit = new AuditingModel();
	                  audit.setName(loggedOnUser);
	                  audit.setOperation("Edit Meta Data");
	                  audit.setStartTime(new Date());
	                  audit.setPath(permissionGroups.getPath());
	                  auditingService.saveAuditInfo(audit);
				}
				
			 } else {
				 return new ResponseEntity<>("Permission group cannot be updated", HttpStatus.OK);
			 }
			 return new ResponseEntity<>("SUCCESS", HttpStatus.OK);
		 }
}
