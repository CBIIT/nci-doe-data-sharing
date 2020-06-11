package gov.nih.nci.doe.web.controller;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.context.request.WebRequest;

import gov.nih.nci.doe.web.domain.MetaDataPermissions;
import gov.nih.nci.doe.web.model.AuditingModel;
import gov.nih.nci.doe.web.model.DoeResponse;
import gov.nih.nci.doe.web.model.DoeUsersModel;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.model.PermissionsModel;
import gov.nih.nci.doe.web.service.AuditingService;
import gov.nih.nci.doe.web.service.AuthenticateService;
import gov.nih.nci.doe.web.service.LookUpService;
import gov.nih.nci.doe.web.service.MailService;
import gov.nih.nci.doe.web.service.MetaDataPermissionsService;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.doe.web.util.LambdaUtils;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionRegistrationDTO;

public abstract class AbstractDoeController {
	
	@Value("${gov.nih.nci.hpc.ssl.cert}")
	protected String sslCertPath;
	@Value("${gov.nih.nci.hpc.ssl.cert.password}")
	protected String sslCertPassword;
	
	@Autowired
	private AuthenticateService authenticateService;
	
	@Autowired
	MetaDataPermissionsService metaDataPermissionService;
	
    @Autowired
    public AuditingService auditingService;
    
	 @Autowired
	 LookUpService lookUpService;
    
	 @Autowired
	 MailService mailService;
	 
	@Value("${doe.basePath}")
	String basePath;
    
    @Value("${gov.nih.nci.hpc.server.collection}")
	private String serviceURL;

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
		  Boolean isAnonymousUSer = auth.getAuthorities().stream().anyMatch(o -> o.getAuthority().equals("ROLE_ANONYMOUS"));
			if(auth.isAuthenticated() && Boolean.FALSE.equals(isAnonymousUSer)) {
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
		
		 @GetMapping(value = "/metaDataPermissionsList")
		 public ResponseEntity<?>  getMetaDataPermissionsList()  { 
			 log.info("get meta data permissions list");
			 String loggedOnUser = getLoggedOnUserInfo();
			 List<KeyValueBean> keyValueBeanResults = new ArrayList<>();
			 if(!StringUtils.isEmpty(loggedOnUser)) {
				 DoeUsersModel user = authenticateService.getUserInfo(loggedOnUser);
				 if(user != null && !StringUtils.isEmpty(user.getProgramName())) {
					 List<String> progList = Arrays.asList(user.getProgramName().split(","));
					 progList.stream().forEach(e -> keyValueBeanResults.add(new KeyValueBean(e, e))); 
				 }
			 }
			 return new ResponseEntity<>(keyValueBeanResults, null, HttpStatus.OK);
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
		    	 metaDataPermissionService.savePermissionsList(loggedOnUser, String.join(",", newSelectedPermissionList), Integer.valueOf(collectionId));
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
		        metaDataPermissionService.savePermissionsList(loggedOnUser, String.join(",", addedPermissions), Integer.valueOf(collectionId));

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
			 log.info("permissionGroups" + permissionGroups.getDataLevelAccessGroups());
			 //notify users
			 MetaDataPermissions perm = metaDataPermissionService.getMetaDataPermissionsOwnerByCollectionId
					 (Integer.valueOf(permissionGroups.getStudyCollectionId()));
			 
			 mailService.sendNotifyUsersForAccessGroups(perm.getUserGroupId());

			 return "SUCCESS";
		 }
		 
		 
		 @GetMapping(value = "/updateAccessGroupMetaData")
		 public ResponseEntity<?> saveAccessGroup(HttpSession session,@RequestHeader HttpHeaders headers,
					 PermissionsModel permissionGroups)  { 
			 log.info("get meta data permissions list");
			 
			 //validate if the edit can be done
			 if(permissionGroups.getDataLevelAccessGroups().contains(permissionGroups.getProgramLevelAccessGroups())
					 && permissionGroups.getDataLevelAccessGroups().contains(permissionGroups.getStudyLevelAccessGroups())
					 && permissionGroups.getStudyLevelAccessGroups().contains(permissionGroups.getProgramLevelAccessGroups())) {
			 String loggedOnUser = getLoggedOnUserInfo();
			 String authToken = (String) session.getAttribute("writeAccessUserToken");
			 HpcCollectionRegistrationDTO dto = new HpcCollectionRegistrationDTO();
			 List<HpcMetadataEntry> metadataEntries = new ArrayList<>();
			 HpcMetadataEntry entry = new HpcMetadataEntry();
				entry.setAttribute("access_group");
				entry.setValue(permissionGroups.getSelectedAccessGroups());
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
		 
		 
	public String getAttributeValue(String attrName, List<HpcMetadataEntry> list,String levelName) {
				if (list == null)
					return null;
				
				HpcMetadataEntry entry =  list.stream().filter(e -> e.getAttribute().equalsIgnoreCase(attrName) && 
						levelName.equalsIgnoreCase(e.getLevelLabel())).
						findAny().orElse(null);
				if(entry !=null) {
					return entry.getValue();
				}
				return null;
	}
			
			
			
	public String getPermissionRole(String user,Integer collectionId,List<KeyValueBean> loggedOnUserPermissions) {

				if(!StringUtils.isEmpty(user)) {			
					List<String> loggedOnUserPermList = new ArrayList<String>();		
					loggedOnUserPermissions.stream().forEach(e -> loggedOnUserPermList.add(e.getKey()));
					
					if(!CollectionUtils.isEmpty(loggedOnUserPermList)) {
						List<MetaDataPermissions> permissionList =  metaDataPermissionService.getAllMetaDataPermissionsByCollectionId(collectionId);
						Boolean isOwner = permissionList.stream().anyMatch(o -> (user.equalsIgnoreCase(o.getUserGroupId()) && o.getIsOwner()));
						Boolean isGroupUser = permissionList.stream().anyMatch(o -> (loggedOnUserPermList.contains(o.getUserGroupId()) && o.getIsGroup()));
							if(Boolean.TRUE.equals(isOwner)) {
								return "Owner";						
							} else if(Boolean.TRUE.equals(isGroupUser)) {
								return "Group User";
							}
					}			
				}
				return "No Permissions";
	}

}