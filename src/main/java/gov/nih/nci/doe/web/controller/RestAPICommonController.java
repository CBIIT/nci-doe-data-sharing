package gov.nih.nci.doe.web.controller;

import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.util.UriComponentsBuilder;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.MappingJsonFactory;
import org.springframework.http.MediaType;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.AuditingModel;
import gov.nih.nci.doe.web.model.DoeUsersModel;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.service.TaskManagerService;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionRegistrationDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectDownloadResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectRegistrationRequestDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDownloadRequestDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationResponseDTO;
import gov.nih.nci.hpc.dto.datasearch.HpcCompoundMetadataQueryDTO;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;
import javax.ws.rs.core.Response;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.cxf.jaxrs.client.WebClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;

@CrossOrigin
@RestController
@RequestMapping(value = "/api")
public class RestAPICommonController extends AbstractDoeController{
	
	 @Value("${gov.nih.nci.hpc.server.dataObject}")
	 private String dataObjectServiceURL;
	 
	 @Value("${gov.nih.nci.hpc.server.v2.dataObject}")
	 private String dataObjectAsyncServiceURL;
	 
	 @Value("${gov.nih.nci.hpc.server.collection}")
	 private String serviceURL;
	 
	 @Value("${gov.nih.nci.hpc.server.search.collection.compound}")
	 private String compoundCollectionSearchServiceURL;
	 
	 @Value("${gov.nih.nci.hpc.server.search.dataobject.compound}")
	 private String compoundDataObjectSearchServiceURL;
	 
	 @Value("${gov.nih.nci.hpc.server.v2.bulkregistration}")
	 private String bulkRegistrationURL;
	 
	 @Autowired
	TaskManagerService taskManagerService;
	 
	 @PostMapping(value="/dataObject/**/download")
	 public ResponseEntity<?> synchronousDownload(@RequestHeader HttpHeaders headers, 
			 HttpServletRequest request, HttpSession session,
			 HttpServletResponse response) {
		 
		 String path = request.getRequestURI().split(request.getContextPath() + "/dataObject/")[1];
		 Integer index=path.lastIndexOf('/');
		 path = path.substring(0,index);
		 log.info("download sync:");
	     log.info("Headers: {}", headers);
	     log.info("pathName: " +path);

	      try {
	          String authToken = (String) session.getAttribute("writeAccessUserToken");
	          log.info("authToken: " + authToken);
	          if (authToken == null) {
	            return null;
	          }
	          String doeLogin = (String) session.getAttribute("doeLogin");
	          log.info("doeLogin: " + doeLogin);
	          if (doeLogin == null) {
	            return null;
	          }
     			      	
	          final String serviceURL = UriComponentsBuilder.fromHttpUrl(
	            this.dataObjectServiceURL).path("/{dme-archive-path}/download")
	            .buildAndExpand(path).encode().toUri()
	            .toURL().toExternalForm();

	          final HpcDownloadRequestDTO dto = new HpcDownloadRequestDTO();
	          dto.setGenerateDownloadRequestURL(true);

	          WebClient client = DoeClientUtil.getWebClient(serviceURL, sslCertPath, sslCertPassword);
	          client.header("Authorization", "Bearer " + authToken);

	          Response restResponse = client.invoke("POST", dto);
	          log.info("rest response:" + restResponse.getStatus());
	          if (restResponse.getStatus() == 200) {
	                HpcDataObjectDownloadResponseDTO downloadDTO =
	                  (HpcDataObjectDownloadResponseDTO) DoeClientUtil.getObject(
	                  restResponse, HpcDataObjectDownloadResponseDTO.class);
	                downloadToUrl(downloadDTO.getDownloadRequestURL(), 1000000,"test", response);	          
	          }   
	      } catch (Exception e) {
	          log.error("error in download" + e);
	      }
	        return null;
	 }
	 
	 
	 @PostMapping(value="/v2/dataObject/**/download",consumes= {MediaType.APPLICATION_JSON_VALUE}, 
     produces= {MediaType.APPLICATION_JSON_VALUE,MediaType.APPLICATION_OCTET_STREAM_VALUE})
	 public String synchronousDownload(@RequestHeader HttpHeaders headers, HttpSession session,
			 HttpServletResponse response,HttpServletRequest request,
			@RequestBody @Valid gov.nih.nci.hpc.dto.datamanagement.v2.HpcDownloadRequestDTO downloadRequest) {

		 log.info("download async:");
	     log.info("Headers: {}", headers);
	     
	     String path = request.getRequestURI().split(request.getContextPath() + "/v2/dataObject/")[1];
		 Integer index=path.lastIndexOf('/');
		 path = path.substring(0,index);
		 
	     log.info("pathName: " +path);

	      try {
	          String authToken = (String) session.getAttribute("writeAccessUserToken");
	          log.info("authToken: " + authToken);
	          if (authToken == null) {
	            return null;
	          }
	          String doeLogin = (String) session.getAttribute("doeLogin");
	          log.info("doeLogin: " + doeLogin);
	          if (doeLogin == null) {
	            return null;
	          }

	          final String serviceURL = UriComponentsBuilder.fromHttpUrl(this.dataObjectAsyncServiceURL)
	        	        .path("/{dme-archive-path}/download").buildAndExpand(path).encode().toUri().toURL().toExternalForm();

	          WebClient client = DoeClientUtil.getWebClient(serviceURL, sslCertPath, sslCertPassword);
	          client.header("Authorization", "Bearer " + authToken);

	          Response restResponse = client.invoke("POST", downloadRequest);
	          log.info("rest response:" + restResponse.getStatus());
	          if (restResponse.getStatus() == 200) {
	        	  HpcDataObjectDownloadResponseDTO downloadDTO =
	        	          (HpcDataObjectDownloadResponseDTO) DoeClientUtil.getObject(restResponse,
	        	              HpcDataObjectDownloadResponseDTO.class);
	        	  String taskId = "Unknown";
	              if (downloadDTO != null)
	                taskId = downloadDTO.getTaskId();
	              return "taskId: " + taskId;
	          }    else {
	        	  return "error in download";
	          }
	      } catch (Exception e) {
	          log.error("error in download" + e);
	      }
	        return null;
	 }
	 
	 @GetMapping(value="/v2/dataObject/**", produces= {MediaType.APPLICATION_JSON_VALUE,MediaType.APPLICATION_OCTET_STREAM_VALUE})
	  public HpcDataObjectListDTO getDataObject(@RequestHeader HttpHeaders headers, HttpSession session,
				 HttpServletResponse response,HttpServletRequest request, @RequestParam("includeAcl") Boolean includeAcl) {

		 log.info("get dataobject:");
	     log.info("Headers: {}", headers);	     
	     String path = request.getRequestURI().split(request.getContextPath() + "/v2/dataObject/")[1];
	     log.info("pathName: " +path);

	      try {
	          String authToken = (String) session.getAttribute("writeAccessUserToken");
	          log.info("authToken: " + authToken);
	          
	          if (authToken == null) {
	            return null;
	          }

	          return DoeClientUtil.getDatafiles(authToken, dataObjectServiceURL, 
					  path, true, includeAcl,sslCertPath, sslCertPassword);
	        	 
	      } catch (Exception e) {
	          log.error("error in download" + e);
	      }
	        return null;
	 }
	 
	 @GetMapping(value="/collection/**", produces= {MediaType.APPLICATION_JSON_VALUE,MediaType.APPLICATION_OCTET_STREAM_VALUE})
	 public HpcCollectionListDTO getCollection(@RequestHeader HttpHeaders headers, HttpSession session,
			 HttpServletResponse response,HttpServletRequest request, @RequestParam("list") Boolean list) {
		 log.info("download async:");
	     log.info("Headers: {}", headers);
	     
	     String path = request.getRequestURI().split(request.getContextPath() + "/collection/")[1];		 
	     log.info("pathName: " +path);

	      try {
	          String authToken = (String) session.getAttribute("writeAccessUserToken");
	          log.info("authToken: " + authToken);
	          
	          if (authToken == null) {
	            return null;
	          }
	          
	          return  DoeClientUtil.getCollection(authToken, serviceURL, 
						path, list, sslCertPath,sslCertPassword);
	         
	        } catch (Exception e) {
               log.error("error in download" + e);
          }
       return null;
   }
	 
	    @PutMapping(value="/collection/**")
		public String registerCollection(@RequestHeader HttpHeaders headers, HttpSession session,
				 HttpServletResponse response,HttpServletRequest request,
				 @RequestBody @Valid HpcCollectionRegistrationDTO collectionRegistration) {
	    	log.info("register collection:");
		     log.info("Headers: {}", headers);
		     
		     String path = request.getRequestURI().split(request.getContextPath() + "/collection/")[1];		 
		     log.info("pathName: " +path);
		     
		      try {
		          String authToken = (String) session.getAttribute("writeAccessUserToken");
		          log.info("authToken: " + authToken);
		          
		          if (authToken == null) {
		            return null;
		          }
		          
		          String doeLogin = (String) session.getAttribute("doeLogin");
		          log.info("doeLogin: " + doeLogin);
		          if (doeLogin == null) {
		            return null;
		          }
		          if(StringUtils.isNotEmpty(doeLogin) && Boolean.TRUE.equals(isUploader(doeLogin))) {
		        	  String parentPath = null;
		        		if (path.lastIndexOf('/') != -1) {
							parentPath = path.substring(0, path.lastIndexOf('/'));
		        		} else {
							parentPath = path;
						}
						  if (!parentPath.isEmpty()) {
							 if(!parentPath.equalsIgnoreCase(basePath)) {
							      HpcCollectionListDTO parentCollectionDto = DoeClientUtil.getCollection(authToken, 
									serviceURL, parentPath, true, sslCertPath, sslCertPassword);
							      Boolean isValidPermissions = verifyCollectionPermissions(doeLogin,parentPath,parentCollectionDto);
							      if (Boolean.FALSE.equals(isValidPermissions)) {
								     return "Insufficient privileges to create collection";
							      }
						    }
						} else {				
								return "Invalid parent in Collection Path";
						}
						
							// Validate Collection path
							try {
								HpcCollectionListDTO collection = DoeClientUtil.getCollection(authToken, serviceURL,
                                                                  path, false, true, sslCertPath, sslCertPassword);
								if (collection != null && collection.getCollections() != null && CollectionUtils.isNotEmpty(collection.getCollections())) {
									 return "Collection already exists: " + path;
								}
							} catch (DoeWebException e) {
								log.debug("Error in validating collection path" + e.getMessage());	
							}
							try {
								boolean created = DoeClientUtil.updateCollection(authToken, serviceURL, collectionRegistration,
										path, sslCertPath, sslCertPassword);
								if (created) {	
									//after collection is created, store the permissions.
									String progList = request.getParameter("metaDataPermissionsList");
										log.info("selected permissions" + progList);
										HpcCollectionListDTO collections = DoeClientUtil.getCollection(authToken, serviceURL, 
												path, false, sslCertPath,sslCertPassword);
										if (collections != null && collections.getCollections() != null
												&& !CollectionUtils.isEmpty(collections.getCollections())) {
											HpcCollectionDTO collection = collections.getCollections().get(0);
											metaDataPermissionService.savePermissionsList(getLoggedOnUserInfo(),progList,collection.getCollection().getCollectionId(),path);
										}
									return  "Collection is created!";
								} 
							} catch (Exception e) {
								log.debug("Error in create collection" + e.getMessage());
							} 
		          }
		        } catch (Exception e) {
	               log.error("error in create collection" + e);
	          }
	       return null;
	 }
	    
	    @PutMapping(value="/v2/dataObject/**",consumes= {MediaType.MULTIPART_FORM_DATA_VALUE},produces= {MediaType.APPLICATION_XML_VALUE,MediaType.APPLICATION_JSON_VALUE})
		public String registerDataObject(@RequestHeader HttpHeaders headers, HttpSession session,
				 HttpServletResponse response,HttpServletRequest request, 
				 @RequestBody @Valid HpcDataObjectRegistrationRequestDTO dataObjectRegistration,
				 @RequestParam("doeDataFile") MultipartFile doeDataFile) {
	       	log.info("register data files:");
		     log.info("Headers: {}", headers);
		     
		     String path = request.getRequestURI().split(request.getContextPath() + "/collection/")[1];		 
		     log.info("pathName: " +path);
		     
		      try {
		          String authToken = (String) session.getAttribute("writeAccessUserToken");
		          log.info("authToken: " + authToken);
		          
		          if (authToken == null) {
		            return null;
		          }
		          
		          String doeLogin = (String) session.getAttribute("doeLogin");
		          log.info("doeLogin: " + doeLogin);
		          if (doeLogin == null) {
		            return null;
		          }
		        if(StringUtils.isNotEmpty(doeLogin) && Boolean.TRUE.equals(isUploader(doeLogin))) {
		      	    String parentPath = null;
					parentPath = path.substring(0, path.lastIndexOf('/'));
					
					if (!parentPath.isEmpty()) {
						HpcCollectionListDTO parentCollectionDto = DoeClientUtil.getCollection(authToken, serviceURL, parentPath, true, sslCertPath,sslCertPassword);
						Boolean isValidPermissions = verifyCollectionPermissions(doeLogin,parentPath,parentCollectionDto);
						if (Boolean.FALSE.equals(isValidPermissions)) {
								return "Insufficient privileges to add data files.";
							}
					   }

					boolean created =  DoeClientUtil.registerDatafile(authToken, doeDataFile, dataObjectServiceURL, dataObjectRegistration,
							path, sslCertPath, sslCertPassword);
					if(created) {
						
						//store the auditing info
		                AuditingModel audit = new AuditingModel();
		                audit.setName(doeLogin);
		                audit.setOperation("Upload Single File");
		                audit.setStartTime(new Date());
		                audit.setPath(path);
		                auditingService.saveAuditInfo(audit);
		                
						return "The system has registered your file.";
					}	    	        		          
		             }  
		      }     catch (Exception e) {
		               log.error("error in download" + e);
		          }
		      return "error in registration";
	     }
	    
	    @PutMapping(value="/v2/registration",consumes= {MediaType.MULTIPART_FORM_DATA_VALUE},produces= {MediaType.APPLICATION_XML_VALUE,MediaType.APPLICATION_JSON_VALUE})
		public String registerBulkDataObjects(@RequestHeader HttpHeaders headers, HttpSession session,
				 HttpServletResponse response,HttpServletRequest request,
				 @RequestBody @Valid gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO bulkDataObjectRegistrationRequest) {
	    	log.info("register data files:");
		     log.info("Headers: {}", headers);
		     
		     String path = request.getRequestURI().split(request.getContextPath() + "/collection/")[1];		 
		     log.info("pathName: " +path);
		     
		      try {
		          String authToken = (String) session.getAttribute("writeAccessUserToken");
		          log.info("authToken: " + authToken);
		          
		          if (authToken == null) {
		            return null;
		          }
		          
		          String doeLogin = (String) session.getAttribute("doeLogin");
		          log.info("doeLogin: " + doeLogin);
		          if (doeLogin == null) {
		            return null;
		          }
		        if(StringUtils.isNotEmpty(doeLogin) && Boolean.TRUE.equals(isUploader(doeLogin))) {
		      	    String parentPath = null;
					parentPath = path.substring(0, path.lastIndexOf('/'));
					
					if (!parentPath.isEmpty()) {
						HpcCollectionListDTO parentCollectionDto = DoeClientUtil.getCollection(authToken, serviceURL, parentPath, true, sslCertPath,sslCertPassword);
						Boolean isValidPermissions = verifyCollectionPermissions(doeLogin,parentPath,parentCollectionDto);
						if (Boolean.FALSE.equals(isValidPermissions)) {
								return "Insufficient privileges to add data files.";
							}
					   }

					HpcBulkDataObjectRegistrationResponseDTO responseDTO = DoeClientUtil.registerBulkDatafiles(authToken,
							bulkRegistrationURL, bulkDataObjectRegistrationRequest, sslCertPath, sslCertPassword);
					
					if (responseDTO != null) {				
						    String taskId = responseDTO.getTaskId();
						    String name = path.substring(path.lastIndexOf('/') + 1);
						    taskManagerService.saveTransfer(taskId,"Upload",null,name,doeLogin);
						    
						    //store the auditing info
			                  AuditingModel audit = new AuditingModel();
			                  audit.setName(doeLogin);
			                  audit.setOperation("Upload");
			                  audit.setStartTime(new Date());
			                  audit.setPath(path);
			                  audit.setTaskId(taskId);
			                  auditingService.saveAuditInfo(audit);
			                  
							return "Your bulk data file registration request has the following task ID: " +taskId;			
				
						
					}	    	        		          
		             }  
		      }     catch (Exception e) {
		               log.error("error in download" + e);
		          }
		      return "error in registration";
	    }
	    @PostMapping(value="/collection/query",consumes= {MediaType.APPLICATION_XML_VALUE,MediaType.APPLICATION_JSON_VALUE},
	    produces= {MediaType.APPLICATION_XML_VALUE,MediaType.APPLICATION_JSON_VALUE})
	    public HpcCollectionListDTO queryCollections(@RequestHeader HttpHeaders headers, HttpSession session,
				 HttpServletResponse response,HttpServletRequest request, HpcCompoundMetadataQueryDTO compoundMetadataQuery) {
	    	
	    	 String authToken = (String) session.getAttribute("writeAccessUserToken");
	          log.info("authToken: " + authToken);
	          
	          if (authToken == null) {
	            return null;
	          }
	          compoundMetadataQuery.setDetailedResponse(true);
	    	 UriComponentsBuilder ucBuilder  = UriComponentsBuilder.fromHttpUrl(compoundCollectionSearchServiceURL);		     		    
		    
		    if (ucBuilder == null) {
			      return null;
			}

		    ucBuilder.queryParam("returnParent", Boolean.TRUE);
		    String requestURL;
			try {
				requestURL = ucBuilder.build().encode().toUri().toURL().toExternalForm();
				 WebClient client = DoeClientUtil.getWebClient(requestURL, sslCertPath, sslCertPassword);
					client.header("Authorization", "Bearer " + authToken);	
					Response restResponse = client.invoke("POST", compoundMetadataQuery);
					if (restResponse.getStatus() == 200) {
						MappingJsonFactory factory = new MappingJsonFactory();
						JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
						HpcCollectionListDTO collections = parser.readValueAs(HpcCollectionListDTO.class);
						return collections;
					}
			} catch (Exception e) {
				log.error("error in download" + e);
			}

		   return null;
	    }
	    
	    @PostMapping(value="/dataObject/query",consumes= {MediaType.APPLICATION_XML_VALUE,MediaType.APPLICATION_JSON_VALUE},
	    	    produces= {MediaType.APPLICATION_XML_VALUE,MediaType.APPLICATION_JSON_VALUE})
	    public HpcDataObjectListDTO queryDataObjects(@RequestHeader HttpHeaders headers, HttpSession session,
				 HttpServletResponse response,HttpServletRequest request,@RequestParam("returnParent") Boolean returnParent,
	  		  HpcCompoundMetadataQueryDTO compoundMetadataQuery) {
	    	 String authToken = (String) session.getAttribute("writeAccessUserToken");
	          log.info("authToken: " + authToken);
	          
	          if (authToken == null) {
	            return null;
	          }
	          compoundMetadataQuery.setDetailedResponse(true);
	    	 UriComponentsBuilder ucBuilder  = UriComponentsBuilder.fromHttpUrl(compoundDataObjectSearchServiceURL);		     		    
		    
		    if (ucBuilder == null) {
			      return null;
			}

		    ucBuilder.queryParam("returnParent", Boolean.TRUE);
		    String requestURL;
			try {
				requestURL = ucBuilder.build().encode().toUri().toURL().toExternalForm();
				 WebClient client = DoeClientUtil.getWebClient(requestURL, sslCertPath, sslCertPassword);
					client.header("Authorization", "Bearer " + authToken);	
					Response restResponse = client.invoke("POST", compoundMetadataQuery);
					if (restResponse.getStatus() == 200) {
						MappingJsonFactory factory = new MappingJsonFactory();
						JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
						HpcDataObjectListDTO dataObjects = parser.readValueAs(HpcDataObjectListDTO.class);
						return dataObjects;
					}
			} catch (Exception e) {
				log.error("error in download" + e);
			}

		   return null;
	    }
	    
	    private Boolean verifyCollectionPermissions(String loggedOnUser, String parentPath,HpcCollectionListDTO parentCollectionDto) {

			 List<KeyValueBean> keyValueBeanResults = new ArrayList<>();
			 if(!StringUtils.isEmpty(loggedOnUser)) {
				 DoeUsersModel user = authenticateService.getUserInfo(loggedOnUser);
				 if(user != null && !StringUtils.isEmpty(user.getProgramName())) {
					 List<String> progList = Arrays.asList(user.getProgramName().split(","));
					 progList.stream().forEach(e -> keyValueBeanResults.add(new KeyValueBean(e, e))); 
				 }
			 }

			 if (!parentPath.equalsIgnoreCase(basePath) && parentCollectionDto != null && parentCollectionDto.getCollections() != null
						&& !CollectionUtils.isEmpty(parentCollectionDto.getCollections())) {
					HpcCollectionDTO collection = parentCollectionDto.getCollections().get(0);				
					String role = getPermissionRole(loggedOnUser,collection.getCollection().getCollectionId(),keyValueBeanResults);
					if(StringUtils.isNotEmpty(role) && (role.equalsIgnoreCase("Owner")|| role.equalsIgnoreCase("Group User"))) {
						return true;
					} 
			    }
				return false;
	    	
	    }
	    
	    private Boolean isUploader(String emailAddr) {
	    	 if(!StringUtils.isEmpty(emailAddr)) {
				  DoeUsersModel user =  authenticateService.getUserInfo(emailAddr);
				  if(user.getIsWrite() != null && user.getIsWrite()) {
					return true;  
				  }
			  }
	    	 
	    	 return false;
	    }
}
