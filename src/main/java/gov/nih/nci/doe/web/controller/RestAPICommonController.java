package gov.nih.nci.doe.web.controller;

import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
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
import gov.nih.nci.hpc.domain.metadata.HpcCompoundMetadataQuery;
import gov.nih.nci.hpc.domain.metadata.HpcCompoundMetadataQueryOperator;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataQuery;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataQueryLevelFilter;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataQueryOperator;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionRegistrationDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectDownloadResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDownloadRequestDTO;
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
import org.springframework.http.HttpStatus;
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
	 public ResponseEntity<?> asynchronousDownload(@RequestHeader HttpHeaders headers, 
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

	          Boolean isPermissions = false;
	          String parentPath = null;
      		if (path.lastIndexOf('/') != -1) {
					parentPath = path.substring(0, path.lastIndexOf('/'));
      		}
	          HpcCollectionListDTO collectionDto = DoeClientUtil.getCollection(authToken, 
						serviceURL, parentPath, true, sslCertPath, sslCertPassword);
	          HpcCollectionDTO result = collectionDto.getCollections().get(0);
	          String accessGrp = getAttributeValue("access_group", result.getMetadataEntries().getSelfMetadataEntries());
	          
	          if(StringUtils.isNotEmpty(accessGrp) && "public".equalsIgnoreCase(accessGrp)) {
	        	  isPermissions = true;
	          } else if(StringUtils.isNotEmpty(accessGrp) && !"public".equalsIgnoreCase(accessGrp) 
	        		  && Boolean.TRUE.equals(verifyCollectionPermissions(doeLogin,path,collectionDto))){
	        		   isPermissions = true;   
	             }
	          if(Boolean.TRUE.equals(isPermissions)) {   	
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
	          }
	          return new ResponseEntity<>("Error in download", HttpStatus.OK);
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

	          Boolean isPermissions = false;
	          HpcCollectionListDTO collectionDto = DoeClientUtil.getCollection(authToken, 
						serviceURL, path, true, sslCertPath, sslCertPassword);
	          HpcCollectionDTO result = collectionDto.getCollections().get(0);
	          String accessGrp = getAttributeValue("access_group", result.getMetadataEntries().getSelfMetadataEntries());
	          
	          if(StringUtils.isNotEmpty(accessGrp) && "public".equalsIgnoreCase(accessGrp)) {
	        	  isPermissions = true;
	          } else if(StringUtils.isNotEmpty(accessGrp) && !"public".equalsIgnoreCase(accessGrp) 
	        		  && Boolean.TRUE.equals(verifyCollectionPermissions(doeLogin,path,collectionDto))){
	        		   isPermissions = true;   
	             }
	          if(Boolean.TRUE.equals(isPermissions)) {

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
	          }
	          return "error in download";
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
	          Boolean isPermissions = false;
	          String doeLogin = (String) session.getAttribute("doeLogin");
	          log.info("doeLogin: " + doeLogin);
	          HpcCollectionListDTO collectionDto = DoeClientUtil.getCollection(authToken, 
						serviceURL, path, true, sslCertPath, sslCertPassword);
	          HpcCollectionDTO result = collectionDto.getCollections().get(0);
	          String accessGrp = getAttributeValue("access_group", result.getMetadataEntries().getSelfMetadataEntries());
	          
	          if(StringUtils.isNotEmpty(accessGrp) && "public".equalsIgnoreCase(accessGrp)) {
	        	  isPermissions = true;
	          } else if(StringUtils.isNotEmpty(accessGrp) && !"public".equalsIgnoreCase(accessGrp) && 
	        		  StringUtils.isNotEmpty(doeLogin) && Boolean.TRUE.equals(verifyCollectionPermissions(doeLogin,path,collectionDto))){
	        		   isPermissions = true;   
	             }
	          if(Boolean.TRUE.equals(isPermissions)) {
	            return DoeClientUtil.getDatafiles(authToken, dataObjectServiceURL, 
					  path, true, includeAcl,sslCertPath, sslCertPassword);
	          } 
	          
	          return null;
	        	 
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
	          
	          Boolean isPermissions = false;
	          String doeLogin = (String) session.getAttribute("doeLogin");
	          log.info("doeLogin: " + doeLogin);
	          HpcCollectionListDTO collectionDto = DoeClientUtil.getCollection(authToken, serviceURL,path, list, sslCertPath,sslCertPassword);
	          
	          
	          HpcCollectionDTO result = collectionDto.getCollections().get(0);
	          String accessGrp = getAttributeValue("access_group", result.getMetadataEntries().getSelfMetadataEntries());
	          
	          if(StringUtils.isNotEmpty(accessGrp) && "public".equalsIgnoreCase(accessGrp)) {
	        	  isPermissions = true;
	          } else if(StringUtils.isNotEmpty(accessGrp) && !"public".equalsIgnoreCase(accessGrp) && 
	        		  StringUtils.isNotEmpty(doeLogin) && Boolean.TRUE.equals(verifyCollectionPermissions(doeLogin,path,collectionDto))){
	        		   isPermissions = true;   
	             }
	          if(Boolean.TRUE.equals(isPermissions)) {
	          
	        	  return collectionDto;
	          } 
	            return null;
	          
	         
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
											metaDataPermissionService.savePermissionsList(doeLogin,progList,collection.getCollection().getCollectionId(),path);
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
	    
	    @PutMapping(value="/v2/dataObject/**")
		public String registerDataObject(@RequestHeader HttpHeaders headers, HttpSession session,
				 HttpServletResponse response,HttpServletRequest request, 
				 @RequestPart("dataObjectRegistration") @Valid gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationRequestDTO dataObjectRegistration,
				 @RequestBody(required=false) @Valid MultipartFile doeDataFile) {
	       	log.info("register data files:");
		     log.info("Headers: {}", headers);
		     
		     String path = request.getRequestURI().split(request.getContextPath() + "/v2/dataObject/")[1];		 
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

			            
					boolean created =  DoeClientUtil.registerDatafile(authToken, doeDataFile, dataObjectAsyncServiceURL, dataObjectRegistration,
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
	    
	   /* @PutMapping(value="/v2/registration",consumes= {MediaType.MULTIPART_FORM_DATA_VALUE},produces= {MediaType.APPLICATION_XML_VALUE,MediaType.APPLICATION_JSON_VALUE})
		public String registerBulkDataObjects(@RequestHeader HttpHeaders headers, HttpSession session,
				 HttpServletResponse response,HttpServletRequest request,
				 @RequestBody @Valid gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO bulkDataObjectRegistrationRequest) {
	    	log.info("register data files:");
		     log.info("Headers: {}", headers);
		     
		     String path = request.getRequestURI().split(request.getContextPath() + "/v2/registration/")[1];		 
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
	    }*/
	    
	    @PostMapping(value="/collection/query",consumes= {MediaType.APPLICATION_XML_VALUE,MediaType.APPLICATION_JSON_VALUE},
	    	    produces= {MediaType.APPLICATION_XML_VALUE,MediaType.APPLICATION_JSON_VALUE})
	    public HpcCollectionListDTO queryCollections(@RequestHeader HttpHeaders headers, HttpSession session,
				 HttpServletResponse response,HttpServletRequest request, 
				 @RequestBody @Valid HpcCompoundMetadataQueryDTO compoundMetadataQuery) {
	    	
	    	String authToken = (String) session.getAttribute("hpcUserToken");
	    	if(StringUtils.isEmpty(authToken)) {
	    	authToken = (String) session.getAttribute("writeAccessUserToken");
	    	}
	    	
	          log.info("authToken: " + authToken);
	          
	          if (authToken == null) {
	            return null;
	          }

	          String doeLogin = (String) session.getAttribute("doeLogin");
	          log.info("doeLogin: " + doeLogin);
	          
	         
	        	 HpcCompoundMetadataQuery query = compoundMetadataQuery.getCompoundQuery();
	        	// add criteria for access group public and other prog names for logged on user.
	       	  List<KeyValueBean> loggedOnUserPermissions = new ArrayList<>();
	       	  if(StringUtils.isNotEmpty(doeLogin)) {
	       	    DoeUsersModel user = authenticateService.getUserInfo(doeLogin);
			       if(user != null && !StringUtils.isEmpty(user.getProgramName())) {
				        List<String> progList = Arrays.asList(user.getProgramName().split(","));
				        progList.stream().forEach(e -> loggedOnUserPermissions.add(new KeyValueBean(e, e))); 
			        }
	       	    }
			 
	       		HpcCompoundMetadataQuery query1 = new HpcCompoundMetadataQuery();
	       		query1.setOperator(HpcCompoundMetadataQueryOperator.OR);
	       		List<HpcMetadataQuery> queries1 = new ArrayList<HpcMetadataQuery>();
	       		
	       		// perform OR operation of public access and logged on users access groups 
	       		HpcMetadataQuery q = new HpcMetadataQuery();
	       		HpcMetadataQueryLevelFilter levelFilter = new HpcMetadataQueryLevelFilter();
	       		levelFilter.setLabel("Asset");
	       		levelFilter.setOperator(HpcMetadataQueryOperator.EQUAL);
	       		q.setLevelFilter(levelFilter);
	       		q.setAttribute("access_group");
	       		q.setValue("public");
	       		q.setOperator(HpcMetadataQueryOperator.EQUAL);
	       		queries1.add(q);

	       		for(KeyValueBean x :loggedOnUserPermissions) {
	       			HpcMetadataQuery q1 = new HpcMetadataQuery();
	       			HpcMetadataQueryLevelFilter levelFilter1 = new HpcMetadataQueryLevelFilter();
	       			levelFilter1.setLabel("Asset");
	       		    levelFilter1.setOperator(HpcMetadataQueryOperator.EQUAL);
	       			q1.setAttribute("access_group");
	       			q1.setValue("%"+x.getValue()+"%");
	       			q1.setLevelFilter(levelFilter1);
	       			q1.setOperator(HpcMetadataQueryOperator.LIKE);
	       			queries1.add(q1);
	       		}

	       		query1.getQueries().addAll(queries1);
	       		
	       		
	       		//perform and operation of query and query1
	       		HpcCompoundMetadataQuery query2 = new HpcCompoundMetadataQuery();
	       		query2.setOperator(HpcCompoundMetadataQueryOperator.AND);
	       		query2.getCompoundQueries().add(query1);
	       		query2.getCompoundQueries().add(query);

	       		compoundMetadataQuery.setCompoundQuery(query2);
	         
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
	    public ResponseEntity<?> queryDataObjects(@RequestHeader HttpHeaders headers, HttpSession session,
				 HttpServletResponse response,HttpServletRequest request,@RequestParam("returnParent") Boolean returnParent,
				 @RequestBody @Valid HpcCompoundMetadataQueryDTO compoundMetadataQuery) {
	    	 
	    	
	    	String authToken = (String) session.getAttribute("hpcUserToken");
	    	if(StringUtils.isEmpty(authToken)) {
	    	authToken = (String) session.getAttribute("writeAccessUserToken");
	    	}
	    	
	          log.info("authToken: " + authToken);
	          
	          if (authToken == null) {
	            return null;
	          }
	          
	          String doeLogin = (String) session.getAttribute("doeLogin");
	          log.info("doeLogin: " + doeLogin);
	          
	          if(Boolean.TRUE.equals(returnParent)) {
		        	 HpcCompoundMetadataQuery query = compoundMetadataQuery.getCompoundQuery();
		        	// add criteria for access group public and other prog names for logged on user.
		       	  List<KeyValueBean> loggedOnUserPermissions = new ArrayList<>();
		       		if(StringUtils.isNotEmpty(doeLogin)) {
		             	DoeUsersModel user = authenticateService.getUserInfo(doeLogin);
				        if(user != null && !StringUtils.isEmpty(user.getProgramName())) {
					     List<String> progList = Arrays.asList(user.getProgramName().split(","));
					    progList.stream().forEach(e -> loggedOnUserPermissions.add(new KeyValueBean(e, e))); 
				        }
		       		}
				 
		       		HpcCompoundMetadataQuery query1 = new HpcCompoundMetadataQuery();
		       		query1.setOperator(HpcCompoundMetadataQueryOperator.OR);
		       		List<HpcMetadataQuery> queries1 = new ArrayList<HpcMetadataQuery>();
		       		
		       		// perform OR operation of public access and logged on users access groups 
		       		HpcMetadataQuery q = new HpcMetadataQuery();
		       		HpcMetadataQueryLevelFilter levelFilter = new HpcMetadataQueryLevelFilter();
		       		levelFilter.setLabel("Asset");
		       		levelFilter.setOperator(HpcMetadataQueryOperator.EQUAL);
		       		q.setLevelFilter(levelFilter);
		       		q.setAttribute("access_group");
		       		q.setValue("public");
		       		q.setOperator(HpcMetadataQueryOperator.EQUAL);
		       		queries1.add(q);

		       		for(KeyValueBean x :loggedOnUserPermissions) {
		       			HpcMetadataQuery q1 = new HpcMetadataQuery();
		       			HpcMetadataQueryLevelFilter levelFilter1 = new HpcMetadataQueryLevelFilter();
		       			levelFilter1.setLabel("Asset");
		       		    levelFilter1.setOperator(HpcMetadataQueryOperator.EQUAL);
		       			q1.setAttribute("access_group");
		       			q1.setValue("%"+x.getValue()+"%");
		       			q1.setLevelFilter(levelFilter1);
		       			q1.setOperator(HpcMetadataQueryOperator.LIKE);
		       			queries1.add(q1);
		       		}

		       		query1.getQueries().addAll(queries1);
		       		
		       		
		       		//perform and operation of query and query1
		       		HpcCompoundMetadataQuery query2 = new HpcCompoundMetadataQuery();
		       		query2.setOperator(HpcCompoundMetadataQueryOperator.AND);
		       		query2.getCompoundQueries().add(query1);
		       		query2.getCompoundQueries().add(query);

		       		compoundMetadataQuery.setCompoundQuery(query2);
		         }
	          compoundMetadataQuery.setDetailedResponse(true);
	    	 UriComponentsBuilder ucBuilder  = UriComponentsBuilder.fromHttpUrl(compoundDataObjectSearchServiceURL);		     		    
		    
		    if (ucBuilder == null) {
			      return null;
			}

		    ucBuilder.queryParam("returnParent", returnParent);
		    String requestURL;
			try {
				requestURL = ucBuilder.build().encode().toUri().toURL().toExternalForm();
				 WebClient client = DoeClientUtil.getWebClient(requestURL, sslCertPath, sslCertPassword);
					client.header("Authorization", "Bearer " + authToken);	
					Response restResponse = client.invoke("POST", compoundMetadataQuery);
					if (restResponse.getStatus() == 200) {
						MappingJsonFactory factory = new MappingJsonFactory();
						JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
						if(Boolean.TRUE.equals(returnParent)) {
							HpcCollectionListDTO dataObjects = parser.readValueAs(HpcCollectionListDTO.class);
							return new ResponseEntity<>(dataObjects, HttpStatus.OK);
						} else if(Boolean.FALSE.equals(returnParent)) {
							HpcDataObjectListDTO dataObjects = parser.readValueAs(HpcDataObjectListDTO.class);
							return new ResponseEntity<>(dataObjects, HttpStatus.OK);
						}
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
	    
	    public String getAttributeValue(String attrName, List<HpcMetadataEntry> list) {
			if (list == null)
				return null;
			
			HpcMetadataEntry entry =  list.stream().filter(e -> e.getAttribute().equalsIgnoreCase(attrName)).
					findAny().orElse(null);
			if(entry !=null) {
				return entry.getValue();
			}
			return null;
}
}
