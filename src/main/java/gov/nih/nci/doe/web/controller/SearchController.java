package gov.nih.nci.doe.web.controller;


import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.ws.rs.core.Response;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.cxf.jaxrs.client.WebClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;

import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.util.UriComponentsBuilder;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.MappingJsonFactory;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import gov.nih.nci.doe.web.domain.LookUp;
import gov.nih.nci.doe.web.model.DoeSearch;
import gov.nih.nci.doe.web.model.DoeSearchResult;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.service.ConsortiumService;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.doe.web.util.LambdaUtils;
import gov.nih.nci.hpc.domain.metadata.HpcCompoundMetadataQuery;
import gov.nih.nci.hpc.domain.metadata.HpcCompoundMetadataQueryOperator;
import gov.nih.nci.hpc.domain.metadata.HpcCompoundMetadataQueryType;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataLevelAttributes;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataQuery;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataQueryAttributeMatch;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataQueryLevelFilter;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataQueryOperator;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementModelDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcMetadataAttributesListDTO;
import gov.nih.nci.hpc.dto.datasearch.HpcCompoundMetadataQueryDTO;





/**
 *
 * Search Controller
 *
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/search")
public class SearchController extends AbstractDoeController {

	@Value("${gov.nih.nci.hpc.server.collection}")
	private String collectionServiceURL;
	@Value("${gov.nih.nci.hpc.server.search.collection.compound}")
	private String compoundCollectionSearchServiceURL;
	@Value("${gov.nih.nci.hpc.server.search.dataobject.compound}")
	private String compoundDataObjectSearchServiceURL;
	@Value("${gov.nih.nci.hpc.server.dataObject}")
	private String datafileServiceURL;
	@Value("${gov.nih.nci.hpc.server.model}")
	private String modelServiceURL;
	@Value("${gov.nih.nci.hpc.server.metadataattributes}")
	private String hpcMetadataAttrsURL;
	

	@Value("${gov.nih.nci.hpc.server.collection}")
	private String serviceURL;
	
	@Value("${gov.nih.nci.hpc.server.model}")
	private String hpcModelURL;

	 
	 @Autowired
	 ConsortiumService consortiumService;
	
	    @GetMapping
	    public ResponseEntity<?> search(HttpSession session,@RequestHeader HttpHeaders headers,
	    		HttpServletRequest request, DoeSearch search) {
		
		String authToken = (String) session.getAttribute("hpcUserToken");
		HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");
		if (modelDTO == null) {
			modelDTO = DoeClientUtil.getDOCModel(authToken, hpcModelURL, sslCertPath, sslCertPassword);
			session.setAttribute("userDOCModel", modelDTO);
		}
		
		
		List<String> systemAttrs = modelDTO.getCollectionSystemGeneratedMetadataAttributeNames();
		List<String> dataObjectsystemAttrs = modelDTO.getDataObjectSystemGeneratedMetadataAttributeNames();
		systemAttrs.addAll(dataObjectsystemAttrs);
		systemAttrs.add("collection_type");
		systemAttrs.add("access_group");
		session.setAttribute("systemAttrs", systemAttrs);
		
		List<DoeSearchResult> results = new ArrayList<>();

		try {			
			HpcCompoundMetadataQueryDTO compoundQuery = constructCriteria(search);
			compoundQuery.setDetailedResponse(true);
			serviceURL = compoundDataObjectSearchServiceURL;
			
			 UriComponentsBuilder ucBuilder  = UriComponentsBuilder.fromHttpUrl(compoundDataObjectSearchServiceURL);		     		    
			    
			    if (ucBuilder == null) {
				      return null;
				    }

			    ucBuilder.queryParam("returnParent", Boolean.TRUE);
			    final String requestURL = ucBuilder.build().encode().toUri().toURL().toExternalForm();

			    WebClient client = DoeClientUtil.getWebClient(requestURL, sslCertPath, sslCertPassword);
				client.header("Authorization", "Bearer " + authToken);	
				Response restResponse = client.invoke("POST", compoundQuery);
				
			if (restResponse.getStatus() == 200) {
				session.setAttribute("compoundQuery", compoundQuery);
				if (search.getSearchType() != null && search.getSearchType().equals("dataobject")) {
					results = processResponseResults(systemAttrs, restResponse);
					return new ResponseEntity<>(results, HttpStatus.OK);
				} 
				
			} else if(restResponse.getStatus() == 204) {
				return new ResponseEntity<>(results, HttpStatus.OK);
			}
		}  catch (Exception e) {
			log.error(e.getMessage(), e);
			
		} 
		return new ResponseEntity<>(results, HttpStatus.NO_CONTENT);
		
	}
	
	private List<DoeSearchResult> processResponseResults(List<String> systemAttrs, Response restResponse) throws  IOException {
		
		List<DoeSearchResult> returnResults = new ArrayList<DoeSearchResult>();

		returnResults = processCollectionResults(systemAttrs, restResponse);
		
		return returnResults;
			
	}



	@SuppressWarnings("unchecked")
	private List<DoeSearchResult>  processCollectionResults(List<String> systemAttrs, Response restResponse) throws IOException {
		MappingJsonFactory factory = new MappingJsonFactory();
		JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
		HpcCollectionListDTO collections = parser.readValueAs(HpcCollectionListDTO.class);

		List<HpcCollectionDTO> searchResults = collections.getCollections();
		List<DoeSearchResult> returnResults = new ArrayList<DoeSearchResult>();
		List<KeyValueBean> loggedOnUserPermissions = (List<KeyValueBean>) getMetaDataPermissionsList().getBody();
		String user = getLoggedOnUserInfo();
		for (HpcCollectionDTO result : searchResults) {
			
			DoeSearchResult returnResult = new DoeSearchResult();
			String studyPath = result.getCollection().getCollectionParentName();
			String programPath = studyPath.substring(0, studyPath.lastIndexOf('/'));
			Integer studyCollectionId = getCollectionId(result.getMetadataEntries().getParentMetadataEntries(),"Study");
			Integer programCollectionId  = getCollectionId(result.getMetadataEntries().getParentMetadataEntries(),"Program");
			returnResult.setDataSetCollectionId(result.getCollection().getCollectionId());
			returnResult.setDataSetPermissionRole(getPermissionRole(user,result.getCollection().getCollectionId(),loggedOnUserPermissions));
			returnResult.setStudyCollectionId(studyCollectionId);
			returnResult.setProgramCollectionId(programCollectionId);
			returnResult.setStudyPermissionRole(getPermissionRole(user,studyCollectionId,loggedOnUserPermissions));
			returnResult.setProgramPermissionRole(getPermissionRole(user,programCollectionId,loggedOnUserPermissions));
			returnResult.setDataSetPath(result.getCollection().getCollectionName());
            returnResult.setDataSetName(getAttributeValue("dataset_name", result.getMetadataEntries().getSelfMetadataEntries(),"Dataset"));
            returnResult.setDataSetDescription(getAttributeValue("description", result.getMetadataEntries().getSelfMetadataEntries(),"Dataset"));
            returnResult.setStudyPath(studyPath);
            returnResult.setInstitutePath(programPath);
            returnResult.setSelfMetadata(getUserMetadata(result.getMetadataEntries().getSelfMetadataEntries(),"Dataset", systemAttrs));
			returnResult.setStudyUserMetadata(getUserMetadata(result.getMetadataEntries().getParentMetadataEntries(),"Study", systemAttrs));
			returnResult.setInstituteUserMetadata(getUserMetadata(result.getMetadataEntries().getParentMetadataEntries(),"Program", systemAttrs));
			returnResult.setProgramName(getAttributeValue("program_name", result.getMetadataEntries().getParentMetadataEntries(),"Program"));
            returnResult.setStudyName(getAttributeValue("study_name", result.getMetadataEntries().getParentMetadataEntries(),"Study"));
			returnResult.setDataLevelAccessGroups(getAttributeValue("access_group", result.getMetadataEntries().getSelfMetadataEntries(),"Dataset"));
			returnResult.setStudyLevelAccessGroups(getAttributeValue("access_group", result.getMetadataEntries().getParentMetadataEntries(),"Study"));
			returnResult.setProgramLevelAccessGroups(getAttributeValue("access_group", result.getMetadataEntries().getParentMetadataEntries(),"Program"));
            returnResults.add(returnResult);
		}
		
		return returnResults;
	}
	
	
	
	private Integer getCollectionId(List<HpcMetadataEntry> list,String levelName) {
		if (list == null)
			return null;
		
		HpcMetadataEntry entry =  list.stream().filter(e -> levelName.equalsIgnoreCase(e.getLevelLabel())).
				findAny().orElse(null);
		if(entry !=null) {
			return entry.getCollectionId();
		}
		return null;
	}

	private HpcCompoundMetadataQueryDTO constructCriteria(DoeSearch search) {
		HpcCompoundMetadataQueryDTO dto = new HpcCompoundMetadataQueryDTO();
		dto.setTotalCount(true);
		HpcCompoundMetadataQuery query = buildSimpleSearch(search);
		dto.setCompoundQuery(query);
		dto.setDetailedResponse(search.isDetailed());
		dto.setCompoundQueryType(HpcCompoundMetadataQueryType.COLLECTION);
		dto.setPage(search.getPageNumber());
		dto.setPageSize(search.getPageSize());
		return dto;
	}

	@SuppressWarnings("unchecked")
	private HpcCompoundMetadataQuery buildSimpleSearch(DoeSearch search) {
		
		HpcCompoundMetadataQuery query = new HpcCompoundMetadataQuery();
		query.setOperator(HpcCompoundMetadataQueryOperator.AND);
		Map<String, HpcMetadataQuery> queriesMap = getQueries(search);
		List<HpcMetadataQuery> queries = new ArrayList<HpcMetadataQuery>();
		Iterator<String> iter = queriesMap.keySet().iterator();
		while (iter.hasNext())
			queries.add(queriesMap.get(iter.next()));

		query.getQueries().addAll(queries);
		
		
	// add criteria for access group public and other prog names for logged on user.
	  List<KeyValueBean> loggedOnUserPermissions = (List<KeyValueBean>) getMetaDataPermissionsList().getBody();
		
		HpcCompoundMetadataQuery query1 = new HpcCompoundMetadataQuery();
		query1.setOperator(HpcCompoundMetadataQueryOperator.OR);
		List<HpcMetadataQuery> queries1 = new ArrayList<HpcMetadataQuery>();
		
		// perform OR operation of public access and logged on users access groups 
		HpcMetadataQuery q = new HpcMetadataQuery();
		HpcMetadataQueryLevelFilter levelFilter = new HpcMetadataQueryLevelFilter();
		levelFilter.setLabel("Dataset");
		levelFilter.setOperator(HpcMetadataQueryOperator.EQUAL);
		q.setLevelFilter(levelFilter);
		q.setAttribute("access_group");
		q.setValue("public");
		q.setOperator(HpcMetadataQueryOperator.EQUAL);
		queries1.add(q);

		for(KeyValueBean x :loggedOnUserPermissions) {
			HpcMetadataQuery q1 = new HpcMetadataQuery();
			HpcMetadataQueryLevelFilter levelFilter1 = new HpcMetadataQueryLevelFilter();
			levelFilter1.setLabel("Dataset");
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
		
		
		return query2;
		

	}

	private Map<String, HpcMetadataQuery> getQueries(DoeSearch search) {
		Map<String, HpcMetadataQuery> queries = new HashMap<String, HpcMetadataQuery>();
		
		for (int i = 0; i < search.getAttrName().length; i++) {
			String rowId = search.getRowId()[i];
			String attrName = search.getAttrName()[i];
			String attrValue = search.getAttrValue()[i];
			String operator = search.getOperator()[i];
			String level = null;
			boolean selfMetadata = search.getIsExcludeParentMetadata()[i]; 
				
			 LookUp val = lookUpService.getLookUpByDisplayName(attrName);

			 if(val != null) {
				 level = val.getLevelName();
			 } else {
				 level = search.getLevel()[i];
			 }
			 
			if (!attrValue.isEmpty()) {
				HpcMetadataQuery criteria = new HpcMetadataQuery();
				 if (StringUtils.isEmpty(attrName) || StringUtils.isBlank(attrName) || "ANY".equalsIgnoreCase(attrName)) {
					criteria.setAttributeMatch(HpcMetadataQueryAttributeMatch.ANY);
				 } else {					 
					 if(val != null) {
						 criteria.setAttribute(val.getAttrName());
					 } else {
						 criteria.setAttribute(attrName);
					 }
					
				}
				criteria.setValue(attrValue);
				criteria.setOperator(HpcMetadataQueryOperator.fromValue(operator));
				//If its a timestamp operator, specify the format
				if (operator.startsWith("TIMESTAMP_GREATER")) {
					criteria.setValue(attrValue.concat(" 00:00:00").replace("/", "-"));
					criteria.setFormat("MM-DD-YYYY HH24:MI:SS");
				}
				if (operator.startsWith("TIMESTAMP_LESS")) {
					criteria.setValue(attrValue.concat(" 23:59:59").replace("/", "-"));
					criteria.setFormat("MM-DD-YYYY HH24:MI:SS");
				}
				if (level != null) {
					HpcMetadataQueryLevelFilter levelFilter = new HpcMetadataQueryLevelFilter();
					if (selfMetadata) {
					    levelFilter.setLevel(1);
					    levelFilter.setOperator(HpcMetadataQueryOperator.EQUAL);
					}
					else if (level.equals("ANY")) {
						levelFilter.setLevel(1);
						levelFilter.setOperator(HpcMetadataQueryOperator.NUM_GREATER_OR_EQUAL);
					} else {
						if (level.equals("Data file") || level.equals("DataObject"))
							levelFilter.setLevel(1);
						else
							levelFilter.setLabel(level);
						levelFilter.setOperator(HpcMetadataQueryOperator.EQUAL);
					}
					
					criteria.setLevelFilter(levelFilter);
				}
				queries.put(rowId, criteria);
			}
		}
		return queries;
	}
	
	
	    @GetMapping(value = "/search-list")
	    public ResponseEntity<?> getLevelList(HttpSession session,@RequestHeader HttpHeaders headers) {
	        log.info("getting search list");
	        try {
	        	
	            List<KeyValueBean> keyValueBeanResults = new ArrayList<>();
	        	List<String> collectionLevels = new ArrayList<String>();
	        	List<String> attrNamesList = new ArrayList<String>();
	        	String authToken = (String) session.getAttribute("hpcUserToken");
	        	
	  		  HpcMetadataAttributesListDTO dto = DoeClientUtil.getMetadataAttrNames(authToken, hpcMetadataAttrsURL,
	  					sslCertPath, sslCertPassword);
	  		  
	  		if (dto != null && dto.getCollectionMetadataAttributes() != null) {
				for (HpcMetadataLevelAttributes levelAttrs : dto.getCollectionMetadataAttributes()) {
					String label = levelAttrs.getLevelLabel();
					if (label == null)
						continue;
					collectionLevels.addAll(levelAttrs.getMetadataAttributes());
					
				}
			}
	  		
	  		if (dto != null && dto.getDataObjectMetadataAttributes() != null) {
				for (HpcMetadataLevelAttributes levelAttrs : dto.getDataObjectMetadataAttributes()) {
					String label = levelAttrs.getLevelLabel();
					if (label == null)
						continue;
					collectionLevels.addAll(levelAttrs.getMetadataAttributes());
					
				}
			}
	  		
	  		//remove duplicates from collectionlevels list
	        Set<String> set = new HashSet<>(); 
	        set.addAll(collectionLevels); 
	        collectionLevels.clear();  
	        collectionLevels.addAll(set); 

			if(CollectionUtils.isNotEmpty(collectionLevels)) {
				
				HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");
				if (modelDTO == null) {
					modelDTO = DoeClientUtil.getDOCModel(authToken, hpcModelURL, sslCertPath, sslCertPassword);
					session.setAttribute("userDOCModel", modelDTO);
				}
				
				
				List<String> systemAttrs = modelDTO.getCollectionSystemGeneratedMetadataAttributeNames();
				List<String> dataObjectsystemAttrs = modelDTO.getDataObjectSystemGeneratedMetadataAttributeNames();
				systemAttrs.addAll(dataObjectsystemAttrs);
				systemAttrs.add("collection_type");
				systemAttrs.add("access_group");
				session.setAttribute("systemAttrs", systemAttrs);
				
				List<String> userList = LambdaUtils.filter(collectionLevels, (String n) ->!systemAttrs.contains(n));
				
				List<LookUp> results = lookUpService.getAllDisplayNames();	
		     	List<String> lookUpList = LambdaUtils.map(results,LookUp::getDisplayName);
		    	List<String> lookUpAttrnamesList = LambdaUtils.map(results,LookUp::getAttrName);
		     	attrNamesList.addAll(lookUpList);
		     	
		     	List<String> userDefinedMetadaList = LambdaUtils.filter(userList, (String n) ->!lookUpAttrnamesList.contains(n));
		     	
		     	attrNamesList.addAll(userDefinedMetadaList);
		     	
		     	attrNamesList.stream().forEach(e -> keyValueBeanResults.add(new KeyValueBean(e, e))); 
		     	keyValueBeanResults.sort(Comparator.comparing(KeyValueBean::getKey,String.CASE_INSENSITIVE_ORDER));
				
			}
	  			return new ResponseEntity<>(keyValueBeanResults, headers, HttpStatus.OK);
	            
	        } catch (Exception e) {
	            log.error(e.getMessage(), e);
	           
	            return new ResponseEntity<>(null, headers, HttpStatus.SERVICE_UNAVAILABLE);
	        }
	    }
	    
	    
		
	    @GetMapping(value = "/adv-search-list")
	    public ResponseEntity<?> getAdvancedSearchList(HttpSession session,@RequestHeader HttpHeaders headers) {
	        log.info("getting search list");
	        try {
	        	List<LookUp> results = new ArrayList<LookUp>();
	        	results = lookUpService.getAllDisplayNames();	
	  			return new ResponseEntity<>(results, headers, HttpStatus.OK);
	  		
	            
	        } catch (Exception e) {
	            log.error(e.getMessage(), e);
	           
	            return new ResponseEntity<>(null, headers, HttpStatus.SERVICE_UNAVAILABLE);
	        }
	    }
	    
}
