package gov.nih.nci.doe.web.controller;


import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.ws.rs.core.Response;

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
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.client.RestClientException;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.MappingJsonFactory;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

import gov.nih.nci.doe.web.domain.LookUp;
import gov.nih.nci.doe.web.model.DoeSearch;
import gov.nih.nci.doe.web.model.DoeSearchResult;
import gov.nih.nci.doe.web.service.LookUpService;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.domain.metadata.HpcCompoundMetadataQuery;
import gov.nih.nci.hpc.domain.metadata.HpcCompoundMetadataQueryOperator;
import gov.nih.nci.hpc.domain.metadata.HpcCompoundMetadataQueryType;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntries;
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
	 LookUpService lookUpService;
	
	@RequestMapping(method = RequestMethod.GET)
	    public ResponseEntity<?> search(HttpSession session,@RequestHeader HttpHeaders headers,HttpServletRequest request, DoeSearch search ) {
		
		String authToken = (String) session.getAttribute("hpcUserToken");
		HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");
		if (modelDTO == null) {
			modelDTO = DoeClientUtil.getDOCModel(authToken, hpcModelURL, sslCertPath, sslCertPassword);
			session.setAttribute("userDOCModel", modelDTO);
		}
		
		
		List<String> systemAttrs = modelDTO.getCollectionSystemGeneratedMetadataAttributeNames();
		session.setAttribute("systemAttrs", systemAttrs);
		
		List<DoeSearchResult> results = new ArrayList<DoeSearchResult>();	
		

		try {			
			HpcCompoundMetadataQueryDTO compoundQuery = constructCriteria(search);
			compoundQuery.setDetailedResponse(true);
			String serviceURL = compoundDataObjectSearchServiceURL;
			if (search.getSearchType() != null && search.getSearchType().equals("collection")) {
				serviceURL = compoundCollectionSearchServiceURL;
			} else {
				serviceURL =  compoundDataObjectSearchServiceURL;
			}
			
			WebClient client = DoeClientUtil.getWebClient(serviceURL, sslCertPath, sslCertPassword);
			client.header("Authorization", "Bearer " + authToken);

			Response restResponse = client.invoke("POST", compoundQuery);
			if (restResponse.getStatus() == 200) {
				session.setAttribute("compoundQuery", compoundQuery);
				session.setAttribute("hpcSearch", search);
				if (search.getSearchType() != null && search.getSearchType().equals("collection")) {
					results = processResponseResults(search, systemAttrs, restResponse);
					return new ResponseEntity<>(results, HttpStatus.OK);
				} 
				
			} else if(restResponse.getStatus() == 204) {
				return new ResponseEntity<>(results, HttpStatus.OK);
			}
		} catch (com.fasterxml.jackson.databind.JsonMappingException e) {
			log.error(e.getMessage(), e);
		} catch (HttpStatusCodeException e) {
			e.printStackTrace();		
		} catch (RestClientException e) {
			e.printStackTrace();
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {

		}

		return new ResponseEntity<>(results, HttpStatus.NO_CONTENT);
		
	}
	
	private List<DoeSearchResult> processResponseResults(DoeSearch search,List<String> systemAttrs, Response restResponse) throws JsonParseException, IOException {
		
		List<DoeSearchResult> returnResults = new ArrayList<DoeSearchResult>();

		returnResults = processCollectionResults(search, systemAttrs, restResponse);
		
		return returnResults;
			
	}



	private List<DoeSearchResult>  processCollectionResults(DoeSearch search,List<String> systemAttrs, Response restResponse) throws JsonParseException, IOException {
		MappingJsonFactory factory = new MappingJsonFactory();
		JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
		HpcCollectionListDTO collections = parser.readValueAs(HpcCollectionListDTO.class);

		List<HpcCollectionDTO> searchResults = collections.getCollections();
		List<DoeSearchResult> returnResults = new ArrayList<DoeSearchResult>();
		for (HpcCollectionDTO result : searchResults) {
			DoeSearchResult returnResult = new DoeSearchResult();
			returnResult.setDataSetPath(result.getCollection().getCollectionName());
            returnResult.setDataSetName(getAttributeValue("name", result.getMetadataEntries()));
            returnResult.setDataSetDescription(getAttributeValue("description", result.getMetadataEntries()));
            returnResult.setStudyPath(result.getCollection().getCollectionParentName());
            returnResult.setNumOfDataSets(3);
            returnResult.setSelfMetadata(getUserMetadata(result.getMetadataEntries().getSelfMetadataEntries(),"Data_Set", systemAttrs));
			returnResult.setStudyUserMetadata(getUserMetadata(result.getMetadataEntries().getParentMetadataEntries(),"Study", systemAttrs));
			returnResult.setInstituteUserMetadata(getUserMetadata(result.getMetadataEntries().getParentMetadataEntries(),"Institute", systemAttrs));
			returnResult.setInstituteName(getAttributeValue("institute_name", result.getMetadataEntries()));
            returnResult.setStudyName(getAttributeValue("study_name", result.getMetadataEntries()));
			returnResults.add(returnResult);
		}
		
		return returnResults;
	}
	

	
	private String getAttributeValue(String attrName, HpcMetadataEntries entries) {
		if (entries == null)
			return null;

		List<HpcMetadataEntry> selfEntries = entries.getSelfMetadataEntries();
		for (HpcMetadataEntry entry : selfEntries) {
			
			if (entry.getAttribute().equals(attrName))
				return entry.getValue();
		}

		List<HpcMetadataEntry> parentEntries = entries.getParentMetadataEntries();
		for (HpcMetadataEntry entry : parentEntries) {
			if (StringUtils.equals(entry.getAttribute(), attrName))
				return entry.getValue();
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

	private HpcCompoundMetadataQuery buildSimpleSearch(DoeSearch search) {
		HpcCompoundMetadataQuery query = new HpcCompoundMetadataQuery();
		query.setOperator(HpcCompoundMetadataQueryOperator.AND);
		Map<String, HpcMetadataQuery> queriesMap = getQueries(search);
		List<HpcMetadataQuery> queries = new ArrayList<HpcMetadataQuery>();
		Iterator<String> iter = queriesMap.keySet().iterator();
		while (iter.hasNext())
			queries.add(queriesMap.get(iter.next()));

		query.getQueries().addAll(queries);
		return query;
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
	    public ResponseEntity<?> getLevelList(HttpSession session,@RequestHeader HttpHeaders headers,@RequestParam(value = "levelAttr", required = false) String levelAttr) {
	        log.info("getting search list");
	        try {
	        	List<String> collectionLevels = new ArrayList<String>();
	        	List<String> attrList = new ArrayList<String>();
	        	
	        	String authToken = (String) session.getAttribute("hpcUserToken");
	        	
	  		  HpcMetadataAttributesListDTO dto = DoeClientUtil.getMetadataAttrNames(authToken, hpcMetadataAttrsURL,
	  					sslCertPath, sslCertPassword);
	  		  
	  		if (dto != null && dto.getCollectionMetadataAttributes() != null) {
				for (HpcMetadataLevelAttributes levelAttrs : dto.getCollectionMetadataAttributes()) {
					String label = levelAttrs.getLevelLabel();
					if (label == null)
						continue;
					collectionLevels.add(label);
					
					if(StringUtils.isNotEmpty(levelAttr) && label.equalsIgnoreCase(levelAttr)) {
						attrList.addAll(levelAttrs.getMetadataAttributes());
					}
					
				}
			}
	  		
	  		if(StringUtils.isNotEmpty(levelAttr) && StringUtils.isNotBlank(levelAttr)) {
	  			return new ResponseEntity<>(attrList, headers, HttpStatus.OK);
	  		} else {
	  			return new ResponseEntity<>(collectionLevels, headers, HttpStatus.OK);
	  		}
	            
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
	    
		public HpcCollectionDTO getCollection(String path, HttpSession session, HttpServletRequest request) {
			log.info("get collection for path" + path);
			try {
				// User Session validation				
				String authToken = (String) session.getAttribute("hpcUserToken");
				if ( authToken == null || path ==null) {
					return null;
				}

				// Get collection
				HpcCollectionListDTO collections = DoeClientUtil.getCollection(authToken, serviceURL, path, false,
						false, true, sslCertPath, sslCertPassword);
			
				if (collections != null && collections.getCollections() != null
						&& collections.getCollections().size() > 0) {
					
					HpcCollectionDTO collection = collections.getCollections().get(0);
					return collection;
	           
			     } 
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
			return null;
		}
		



}
