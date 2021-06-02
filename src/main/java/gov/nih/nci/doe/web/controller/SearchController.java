package gov.nih.nci.doe.web.controller;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.ws.rs.core.Response;

import org.apache.commons.collections.CollectionUtils;
import org.apache.cxf.jaxrs.client.WebClient;
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
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.domain.LookUp;
import gov.nih.nci.doe.web.model.DoeSearch;
import gov.nih.nci.doe.web.model.DoeSearchResult;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.doe.web.util.HibernateProxyTypeAdapter;
import gov.nih.nci.doe.web.util.LambdaUtils;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataLevelAttributes;
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
	@Value("${gov.nih.nci.hpc.server.metadataattributes}")
	private String hpcMetadataAttrsURL;

	@Value("${gov.nih.nci.hpc.server.collection}")
	private String serviceURL;

	@GetMapping
	public ResponseEntity<?> search(HttpSession session, @RequestHeader HttpHeaders headers, HttpServletRequest request,
			DoeSearch search) throws DoeWebException {

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
			log.info("search compund query" + compoundQuery);
			serviceURL = compoundDataObjectSearchServiceURL;

			UriComponentsBuilder ucBuilder = UriComponentsBuilder.fromHttpUrl(compoundDataObjectSearchServiceURL);

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
				GsonBuilder b = new GsonBuilder();
				b.registerTypeAdapterFactory(HibernateProxyTypeAdapter.FACTORY);
				Gson gson = b.create();
				String searchQuery = gson.toJson(search);
				session.setAttribute("searchQuery", searchQuery);
				log.info("Search query" + search);
				if (search.getSearchType() != null && search.getSearchType().equals("dataobject")) {
					results = processResponseResults(systemAttrs, restResponse);
					return new ResponseEntity<>(results, HttpStatus.OK);
				}

			} else if (restResponse.getStatus() == 204) {
				return new ResponseEntity<>(results, HttpStatus.OK);
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);

		}
		return new ResponseEntity<>(results, HttpStatus.NO_CONTENT);

	}

	private List<DoeSearchResult> processResponseResults(List<String> systemAttrs, Response restResponse)
			throws IOException {

		List<DoeSearchResult> returnResults = new ArrayList<DoeSearchResult>();

		returnResults = processCollectionResults(systemAttrs, restResponse);

		return returnResults;

	}

	@SuppressWarnings("unchecked")
	private List<DoeSearchResult> processCollectionResults(List<String> systemAttrs, Response restResponse)
			throws IOException {
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
			Integer studyCollectionId = getCollectionId(result.getMetadataEntries().getParentMetadataEntries(),
					"Study");
			Integer programCollectionId = getCollectionId(result.getMetadataEntries().getParentMetadataEntries(),
					"Program");
			returnResult.setDataSetCollectionId(result.getCollection().getCollectionId());
			returnResult.setDataSetPermissionRole(
					getPermissionRole(user, result.getCollection().getCollectionId(), loggedOnUserPermissions));
			returnResult.setStudyCollectionId(studyCollectionId);
			returnResult.setProgramCollectionId(programCollectionId);
			returnResult.setStudyPermissionRole(getPermissionRole(user, studyCollectionId, loggedOnUserPermissions));
			returnResult
					.setProgramPermissionRole(getPermissionRole(user, programCollectionId, loggedOnUserPermissions));
			returnResult.setDataSetPath(result.getCollection().getCollectionName());
			returnResult.setDataSetName(
					getAttributeValue("asset_name", result.getMetadataEntries().getSelfMetadataEntries(), "Asset"));
			returnResult.setDataSetDescription(
					getAttributeValue("description", result.getMetadataEntries().getSelfMetadataEntries(), "Asset"));
			returnResult.setStudyPath(studyPath);
			returnResult.setInstitutePath(programPath);
			returnResult.setSelfMetadata(
					getUserMetadata(result.getMetadataEntries().getSelfMetadataEntries(), "Asset", systemAttrs));
			returnResult.setStudyUserMetadata(
					getUserMetadata(result.getMetadataEntries().getParentMetadataEntries(), "Study", systemAttrs));
			returnResult.setInstituteUserMetadata(
					getUserMetadata(result.getMetadataEntries().getParentMetadataEntries(), "Program", systemAttrs));
			returnResult.setProgramName(getAttributeValue("program_name",
					result.getMetadataEntries().getParentMetadataEntries(), "Program"));
			returnResult.setStudyName(
					getAttributeValue("study_name", result.getMetadataEntries().getParentMetadataEntries(), "Study"));
			returnResult.setDataLevelAccessGroups(
					getAttributeValue("access_group", result.getMetadataEntries().getSelfMetadataEntries(), "Asset"));
			returnResult.setDataSetdmeDataId(webUrl + "/searchTab?dme_data_id="
					+ getAttributeValue("dme_data_id", result.getMetadataEntries().getSelfMetadataEntries(), "Asset"));
			returnResult.setAssetType(
					getAttributeValue("asset_type", result.getMetadataEntries().getSelfMetadataEntries(), "Asset"));
			returnResult.setDmeDataId(
					getAttributeValue("dme_data_id", result.getMetadataEntries().getSelfMetadataEntries(), "Asset"));
			returnResults.add(returnResult);

		}

		return returnResults;
	}

	private Integer getCollectionId(List<HpcMetadataEntry> list, String levelName) {
		if (list == null)
			return null;

		HpcMetadataEntry entry = list.stream().filter(e -> levelName.equalsIgnoreCase(e.getLevelLabel())).findAny()
				.orElse(null);
		if (entry != null) {
			return entry.getCollectionId();
		}
		return null;
	}

	@GetMapping(value = "/search-list")
	public ResponseEntity<?> getLevelList(HttpSession session, @RequestHeader HttpHeaders headers) {
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

			// remove duplicates
			List<String> newCollectionLevels = collectionLevels.stream().distinct().collect(Collectors.toList());

			if (CollectionUtils.isNotEmpty(newCollectionLevels)) {

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

				List<String> userList = LambdaUtils.filter(newCollectionLevels, (String n) -> !systemAttrs.contains(n));

				List<LookUp> results = lookUpService.getAllDisplayNames();
				List<String> lookUpList = LambdaUtils.map(results, LookUp::getDisplayName);
				List<String> lookUpAttrnamesList = LambdaUtils.map(results, LookUp::getAttrName);
				attrNamesList.addAll(lookUpList);

				List<String> userDefinedMetadaList = LambdaUtils.filter(userList,
						(String n) -> !lookUpAttrnamesList.contains(n));

				attrNamesList.addAll(userDefinedMetadaList);

				Set<String> duplicates = new HashSet<>();
				duplicates.addAll(attrNamesList);
				attrNamesList.clear();
				attrNamesList.addAll(duplicates);

				attrNamesList.stream().forEach(e -> keyValueBeanResults.add(new KeyValueBean(e, e)));
				keyValueBeanResults.sort(Comparator.comparing(KeyValueBean::getKey, String.CASE_INSENSITIVE_ORDER));

			}
			return new ResponseEntity<>(keyValueBeanResults, headers, HttpStatus.OK);

		} catch (Exception e) {
			log.error(e.getMessage(), e);

			return new ResponseEntity<>(null, headers, HttpStatus.SERVICE_UNAVAILABLE);
		}
	}

}
