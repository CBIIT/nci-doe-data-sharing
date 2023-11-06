package gov.nih.nci.doe.web.controller;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.ws.rs.core.Response;

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
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.MappingJsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.introspect.AnnotationIntrospectorPair;
import com.fasterxml.jackson.databind.introspect.JacksonAnnotationIntrospector;
import com.fasterxml.jackson.databind.type.TypeFactory;
import com.fasterxml.jackson.module.jaxb.JaxbAnnotationIntrospector;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.DoeSearch;
import gov.nih.nci.doe.web.model.DoeSearchResult;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.doe.web.util.HibernateProxyTypeAdapter;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementModelDTO;
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

	@Value("${asset.bulk.collections.size}")
	public String bulkCollectionSize;

	@GetMapping
	public ResponseEntity<?> search(HttpSession session, @RequestHeader HttpHeaders headers, HttpServletRequest request,
			DoeSearch search) throws DoeWebException {

		log.info("Search controller");
		String authToken = (String) session.getAttribute("hpcUserToken");
		HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");
		if (modelDTO == null) {
			modelDTO = DoeClientUtil.getDOCModel(authToken, hpcModelURL);
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
			HpcCompoundMetadataQueryDTO compoundQuery = constructCriteria(search, "Asset");
			compoundQuery.setDetailedResponse(true);
			log.info("search compund query" + compoundQuery);

			UriComponentsBuilder ucBuilder = UriComponentsBuilder.fromHttpUrl(compoundCollectionSearchServiceURL);

			if (ucBuilder == null) {
				return null;
			}

			final String requestURL = ucBuilder.build().encode().toUri().toURL().toExternalForm();

			WebClient client = DoeClientUtil.getWebClient(requestURL);
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
				results = processCollectionResults(systemAttrs, restResponse, search);
				return new ResponseEntity<>(results, HttpStatus.OK);

			} else if (restResponse.getStatus() == 204) {
				return new ResponseEntity<>(results, HttpStatus.OK);
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);

		}
		return new ResponseEntity<>(results, HttpStatus.NO_CONTENT);

	}

	@SuppressWarnings("unchecked")
	private List<DoeSearchResult> processCollectionResults(List<String> systemAttrs, Response restResponse,
			DoeSearch search) throws IOException {

		ObjectMapper mapper = new ObjectMapper();
		AnnotationIntrospectorPair intr = new AnnotationIntrospectorPair(
				new JaxbAnnotationIntrospector(TypeFactory.defaultInstance()), new JacksonAnnotationIntrospector());
		mapper.setAnnotationIntrospector(intr);
		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

		MappingJsonFactory factory = new MappingJsonFactory(mapper);
		JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
		HpcCollectionListDTO collections = parser.readValueAs(HpcCollectionListDTO.class);
		String user = getLoggedOnUserInfo();
		List<HpcCollectionDTO> searchResults = collections.getCollections();
		List<DoeSearchResult> returnResults = new ArrayList<DoeSearchResult>();
		List<KeyValueBean> loggedOnUserPermissions = (List<KeyValueBean>) getMetaDataPermissionsList(user).getBody();

		for (HpcCollectionDTO result : searchResults) {

			String dataSetPermissionRole = getPermissionRole(user, result.getCollection().getCollectionId(),
					loggedOnUserPermissions);
			List<HpcMetadataEntry> selfMetadatEntries = result.getMetadataEntries().getSelfMetadataEntries();
			String assetType = getAttributeValue("asset_type", selfMetadatEntries, "Asset");

			DoeSearchResult returnResult = new DoeSearchResult();
			String collectionSize = getAttributeValue("collection_size", selfMetadatEntries, "Asset");

			String studyPath = result.getCollection().getCollectionParentName();
			String programPath = studyPath.substring(0, studyPath.lastIndexOf('/'));
			Integer studyCollectionId = getCollectionId(result.getMetadataEntries().getParentMetadataEntries(),
					"Study");
			Integer programCollectionId = getCollectionId(result.getMetadataEntries().getParentMetadataEntries(),
					"Program");

			returnResult.setIsBulkAsset(
					(collectionSize != null && Long.valueOf(collectionSize) > Long.valueOf(bulkCollectionSize))
							? Boolean.TRUE
							: Boolean.FALSE);
			returnResult.setDataSetPath(result.getCollection().getCollectionName());
			returnResult.setDataSetCollectionId(result.getCollection().getCollectionId());
			returnResult.setStudyCollectionId(studyCollectionId);
			returnResult.setProgramCollectionId(programCollectionId);

			returnResult.setDataSetPermissionRole(dataSetPermissionRole);

			returnResult.setStudyPermissionRole(getPermissionRole(user, studyCollectionId, loggedOnUserPermissions));
			returnResult
					.setProgramPermissionRole(getPermissionRole(user, programCollectionId, loggedOnUserPermissions));

			returnResult.setDataSetName(getAttributeValue("asset_name", selfMetadatEntries, "Asset"));
			returnResult.setDataSetDescription(getAttributeValue("description", selfMetadatEntries, "Asset"));
			returnResult.setStudyPath(studyPath);
			returnResult.setInstitutePath(programPath);
			returnResult.setProgramName(getAttributeValue("program_name",
					result.getMetadataEntries().getParentMetadataEntries(), "Program"));
			returnResult.setStudyName(
					getAttributeValue("study_name", result.getMetadataEntries().getParentMetadataEntries(), "Study"));
			returnResult.setDataSetdmeDataId(webServerName + "/searchTab?dme_data_id="
					+ getAttributeValue("dme_data_id", selfMetadatEntries, "Asset"));
			returnResult.setAssetType(assetType);
			returnResult.setDmeDataId(getAttributeValue("dme_data_id", selfMetadatEntries, "Asset"));
			returnResult.setIsReferenceDataset(getAttributeValue("is_reference_dataset", selfMetadatEntries, "Asset"));
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

}
