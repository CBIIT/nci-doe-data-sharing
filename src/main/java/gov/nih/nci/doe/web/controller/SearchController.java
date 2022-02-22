package gov.nih.nci.doe.web.controller;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
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
import com.fasterxml.jackson.databind.MappingJsonFactory;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.domain.ModelInfo;
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
import io.micrometer.core.instrument.util.StringUtils;

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

			// ucBuilder.queryParam("returnParent", Boolean.TRUE);
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
				if (search.getSearchType() != null && search.getSearchType().equals("dataobject")) {
					results = processCollectionResults(systemAttrs, restResponse, search);
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

	@SuppressWarnings("unchecked")
	private List<DoeSearchResult> processCollectionResults(List<String> systemAttrs, Response restResponse,
			DoeSearch search) throws IOException {
		MappingJsonFactory factory = new MappingJsonFactory();
		JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
		HpcCollectionListDTO collections = parser.readValueAs(HpcCollectionListDTO.class);
		String user = getLoggedOnUserInfo();
		List<HpcCollectionDTO> searchResults = collections.getCollections();
		List<DoeSearchResult> returnResults = new ArrayList<DoeSearchResult>();
		List<KeyValueBean> loggedOnUserPermissions = (List<KeyValueBean>) getMetaDataPermissionsList(user).getBody();

		List<String> bulkAssetsList = Arrays.asList(bulkAssetsPaths.split(","));

		for (HpcCollectionDTO result : searchResults) {

			String absolutePath = result.getCollection().getAbsolutePath();

			if (StringUtils.isNotEmpty(absolutePath)) {

				String[] list = absolutePath.split("/");
				// if the list.length is 5, the collection is an Asset.
				// this validation is done since only assets are display on search results
				if (list.length == 5) {

					String dataSetPermissionRole = getPermissionRole(user, result.getCollection().getCollectionId(),
							loggedOnUserPermissions);
					List<HpcMetadataEntry> selfMetadatEntries = result.getMetadataEntries().getSelfMetadataEntries();
					String assetType = getAttributeValue("asset_type", selfMetadatEntries, "Asset");
					// criteria to filter the assets available for predictions and to show
					// collections available for edit when the checkbox is
					// clicked from search

					Boolean isShow = false;
					Boolean showGeneratePredictions = false;

					ModelInfo modelInfo = modelInfoService.getModelInfo(result.getCollection().getCollectionName());

					if (Boolean.TRUE.equals(getIsUploader()) && "Model".equalsIgnoreCase(assetType) && modelInfo != null
							&& Boolean.TRUE.equals(modelInfo.getIsExternalDatasetSupported())) {

						showGeneratePredictions = true;
					}

					if ("false".equalsIgnoreCase(search.getIsShowMyCollection())
							&& "false".equalsIgnoreCase(search.getShowModelAnalysisResults())) {
						isShow = true;
					} else if ("true".equalsIgnoreCase(search.getIsShowMyCollection())
							&& !"No Permissions".equalsIgnoreCase(dataSetPermissionRole)
							&& ("true".equalsIgnoreCase(search.getShowModelAnalysisResults())
									&& Boolean.TRUE.equals(showGeneratePredictions))) {
						isShow = true;
					} else if ("true".equalsIgnoreCase(search.getIsShowMyCollection())
							&& !"No Permissions".equalsIgnoreCase(dataSetPermissionRole)
							&& "false".equalsIgnoreCase(search.getShowModelAnalysisResults())) {
						isShow = true;
					} else if ("true".equalsIgnoreCase(search.getShowModelAnalysisResults())
							&& Boolean.TRUE.equals(showGeneratePredictions)
							&& "false".equalsIgnoreCase(search.getIsShowMyCollection())) {
						isShow = true;
					}

					if (Boolean.TRUE.equals(isShow)) {

						DoeSearchResult returnResult = new DoeSearchResult();

						String studyPath = result.getCollection().getCollectionParentName();
						String programPath = studyPath.substring(0, studyPath.lastIndexOf('/'));
						Integer studyCollectionId = getCollectionId(
								result.getMetadataEntries().getParentMetadataEntries(), "Study");
						Integer programCollectionId = getCollectionId(
								result.getMetadataEntries().getParentMetadataEntries(), "Program");

						Boolean isBulkAsset = bulkAssetsList.stream()
								.anyMatch(s -> result.getCollection().getCollectionName().equalsIgnoreCase(s));
						returnResult.setIsBulkAsset(isBulkAsset);
						returnResult.setDataSetPath(result.getCollection().getCollectionName());
						returnResult.setDataSetCollectionId(result.getCollection().getCollectionId());
						returnResult.setStudyCollectionId(studyCollectionId);
						returnResult.setProgramCollectionId(programCollectionId);

						returnResult.setDataSetPermissionRole(dataSetPermissionRole);

						returnResult.setStudyPermissionRole(
								getPermissionRole(user, studyCollectionId, loggedOnUserPermissions));
						returnResult.setProgramPermissionRole(
								getPermissionRole(user, programCollectionId, loggedOnUserPermissions));

						returnResult.setDataSetName(getAttributeValue("asset_name", selfMetadatEntries, "Asset"));
						returnResult
								.setDataSetDescription(getAttributeValue("description", selfMetadatEntries, "Asset"));
						returnResult.setStudyPath(studyPath);
						returnResult.setInstitutePath(programPath);
						returnResult.setProgramName(getAttributeValue("program_name",
								result.getMetadataEntries().getParentMetadataEntries(), "Program"));
						returnResult.setStudyName(getAttributeValue("study_name",
								result.getMetadataEntries().getParentMetadataEntries(), "Study"));
						returnResult.setDataSetdmeDataId(webServerName + "/searchTab?dme_data_id="
								+ getAttributeValue("dme_data_id", selfMetadatEntries, "Asset"));
						returnResult.setAssetType(assetType);
						returnResult.setDmeDataId(getAttributeValue("dme_data_id", selfMetadatEntries, "Asset"));
						returnResults.add(returnResult);
					}
				}
			}

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
