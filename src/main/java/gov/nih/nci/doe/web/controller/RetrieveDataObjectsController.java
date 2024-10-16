package gov.nih.nci.doe.web.controller;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.ws.rs.core.Response;

import org.apache.commons.collections.CollectionUtils;
import gov.nih.nci.doe.web.util.MiscUtil;
import org.apache.cxf.jaxrs.client.WebClient;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.util.UriComponentsBuilder;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.MappingJsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.introspect.AnnotationIntrospectorPair;
import com.fasterxml.jackson.databind.introspect.JacksonAnnotationIntrospector;
import com.fasterxml.jackson.databind.type.TypeFactory;
import com.fasterxml.jackson.module.jaxb.JaxbAnnotationIntrospector;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.constants.SystemAttributesList;
import gov.nih.nci.doe.web.domain.InferencingTask;
import gov.nih.nci.doe.web.domain.PredictionAccess;
import gov.nih.nci.doe.web.model.DoeDatafileSearchResultDetailed;
import gov.nih.nci.doe.web.model.DoeUsersModel;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.model.MoDaCPredictionsResults;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.doe.web.util.LambdaUtils;
import gov.nih.nci.hpc.domain.datamanagement.HpcCollectionListingEntry;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementModelDTO;

import java.util.Collections;

/**
 *
 * Get Data Objects by Path Controller
 *
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/getDataObjects")
public class RetrieveDataObjectsController extends AbstractDoeController {

	/**
	 * The number of bytes in a kilobyte.
	 */
	public static final long ONE_KB = 1024;

	/**
	 * The number of bytes in a megabyte.
	 */
	public static final long ONE_MB = ONE_KB * ONE_KB;

	/**
	 * The number of bytes in a gigabyte.
	 */
	public static final long ONE_GB = ONE_KB * ONE_MB;

	@GetMapping
	public ResponseEntity<?> getDataObjects(HttpSession session, @RequestHeader HttpHeaders headers,
			@RequestParam(value = "path") String path) throws DoeWebException {

		log.info("get asset files under asset path: " + path);
		return getDataObjectsLevelFiles(session, headers, path, "Asset_Files");

	}

	@GetMapping("/getPredictionFiles")
	public ResponseEntity<?> getPredictionFiles(HttpSession session, @RequestHeader HttpHeaders headers,
			@RequestParam(value = "path") String path) throws DoeWebException {

		log.info("get model analysis files under asset path: " + path);
		return getDataObjectsLevelFiles(session, headers, path, "Prediction_Files");
	}

	@GetMapping("/getMetadata")
	public ResponseEntity<?> getUserMetaDataByPath(@RequestParam(value = "selectedPath") String selectedPath,
			HttpSession session, HttpServletRequest request, HttpServletResponse response) throws DoeWebException {

		log.info("get model analysis files under asset path: " + selectedPath);
		DoeDatafileSearchResultDetailed dataResult = new DoeDatafileSearchResultDetailed();
		String authToken = (String) session.getAttribute("hpcUserToken");
		List<String> systemAttrs = new ArrayList<>();

		HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");

		if (modelDTO != null) {
			systemAttrs = modelDTO.getDataObjectSystemGeneratedMetadataAttributeNames();
		} else {
			modelDTO = DoeClientUtil.getDOCModel(authToken, hpcModelURL);
			session.setAttribute("userDOCModel", modelDTO);

			systemAttrs = modelDTO.getDataObjectSystemGeneratedMetadataAttributeNames();
		}

		gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectDTO datafiles = DoeClientUtil.getDatafiles(authToken,
				dataObjectAsyncServiceURL, selectedPath, false, false);

		if (datafiles != null && datafiles.getDataObject() != null) {
			dataResult = getAssetFile(session, datafiles, systemAttrs);
		}

		return new ResponseEntity<>(dataResult, HttpStatus.OK);

	}

	private List<DoeDatafileSearchResultDetailed> processDataObjectResponseResults(Response restResponse, String path,
			HttpSession session) throws IOException, DoeWebException {

		log.info("process asset files and folders");
		List<DoeDatafileSearchResultDetailed> returnResults = new ArrayList<DoeDatafileSearchResultDetailed>();

		ObjectMapper mapper = new ObjectMapper();
		AnnotationIntrospectorPair intr = new AnnotationIntrospectorPair(
				new JaxbAnnotationIntrospector(TypeFactory.defaultInstance()), new JacksonAnnotationIntrospector());
		mapper.setAnnotationIntrospector(intr);
		mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		MappingJsonFactory factory = new MappingJsonFactory(mapper);
		JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());

		HpcCollectionListDTO dataObjects = parser.readValueAs(HpcCollectionListDTO.class);

		List<HpcCollectionDTO> searchResults = dataObjects.getCollections();

		List<HpcCollectionListingEntry> dataObjectsList = searchResults.get(0).getCollection().getDataObjects();

		List<HpcCollectionListingEntry> subCollections = searchResults.get(0).getCollection().getSubCollections();

		if (!CollectionUtils.isEmpty(dataObjectsList)) {
			for (HpcCollectionListingEntry dataObject : dataObjectsList) {
				String name = dataObject.getPath().substring(dataObject.getPath().lastIndexOf('/') + 1);
				DoeDatafileSearchResultDetailed returnResult = new DoeDatafileSearchResultDetailed();
				returnResult.setPath(dataObject.getPath());
				returnResult.setName(name);
				returnResult.setFileSizeInBytes(dataObject.getDataSize());
				returnResult.setFileSize(MiscUtil.addHumanReadableSize(String.valueOf(dataObject.getDataSize())));
				returnResult.setIsFolder(false);
				returnResults.add(returnResult);
			}

		}

		if (!CollectionUtils.isEmpty(subCollections)) {
			for (HpcCollectionListingEntry collection : subCollections) {
				String name = collection.getPath().substring(collection.getPath().lastIndexOf('/') + 1);
				if (!name.startsWith("Prediction_") && !name.startsWith("Predictions_")) {
					DoeDatafileSearchResultDetailed returnResult = new DoeDatafileSearchResultDetailed();
					returnResult.setPath(collection.getPath());
					returnResult.setName(name);
					returnResult.setCollectionId(collection.getId());
					returnResult.setIsFolder(true);
					returnResults.add(returnResult);
				}
			}

		}

		return returnResults;

	}

	@SuppressWarnings("unchecked")
	private List<MoDaCPredictionsResults> processGeneratedPredDataObjects(HttpSession session, String path)
			throws IOException {

		List<MoDaCPredictionsResults> returnResults = new ArrayList<MoDaCPredictionsResults>();

		String userId = getLoggedOnUserInfo();
		List<String> loggedOnUserPermList = new ArrayList<String>();
		List<KeyValueBean> loggedOnUserPermissions = (List<KeyValueBean>) getMetaDataPermissionsList(session, userId)
				.getBody();
		loggedOnUserPermissions.stream().forEach(e -> loggedOnUserPermList.add(e.getKey()));
		List<String> grpsList = LambdaUtils.map(loggedOnUserPermissions, KeyValueBean::getKey);

		List<PredictionAccess> predictionResults = predictionAccessService.getAllPredictionsForUserByAssetPath(userId,
				grpsList, path);
		if (!CollectionUtils.isEmpty(predictionResults)) {
			for (PredictionAccess collection : predictionResults) {

				String predictionIdentifier = collection.getCollectionPath()
						.substring(collection.getCollectionPath().lastIndexOf('/') + 1);

				List<PredictionAccess> predictionAccessList = predictionAccessService
						.getPredictionAccessByCollectionPath(collection.getCollectionPath());
				if (CollectionUtils.isNotEmpty(predictionAccessList)) {
					List<String> predGrpAccessList = new ArrayList<String>();

					PredictionAccess owner = predictionAccessList.stream()
							.filter(e -> e.getUser() != null && userId.equalsIgnoreCase(e.getUser().getEmailAddrr()))
							.findAny().orElse(null);

					predictionAccessList.stream().filter(
							e -> e.getGroup() != null && loggedOnUserPermList.contains(e.getGroup().getGroupName()))
							.forEach(e -> predGrpAccessList.add(e.getGroup().getGroupName()));

					PredictionAccess publicAccess = predictionAccessList.stream()
							.filter(e -> e.getUser() != null && Boolean.TRUE.equals(e.getIsPublic())).findAny()
							.orElse(null);

					if (owner != null || CollectionUtils.isNotEmpty(predGrpAccessList) || publicAccess != null) {
						MoDaCPredictionsResults predictionResult = new MoDaCPredictionsResults();
						String taskId = predictionIdentifier.substring(predictionIdentifier.lastIndexOf('_') + 1);
						InferencingTask inferencetask = inferencingTaskService.getInferenceByTaskId(taskId);
						if (inferencetask != null) {
							if (owner != null) {
								predictionResult.setPredictionFolderPath(collection.getCollectionPath());
								predictionResult.setIsOwner(Boolean.TRUE);
								predictionResult.setPredCollId(collection.getCollectionId());
								predictionResult.setPredAccessGrps(CollectionUtils.isNotEmpty(predGrpAccessList)
										? String.join(",", predGrpAccessList)
										: null);
								predictionResult.setIsPublic(owner.getIsPublic());
							} else {
								// logged on user has group access to this prediction
								PredictionAccess ownerInfo = predictionAccessList.stream()
										.filter(e -> e.getUser() != null).findAny().orElse(null);
								DoeUsersModel user = authService.getUserInfo(ownerInfo.getUser().getEmailAddrr());
								predictionResult.setIsOwner(Boolean.FALSE);
								predictionResult.setFullName(user.getFirstName() + " " + user.getLastName());

							}
							predictionResult.setInputDatasetPath(inferencetask.getTestDataSetPath());
							predictionResult.setInputDatasetName(inferencetask.getTestDataSetPath()
									.substring(inferencetask.getTestDataSetPath().lastIndexOf('/') + 1));
							predictionResult.setOutcomeFileName(inferencetask.getActualResultsFileName());
							predictionResult.setOutcomeFilePath(inferencetask.getOutcomeFilePath());
							predictionResult.setTaskId(taskId);
							predictionResult.setTaskCompletedDate(inferencetask.getCompletedDate());
							predictionResult.setPredictionsPath(inferencetask.getResultPath());
							predictionResult.setPredictionsName(inferencetask.getResultPath()
									.substring(inferencetask.getResultPath().lastIndexOf('/') + 1));
							predictionResult.setIsReferenceDataset(inferencetask.getIsReferenceAsset());

							returnResults.add(predictionResult);
						}
					}
				}

			}

		}
		return returnResults;

	}

	public List<KeyValueBean> getSystemMetaData(List<HpcMetadataEntry> list, String levelName,
			List<String> systemAttrs) {

		List<KeyValueBean> entryList = new ArrayList<KeyValueBean>();

		for (HpcMetadataEntry entry : list) {
			if (systemAttrs != null && systemAttrs.contains(entry.getAttribute())
					&& SystemAttributesList.dataSystemAttributes.contains(entry.getAttribute()) && (levelName == null
							|| (levelName != null && levelName.equalsIgnoreCase(entry.getLevelLabel())))) {
				KeyValueBean k = null;
				k = new KeyValueBean(entry.getAttribute(), entry.getAttribute(), entry.getValue());
				entryList.add(k);
			}

		}

		Collections.sort(entryList, Comparator.comparing(KeyValueBean::getDisplayName));

		return entryList;
	}

	public ResponseEntity<?> getDataObjectsLevelFiles(HttpSession session, HttpHeaders headers, String path,
			String type) throws DoeWebException {

		log.info("get data objects files for path: " + path);
		String authToken = (String) session.getAttribute("hpcUserToken");

		List<DoeDatafileSearchResultDetailed> dataResults = new ArrayList<DoeDatafileSearchResultDetailed>();
		List<MoDaCPredictionsResults> modelAnalysisFiles = new ArrayList<MoDaCPredictionsResults>();
		try {

			if (type.equalsIgnoreCase("Prediction_Files")) {

				modelAnalysisFiles = processGeneratedPredDataObjects(session, path);
				return new ResponseEntity<>(modelAnalysisFiles, HttpStatus.OK);

			} else {

				final String requestUrl = UriComponentsBuilder.fromHttpUrl(serviceURL).path("{path}/children")
						.buildAndExpand(path).encode().toUri().toURL().toExternalForm();

				WebClient client = DoeClientUtil.getWebClient(requestUrl);
				client.header("Authorization", "Bearer " + authToken);
				Response restResponse = client.invoke("GET", null);

				if (restResponse.getStatus() == 200) {

					dataResults = processDataObjectResponseResults(restResponse, path, session);
					Collections.sort(dataResults,
							Comparator.comparing(DoeDatafileSearchResultDetailed::getIsFolder).reversed());
					return new ResponseEntity<>(dataResults, HttpStatus.OK);

				} else if (restResponse.getStatus() == 204) {

					return new ResponseEntity<>(dataResults, HttpStatus.OK);

				}
			}

		} catch (Exception e) {
			log.error(e.getMessage(), e);

		}
		return new ResponseEntity<>(dataResults, HttpStatus.NO_CONTENT);
	}

	private DoeDatafileSearchResultDetailed getAssetFile(HttpSession session,
			gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectDTO result, List<String> systemAttrs) {

		DoeDatafileSearchResultDetailed returnResult = new DoeDatafileSearchResultDetailed();

		returnResult.setSelfMetadata(
				getUserMetadata(session, result.getMetadataEntries().getSelfMetadataEntries().getUserMetadataEntries(),
						null, systemAttrs, null));
		returnResult.setSystemMetadata(getSystemMetaData(
				result.getMetadataEntries().getSelfMetadataEntries().getSystemMetadataEntries(), null, systemAttrs));

		return returnResult;

	}
}
