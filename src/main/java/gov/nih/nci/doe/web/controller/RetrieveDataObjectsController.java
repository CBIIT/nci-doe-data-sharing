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
import org.apache.commons.io.FileUtils;
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
import gov.nih.nci.doe.web.model.DoeDatafileSearchResultDetailed;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.model.MoDaCPredictionsResults;
import gov.nih.nci.doe.web.util.DoeClientUtil;
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
			dataResult = getAssetFile(datafiles, systemAttrs);
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
				returnResult.setFileSize(addHumanReadableSize(dataObject.getDataSize()));
				returnResult.setIsFolder(false);
				returnResults.add(returnResult);
			}

		}

		if (!CollectionUtils.isEmpty(subCollections)) {
			for (HpcCollectionListingEntry collection : subCollections) {
				String name = collection.getPath().substring(collection.getPath().lastIndexOf('/') + 1);
				if (!name.startsWith("Predictions_")) {
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

	private List<MoDaCPredictionsResults> processGeneratedPredDataObjects(Response restResponse, String path,
			HttpSession session) throws IOException {

		List<MoDaCPredictionsResults> returnResults = new ArrayList<MoDaCPredictionsResults>();

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
		String userId = getLoggedOnUserInfo();

		if (!CollectionUtils.isEmpty(dataObjectsList)) {
			for (HpcCollectionListingEntry dataObject : dataObjectsList) {

				String name = dataObject.getPath().substring(dataObject.getPath().lastIndexOf('/') + 1);

				if (name.startsWith("y_pred_")) {
					InferencingTask task = inferencingTaskService.getInferenceByUserIdAndPredName(userId,
							dataObject.getPath());
					MoDaCPredictionsResults predNameResult = returnResults.stream().filter(
							e -> e.getPredictionsName() != null && e.getPredictionsName().equalsIgnoreCase(name))
							.findAny().orElse(null);

					if (predNameResult == null && task != null) {
						MoDaCPredictionsResults returnResult = new MoDaCPredictionsResults();
						String inputDatasetName = task.getTestDataSetPath()
								.substring(task.getTestDataSetPath().lastIndexOf('/') + 1);
						returnResult.setPredictionsName(name);
						returnResult.setPredictionsPath(dataObject.getPath());
						returnResult.setInputDatasetPath(task.getTestDataSetPath());
						returnResult.setInputDatasetName(inputDatasetName);
						returnResult.setTaskCompletedDate(task.getCompletedDate());
						returnResult.setOutcomeFileName(task.getActualResultsFileName());
						returnResult.setOutcomeFilePath(task.getOutcomeFilePath());
						returnResult.setTaskId(task.getTaskId());
						returnResults.add(returnResult);
					}

				} else {

					InferencingTask taskByInputName = inferencingTaskService.getInferenceByUserIdAndInputName(userId,path,
							dataObject.getPath());
					MoDaCPredictionsResults inputNameResult = returnResults.stream().filter(
							e -> e.getInputDatasetName() != null && e.getInputDatasetName().equalsIgnoreCase(name))
							.findAny().orElse(null);
					if (taskByInputName != null && inputNameResult == null) {
						MoDaCPredictionsResults returnResult = new MoDaCPredictionsResults();
						String predName = taskByInputName.getResultPath()
								.substring(taskByInputName.getResultPath().lastIndexOf('/') + 1);
						returnResult.setInputDatasetPath(dataObject.getPath());
						returnResult.setInputDatasetName(name);
						returnResult.setPredictionsName(predName);
						returnResult.setPredictionsPath(taskByInputName.getResultPath());
						returnResult.setTaskCompletedDate(taskByInputName.getCompletedDate());
						returnResult.setOutcomeFileName(taskByInputName.getActualResultsFileName());
						returnResult.setOutcomeFilePath(taskByInputName.getOutcomeFilePath());
						returnResult.setTaskId(taskByInputName.getTaskId());
						returnResults.add(returnResult);
					} else {
						InferencingTask task = inferencingTaskService.getInferenceByUserIdAndOutcomeName(userId, path,name);
						MoDaCPredictionsResults outcomeNameResult = returnResults.stream().filter(
								e -> e.getOutcomeFileName() != null && e.getOutcomeFileName().equalsIgnoreCase(name))
								.findAny().orElse(null);
						if (task != null && outcomeNameResult == null) {
							MoDaCPredictionsResults returnResult = new MoDaCPredictionsResults();
							String inputDatasetName = task.getTestDataSetPath()
									.substring(task.getTestDataSetPath().lastIndexOf('/') + 1);
							String predName = task.getResultPath().substring(task.getResultPath().lastIndexOf('/') + 1);
							returnResult.setPredictionsName(predName);
							returnResult.setPredictionsPath(task.getResultPath());
							returnResult.setInputDatasetPath(task.getTestDataSetPath());
							returnResult.setInputDatasetName(inputDatasetName);
							returnResult.setTaskCompletedDate(task.getCompletedDate());
							returnResult.setOutcomeFileName(name);
							returnResult.setOutcomeFilePath(dataObject.getPath());
							returnResult.setTaskId(task.getTaskId());
							returnResults.add(returnResult);
						}
					}

				}

			}

		}
		return returnResults;

	}

	private String addHumanReadableSize(Long value) {
		if (value != null) {
			return FileUtils.byteCountToDisplaySize(value);
		}

		return null;
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

				String loggedOnUserFolderPath = path + "/Predictions_" + getLoggedOnUserInfo();

				final String requestUrl = UriComponentsBuilder.fromHttpUrl(serviceURL).path("{path}/children")
						.buildAndExpand(loggedOnUserFolderPath).encode().toUri().toURL().toExternalForm();

				WebClient client = DoeClientUtil.getWebClient(requestUrl);
				client.header("Authorization", "Bearer " + authToken);
				Response restResponse = client.invoke("GET", null);

				if (restResponse.getStatus() == 200) {

					modelAnalysisFiles = processGeneratedPredDataObjects(restResponse, loggedOnUserFolderPath, session);
					return new ResponseEntity<>(modelAnalysisFiles, HttpStatus.OK);

				} else if (restResponse.getStatus() == 204) {

					return new ResponseEntity<>(modelAnalysisFiles, HttpStatus.OK);

				}

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

	private DoeDatafileSearchResultDetailed getAssetFile(gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectDTO result,
			List<String> systemAttrs) {

		DoeDatafileSearchResultDetailed returnResult = new DoeDatafileSearchResultDetailed();

		returnResult.setSelfMetadata(getUserMetadata(
				result.getMetadataEntries().getSelfMetadataEntries().getUserMetadataEntries(), null, systemAttrs));
		returnResult.setSystemMetadata(getSystemMetaData(
				result.getMetadataEntries().getSelfMetadataEntries().getSystemMetadataEntries(), null, systemAttrs));

		return returnResult;

	}
}
