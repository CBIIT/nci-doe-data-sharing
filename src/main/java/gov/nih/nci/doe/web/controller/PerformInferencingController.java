package gov.nih.nci.doe.web.controller;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;
import javax.servlet.http.HttpServletRequest;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.cxf.jaxrs.client.WebClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.util.UriComponentsBuilder;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.MappingJsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;

import javax.ws.rs.core.Response;
import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.domain.InferencingTask;
import gov.nih.nci.doe.web.domain.ModelInfo;
import gov.nih.nci.doe.web.model.DoeSearch;
import gov.nih.nci.doe.web.model.InferencingTaskModel;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.domain.datamanagement.HpcCollectionListingEntry;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectDownloadResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationStatusDTO;

@Controller
@EnableAutoConfiguration
@RequestMapping("/performInferencing")
public class PerformInferencingController extends AbstractDoeController {

	@Value("${gov.nih.nci.hpc.server.v2.bulkregistration}")
	private String bulkRegistrationURL;

	@Value("${gov.nih.nci.hpc.server.dataObject}")
	private String dataObjectServiceURL;

	/*
	 * Get all reference data sets wi
	 */
	@GetMapping(value = "/getReferenceDatasets")
	public ResponseEntity<?> getReferenceDatasets(@RequestParam(value = "assetPath") String assetPath,
			HttpSession session, @RequestHeader HttpHeaders headers) throws DoeWebException {

		log.info("get reference datasets list" + assetPath);
		List<KeyValueBean> referenceDatasetList = getAllReferenceDatasetListForAssetPath(assetPath, session);

		return new ResponseEntity<>(referenceDatasetList, HttpStatus.OK);

	}

	@GetMapping
	public ResponseEntity<?> getInferencingTasks(HttpSession session, @RequestHeader HttpHeaders headers,
			HttpServletRequest request, @RequestParam(value = "userId") String userId)
			throws DoeWebException, JsonParseException, IOException {

		log.info("get all inferencing Tasks for user: " + userId);
		List<InferencingTask> getAllInferencingTasks = inferencingTaskService.getAllTaskByUserId(userId);
		if (CollectionUtils.isNotEmpty(getAllInferencingTasks)) {

			String authToken = (String) session.getAttribute("writeAccessUserToken");
			for (InferencingTask t : getAllInferencingTasks) {

				if (t.getDmeTaskId() != null && t.getStatus() != null && !t.getStatus().equalsIgnoreCase("COMPLETED")) {
					log.info("verify the status for : " + t.getTestDataSetPath());
					UriComponentsBuilder ucBuilder1 = UriComponentsBuilder.fromHttpUrl(bulkRegistrationURL)
							.path("/{dme-archive-path}");
					final String serviceURL = ucBuilder1.buildAndExpand(t.getDmeTaskId()).encode().toUri().toURL()
							.toExternalForm();

					WebClient client = DoeClientUtil.getWebClient(serviceURL);
					client.header("Authorization", "Bearer " + authToken);
					Response restResponse = client.invoke("GET", null);

					// if the file is available, call the flask web service
					if (restResponse.getStatus() == 200) {
						ObjectMapper mapper = new ObjectMapper();
						MappingJsonFactory factory = new MappingJsonFactory(mapper);
						JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
						HpcBulkDataObjectRegistrationStatusDTO dto = parser
								.readValueAs(HpcBulkDataObjectRegistrationStatusDTO.class);
						if (dto != null) {
							if (dto.getTask().getCompleted() != null) {
								t.setStatus("COMPLETED");
								t.setCompletedDate(dto.getTask().getCompleted().getTime());
								inferencingTaskService.save(t);
							}
						}
					}
				}

			}

		}
		return new ResponseEntity<>(getAllInferencingTasks, headers, HttpStatus.OK);
	}

	@PostMapping(value = "/performModelAnalysis")
	@ResponseBody
	public String performModelAnalysis(HttpSession session, @RequestHeader HttpHeaders headers,
			HttpServletRequest request, HttpServletResponse response, @Valid InferencingTaskModel inference)
			throws Exception {

		log.info("perform model analysis for reference dataset");

		String taskId = UUID.randomUUID().toString();
		String applicableModelNames = inference.getApplicableModelNames();

		// get the model info based on selected applicable model name
		ModelInfo modelInfo = modelInfoService.getModelInfo(applicableModelNames);
		// create a file name for y_pred file
		String resultPath = inference.getAssetPath() + "/y_pred_" + taskId + ".csv";

		String testInputName = inference.getTestInputPath()
				.substring(inference.getTestInputPath().lastIndexOf('/') + 1);
		String outcomeFileName = inference.getOutcomeFilePath()
				.substring(inference.getOutcomeFilePath().lastIndexOf('/') + 1);

		try {

			if (StringUtils.isNotEmpty(outcomeFileName)) {

				String status = uploadFileToMount(session, outcomeFileName, inference.getOutcomeFilePath());
				if ("SUCCESS".equalsIgnoreCase(status)) {

					inference.setOutcomeFileName(outcomeFileName);

				}

			}

			if (StringUtils.isNotEmpty(inference.getTestInputPath())) {
				// copy the reference dataset to mount location

				uploadFileToMount(session, testInputName, inference.getTestInputPath());

			}

			// save the inferencing task
			inference.setIsReferenceAsset(Boolean.TRUE);
			inference.setTaskId(taskId);
			inference.setModelPath(modelInfo.getModelPath());
			inference.setUserId(getLoggedOnUserInfo());
			inference.setResultPath(resultPath);
			inference.setTestInputPath(inference.getTestInputPath());
			inferencingTaskService.saveInferenceTask(inference);

			return "Perform model analysis task submitted. Your task id is " + taskId;
		} catch (Exception e) {
			log.error("Exception in performing model analysis: " + e);
			throw new DoeWebException("Exception in performing model analysis: " + e);
		}

	}

	@PostMapping
	@ResponseBody
	public String performInfer(@RequestParam("uploadTestInferFile") MultipartFile uploadTestInferFile,
			@RequestParam("uploadTestOutputFile") MultipartFile uploadTestOutputFile, HttpSession session,
			@RequestHeader HttpHeaders headers, HttpServletRequest request, @Valid InferencingTaskModel inference)
			throws Exception, IOException {

		log.info("perform inferencing for test dataset: " + uploadTestInferFile.getOriginalFilename());
		String outputFileName = uploadTestOutputFile != null ? uploadTestOutputFile.getOriginalFilename() : null;

		log.info("user provided output file to evaluate model :" + outputFileName);

		String user = getLoggedOnUserInfo();
		String referenceDatasets = inference.getReferenceDatasetList();
		String testInputPath = inference.getTestInputPath();
		try {
			if (StringUtils.isNotEmpty(referenceDatasets)) {
				String authToken = (String) session.getAttribute("hpcUserToken");
				/*
				 * When reference dataset option is selected, the test input path is the asset
				 * path
				 */

				List<String> referenceDatasetsList = Arrays.asList(referenceDatasets.split(","));
				for (String referenceDatasetPath : referenceDatasetsList) {
					// create a modac task Id
					String taskId = UUID.randomUUID().toString();
					InferencingTaskModel inferenceDataset = new InferencingTaskModel();
					// create a file name for y_pred file and append to the asset Path
					String resultPath = testInputPath + "/y_pred_" + taskId + ".csv";

					// get the files under each reference dataset

					HpcCollectionListDTO collectionDto = DoeClientUtil.getCollection(authToken, serviceURL,
							referenceDatasetPath, true);
					HpcCollectionDTO result = collectionDto.getCollections().get(0);
					String resultFileName = getAttributeValue("outcome_file_name",
							result.getMetadataEntries().getSelfMetadataEntries(), null);
					if (StringUtils.isEmpty(resultFileName)) {
						return "Outcome file not found in reference dataset";
					} else {
						List<HpcCollectionListingEntry> dataObjectsList = result.getCollection().getDataObjects();
						dataObjectsList.stream().forEach(e -> {
							String path = e.getPath();
							String name = path.substring(path.lastIndexOf('/') + 1);
							/*
							 * check for outcome file name and input dataset paths and upload to mount
							 */
							if (StringUtils.isNotEmpty(name) && name.contains(resultFileName)) {
								inferenceDataset.setOutcomeFileName(name);
								inferenceDataset.setOutcomeFilePath(path);

							} else {
								inferenceDataset.setTestInputPath(path);
							}

							try {
								uploadFileToMount(session, path, name);
							} catch (Exception e1) {
								// TODO Auto-generated catch block
								e1.printStackTrace();
							}
						});
						inferenceDataset.setResultPath(resultPath);
						inferenceDataset.setIsReferenceAsset(Boolean.FALSE);
						inferenceDataset.setAssetPath(testInputPath);
						inferenceDataset.setTaskId(taskId);
						inferenceDataset.setUserId(user);
						inferenceDataset.setModelPath(inference.getModelPath());
						inferenceDataset.setUploadFrom(inference.getUploadFrom());
					}

				}
				return "Perform inferencing task submitted.";
			} else {

				/*
				 * When the input file is uploaded, the test input path is the path of the file
				 * which is used to generate predictions
				 */

				String parentPath = testInputPath != null ? testInputPath.substring(0, testInputPath.lastIndexOf('/'))
						: null;
				// create a modac task Id
				String taskId = UUID.randomUUID().toString();

				// create a file name for y_pred file and append to the asset Path
				String resultPath = parentPath + "/y_pred_" + taskId + ".csv";

				String testInputName = testInputPath.substring(testInputPath.lastIndexOf('/') + 1);

				// check if the file name is already used for inferencing for the same user and
				// same model path and is not in failed status
				if (Boolean.TRUE
						.equals(inferencingTaskService.checkifFileExistsForUser(user, parentPath, testInputName))) {
					return "Input file name already exists";
				}

				if (StringUtils.isNotEmpty(outputFileName)) {

					inference.setOutcomeFileName(outputFileName);
					Files.copy(uploadTestOutputFile.getInputStream(), Paths.get(uploadPath + outputFileName),
							StandardCopyOption.REPLACE_EXISTING);
				}

				// save the inferencing task
				inference.setIsReferenceAsset(Boolean.FALSE);
				inference.setTaskId(taskId);
				inference.setResultPath(resultPath);
				inference.setAssetPath(parentPath);
				inference.setTestInputPath(testInputPath);
				inference.setUserId(user);

				inferencingTaskService.saveInferenceTask(inference);

				// copy the test dataset file to IRODsTest mount
				Files.copy(uploadTestInferFile.getInputStream(), Paths.get(uploadPath + testInputName),
						StandardCopyOption.REPLACE_EXISTING);

				return "Perform inferencing task submitted. Your task id is " + taskId;
			}

		} catch (Exception e) {
			log.error("Exception in performing inferencing: " + e);
			throw new DoeWebException("Exception in performing inferencing:" + e);
		}
	}

	public List<KeyValueBean> getAllReferenceDatasetListForAssetPath(String assetPath, HttpSession session)
			throws DoeWebException {
		/*
		 * get all reference datasets with is_reference_dataset metadata attr true and
		 * applicable model name matches asset path
		 */

		log.info("get all reference datasets with applicable model name metadata as: " + assetPath);
		DoeSearch search = new DoeSearch();
		String[] attrNames = { "collection_type", "asset_type", "is_reference_dataset", "applicable_model_name" };
		String[] attrValues = { "Asset", "Dataset", "Yes", "%" + assetPath + "%" };
		String[] levelValues = { "Asset", "Asset", "Asset", "Asset" };
		boolean[] isExcludeParentMetadata = { false, false, false, false };
		String[] rowIds = { "1", "2", "3", "4" };
		String[] operators = { "EQUAL", "EQUAL", "EQUAL", "LIKE" };

		search.setAttrName(attrNames);
		search.setAttrValue(attrValues);
		search.setLevel(levelValues);
		search.setIsExcludeParentMetadata(isExcludeParentMetadata);
		search.setRowId(rowIds);
		search.setOperator(operators);
		return getPathsForSearch(search, session);

	}

	private String uploadFileToMount(HttpSession session, String fileName, String filePath)
			throws DoeWebException, IOException {

		log.info("Upload file to mount location" + fileName);
		String authToken = (String) session.getAttribute("writeAccessUserToken");

		if (StringUtils.isNotEmpty(fileName)) {

			// copy the results file to mount location
			Response restResponseModelFile = DoeClientUtil.getPreSignedUrl(authToken, dataObjectServiceURL, filePath);

			log.info("rest response:" + restResponseModelFile.getStatus());
			if (restResponseModelFile.getStatus() == 200) {
				MappingJsonFactory factory = new MappingJsonFactory();
				JsonParser parser = factory.createParser((InputStream) restResponseModelFile.getEntity());
				HpcDataObjectDownloadResponseDTO dataObject = parser
						.readValueAs(HpcDataObjectDownloadResponseDTO.class);

				WebClient client = DoeClientUtil.getWebClient(dataObject.getDownloadRequestURL());
				Response restResponseForModelFileCopy = client.invoke("GET", null);

				Files.copy((InputStream) restResponseForModelFileCopy.getEntity(), Paths.get(uploadPath + fileName),
						StandardCopyOption.REPLACE_EXISTING);
				return "SUCCESS";

			}

		}

		return "FAILURE";
	}
}
