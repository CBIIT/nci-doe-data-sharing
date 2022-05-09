package gov.nih.nci.doe.web.controller;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
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
import gov.nih.nci.doe.web.model.InferencingTaskModel;
import gov.nih.nci.doe.web.util.DoeClientUtil;
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

		String authToken = (String) session.getAttribute("writeAccessUserToken");
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

				// copy the results file to mount location
				Response restResponseModelFile = DoeClientUtil.getPreSignedUrl(authToken, dataObjectServiceURL,
						inference.getOutcomeFilePath());

				log.info("rest response:" + restResponseModelFile.getStatus());
				if (restResponseModelFile.getStatus() == 200) {
					MappingJsonFactory factory = new MappingJsonFactory();
					JsonParser parser = factory.createParser((InputStream) restResponseModelFile.getEntity());
					HpcDataObjectDownloadResponseDTO dataObject = parser
							.readValueAs(HpcDataObjectDownloadResponseDTO.class);

					WebClient client = DoeClientUtil.getWebClient(dataObject.getDownloadRequestURL());
					Response restResponseForModelFileCopy = client.invoke("GET", null);

					Files.copy((InputStream) restResponseForModelFileCopy.getEntity(),
							Paths.get(uploadPath + outcomeFileName), StandardCopyOption.REPLACE_EXISTING);

					inference.setOutcomeFileName(outcomeFileName);

				}

			}

			if (StringUtils.isNotEmpty(inference.getTestInputPath())) {
				// copy the reference dataset to mount location
				Response restResponse = DoeClientUtil.getPreSignedUrl(authToken, dataObjectServiceURL,
						inference.getTestInputPath());

				log.info("rest response:" + restResponse.getStatus());
				if (restResponse.getStatus() == 200) {
					MappingJsonFactory factory = new MappingJsonFactory();
					JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
					HpcDataObjectDownloadResponseDTO dataObject = parser
							.readValueAs(HpcDataObjectDownloadResponseDTO.class);

					WebClient client = DoeClientUtil.getWebClient(dataObject.getDownloadRequestURL());
					Response restResponse1 = client.invoke("GET", null);

					Files.copy((InputStream) restResponse1.getEntity(), Paths.get(uploadPath + testInputName),
							StandardCopyOption.REPLACE_EXISTING);

				}
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
			throws Exception {

		log.info("perform inferencing for test dataset: " + uploadTestInferFile.getOriginalFilename());
		String outputFileName = uploadTestOutputFile != null ? uploadTestOutputFile.getOriginalFilename() : null;

		log.info("user provided output file to evaluate model :" + outputFileName);

		String testInputPath = inference.getTestInputPath();
		String parentPath = testInputPath != null ? testInputPath.substring(0, testInputPath.lastIndexOf('/')) : null;
		String user = getLoggedOnUserInfo();
		// create a modac task Id
		String taskId = UUID.randomUUID().toString();

		// create a file name for y_pred file and append to the asset Path
		String resultPath = parentPath + "/y_pred_" + taskId + ".csv";

		String testInputName = testInputPath.substring(testInputPath.lastIndexOf('/') + 1);

		// check if the file name is already used for inferencing for the same user and
		// same model path and is not in failed status
		if (Boolean.TRUE.equals(inferencingTaskService.checkifFileExistsForUser(user, parentPath, testInputName))) {
			return "Input file name already exists";
		}

		if (StringUtils.isNotEmpty(outputFileName)) {

			inference.setOutcomeFileName(outputFileName);
			Files.copy(uploadTestOutputFile.getInputStream(), Paths.get(uploadPath + outputFileName),
					StandardCopyOption.REPLACE_EXISTING);
		}

		try {
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
		} catch (Exception e) {
			log.error("Exception in uploading inferencing file: " + e);
			throw new DoeWebException("Exception in uploading inferencing file: " + e);
		}
	}

}
