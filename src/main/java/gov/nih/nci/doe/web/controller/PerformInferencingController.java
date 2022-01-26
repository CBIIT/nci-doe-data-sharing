package gov.nih.nci.doe.web.controller;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.List;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.FilenameUtils;
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
import gov.nih.nci.doe.web.model.InferencingTaskModel;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationStatusDTO;

@Controller
@EnableAutoConfiguration
@RequestMapping("/performInferencing")
public class PerformInferencingController extends AbstractDoeController {

	@Value("${gov.nih.nci.hpc.server.v2.bulkregistration}")
	private String bulkRegistrationURL;

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

				if (t.getStatus() != null && t.getStatus().equalsIgnoreCase("NOTSTARTED")) {
					t.setStatus("Not Started");
				} else if (t.getStatus() != null && t.getStatus().equalsIgnoreCase("INPROGRESS")) {
					t.setStatus("In Progress");
				} else if (t.getStatus() != null && t.getStatus().equalsIgnoreCase("COMPLETED")) {
					t.setStatus("Completed");
				} else if (t.getStatus() != null && t.getStatus().equalsIgnoreCase("FAILED")) {
					t.setStatus("Failed");
				}
			}

		}
		return new ResponseEntity<>(getAllInferencingTasks, headers, HttpStatus.OK);
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

		// create a modac task Id
		String taskId = UUID.randomUUID().toString();

		// create a file name for y_pred file and append to the asset Path
		String resultPath = parentPath + "/y_pred_" + taskId + ".csv";

		// create a unique file name for input path since the same file can be uploaded
		// multiple times by the user and causing an issue in inferencing script to
		// locate the file
		String testInputName = FilenameUtils.getBaseName(testInputPath);
		String testInputExt = FilenameUtils.getExtension(testInputPath);

		String updatedTestInputName = testInputName + "_" + taskId + "." + testInputExt;
		String updatedTestInputPath = parentPath + "/" + updatedTestInputName;

		try {
			// save the inferencing task
			inference.setTaskId(taskId);
			inference.setResultPath(resultPath);
			inference.setAssetPath(parentPath);
			inference.setTestInputPath(updatedTestInputPath);
			inference.setUserId(getLoggedOnUserInfo());
			inference.setOutputResultName(outputFileName);

			inferencingTaskService.saveInferenceTask(inference);

			// copy the test dataset file to IRODsTest mount
			Files.copy(uploadTestInferFile.getInputStream(), Paths.get(uploadPath + updatedTestInputName),
					StandardCopyOption.REPLACE_EXISTING);

			if (StringUtils.isNotEmpty(outputFileName)) {
				Files.copy(uploadTestOutputFile.getInputStream(),
						Paths.get(uploadPath + uploadTestOutputFile.getOriginalFilename()),
						StandardCopyOption.REPLACE_EXISTING);
			}

			return "Perform Inferencing task Submitted. Your task Id is " + taskId;
		} catch (Exception e) {
			log.error("Exception in uploading inferencing file: " + e);
			throw new DoeWebException("Exception in uploading inferencing file: " + e);
		}
	}

}
