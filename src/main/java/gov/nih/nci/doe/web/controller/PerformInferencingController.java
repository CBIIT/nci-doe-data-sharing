package gov.nih.nci.doe.web.controller;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.collections.CollectionUtils;
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
import com.jcraft.jsch.ChannelSftp;

import javax.ws.rs.core.Response;
import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.domain.InferencingTask;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationStatusDTO;

@Controller
@EnableAutoConfiguration
@RequestMapping("/performInferencing")
public class PerformInferencingController extends AbstractDoeController {

	@Value("${gov.nih.nci.hpc.server.v2.bulkregistration}")
	private String bulkRegistrationURL;

	@Value("${upload.path}")
	private String uploadPath;

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

	@PostMapping
	@ResponseBody
	public String performInfer(@RequestParam("uploadTestInferFile") MultipartFile uploadTestInferFile,
			@RequestParam("testModelPath") String testModelPath, HttpSession session, HttpServletRequest request,
			HttpServletResponse response) throws Exception {

		log.info("perform inferencing");

		String modelh5Path = request.getParameter("modelPath");

		String parentPath = null;

		parentPath = testModelPath.substring(0, testModelPath.lastIndexOf('/'));

		String taskId = UUID.randomUUID().toString();

		String resultPath = parentPath + "/y_pred_" + taskId;

		try {
			// save the inferencing task
			inferencingTaskService.saveInferenceTask(getLoggedOnUserInfo(), taskId, parentPath, resultPath,
					testModelPath, modelh5Path);

			// copy the test dataset file to IRODsTest
			ChannelSftp channelSftp = setupJsch();
			channelSftp.connect();
			InputStream is = uploadTestInferFile.getInputStream();
			String remoteDir = "/mnt/IRODsTest/" + uploadTestInferFile.getOriginalFilename();

			channelSftp.put(is, remoteDir);

			channelSftp.exit();
			return "Perform Inferencing task Submitted. Your task Id is " + taskId;
		} catch (Exception e) {
			log.error("Exception in uploading inferencing file: " + e);
		}
		return "Error in submitting dataset for inferencing";
	}

}
