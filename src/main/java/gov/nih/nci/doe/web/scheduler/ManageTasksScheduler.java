package gov.nih.nci.doe.web.scheduler;

import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.List;

import javax.ws.rs.core.Response;

import org.apache.commons.collections.CollectionUtils;
import org.apache.cxf.jaxrs.client.WebClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.util.UriComponentsBuilder;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.controller.AbstractDoeController;
import gov.nih.nci.doe.web.domain.Auditing;
import gov.nih.nci.doe.web.domain.InferencingTask;
import gov.nih.nci.doe.web.repository.AuditingRepository;
import gov.nih.nci.doe.web.repository.InferencingTaskRepository;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.domain.datatransfer.HpcFileLocation;
import gov.nih.nci.hpc.domain.datatransfer.HpcUploadSource;
import gov.nih.nci.hpc.domain.datatransfer.HpcUserDownloadRequest;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.dto.datamanagement.HpcDownloadSummaryDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationTaskDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcRegistrationSummaryDTO;

@Component
public class ManageTasksScheduler extends AbstractDoeController {

	@Value("${gov.nih.nci.hpc.server.download}")
	private String queryServiceURL;

	@Value("${gov.nih.nci.hpc.server.user.authenticate}")
	private String authenticateURL;

	@Value("${doe.writeaccount.password}")
	private String writeAccessUserPassword;

	@Value("${doe.writeaccount.username}")
	private String writeAccessUserName;

	@Autowired
	InferencingTaskRepository inferencingTaskRepository;

	@Autowired
	AuditingRepository auditingRepository;

	@Value("${gov.nih.nci.hpc.server.v2.bulkregistration}")
	private String registrationServiceV2URL;

	@Scheduled(cron = "${doe.scheduler.cron.auditing}")
	public void updateAuditingService() throws DoeWebException {

		String authToken = DoeClientUtil.getAuthenticationToken(writeAccessUserName, writeAccessUserPassword,
				authenticateURL);

		String serviceURL = queryServiceURL + "?page=" + 1 + "&totalCount=true";
		HpcDownloadSummaryDTO downloads = DoeClientUtil.getDownloadSummary(authToken, serviceURL);

		final MultiValueMap<String, String> paramsMap = new LinkedMultiValueMap<>();
		paramsMap.set("totalCount", Boolean.TRUE.toString());
		HpcRegistrationSummaryDTO registrations = DoeClientUtil.getRegistrationSummary(authToken,
				registrationServiceV2URL, paramsMap);

		List<HpcUserDownloadRequest> downloadResults = new ArrayList<HpcUserDownloadRequest>();
		if (downloads != null) {
			downloadResults.addAll(downloads.getActiveTasks());
			downloadResults.addAll(downloads.getCompletedTasks());
		}

		List<HpcBulkDataObjectRegistrationTaskDTO> uploadResults = new ArrayList<HpcBulkDataObjectRegistrationTaskDTO>();
		if (registrations != null) {
			uploadResults.addAll(registrations.getActiveTasks());
			uploadResults.addAll(registrations.getCompletedTasks());
		}

		List<Auditing> auditingTaskIds = auditingService.getAllTaskIds();

		if (CollectionUtils.isNotEmpty(auditingTaskIds)) {
			for (Auditing audit : auditingTaskIds) {
				if (audit.getOperation().equalsIgnoreCase("Upload")) {
					HpcBulkDataObjectRegistrationTaskDTO upload = uploadResults.stream()
							.filter(x -> audit.getTaskId().equals(x.getTaskId())).findAny().orElse(null);

					if (upload != null) {
						audit.setCompletionTime(
								(upload != null && upload.getCompleted() != null) ? upload.getCompleted().getTime()
										: null);
						if (upload.getResult() == null) {
							audit.setStatus("In progress");
						} else if (Boolean.TRUE.equals(upload.getResult())) {
							audit.setStatus("Completed");
						} else if (Boolean.FALSE.equals(upload.getResult())) {
							audit.setStatus("Failed");
							List<String> message = new ArrayList<String>();
							upload.getFailedItems().stream().forEach(x -> message.add(x.getMessage()));
							audit.setErrorMsg(message.get(0));
						}
						auditingRepository.save(audit);
					}
				} else if (audit.getOperation().equalsIgnoreCase("Download")) {
					HpcUserDownloadRequest download = downloadResults.stream()
							.filter(x -> audit.getTaskId().equals(x.getTaskId())).findAny().orElse(null);
					if (download != null) {
						audit.setCompletionTime((download != null && download.getCompleted() != null)
								? download.getCompleted().getTime()
								: null);
						if (download != null && download.getResult() != null
								&& download.getResult().value().equals("FAILED")) {
							List<String> message = new ArrayList<String>();
							download.getItems().stream().forEach(x -> message.add(x.getMessage()));
							audit.setStatus("Failed");
							audit.setErrorMsg(message.get(0));

						} else if (download.getResult() != null && download.getResult().value().equals("COMPLETED")) {
							audit.setStatus("Completed");
						} else {
							audit.setStatus("In Progress");
						}
						auditingRepository.save(audit);
					}
				}

			}
		}
	}

	// @Scheduled(cron = "${doe.scheduler.cron.infer}")
	public void performInferencing() throws DoeWebException, MalformedURLException {

		// get all not Started tasks to call the flask web service to perform
		// inferencing

		// the test data set mnt to cloudian
		log.info("scheduler for inferencing");
		List<InferencingTask> getAllNotStartedTasks = inferencingTaskRepository.getAllNotStartedTasks("NOTSTARTED");

		String authToken = DoeClientUtil.getAuthenticationToken(writeAccessUserName, writeAccessUserPassword,
				authenticateURL);
		for (InferencingTask t : getAllNotStartedTasks) {

			try {
				String fileNameOriginal = t.getTestDataSetPath().substring(t.getTestDataSetPath().lastIndexOf('/') + 1,
						t.getTestDataSetPath().length());
				// upload test dataset to cloudian
				log.info("upload test dataset to cloudian: " + fileNameOriginal);
				HpcBulkDataObjectRegistrationRequestDTO registrationDTO = constructV2BulkRequest(t.getUserId(),
						t.getTestDataSetPath(), fileNameOriginal, "input_dataset_" + t.getTaskId(), t.getUserId());

				// call the FileUpload API to upload the file to cloudian
				HpcBulkDataObjectRegistrationResponseDTO responseDTO = DoeClientUtil.registerBulkDatafiles(authToken,
						registrationServiceV2URL, registrationDTO);
				if (responseDTO != null) {
					log.info("dme task id for uplaoding test data set" + responseDTO.getTaskId());
				}

				String dataFilePath = t.getTestDataSetPath();
				String modelh5Path = t.getModelh5Path();
				String resultPath = t.getResultPath();

				String dataFileName = dataFilePath.substring(dataFilePath.lastIndexOf('/') + 1, dataFilePath.length());
				String modelName = modelh5Path.substring(modelh5Path.lastIndexOf('/') + 1, modelh5Path.length());
				String resultFileName = resultPath.substring(resultPath.lastIndexOf('/') + 1, resultPath.length());

				// call flask API
				log.info("call flask web service for " + dataFileName);
				String url = "http://127.0.0.1:5000/modac-routing";
				UriComponentsBuilder ucBuilder = UriComponentsBuilder.fromHttpUrl(url);

				ucBuilder.queryParam("dataFileName", dataFileName);
				ucBuilder.queryParam("modelName", modelName);
				ucBuilder.queryParam("resultFileName", resultFileName);

				final String requestURL = ucBuilder.build().encode().toUri().toURL().toExternalForm();

				WebClient client1 = DoeClientUtil.getWebClient(requestURL);
				Response restResponse1 = client1.invoke("GET", null);

				try {
					log.info("response from flask web service" + restResponse1);
					if (restResponse1.getStatus() == 200 || restResponse1.getStatus() == 201) {
						t.setStatus("INPROGRESS");
						inferencingTaskRepository.saveAndFlush(t);
					}
				} catch (Exception e) {
					log.error(e.getMessage());
				}
			} catch (Exception e) {
				log.error(e.getMessage());
			}

		}

		// verify all the in-progress tasks to check if the y_prediction file is
		// available to download.

		List<InferencingTask> getAllInProgressTasks = inferencingTaskRepository.getAllNotStartedTasks("INPROGRESS");

		for (InferencingTask t : getAllInProgressTasks) {
			// verify if the y_pred file is available in cloudian
			String resultPath = t.getResultPath();

			try {
				String fileNameOriginal = resultPath.substring(resultPath.lastIndexOf('/') + 1, resultPath.length());
				java.io.File file = new java.io.File("/mnt/iRODsScratch/ModaC/" + fileNameOriginal);

				log.info("verify if inferencing file is available on mount " + fileNameOriginal);
				if (file.length() != 0) {
					HpcBulkDataObjectRegistrationRequestDTO registrationDTO = constructV2BulkRequest(t.getUserId(),
							resultPath, fileNameOriginal, "y_pred_" + t.getTaskId(), t.getUserId());

					// call the FileUpload API to upload the file to cloudian
					HpcBulkDataObjectRegistrationResponseDTO responseDTO = DoeClientUtil
							.registerBulkDatafiles(authToken, registrationServiceV2URL, registrationDTO);
					if (responseDTO != null) {
						String dmeTaskId = responseDTO.getTaskId();
						t.setDmeTaskId(dmeTaskId);
						inferencingTaskRepository.saveAndFlush(t);
					}
				}
			} catch (Exception e) {
				log.error("Exception in verifying predictions file and uploading: " + e);
			}

		}
	}

	private gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO constructV2BulkRequest(
			String userId, String path, String fileName, String metadata, String user) {

		log.info("construct dto for fileName : " + fileName + " and path: " + path);
		gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO dto = new gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO();
		gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO file = new gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO();
		HpcUploadSource fileSystemUploadSource = new HpcUploadSource();
		HpcFileLocation location = new HpcFileLocation();
		location.setFileContainerId("at-s-is2s:/ifs/projects/IRODsTest/scratch");
		location.setFileId("/mnt/IRODsScratch/modac/" + fileName);
		fileSystemUploadSource.setSourceLocation(location);
		file.setFileSystemUploadSource(fileSystemUploadSource);
		file.setPath(path);
		HpcMetadataEntry e = new HpcMetadataEntry();

		e.setAttribute("Model_Analysis_type_name");
		e.setValue(metadata);
		HpcMetadataEntry e1 = new HpcMetadataEntry();

		e1.setAttribute("generate_pred_username");
		e1.setValue(user);

		file.getDataObjectMetadataEntries().add(e);
		file.getDataObjectMetadataEntries().add(e1);
		dto.getDataObjectRegistrationItems().add(file);
		return dto;
	}
}
