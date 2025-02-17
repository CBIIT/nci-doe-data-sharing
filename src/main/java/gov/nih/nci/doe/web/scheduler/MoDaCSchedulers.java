package gov.nih.nci.doe.web.scheduler;

import java.io.File;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import javax.ws.rs.core.Response;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.cxf.jaxrs.client.WebClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.util.UriComponentsBuilder;
import org.apache.commons.lang.StringEscapeUtils;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.MappingJsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.controller.AbstractDoeController;
import gov.nih.nci.doe.web.domain.Auditing;
import gov.nih.nci.doe.web.domain.InferencingTask;
import gov.nih.nci.doe.web.domain.MetaDataPermissions;
import gov.nih.nci.doe.web.domain.TaskManager;
import gov.nih.nci.doe.web.model.PredictionTaskNotification;
import gov.nih.nci.doe.web.model.UploadTaskNotification;
import gov.nih.nci.doe.web.model.DownloadTaskNotification;
import gov.nih.nci.doe.web.repository.AuditingRepository;
import gov.nih.nci.doe.web.repository.InferencingTaskRepository;
import gov.nih.nci.doe.web.repository.MetaDataPermissionsRepository;
import gov.nih.nci.doe.web.repository.TaskManagerRepository;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.domain.datatransfer.HpcDownloadTaskType;
import gov.nih.nci.hpc.domain.datatransfer.HpcFileLocation;
import gov.nih.nci.hpc.domain.datatransfer.HpcUploadSource;
import gov.nih.nci.hpc.domain.datatransfer.HpcUserDownloadRequest;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDownloadStatusDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionRegistrationDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectDownloadStatusDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDownloadSummaryDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationStatusDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationTaskDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcRegistrationSummaryDTO;

public class MoDaCSchedulers extends AbstractDoeController {

	@Value("${gov.nih.nci.hpc.server.download}")
	private String queryServiceURL;

	@Value("${gov.nih.nci.hpc.server.user.authenticate}")
	private String authenticateURL;

	@Value("${doe.readonly.password}")
	private String readOnlyUserPassword;

	@Value("${doe.writeaccount.password}")
	private String writeAccessUserPassword;

	@Value("${doe.readonlyaccount.username}")
	private String readOnlyUserName;

	@Value("${doe.writeaccount.username}")
	private String writeAccessUserName;

	@Value("${modac.flask.webservice}")
	private String modacFlaskServer;

	@Autowired
	InferencingTaskRepository inferencingTaskRepository;

	@Autowired
	private MetaDataPermissionsRepository metaDataPermissionsRepository;

	@Autowired
	AuditingRepository auditingRepository;

	@Autowired
	TaskManagerRepository taskManagerRepository;

	public void init() {
		log.info("scheduler called");
	}

	SimpleDateFormat dateFormat = new SimpleDateFormat("MM-dd-yyyy HH:mm:ss");

	@Scheduled(cron = "${doe.scheduler.cron.notifications}")
	public void updateTaskStatusAndSendNotifications() throws DoeWebException {

		log.info("auditing service scheduler to update status for downloads and uploads");
		String authToken = DoeClientUtil.getAuthenticationToken(writeAccessUserName, writeAccessUserPassword,
				authenticateURL);
		String readToken = DoeClientUtil.getAuthenticationToken(readOnlyUserName, readOnlyUserPassword,
				authenticateURL);

		// update status for all in progress task Ids in auduting table
		List<Auditing> auditingTaskList = auditingService.getAllTaskIdsInprogress();

		// update status for all progress task Ids in task manager table
		List<TaskManager> taskList = taskManagerRepository.getAllTasksForNotification();

		String serviceURL = queryServiceURL + "?page=" + 1 + "&totalCount=true&pageSize=100";
		HpcDownloadSummaryDTO downloads = DoeClientUtil.getDownloadSummary(readToken, serviceURL);

		final MultiValueMap<String, String> paramsMap = new LinkedMultiValueMap<>();
		paramsMap.set("totalCount", Boolean.TRUE.toString());
		HpcRegistrationSummaryDTO registrations = DoeClientUtil.getRegistrationSummary(authToken, bulkRegistrationURL,
				paramsMap);

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

		if (CollectionUtils.isNotEmpty(auditingTaskList)) {
			for (Auditing audit : auditingTaskList) {
				try {

					if (audit.getOperation().equalsIgnoreCase("Upload")) {
						HpcBulkDataObjectRegistrationTaskDTO upload = uploadResults.stream()
								.filter(x -> audit.getTaskId().equals(x.getTaskId())).findAny().orElse(null);

						if (upload != null) {
							updateAuditRowForUpload(audit, upload);

						}
					} else if (audit.getOperation().equalsIgnoreCase("Download")) {
						HpcUserDownloadRequest download = downloadResults.stream()
								.filter(x -> audit.getTaskId().equals(x.getTaskId())).findAny().orElse(null);
						if (download != null) {
							updateAuditRowForDownload(audit, download);
						}
					}
				} catch (Exception e) {
					log.error("Failed to update auditing table" + e.getMessage());
				}
			}
		}

		if (CollectionUtils.isNotEmpty(taskList)) {
			for (TaskManager task : taskList) {
				try {

					if (task.getTaskType().equalsIgnoreCase("Upload")) {
						HpcBulkDataObjectRegistrationTaskDTO upload = uploadResults.stream()
								.filter(x -> task.getTaskId().equals(x.getTaskId())).findAny().orElse(null);

						if (upload != null) {
							updateTaskRowForUpload(task, upload);

						}
					} else if (task.getTaskType().equalsIgnoreCase("Download")) {
						HpcUserDownloadRequest download = downloadResults.stream()
								.filter(x -> task.getTaskId().equals(x.getTaskId())).findAny().orElse(null);
						if (download != null) {
							updateTaskRowForDownload(task, download, readToken);
						}
					}
				} catch (Exception e) {
					log.error("Failed to update task table" + e.getMessage());
				}
			}
		}

	}

	private void updateTaskRowForUpload(TaskManager task, HpcBulkDataObjectRegistrationTaskDTO upload)
			throws DoeWebException {

		String filteredMessage = "";
		Date completedDate = (upload != null && upload.getCompleted() != null) ? upload.getCompleted().getTime() : null;
		if (upload.getResult() == null) {
			task.setStatus("In progress");
		} else if (Boolean.TRUE.equals(upload.getResult())) {
			task.setStatus("Completed");
		} else if (Boolean.FALSE.equals(upload.getResult())) {
			task.setStatus("Failed");

			List<String> message = new ArrayList<String>();

			upload.getFailedItems().stream().forEach(x -> {
				if (x.getMessage() != null && !message.contains(x.getMessage())) {
					message.add(StringEscapeUtils.escapeHtml((x.getMessage().replaceAll("[\']", ""))));
				}
			});

			filteredMessage = CollectionUtils.isNotEmpty(message) ? String.join(",", message) : "";

		}
		taskManagerRepository.saveAndFlush(task);

		// send notification for completed and failed status
		if (task.getStatus().equalsIgnoreCase("Failed") || task.getStatus().equalsIgnoreCase("COMPLETED")
				|| task.getStatus().equalsIgnoreCase("CANCELLED")) {
			UploadTaskNotification notification = new UploadTaskNotification();
			notification.setTaskId(task.getTaskId());
			notification.setUserId(task.getUserId());
			notification.setErrorMsg(filteredMessage);
			if (completedDate != null) {
				String completedDateFormatted = dateFormat.format(completedDate);
				notification.setCompletedDate(completedDateFormatted);
			}

			notification.setStatus(getDisplayStatusInMailBody(task.getStatus()));
			notification.setDisplayStatus(getDisplayStatusInMailSubject(task.getStatus()));
			StringBuilder sourcePathBuilder = new StringBuilder();
			if (task.getStatus().equalsIgnoreCase("COMPLETED")) {
				upload.getCompletedItems().forEach(x -> {
					sourcePathBuilder.append(x.getPath()).append(", ");
				});

			} else {
				upload.getFailedItems().forEach(x -> {
					sourcePathBuilder.append(x.getPath()).append(", ");
				});

			}
			// Remove the trailing comma and space, if any
			if (sourcePathBuilder.length() > 0) {
				sourcePathBuilder.setLength(sourcePathBuilder.length() - 2);
			}

			String sourcePaths = sourcePathBuilder.toString();
			notification.setRegistrationItems(sourcePaths);

			String msg = mailService.sendUploadTaskNotification(notification);
			if ("SUCCESS".equalsIgnoreCase(msg)) {
				task.setIsNotified(Boolean.TRUE);
				taskManagerRepository.saveAndFlush(task);
			}
		}

	}

	private void updateTaskRowForDownload(TaskManager task, HpcUserDownloadRequest download, String token)
			throws DoeWebException {

		String filteredMessage = "";
		Date completedDate = (download != null && download.getCompleted() != null) ? download.getCompleted().getTime()
				: null;
		if (download != null && download.getResult() != null && download.getResult().value().equals("FAILED")) {

			task.setStatus("Failed");
			List<String> message = new ArrayList<String>();

			download.getItems().stream().forEach(x -> {
				if (x.getMessage() != null && !message.contains(x.getMessage())) {
					message.add(x.getMessage().replaceAll("[\']", ""));

				}
			});

			if (CollectionUtils.isEmpty(message)) {
				message.add(download.getResult() != null ? download.getResult().value().replaceAll("[\']", "") : null);
			}

			filteredMessage = CollectionUtils.isNotEmpty(message)
					? StringEscapeUtils.escapeHtml(String.join(",", message))
					: "";

		} else if (download.getResult() != null && download.getResult().value().equals("COMPLETED")) {
			task.setStatus("Completed");
		} else if (download.getResult() != null && download.getResult().value().equals("CANCELLED")) {
			task.setStatus("Cancelled");
		} else {
			task.setStatus("In Progress");
		}
		taskManagerRepository.saveAndFlush(task);

		// send notification for completed and failed status
		if (task.getStatus().equalsIgnoreCase("Failed") || task.getStatus().equalsIgnoreCase("COMPLETED")
				|| task.getStatus().equalsIgnoreCase("Cancelled")) {

			DownloadTaskNotification notification = new DownloadTaskNotification();

			StringBuilder sourcePathBuilder = new StringBuilder();
			download.getItems().forEach(x -> {
				sourcePathBuilder.append(x.getPath()).append(", ");
			});

			// Remove the trailing comma and space, if any
			if (sourcePathBuilder.length() > 0) {
				sourcePathBuilder.setLength(sourcePathBuilder.length() - 2);
			}

			String sourcePath = StringUtils.isNotEmpty(download.getPath()) ? download.getPath()
					: sourcePathBuilder.toString();

			notification.setTaskId(task.getTaskId());
			notification.setUserId(task.getUserId());
			notification.setErrorMsg(filteredMessage);
			if (completedDate != null) {
				String completedDateFormatted = dateFormat.format(completedDate);
				notification.setCompletedDate(completedDateFormatted);
			}

			notification.setStatus(getDisplayStatusInMailBody(task.getStatus()));
			notification.setDisplayStatus(getDisplayStatusInMailSubject(task.getStatus()));
			notification.setDestinationType(
					download.getDestinationType() != null ? download.getDestinationType().toString() : "");
			String downloadTaskType = "";

			if (download.getType().equals(HpcDownloadTaskType.COLLECTION)) {
				downloadTaskType = "Collection";
			} else if (download.getType().equals(HpcDownloadTaskType.COLLECTION_LIST)) {
				downloadTaskType = "Collections";
			} else if (download.getType().equals(HpcDownloadTaskType.DATA_OBJECT)) {
				downloadTaskType = "File";
			} else if (download.getType().equals(HpcDownloadTaskType.DATA_OBJECT_LIST)) {
				downloadTaskType = "Files";
			}
			notification.setDownloadType(downloadTaskType);
			notification.setSourcePath(sourcePath);

			// get target path from download status by taskId API /download/{taskId} or
			// /dataObject/download/{taskId}

			String targetPath = "";

			if ("File".equalsIgnoreCase(downloadTaskType)) {
				HpcDataObjectDownloadStatusDTO dataObjectStatusDto = DoeClientUtil.getDataObjectStatusTaskId(token,
						dataObjectDownloadServiceURL, task.getTaskId());
				if (dataObjectStatusDto != null) {
					HpcFileLocation destinationLoc = dataObjectStatusDto.getDestinationLocation();
					targetPath = getFileLocation(destinationLoc);
				}
			} else {

				String taskStatusUrl = "Collection".equalsIgnoreCase(downloadTaskType) ? collectionDownloadServiceURL
						: queryServiceURL;
				HpcCollectionDownloadStatusDTO dataObjectCollectionListStatusDto = DoeClientUtil
						.getDataObjectListOrCollectionStatusByTaskId(token, taskStatusUrl, task.getTaskId());
				HpcFileLocation destinationLoc = dataObjectCollectionListStatusDto.getDestinationLocation();
				targetPath = getFileLocation(destinationLoc);
			}

			notification.setTargetPath(targetPath);

			String msg = mailService.sendDownloadTaskNotification(notification);
			if ("SUCCESS".equalsIgnoreCase(msg)) {
				task.setIsNotified(Boolean.TRUE);
				taskManagerRepository.saveAndFlush(task);
			}
		}
	}

	private void updateAuditRowForUpload(Auditing audit, HpcBulkDataObjectRegistrationTaskDTO upload) {
		audit.setCompletionTime(
				(upload != null && upload.getCompleted() != null) ? upload.getCompleted().getTime() : null);
		if (upload.getResult() == null) {
			audit.setStatus("In progress");
		} else if (Boolean.TRUE.equals(upload.getResult())) {
			audit.setStatus("Completed");
		} else if (Boolean.FALSE.equals(upload.getResult())) {
			audit.setStatus("Failed");

			List<String> message = new ArrayList<String>();

			upload.getFailedItems().stream().forEach(x -> {
				if (x.getMessage() != null && !message.contains(x.getMessage())) {
					message.add(StringEscapeUtils.escapeHtml((x.getMessage().replaceAll("[\']", ""))));
				}
			});

			String filteredMessage = CollectionUtils.isNotEmpty(message) ? String.join(",", message) : "";
			audit.setErrorMsg(filteredMessage);
		}
		auditingRepository.saveAndFlush(audit);
	}

	private void updateAuditRowForDownload(Auditing audit, HpcUserDownloadRequest download) {
		audit.setCompletionTime(
				(download != null && download.getCompleted() != null) ? download.getCompleted().getTime() : null);
		if (download != null && download.getResult() != null && download.getResult().value().equals("FAILED")) {

			audit.setStatus("Failed");
			List<String> message = new ArrayList<String>();

			download.getItems().stream().forEach(x -> {
				if (x.getMessage() != null && !message.contains(x.getMessage())) {
					message.add(x.getMessage().replaceAll("[\']", ""));
				}
			});

			if (CollectionUtils.isEmpty(message)) {
				message.add(download.getResult() != null ? download.getResult().value().replaceAll("[\']", "") : null);
			}

			String filteredMessage = CollectionUtils.isNotEmpty(message)
					? StringEscapeUtils.escapeHtml(String.join(",", message))
					: "";

			audit.setErrorMsg(filteredMessage);

		} else if (download.getResult() != null && download.getResult().value().equals("COMPLETED")) {
			audit.setStatus("Completed");
		} else if (download.getResult() != null && download.getResult().value().equals("CANCELLED")) {
			audit.setStatus("Cancelled");
		} else {
			audit.setStatus("In Progress");
		}
		auditingRepository.saveAndFlush(audit);
	}

	@Scheduled(cron = "${doe.scheduler.cron.cleanup}")
	public void deleteInferncingFilesFromMount() throws DoeWebException {

		log.info("delete any intermediate files from mount when generating predictions");

		// retrieve all completed and failed inferencing tasks for the past 24 hours
		List<InferencingTask> getAllCompletedAndFailedTasks = inferencingTaskRepository.getAllCompletedAndFailedTasks();

		for (InferencingTask t : getAllCompletedAndFailedTasks) {

			log.info("verify the inferencing files for : " + t.getTestDataSetPath() + " and task Id is : "
					+ t.getTaskId());
			try {
				String dataFilePath = t.getTestDataSetPath();
				String resultPath = t.getResultPath();
				String outcomeFilePath = t.getActualResultsFileName();

				String dataFileName = dataFilePath != null
						? dataFilePath.substring(dataFilePath.lastIndexOf('/') + 1, dataFilePath.length())
						: null;
				String predFileName = resultPath != null
						? resultPath.substring(resultPath.lastIndexOf('/') + 1, resultPath.length())
						: null;

				String outcomeFileName = outcomeFilePath != null
						? outcomeFilePath.substring(outcomeFilePath.lastIndexOf('/') + 1, outcomeFilePath.length())
						: null;

				File predFile = new File(uploadPath + predFileName);
				File errorFile = new File(uploadPath + predFileName + "_error.txt");
				File outcomeFile = new File(uploadPath + outcomeFileName);
				File inputFile = new File(uploadPath + dataFileName);

				// verify and delete predictions file
				if (predFile.exists()) {
					if (predFile.delete()) {
						log.info("Pred file deleted successfully: " + predFile);
					} else {
						log.info("Failed to delete the pred file: " + predFile);
					}
				} else {
					log.info("Pred file does not exist: " + predFile);
				}

				// verify and delete error file
				if (errorFile.exists()) {
					if (errorFile.delete()) {
						log.info("error file deleted successfully: " + errorFile);
					} else {
						log.info("Failed to delete the error file: " + errorFile);
					}
				} else {
					log.info("error file does not exist: " + errorFile);
				}

				// verify and delete outcome file
				if (outcomeFile.exists()) {
					if (outcomeFile.delete()) {
						log.info("outcome file deleted successfully: " + outcomeFile);
					} else {
						log.info("Failed to delete the outcome file: " + outcomeFile);
					}
				} else {
					log.info("outcome file does not exist: " + outcomeFile);
				}

				// verify and delete input file
				if (inputFile.exists()) {
					if (inputFile.delete()) {
						log.info("inputFile file deleted successfully: " + inputFile);
					} else {
						log.info("Failed to delete the input file: " + inputFile);
					}
				} else {
					log.info("input file does not exist: " + inputFile);
				}

			} catch (Exception e) {
				log.error("Exception in deleting inferencing files from mount: " + e);
				throw new DoeWebException(e.getMessage());
			}
		}

	}

	@Scheduled(cron = "${doe.scheduler.cron.infer}")
	public void performInferencing() throws DoeWebException, MalformedURLException, ParseException {

		// get all not Started tasks to call the flask web service to perform
		// inferencing
		log.info("generate prediction scheduler");
		String authToken = DoeClientUtil.getAuthenticationToken(writeAccessUserName, writeAccessUserPassword,
				authenticateURL);
		List<InferencingTask> getAllNotStartedTasks = inferencingTaskRepository.getAllNotStartedTasks("NOTSTARTED");

		for (InferencingTask t : getAllNotStartedTasks) {
			log.info("call flask API for File Path: " + t.getTestDataSetPath());
			;
			try {
				String dataFilePath = t.getTestDataSetPath();
				String modelh5Path = t.getModelh5Path();
				String resultPath = t.getResultPath();

				String dataFileName = dataFilePath != null
						? dataFilePath.substring(dataFilePath.lastIndexOf('/') + 1, dataFilePath.length())
						: null;
				String modelName = modelh5Path != null
						? modelh5Path.substring(modelh5Path.lastIndexOf('/') + 1, modelh5Path.length())
						: null;
				String resultFileName = resultPath != null
						? resultPath.substring(resultPath.lastIndexOf('/') + 1, resultPath.length())
						: null;
				if (StringUtils.isNotEmpty(dataFileName) && StringUtils.isNotEmpty(modelName)
						&& StringUtils.isNotEmpty(resultFileName) && t.getBatchId() == null) {
					// call flask API for each not started task
					log.info("call flask web service for " + dataFileName);
					String url = modacFlaskServer + "modac-routing";
					UriComponentsBuilder ucBuilder = UriComponentsBuilder.fromHttpUrl(url);

					ucBuilder.queryParam("dataFileName", dataFileName);
					ucBuilder.queryParam("modelName", modelName);
					ucBuilder.queryParam("resultFileName", resultFileName);
					ucBuilder.queryParam("uploadFrom", t.getUploadFrom());
					ucBuilder.queryParam("outputResultsName",
							t.getActualResultsFileName() != null ? t.getActualResultsFileName() : "None");

					final String requestURL = ucBuilder.build().encode().toUri().toURL().toExternalForm();

					WebClient client1 = DoeClientUtil.getWebClient(requestURL);
					Response restResponse = client1.invoke("GET", null);

					try {
						log.info("response from flask web service" + restResponse);
						if (restResponse.getStatus() == 200 || restResponse.getStatus() == 201) {
							MappingJsonFactory factory = new MappingJsonFactory();
							JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
							String jobId = parser.readValueAs(String.class);
							if (StringUtils.isNotEmpty(jobId)) {
								t.setBatchId(jobId);
								t.setStatus("INPROGRESS");
								inferencingTaskRepository.saveAndFlush(t);
							}

						} else {
							String errorMessage = DoeClientUtil.getErrorMessage(restResponse);
							throw new DoeWebException(errorMessage, restResponse.getStatus());
						}
					} catch (Exception e) {
						throw new DoeWebException(e.getMessage());
					}
				}
			} catch (Exception e) {
				log.error(e.getMessage());
			}

		}

		// verify all the in-progress tasks with dmeTaskId is null to check if the
		// y_prediction file is
		// available to be uploaded to Cloudian.
		InProgressTaskForInferencing(authToken);

		// get All in-progress tasks with dmetaskId not null and completed tasks with
		// is_notified null to
		// send notification to user

		getTasksForSendingPredictionNotification(authToken);
	}

	@Scheduled(cron = "${doe.scheduler.collection.permissions}")
	public void updateCollectionPermissionsForBulkAssetUploads() throws DoeWebException {

		log.info("collection permissions update scheduler for tasks created during bulk asset uploads");

		// get all rows with empty collection Id and verify if collection exists
		List<MetaDataPermissions> collectionPermList = metaDataPermissionsRepository
				.getAllCollectionsWithEmptyCollectionId();
		if (CollectionUtils.isNotEmpty(collectionPermList)) {
			log.info("empty collection Id's exist");
			String authToken = DoeClientUtil.getAuthenticationToken(writeAccessUserName, writeAccessUserPassword,
					authenticateURL);
			for (MetaDataPermissions collection : collectionPermList) {
				log.info("verifying collection Id for path: " + collection.getCollectionPath());

				// find Collection path in task manager table and get task Id

				List<TaskManager> taskList = taskManagerRepository
						.findByPath("%" + collection.getCollectionPath() + "%");
				if (CollectionUtils.isNotEmpty(taskList)) {
					TaskManager task = taskList.get(0);
					try {
						if (task != null && ("COMPLETED".equalsIgnoreCase(task.getStatus())
								|| "FAILED".equalsIgnoreCase(task.getStatus()))) {

							/*
							 * If the task reaches terminal status, then get collection Id by path. Do this
							 * for both completed and failed status since failed task id can have more than
							 * one asset paths and some assets might get uploaded even though the task
							 * failed.
							 */

							HpcCollectionListDTO collectionDto = DoeClientUtil.getCollection(authToken, serviceURL,
									collection.getCollectionPath(), false);

							if (collectionDto != null && collectionDto.getCollections() != null
									&& CollectionUtils.isNotEmpty(collectionDto.getCollections())) {
								// collection exists
								log.info("updating collection Id for path:" + collection.getCollectionPath());
								HpcCollectionDTO savedCollectionDto = collectionDto.getCollections().get(0);
								Integer collectionId = savedCollectionDto.getCollection().getCollectionId();

								collection.setCollectionId(collectionId);
								metaDataPermissionsRepository.saveAndFlush(collection);

								String userID = collection.getUser() != null ? collection.getUser().getEmailAddrr()
										: null;

								// store the access_group metadata in MoDaC DB
								HpcMetadataEntry selectedEntry = savedCollectionDto.getMetadataEntries()
										.getSelfMetadataEntries().stream()
										.filter(e -> e.getAttribute().equalsIgnoreCase("access_group")).findAny()
										.orElse(null);

								if (selectedEntry != null) {
									accessGroupsService.saveAccessGroups(collectionId,
											savedCollectionDto.getCollection().getAbsolutePath(),
											selectedEntry.getValue(), userID);
								}
							} else {
								// delete the row in collection_permissions table since collection does not
								// exist
								metaDataPermissionService
										.deletePermissionByCollectionPath(collection.getCollectionPath());
							}

						}

					} catch (DoeWebException e) {

						log.debug("Error in getting collection: " + e.getMessage());
						// Delete the row in collection_permissions table since collection does not
						// exist
						try {
							metaDataPermissionService.deletePermissionByCollectionPath(collection.getCollectionPath());
							log.info("Collection deleted: " + collection.getCollectionPath());
						} catch (Exception ex) {
							log.error("Unexpected exception while deleting collection: " + ex.getMessage());
						}
					}
				}

			}

		}

	}

	private void InProgressTaskForInferencing(String authToken) throws DoeWebException {
		List<InferencingTask> getAllInProgressTasks = inferencingTaskRepository.getAllInProgressTasks("INPROGRESS");

		for (InferencingTask t : getAllInProgressTasks) {

			log.info("verifying inprogress task for" + t.getTestDataSetPath());
			// get the result path and verify if the y_pred file is available in cloudian
			String resultPath = t.getResultPath();
			String predFileName = resultPath.substring(resultPath.lastIndexOf('/') + 1, resultPath.length());
			String testDataSetFileName = t.getTestDataSetPath().substring(t.getTestDataSetPath().lastIndexOf('/') + 1,
					t.getTestDataSetPath().length());

			String outcomeFileName = t.getActualResultsFileName() != null ? t.getActualResultsFileName().substring(
					t.getActualResultsFileName().lastIndexOf('/') + 1, t.getActualResultsFileName().length()) : null;

			try {
				log.info("verify if inferencing file is available on mount " + predFileName);

				boolean check = new File(uploadPath + predFileName).exists();
				boolean isErrorFile = new File(uploadPath + predFileName + "_error.txt").exists();

				// if predictions file is available, create a collection folder under Asset and
				// upload
				// prediction under this folder
				if (Boolean.TRUE.equals(check)) {
					log.info("pred file available on mount: " + predFileName);
					String parentPath = t.getAssetPath();
					String folderPath = parentPath + "/Prediction_" + t.getTaskId();

					Boolean isFolderPathExists = false;

					try {
						HpcCollectionListDTO collection = DoeClientUtil.getCollection(authToken, serviceURL, folderPath,
								false, true);
						if (collection != null && collection.getCollections() != null
								&& CollectionUtils.isNotEmpty(collection.getCollections())) {
							log.info("Collection already exists: " + folderPath);
							isFolderPathExists = true;
						}
					} catch (DoeWebException e) {
						log.debug("Error in get collection of type folder" + e.getMessage());
					}

					if (Boolean.FALSE.equals(isFolderPathExists)) {

						log.info("Collection does not exist. creating collection of type Folder" + folderPath);
						HpcCollectionRegistrationDTO dto = new HpcCollectionRegistrationDTO();
						List<HpcMetadataEntry> metadataEntries = new ArrayList<>();

						HpcMetadataEntry entry = new HpcMetadataEntry();
						entry.setValue("Prediction");
						entry.setAttribute("collection_type");
						metadataEntries.add(entry);

						// add folder identifier metadata
						HpcMetadataEntry folderIdentifierMetadata = new HpcMetadataEntry();
						folderIdentifierMetadata.setValue("Prediction_" + t.getTaskId());
						folderIdentifierMetadata.setAttribute("prediction_identifier");
						metadataEntries.add(folderIdentifierMetadata);

						dto.getMetadataEntries().addAll(metadataEntries);
						Integer responseStatus = DoeClientUtil.updateCollection(authToken, serviceURL, dto, folderPath);
						if (responseStatus == 200 || responseStatus == 201) {
							log.info("collection created: " + "Prediction_" + t.getTaskId());
							// save folder permissions to MoDaC
							HpcCollectionListDTO collections = DoeClientUtil.getCollection(authToken, serviceURL,
									folderPath, false);

							if (collections != null && collections.getCollections() != null
									&& !CollectionUtils.isEmpty(collections.getCollections())) {

								HpcCollectionDTO collection = collections.getCollections().get(0);

								// save collection prediction permissions in MoDaC DB

								predictionAccessService.savePredictionAccess(t.getUserId(), null,
										collection.getCollection().getCollectionId(), folderPath);
							}
						}
					}

					if (t.getIsReferenceAsset() == null || Boolean.FALSE.equals(t.getIsReferenceAsset())) {

						// upload input dataset file
						log.info("upload test dataset to cloudian: " + testDataSetFileName);
						HpcBulkDataObjectRegistrationRequestDTO registrationDTO = constructV2BulkRequest(t.getUserId(),
								folderPath + "/" + testDataSetFileName, testDataSetFileName);

						// call the FileUpload API to upload the test dataset file to cloudian
						HpcBulkDataObjectRegistrationResponseDTO responseDTO = DoeClientUtil
								.registerBulkDatafiles(authToken, bulkRegistrationURL, registrationDTO);
						if (responseDTO != null) {
							log.info("dme task id for uplaoding test data set" + responseDTO.getTaskId());
							t.setTestDataSetPath(folderPath + "/" + testDataSetFileName);
						}

						// upload outcome file if exists to cloudian
						if (StringUtils.isNotEmpty(outcomeFileName)) {
							HpcBulkDataObjectRegistrationRequestDTO outcomeDTO = constructV2BulkRequest(t.getUserId(),
									folderPath + "/" + outcomeFileName, outcomeFileName);

							// call the FileUpload API to upload the test dataset file to cloudian
							HpcBulkDataObjectRegistrationResponseDTO outcomeResponseDto = DoeClientUtil
									.registerBulkDatafiles(authToken, bulkRegistrationURL, outcomeDTO);
							if (outcomeResponseDto != null) {
								log.info("dme task id for uplaoding test data set" + outcomeResponseDto.getTaskId());
								t.setOutcomeFilePath(folderPath + "/" + outcomeFileName);
							}
						}
					}

					// upload predictions file to cloudian

					HpcBulkDataObjectRegistrationRequestDTO registrationDTO1 = constructV2BulkRequest(t.getUserId(),
							folderPath + "/" + predFileName, predFileName);

					// call the FileUpload API to upload the pred file to cloudian
					HpcBulkDataObjectRegistrationResponseDTO responseDTO1 = DoeClientUtil
							.registerBulkDatafiles(authToken, bulkRegistrationURL, registrationDTO1);
					if (responseDTO1 != null) {
						String dmeTaskId = responseDTO1.getTaskId();
						t.setResultPath(folderPath + "/" + predFileName);
						t.setDmeTaskId(dmeTaskId);
						inferencingTaskRepository.saveAndFlush(t);
					}

				} else if (Boolean.TRUE.equals(isErrorFile)) {
					log.info("error file available on mount: " + predFileName + "_error.txt");
					t.setStatus("FAILED");
					// read contents of error file
					String content = Files.lines(Paths.get(uploadPath + predFileName + "_error.txt"))
							.collect(Collectors.joining(System.lineSeparator()));

					t.setErrorMessage(content);
					inferencingTaskRepository.saveAndFlush(t);

				}

			} catch (Exception e) {
				log.error("Exception in verifying predictions file and uploading: " + e);
				throw new DoeWebException(e.getMessage());
			}

		}
	}

	private void getTasksForSendingPredictionNotification(String authToken) throws DoeWebException {

		log.info(" get all Tasks for sending notification");
		List<InferencingTask> getTasksForSendingNotification = inferencingTaskRepository
				.getTasksForSendingNotification();

		for (InferencingTask t : getTasksForSendingNotification) {

			PredictionTaskNotification task = new PredictionTaskNotification();
			task.setUserId(t.getUserId());
			task.setResultPath(t.getResultPath());
			task.setInputDataset(t.getTestDataSetPath());
			task.setTaskId(t.getTaskId());

			String msg = "";

			if (t.getStatus().equalsIgnoreCase("INPROGRESS")) {
				try {
					log.info("verify the status for : " + t.getTestDataSetPath());
					UriComponentsBuilder ucBuilder1 = UriComponentsBuilder.fromHttpUrl(bulkRegistrationURL)
							.path("/{dme-archive-path}");
					final String serviceURL = ucBuilder1.buildAndExpand(t.getDmeTaskId()).encode().toUri().toURL()
							.toExternalForm();

					WebClient client = DoeClientUtil.getWebClient(serviceURL);
					client.header("Authorization", "Bearer " + authToken);
					Response restResponse = client.invoke("GET", null);

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
								inferencingTaskRepository.save(t);
								String completedDateFormatted = dateFormat.format(t.getCompletedDate());
								task.setCompletedDate(completedDateFormatted);
								task.setStatus(t.getStatus());
								msg = mailService.sendPredictionTaskNotification(task);

							}
						}
					}
				} catch (Exception e) {
					log.error("Exception in sending prediction notification: " + e);
					throw new DoeWebException(e.getMessage());
				}
			} else {
				String completedDateFormatted = dateFormat.format(t.getCompletedDate());
				task.setCompletedDate(completedDateFormatted);
				task.setStatus(getDisplayStatusInMailBody(t.getStatus()));
				task.setDisplayStatus(getDisplayStatusInMailSubject(t.getStatus()));
				msg = mailService.sendPredictionTaskNotification(task);

			}
			if (msg == "SUCCESS") {
				t.setIsNotified(Boolean.TRUE);
				inferencingTaskRepository.save(t);
			}
		}

	}

	private String getFileLocation(HpcFileLocation fileLocation) {
		StringBuilder fileLocationStr = new StringBuilder();
		if (!StringUtils.isEmpty(fileLocation.getFileContainerName())) {
			fileLocationStr.append(fileLocation.getFileContainerName() + ":");
		} else if (!StringUtils.isEmpty(fileLocation.getFileContainerId())) {
			fileLocationStr.append(fileLocation.getFileContainerId() + ":");
		}
		if (!StringUtils.isEmpty(fileLocation.getFileId())) {
			fileLocationStr.append(fileLocation.getFileId());
		}

		return fileLocationStr.toString();

	}

	private gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO constructV2BulkRequest(
			String userId, String path, String fileName) {

		log.info("construct dto for fileName : " + fileName + " and path: " + path);
		gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO dto = new gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO();
		gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO file = new gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO();
		HpcUploadSource fileSystemUploadSource = new HpcUploadSource();
		HpcFileLocation location = new HpcFileLocation();
		location.setFileContainerId(containerId);
		location.setFileId(uploadPath + fileName);
		fileSystemUploadSource.setSourceLocation(location);
		file.setFileSystemUploadSource(fileSystemUploadSource);
		file.setPath(path);

		dto.getDataObjectRegistrationItems().add(file);
		return dto;
	}

	private String getDisplayStatusInMailBody(String taskStatus) {
		String displayTaskStatus = "";
		switch (taskStatus.toLowerCase()) {
		case "completed":
			displayTaskStatus = "is completed";
			break;
		case "failed":
			displayTaskStatus = "failed";
			break;
		case "cancelled":
			displayTaskStatus = "is cancelled";
			break;
		default:
			displayTaskStatus = "is In Progress";
			break;
		}

		return displayTaskStatus;
	}

	private String getDisplayStatusInMailSubject(String taskStatus) {
		String displayTaskStatus = "";
		switch (taskStatus.toLowerCase()) {
		case "completed":
			displayTaskStatus = "Completed";
			break;
		case "failed":
			displayTaskStatus = "Failed";
			break;
		case "cancelled":
			displayTaskStatus = "Cancelled";
			break;
		default:
			displayTaskStatus = "In Progress";
			break;
		}

		return displayTaskStatus;
	}

}
