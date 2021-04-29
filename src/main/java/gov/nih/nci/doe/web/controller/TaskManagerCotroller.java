package gov.nih.nci.doe.web.controller;

import javax.servlet.http.HttpSession;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.domain.TaskManager;
import gov.nih.nci.doe.web.model.TaskManagerDto;
import gov.nih.nci.doe.web.service.TaskManagerService;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.doe.web.util.LambdaUtils;
import gov.nih.nci.hpc.domain.datatransfer.HpcDataTransferType;
import gov.nih.nci.hpc.domain.datatransfer.HpcDownloadResult;
import gov.nih.nci.hpc.domain.datatransfer.HpcDownloadTaskType;
import gov.nih.nci.hpc.domain.datatransfer.HpcUserDownloadRequest;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDownloadStatusDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectDownloadStatusDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDownloadSummaryDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationStatusDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationTaskDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcRegistrationSummaryDTO;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

/**
 *
 * DOE Task Manager Controller
 *
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/tasks")
public class TaskManagerCotroller extends AbstractDoeController {

	@Autowired
	TaskManagerService taskManagerService;

	@Value("${gov.nih.nci.hpc.server.collection.download}")
	private String collectionDownloadServiceURL;

	@Value("${gov.nih.nci.hpc.server.download}")
	private String queryServiceURL;

	@Value("${gov.nih.nci.hpc.server.v2.bulkregistration}")
	private String registrationServiceUrl;

	@Value("${gov.nih.nci.hpc.server.dataObject.download}")
	private String dataObjectDownloadServiceURL;

	SimpleDateFormat format = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");

	@GetMapping
	public ResponseEntity<?> getStatus(HttpSession session, @RequestHeader HttpHeaders headers,
			HttpServletRequest request, @RequestParam(value = "userId") String userId) throws DoeWebException {

		log.info("get all tasks by user Id");
		String authToken = (String) session.getAttribute("writeAccessUserToken");
		String readToken = (String) session.getAttribute("hpcUserToken");
		try {
			List<TaskManager> results = new ArrayList<TaskManager>();
			results = taskManagerService.getAllByUserId(userId);
			List<String> taskIds = LambdaUtils.map(results, TaskManager::getTaskId);

			String serviceURL = queryServiceURL + "?page=" + 1 + "&totalCount=true";
			HpcDownloadSummaryDTO downloads = DoeClientUtil.getDownloadSummary(authToken, serviceURL, sslCertPath,
					sslCertPassword);

			HpcDownloadSummaryDTO downloads1 = DoeClientUtil.getDownloadSummary(readToken, serviceURL, sslCertPath,
					sslCertPassword);

			final MultiValueMap<String, String> paramsMap = new LinkedMultiValueMap<>();
			paramsMap.set("totalCount", Boolean.TRUE.toString());
			HpcRegistrationSummaryDTO registrations = DoeClientUtil.getRegistrationSummary(authToken,
					registrationServiceUrl, paramsMap, sslCertPath, sslCertPassword);

			// get the task end date

			List<HpcUserDownloadRequest> downloadResults = new ArrayList<HpcUserDownloadRequest>();
			if (downloads != null) {
				downloadResults.addAll(downloads.getActiveTasks());
				downloadResults.addAll(downloads.getCompletedTasks());
			}

			if (downloads1 != null) {
				downloadResults.addAll(downloads1.getActiveTasks());
				downloadResults.addAll(downloads1.getCompletedTasks());
			}

			List<HpcBulkDataObjectRegistrationTaskDTO> uploadResults = new ArrayList<HpcBulkDataObjectRegistrationTaskDTO>();
			if (registrations != null) {
				uploadResults.addAll(registrations.getActiveTasks());
				uploadResults.addAll(registrations.getCompletedTasks());
			}

			List<HpcUserDownloadRequest> finalTaskIds = LambdaUtils.filter(downloadResults,
					(HpcUserDownloadRequest n) -> taskIds.contains(n.getTaskId()));

			List<TaskManagerDto> taskResults = new ArrayList<TaskManagerDto>();

			for (HpcUserDownloadRequest download : finalTaskIds) {
				TaskManagerDto task = new TaskManagerDto();
				TaskManager t = results.stream().filter(x -> download.getTaskId().equals(x.getTaskId())).findAny()
						.orElse(null);
				String path = download.getPath();

				if ((StringUtils.isEmpty(path) || StringUtils.isBlank(path))
						&& CollectionUtils.isNotEmpty(download.getItems())) {
					path = download.getItems().get(0).getPath();
				}
				if (StringUtils.isNotEmpty(path)) {
					String[] collectionNames = path.split("/");
					task.setProgName(collectionNames[2]);
					task.setStudyName(collectionNames[3]);
					task.setDataSetName(collectionNames[4]);
				}

				if (StringUtils.isNotEmpty(task.getDataSetName()) && StringUtils.isNotEmpty(t.getTaskName())) {
					task.setTaskName("<a href=" + webUrl + "/assetDetails?assetIdentifier=" + task.getDataSetName()
							+ ">" + t.getTaskName() + "</a>");
				} else {
					task.setTaskName(t.getTaskName());
				}

				task.setTaskCreatedDate(t.getTaskDate());
				task.setTaskId(download.getTaskId());
				task.setTaskCompletedDate(
						download.getCompleted() != null ? format.format(download.getCompleted().getTime()) : "");

				task.setUserId(t.getUserId());
				task.setTaskType(t.getTaskType());
				if (download.getResult() == null) {
					task.setTransferStatus("In Progress");
				} else if (download.getResult() != null && download.getResult().value().equals("CANCELLED")) {
					task.setTransferStatus("Cancelled");

				} else if (download.getResult() != null && download.getResult().value().equals("COMPLETED")) {
					task.setTransferStatus("Completed");
				} else {
					retryDownLoadFunc(session, download, task, t);
				}

				taskResults.add(task);
			}

			// same for uploads

			List<HpcBulkDataObjectRegistrationTaskDTO> finalTaskUploadIds = LambdaUtils.filter(uploadResults,
					(HpcBulkDataObjectRegistrationTaskDTO n) -> taskIds.contains(n.getTaskId()));

			List<TaskManagerDto> uploadTaskResults = new ArrayList<TaskManagerDto>();

			for (HpcBulkDataObjectRegistrationTaskDTO upload : finalTaskUploadIds) {
				TaskManagerDto task = new TaskManagerDto();
				TaskManager t = results.stream().filter(x -> upload.getTaskId().equals(x.getTaskId())).findAny()
						.orElse(null);
				String path = null;
				task.setTaskId(upload.getTaskId());
				task.setTaskCreatedDate(t != null ? t.getTaskDate() : null);
				task.setTaskCompletedDate(
						upload.getCompleted() != null ? format.format(upload.getCompleted().getTime()) : "");

				task.setUserId(t != null ? t.getUserId() : "");
				task.setTaskType(t != null ? t.getTaskType() : "");
				if (upload.getResult() == null) {
					task.setTransferStatus("In Progress");
					path = upload.getInProgressItems().get(0).getPath();

				} else if (Boolean.TRUE.equals(upload.getResult())) {
					task.setTransferStatus("Completed");
					path = upload.getCompletedItems().get(0).getPath();
				} else if (Boolean.FALSE.equals(upload.getResult())) {
					retryUploadFunc(session, upload, task, t);
					path = upload.getFailedItems().get(0).getPath();

				}
				if (StringUtils.isNotEmpty(path)) {
					String[] collectionNames = path.split("/");
					task.setProgName(collectionNames[2]);
					task.setStudyName(collectionNames[3]);
					task.setDataSetName(collectionNames[4]);
				}

				if (StringUtils.isNotEmpty(task.getDataSetName()) && t != null
						&& StringUtils.isNotEmpty(t.getTaskName())) {
					task.setTaskName("<a href=" + webUrl + "/assetDetails?assetIdentifier=" + task.getDataSetName()
							+ ">" + t.getTaskName() + "</a>");
				} else {
					task.setTaskName(t != null ? t.getTaskName() : "");
				}

				uploadTaskResults.add(task);
			}

			taskResults.addAll(uploadTaskResults);

			return new ResponseEntity<>(taskResults, headers, HttpStatus.OK);

		} catch (Exception e) {
			throw new DoeWebException("Failed to get tasks " + e.getMessage());
		}

	}

	private void retryUploadFunc(HttpSession session, HpcBulkDataObjectRegistrationTaskDTO upload, TaskManagerDto t,
			TaskManager task) throws DoeWebException {

		String authToken = (String) session.getAttribute("writeAccessUserToken");
		List<String> message = new ArrayList<String>();
		upload.getFailedItems().stream().forEach(x -> message.add(x.getMessage()));

		HpcBulkDataObjectRegistrationStatusDTO uploadTask = DoeClientUtil.getDataObjectRegistrationTask(authToken,
				registrationServiceUrl, upload.getTaskId(), sslCertPath, sslCertPassword);

		Boolean retry = false;
		if (uploadTask != null && uploadTask.getTask() != null) {
			List<HpcDataObjectRegistrationItemDTO> failedRequests = uploadTask.getTask().getFailedItemsRequest();
			if (failedRequests != null && !failedRequests.isEmpty())
				retry = true;

			for (HpcDataObjectRegistrationItemDTO dto : failedRequests) {
				// Retry is not available for S3 based bulk requests
				if (dto.getGlobusUploadSource() == null) {
					retry = false;
					break;
				}
			}
			if (CollectionUtils.isEmpty(message)) {
				message.add(uploadTask.getTask().getMessage());
			}
		}

		if (Boolean.TRUE.equals(retry)) {

			t.setTransferStatus("Failed &nbsp;&nbsp; <i class='fas fa-info-circle failureMsg' data-toggle='tooltip' title='"
					+ String.join(",", message) + "'></i>"
					+ "<strong><a style='border: none;background-color: #F39530;height: 23px;width: 37px;border-radius: 11px;float: right;' class='btn btn-link btn-sm' aria-label='Retry Upload' href='#'"
					+ "onclick='retryUpload(\"" + upload.getTaskId() + "\" ,\"" + task.getTaskName() + "\")'>"
					+ "<img style='height: 13px;width: 13px;margin-top: -14px;' data-toggle='tooltip' title='Retry Upload' src='images/Status.refresh_icon-01.png' th:src='@{/images/Status.refresh_icon-01.png}' alt='Status refresh'></a></strong>");
		} else {
			t.setTransferStatus("Failed &nbsp;&nbsp; <i class='fas fa-info-circle failureMsg' data-toggle='tooltip' title='"
					+ String.join(",", message) + "'></i>");
		}
	}

	private void retryDownLoadFunc(HttpSession session, HpcUserDownloadRequest download, TaskManagerDto dto,
			TaskManager task) throws DoeWebException {

		String taskType = download.getType().name();
		String authToken = (String) session.getAttribute("writeAccessUserToken");
		String queryUrl = null;
		Boolean retry = true;
		List<String> message = new ArrayList<String>();
		download.getItems().stream().forEach(x -> message.add(x.getMessage()));

		if (taskType.equalsIgnoreCase(HpcDownloadTaskType.COLLECTION.name())
				|| taskType.equalsIgnoreCase(HpcDownloadTaskType.DATA_OBJECT_LIST.name())
				|| taskType.equalsIgnoreCase(HpcDownloadTaskType.COLLECTION_LIST.name())) {

			if (taskType.equalsIgnoreCase(HpcDownloadTaskType.COLLECTION.name()))
				queryUrl = collectionDownloadServiceURL + "/" + task.getTaskId();
			else
				queryUrl = queryServiceURL + "/" + task.getTaskId();
			HpcCollectionDownloadStatusDTO downloadTask = DoeClientUtil.getDataObjectsDownloadTask(authToken, queryUrl,
					sslCertPath, sslCertPassword);

			if (CollectionUtils.isEmpty(message)) {
				message.add(downloadTask.getMessage());
			}

			if (downloadTask != null
					&& (!CollectionUtils.isEmpty(downloadTask.getFailedItems())
							|| !CollectionUtils.isEmpty(downloadTask.getCanceledItems()))
					&& downloadTask.getDestinationType() != null
					&& (downloadTask.getDestinationType().equals(HpcDataTransferType.S_3)
							|| downloadTask.getDestinationType().equals(HpcDataTransferType.GOOGLE_DRIVE))) {
				retry = false;
			}
		} else if (taskType.equals(HpcDownloadTaskType.DATA_OBJECT.name())) {

			queryUrl = dataObjectDownloadServiceURL + "/" + task.getTaskId();
			HpcDataObjectDownloadStatusDTO downloadTask = DoeClientUtil.getDataObjectDownloadTask(authToken, queryUrl,
					sslCertPath, sslCertPassword);
			if (CollectionUtils.isEmpty(message)) {
				message.add(downloadTask.getMessage());
			}
			if (downloadTask.getResult() != null && !downloadTask.getResult().equals(HpcDownloadResult.COMPLETED)) {
				if (downloadTask.getDestinationType() != null
						&& downloadTask.getDestinationType().equals(HpcDataTransferType.S_3)
						|| downloadTask.getDestinationType().equals(HpcDataTransferType.GOOGLE_DRIVE)) {
					retry = false;
				}
			} else {
				retry = false;
			}
		}

		if (Boolean.TRUE.equals(retry)) {

			dto.setTransferStatus("Failed &nbsp;&nbsp; <i class='fas fa-info-circle failureMsg' data-toggle='tooltip' title='"
					+ String.join(",", message) + "'></i>"
					+ "<strong><a style='border: none;background-color: #F39530; height: 23px;width: 37px;border-radius: 11px;float: right;' class='btn btn-link btn-sm' aria-label='Retry download' href='#' "
					+ "onclick='retryDownload(\"" + download.getTaskId() + "\" ,\"" + task.getTaskName() + "\", \""
					+ download.getType().name() + "\")'>"
					+ "<img style='height: 13px;width: 13px;margin-top: -14px;' data-toggle='tooltip' title='Retry Download' src='images/Status.refresh_icon-01.png' th:src='@{/images/Status.refresh_icon-01.png}' alt='Status refresh'></a></strong>");

		} else {
			dto.setTransferStatus("Failed &nbsp;&nbsp; <i class='fas fa-info-circle failureMsg' data-toggle='tooltip' title='"
					+ String.join(",", message) + "'></i>");
		}

	}

}
