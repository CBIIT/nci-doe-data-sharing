package gov.nih.nci.doe.web.controller;

import javax.servlet.http.HttpSession;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;
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
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.doe.web.util.LambdaUtils;
import gov.nih.nci.hpc.domain.datatransfer.HpcDataTransferType;
import gov.nih.nci.hpc.domain.datatransfer.HpcDataTransferUploadMethod;
import gov.nih.nci.hpc.domain.datatransfer.HpcUserDownloadRequest;
import gov.nih.nci.hpc.dto.datamanagement.HpcDownloadSummaryDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationTaskDTO;
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


	@Value("${gov.nih.nci.hpc.server.download}")
	private String queryServiceURL;

	SimpleDateFormat format = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");

	@GetMapping
	public ResponseEntity<?> getStatus(HttpSession session, @RequestHeader HttpHeaders headers,
			HttpServletRequest request, @RequestParam(value = "showAll", required = false) String showAll)
			throws DoeWebException {

		log.info("get all tasks by user Id");
		String authToken = (String) session.getAttribute("writeAccessUserToken");
		String readToken = (String) session.getAttribute("hpcUserToken");
		try {

			String userId = getLoggedOnUserInfo();
			if (StringUtils.isEmpty(userId)) {
				return new ResponseEntity<>("loginTab", HttpStatus.OK);
			}
			List<TaskManager> results = new ArrayList<TaskManager>();

			if (Boolean.TRUE.equals(getIsAdmin(session)) && "true".equalsIgnoreCase(showAll)) {
				results = taskManagerService.getAlltasks();
			} else {
				results = taskManagerService.getAllByUserId(userId);
			}

			List<String> taskIds = LambdaUtils.map(results, TaskManager::getTaskId);

			String serviceURL = queryServiceURL + "?page=" + 1 + "&totalCount=true&pageSize=100";

			HpcDownloadSummaryDTO downloads = DoeClientUtil.getDownloadSummary(readToken, serviceURL);

			final MultiValueMap<String, String> paramsMap = new LinkedMultiValueMap<>();
			paramsMap.set("totalCount", Boolean.TRUE.toString());
			HpcRegistrationSummaryDTO registrations = DoeClientUtil.getRegistrationSummary(authToken,
					bulkRegistrationURL, paramsMap);

			// get the task end date

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

			List<HpcUserDownloadRequest> finalTaskIds = LambdaUtils.filter(downloadResults,
					(HpcUserDownloadRequest n) -> taskIds.contains(n.getTaskId()));

			List<TaskManagerDto> taskResults = new ArrayList<TaskManagerDto>();

			for (HpcUserDownloadRequest download : finalTaskIds) {
				TaskManagerDto task = new TaskManagerDto();
				TaskManager t = results.stream().filter(x -> download.getTaskId().equals(x.getTaskId())).findAny()
						.orElse(null);

				task = updateDownloadTask(task, t, download);
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
				task = updateUploadTask(task, t, upload, session);

				uploadTaskResults.add(task);
			}

			taskResults.addAll(uploadTaskResults);

			return new ResponseEntity<>(taskResults, headers, HttpStatus.OK);

		} catch (Exception e) {
			throw new DoeWebException("Failed to get tasks " + e.getMessage());
		}

	}

	private TaskManagerDto updateDownloadTask(TaskManagerDto task, TaskManager t, HpcUserDownloadRequest download) {
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
			task.setTaskName("<a href=" + webServerName + "/assetDetails?assetIdentifier=" + task.getDataSetName() + ">"
					+ t.getTaskName() + "</a>");
		} else {
			task.setTaskName(t.getTaskName());
		}

		task.setTaskCreatedDate(t.getTaskDate());
		task.setTaskId(download.getTaskId());
		task.setTaskCompletedDate(
				download.getCompleted() != null ? format.format(download.getCompleted().getTime()) : "");

		task.setUserId(t.getUserId());
		task.setTaskType("&nbsp;&nbsp;" + t.getTaskType());
		if (download.getResult() == null) {
			task.setTransferStatus("&nbsp&nbsp;In Progress");
		} else if (download.getResult() != null && download.getResult().value().equals("CANCELLED")) {
			task.setTransferStatus("&nbsp&nbsp;Cancelled");

		} else if (download.getResult() != null && download.getResult().value().equals("COMPLETED")) {
			task.setTransferStatus("&nbsp&nbsp;Completed");
		} else {
			Boolean retry = false;
			if (download.getDestinationType() != null
					&& download.getDestinationType().equals(HpcDataTransferType.GLOBUS)) {
				retry = true;
			}

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

			if (Boolean.TRUE.equals(retry)) {

				task.setTransferStatus("&nbsp&nbsp;Failed&nbsp;&nbsp;<span class='button4a' error_msg = \""
						+ filteredMessage + "\" data-container='body' data-toggle='popover'"
						+ "data-placement='right' data-trigger='click' data-popover-content='#a02'><img style='width:17px;margin-top:-4px' src='images/infoIcon.svg' alt='failed message'></span>"
						+ "<strong><a style='border: none; padding-top:0px; height: 23px;width: 37px;border-radius: 11px;float: right;margin-right: 10px;' class='btn btn-link btn-sm' aria-label='Retry download' href='#' "
						+ "onclick='retryDownload(\"" + download.getTaskId() + "\" ,\"" + t.getTaskName() + "\", \""
						+ download.getType().name() + "\")'>"
						+ "<img style='height: 29px;width: 29px;margin-top: 2px;' data-toggle='tooltip' title='Retry Download' src='images/refresh-icon.svg' th:src='@{/images/refresh-icon.svg}' alt='Status refresh'></a></strong>");

			} else {
				task.setTransferStatus("&nbsp&nbsp;Failed&nbsp;&nbsp;<span class='button4a' error_msg = \""
						+ filteredMessage + "\" data-container='body' data-toggle='popover'"
						+ "data-placement='right' data-trigger='click' data-popover-content='#a02'><img style='width:17px;'"
						+ "src='images/infoIcon.svg'></span>");
			}
		}
		return task;
	}

	private TaskManagerDto updateUploadTask(TaskManagerDto task, TaskManager t,
			HpcBulkDataObjectRegistrationTaskDTO upload, HttpSession session) throws DoeWebException {
		String path = null;
		task.setTaskId(upload.getTaskId());
		task.setTaskCreatedDate(t != null ? t.getTaskDate() : null);
		task.setTaskCompletedDate(upload.getCompleted() != null ? format.format(upload.getCompleted().getTime()) : "");

		task.setUserId(t != null ? t.getUserId() : "");
		task.setTaskType(t != null ? "&nbsp;&nbsp;" + t.getTaskType() : "");
		if (upload.getResult() == null) {
			task.setTransferStatus("&nbsp&nbsp;In Progress");
			path = upload.getInProgressItems().get(0).getPath();

		} else if (Boolean.TRUE.equals(upload.getResult())) {
			task.setTransferStatus("&nbsp&nbsp;Completed");
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

		if (StringUtils.isNotEmpty(task.getDataSetName()) && t != null && StringUtils.isNotEmpty(t.getTaskName())) {
			task.setTaskName("<a href=" + webServerName + "/assetDetails?assetIdentifier=" + task.getDataSetName() + ">"
					+ t.getTaskName() + "</a>");
		} else {
			task.setTaskName(t != null ? t.getTaskName() : "");
		}

		return task;
	}

	private void retryUploadFunc(HttpSession session, HpcBulkDataObjectRegistrationTaskDTO upload, TaskManagerDto t,
			TaskManager task) throws DoeWebException {

		List<String> message = new ArrayList<String>();

		upload.getFailedItems().stream().forEach(x -> {
			if (x.getMessage() != null && !message.contains(x.getMessage())) {
				message.add(StringEscapeUtils.escapeHtml((x.getMessage().replaceAll("[\']", ""))));
			}
		});

		Boolean retry = false;
		if (upload != null && upload.getUploadMethod() != null
				&& upload.getUploadMethod().equals(HpcDataTransferUploadMethod.GLOBUS)) {
			retry = Boolean.TRUE;
		}

		String filteredMessage = CollectionUtils.isNotEmpty(message) ? String.join(",", message) : "";

		if (Boolean.TRUE.equals(retry)) {

			String taskName = (task.getTaskName() != null || !StringUtils.isEmpty(task.getTaskName()))
					? task.getTaskName()
					: "";

			t.setTransferStatus("&nbsp&nbsp;Failed&nbsp;&nbsp;<span class='button4a' error_msg = '" + filteredMessage
					+ "' data-container='body' data-toggle='popover'"
					+ "data-placement='right' data-trigger='click' data-popover-content='#a02'><img style='width:17px;margin-top:-4px'"
					+ "src='images/infoIcon.svg' alt='failed message'></span>"
					+ "<strong><a style='border: none;padding-top:0px;height: 23px;width: 37px;border-radius: 11px;float: right;margin-right: 10px;' class='btn btn-link btn-sm' aria-label='Retry Upload' href='#'"
					+ "onclick='retryUpload(\"" + upload.getTaskId() + "\" ,\"" + taskName + "\")'>"
					+ "<img style='height: 29px;width: 29px;margin-top: 2px;' data-toggle='tooltip' title='Retry Upload' src='images/refresh-icon.svg' th:src='@{/images/refresh-icon.svg}' alt='Status refresh'></a></strong>");
		} else {
			t.setTransferStatus("&nbsp&nbsp;Failed&nbsp;&nbsp;<span class='button4a' error_msg = '" + filteredMessage
					+ "' data-container='body' data-toggle='popover'"
					+ "data-placement='right' data-trigger='click' data-popover-content='#a02'><img style='width:17px;margin-top:-4px'"
					+ " src='images/infoIcon.svg' alt='failed message'></span>");
		}
	}

}
