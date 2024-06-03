package gov.nih.nci.doe.web.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.apache.commons.lang3.StringUtils;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import gov.nih.nci.doe.web.domain.TaskManager;
import gov.nih.nci.doe.web.model.AjaxResponseBody;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.domain.datatransfer.HpcDownloadTaskType;

import gov.nih.nci.hpc.dto.datamanagement.HpcBulkDataObjectDownloadResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDownloadResponseDTO;

import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectDownloadResponseDTO;

@Controller
@EnableAutoConfiguration
@RequestMapping("/downloadtask")
public class DoeRetryDownloadTaskController extends AbstractDoeController {

	@Value("${gov.nih.nci.hpc.server.download}")
	private String dataObjectsDownloadServiceURL;

	@Value("${gov.nih.nci.hpc.server.collection.download}")
	private String collectionDownloadServiceURL;


	@PostMapping
	@ResponseBody
	public String retryDownload(@RequestParam String taskName, @RequestParam String taskId,
			@RequestParam String taskType, HttpSession session, HttpServletRequest request) {

		log.info("retry download for task id" + taskId + " and task name: " + taskName + " and task type: " + taskType);
		AjaxResponseBody result = new AjaxResponseBody();
		try {
			String authToken = null;
			String loggedOnUser = getLoggedOnUserInfo();
			authToken = (String) session.getAttribute("hpcUserToken");

			if (StringUtils.isEmpty(loggedOnUser) || StringUtils.isEmpty(authToken)) {
				return "redirect:/loginTab";
			}

			TaskManager task = taskManagerService.getLastestTaskById(taskId);

			if (taskType.equals(HpcDownloadTaskType.DATA_OBJECT_LIST.name())
					|| taskType.equals(HpcDownloadTaskType.COLLECTION_LIST.name())) {

				String queryServiceURL = dataObjectsDownloadServiceURL + "/" + taskId + "/retry";

				try {
					HpcBulkDataObjectDownloadResponseDTO downloadDTO = DoeClientUtil
							.retryBulkDataObjectDownloadTask(authToken, queryServiceURL);
					if (downloadDTO != null) {
						log.info("the task id after retry is: " + downloadDTO.getTaskId());
						taskManagerService.saveTransfer(downloadDTO.getTaskId(), "Download", taskType, taskName,
								getLoggedOnUserInfo(), task != null ? task.getPath() : null);
						result.setMessage(
								"Retry bulk download request successful. Task Id: " + downloadDTO.getTaskId());
					}
				} catch (Exception e) {
					log.error("Download request is not successful: " + e);
					result.setMessage("Retry bulk download request is not successful: " + e.getMessage());
				}

			} else if (taskType.equals(HpcDownloadTaskType.COLLECTION.name())) {
				String queryServiceURL = collectionDownloadServiceURL + "/" + taskId + "/retry";

				try {
					HpcCollectionDownloadResponseDTO downloadDTO = DoeClientUtil.retryCollectionDownloadTask(authToken,
							queryServiceURL);
					if (downloadDTO != null) {
						log.info("the task id after retry is: " + downloadDTO.getTaskId());
						taskManagerService.saveTransfer(downloadDTO.getTaskId(), "Download", taskType, taskName,
								getLoggedOnUserInfo(), task != null ? task.getPath() : null);
						result.setMessage("Retry request successful. Task Id: <a href='/tasksTab'>"
								+ downloadDTO.getTaskId() + "</a>");
					}

				} catch (Exception e) {
					log.error("Download request is not successful: " + e);
					result.setMessage(e.getMessage());
				}
			} else if (taskType.equals(HpcDownloadTaskType.DATA_OBJECT.name())) {
				String serviceURL = dataObjectDownloadServiceURL + "/" + taskId + "/retry";

				try {
					HpcDataObjectDownloadResponseDTO downloadDTO = DoeClientUtil.retryDataObjectDownloadTask(authToken,
							serviceURL);
					if (downloadDTO != null) {

						log.info("the task id after retry is: " + downloadDTO.getTaskId());
						taskManagerService.saveTransfer(downloadDTO.getTaskId(), "Download", taskType, taskName,
								getLoggedOnUserInfo(), task != null ? task.getPath() : null);
						result.setMessage("Retry request successful. Task Id: <a href='/tasksTab'>"
								+ downloadDTO.getTaskId() + "</a>");

					}
				} catch (Exception e) {
					log.error("Download request is not successful: " + e);
					result.setMessage(e.getMessage());
				}
			}

		} catch (Exception e) {
			log.error("Download request is not successful: " + e);
			result.setMessage("Download request is not successful: " + e.getMessage());

		}
		log.info("the return result message for the user is : " + result.getMessage());
		return result.getMessage();
	}
}
