package gov.nih.nci.doe.web.controller;

import java.util.Date;
import java.util.List;
import java.util.StringTokenizer;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import com.fasterxml.jackson.annotation.JsonView;

import gov.nih.nci.doe.web.domain.TaskManager;
import gov.nih.nci.doe.web.model.AjaxResponseBody;
import gov.nih.nci.doe.web.model.AuditingModel;
import gov.nih.nci.doe.web.model.DoeDownloadDatafile;
import gov.nih.nci.doe.web.model.Views;
import gov.nih.nci.doe.web.service.TaskManagerService;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.domain.datatransfer.HpcFileLocation;
import gov.nih.nci.hpc.domain.datatransfer.HpcGlobusDownloadDestination;
import gov.nih.nci.hpc.domain.datatransfer.HpcS3Account;
import gov.nih.nci.hpc.domain.datatransfer.HpcS3DownloadDestination;
import gov.nih.nci.hpc.domain.datatransfer.HpcGoogleDownloadDestination;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectDownloadRequestDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcBulkDataObjectDownloadResponseDTO;
import gov.nih.nci.doe.web.service.DoeAuthorizationService;

@Controller
@EnableAutoConfiguration
@RequestMapping("/downloadfiles")
public class DoeDownloadFilesController extends AbstractDoeController {
	@Value("${gov.nih.nci.hpc.server.v2.download}")
	private String downloadServiceURL;

	@Autowired
	TaskManagerService taskManagerService;

	@Autowired
	DoeAuthorizationService doeAuthorizationService;

	@Value("${doe.download.maxlimit}")
	private Integer maxLimit;

	/**
	 * POST action to initiate asynchronous download.
	 *
	 * @param downloadFile
	 * @param model
	 * @param bindingResult
	 * @param session
	 * @param request
	 * @param response
	 * @return
	 */
	@JsonView(Views.Public.class)
	@PostMapping(value = "/download")
	@ResponseBody
	public AjaxResponseBody download(@RequestBody @Valid DoeDownloadDatafile downloadFile, HttpSession session,
			HttpServletRequest request, HttpServletResponse response) {
		AjaxResponseBody result = new AjaxResponseBody();
		try {
			String authToken = null;
			String name = null;
			String loggedOnUser = getLoggedOnUserInfo();
			if (loggedOnUser != null && !StringUtils.isEmpty(loggedOnUser) && !StringUtils.isBlank(loggedOnUser)) {
				authToken = (String) session.getAttribute("hpcUserToken");
				String[] paths = downloadFile.getSelectedPaths().split(",");
				if (paths != null && paths.length == 1) {
					name = downloadFile.getSelectedPaths()
							.substring(downloadFile.getSelectedPaths().lastIndexOf('/') + 1);
					List<TaskManager> taskList = taskManagerService.getTaskDetails(loggedOnUser, name);
					if (CollectionUtils.isNotEmpty(taskList) && taskList.size() > maxLimit) {
						result.setMessage(" You have exceeded the maximum number of download attempts for this asset."
								+ " Please contact <a href='mailto:modac-support@mail.nih.gov?'>modac-support@mail.nih.gov</a> for assistance.");
						return result;
					}
				}
			}
			if (authToken == null) {
				result.setMessage("Invalid user session, expired. Login again.");
				return result;
			}

			HpcBulkDataObjectDownloadRequestDTO dto = new HpcBulkDataObjectDownloadRequestDTO();
			String downloadType = downloadFile.getDownloadType();

			if (downloadFile.getSelectedPaths().isEmpty()) {
				result.setMessage("Data file list is missing!");
			} else {
				StringTokenizer tokens = new StringTokenizer(downloadFile.getSelectedPaths(), ",");
				while (tokens.hasMoreTokens()) {
					if (downloadType.equals("datafiles"))
						dto.getDataObjectPaths().add(tokens.nextToken().trim());
					else
						dto.getCollectionPaths().add(tokens.nextToken().trim());
				}
			}

			if (downloadFile.getSearchType() != null && downloadFile.getSearchType().equals("async")) {
				HpcFileLocation location = new HpcFileLocation();
				location.setFileContainerId(downloadFile.getEndPointName());
				location.setFileId(downloadFile.getEndPointLocation());
				HpcGlobusDownloadDestination globusDownloadDestination = new HpcGlobusDownloadDestination();
				globusDownloadDestination.setDestinationLocation(location);
				dto.setGlobusDownloadDestination(globusDownloadDestination);
			} else if (downloadFile.getSearchType() != null && downloadFile.getSearchType().equals("s3")) {
				HpcFileLocation location = new HpcFileLocation();
				location.setFileContainerId(downloadFile.getBucketName());
				location.setFileId(downloadFile.getS3Path());
				HpcS3DownloadDestination destination = new HpcS3DownloadDestination();
				destination.setDestinationLocation(location);
				HpcS3Account account = new HpcS3Account();
				account.setAccessKey(downloadFile.getAccessKey());
				account.setSecretKey(downloadFile.getSecretKey());
				account.setRegion(downloadFile.getRegion());
				destination.setAccount(account);
				dto.setS3DownloadDestination(destination);
			} else if (downloadFile.getSearchType() != null
					&& downloadFile.getSearchType().equalsIgnoreCase(DoeAuthorizationService.GOOGLE_DRIVE_TYPE)) {
				String accessToken = (String) session.getAttribute("accessToken");
				HpcGoogleDownloadDestination destination = new HpcGoogleDownloadDestination();
				HpcFileLocation location = new HpcFileLocation();
				location.setFileContainerId("MyDrive");
				location.setFileId(downloadFile.getDrivePath().trim());
				destination.setDestinationLocation(location);
				destination.setAccessToken(accessToken);
				dto.setGoogleDriveDownloadDestination(destination);
			} else if (downloadFile.getSearchType() != null
					&& downloadFile.getSearchType().equalsIgnoreCase(DoeAuthorizationService.GOOGLE_CLOUD_TYPE)) {
				String refreshTokenDetailsGoogleCloud = (String) session.getAttribute("refreshTokenDetailsGoogleCloud");
				HpcGoogleDownloadDestination googleCloudDestination = new HpcGoogleDownloadDestination();
				HpcFileLocation location = new HpcFileLocation();
				location.setFileContainerId(downloadFile.getGoogleCloudBucketName());
				location.setFileId(downloadFile.getGoogleCloudPath().trim());
				googleCloudDestination.setDestinationLocation(location);
				googleCloudDestination.setAccessToken(refreshTokenDetailsGoogleCloud);
				dto.setGoogleCloudStorageDownloadDestination(googleCloudDestination);
			}

			HpcBulkDataObjectDownloadResponseDTO downloadDTO = null;
			downloadDTO = DoeClientUtil.downloadFiles(authToken, downloadServiceURL, dto);
			if (downloadDTO != null) {
				String taskId = downloadDTO.getTaskId();
				result.setMessage("Download request successful. Task ID: <a href='/tasksTab'>" + taskId + "</a>");
				if (loggedOnUser != null) {
					taskManagerService.saveTransfer(taskId, "Download", downloadFile.getSearchType(),
							name != null ? name : downloadType, getLoggedOnUserInfo());
					String transferType = downloadFile.getSearchType().equals("async") ? "Globus" : "S3";
					// store the auditing info
					AuditingModel audit = new AuditingModel();
					audit.setName(loggedOnUser);
					audit.setOperation("Download");
					audit.setStartTime(new Date());
					audit.setTransferType(transferType);
					audit.setPath(String.join(",  ", downloadFile.getSelectedPaths()));
					audit.setTaskId(taskId);
					auditingService.saveAuditInfo(audit);

				}
			}
			return result;
		} catch (Exception e) {
			result.setMessage("Download request is not successful: " + e.getMessage());
			return result;
		}
	}
}
