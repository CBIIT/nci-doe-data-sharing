package gov.nih.nci.doe.web.controller;

import java.util.Date;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import com.fasterxml.jackson.annotation.JsonView;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.AjaxResponseBody;
import gov.nih.nci.doe.web.model.AuditingModel;
import gov.nih.nci.doe.web.model.DoeDownloadDatafile;
import gov.nih.nci.doe.web.model.Views;
import gov.nih.nci.doe.web.service.TaskManagerService;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.doe.web.util.MiscUtil;
import gov.nih.nci.hpc.domain.datatransfer.HpcDownloadTaskType;
import gov.nih.nci.hpc.domain.datatransfer.HpcFileLocation;
import gov.nih.nci.hpc.domain.datatransfer.HpcGlobusDownloadDestination;
import gov.nih.nci.hpc.domain.datatransfer.HpcS3Account;
import gov.nih.nci.hpc.domain.datatransfer.HpcS3DownloadDestination;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcDownloadRequestDTO;
import gov.nih.nci.hpc.domain.datatransfer.HpcGoogleDriveDownloadDestination;

import org.springframework.web.util.UriComponentsBuilder;

@Controller
@EnableAutoConfiguration
@RequestMapping("/download")
public class DoeDownloadController extends AbstractDoeController {

	@Value("${gov.nih.nci.hpc.server.v2.collection}")
	private String collectionServiceURL;
	@Autowired
	TaskManagerService taskManagerService;

	@Value("${gov.nih.nci.hpc.server.v2.dataObject}")
	private String dataObjectServiceURL;

	@GetMapping
	public ResponseEntity<?> home(Model model, @RequestParam(value = "type", required = false) String type,
			@RequestParam(value = "downloadFilePath", required = false) String downloadFilePath,
			@RequestParam(value = "action", required = false) String action, HttpSession session,
			HttpServletRequest request) throws DoeWebException {

		// String action = "Drive";
		String downloadType = request.getParameter("type");

		String code = request.getParameter("code");
		log.info("code from download" + code);
		if (code != null) {
			// Return from Google Drive Authorization
			downloadType = (String) session.getAttribute("downloadType");
			final String returnURL = this.webServerName + "/downloadTab";
			try {
				String accessToken = doeAuthorizationService.getToken(code, returnURL);
				session.setAttribute("accessToken", accessToken);
				model.addAttribute("accessToken", accessToken);
			} catch (Exception e) {
				throw new DoeWebException("Failed to redirect from Google for authorization: " + e.getMessage());
			}
			model.addAttribute("asyncSearchType", "drive");
			model.addAttribute("transferType", "drive");
			model.addAttribute("authorized", "true");

		}

		if (action.equals("Drive")) {
			session.setAttribute("downloadType", downloadType);
			String returnURL = this.webServerName + "/downloadTab";
			try {
				return new ResponseEntity<>(doeAuthorizationService.authorize(returnURL), HttpStatus.OK);
			} catch (Exception e) {
				throw new DoeWebException("Failed to redirect to Google for authorization: " + e.getMessage());
			}
		} else if (action.equals("Globus")) {
			session.setAttribute("downloadType", downloadType);
			try {
				final String percentEncodedReturnURL = MiscUtil.performUrlEncoding(this.webServerName + "/downloadTab");

				return new ResponseEntity<>(
						"https://app.globus.org/file-manager?method=GET&action=" + percentEncodedReturnURL,
						HttpStatus.OK);
			} catch (Exception e) {
				throw new DoeWebException("Failed to redirect to Globus: " + e.getMessage());
			}
		}

		return null;

	}

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
	@PostMapping
	@ResponseBody
	public AjaxResponseBody download(@RequestBody @Valid DoeDownloadDatafile downloadFile, HttpSession session,
			HttpServletRequest request, HttpServletResponse response) {
		log.info("download file" + downloadFile.getSelectedPaths());
		AjaxResponseBody result = new AjaxResponseBody();
		try {
			String authToken = null;
			String loggedOnUser = getLoggedOnUserInfo();
			String name = downloadFile.getDestinationPath()
					.substring(downloadFile.getDestinationPath().lastIndexOf('/') + 1);
			if (loggedOnUser != null && !StringUtils.isEmpty(loggedOnUser) && !StringUtils.isBlank(loggedOnUser)) {
				authToken = (String) session.getAttribute("hpcUserToken");
			}

			if (authToken == null) {
				result.setMessage("Invalid user session, expired. Login again.");
				return result;
			}
			final String basisURL = "collection".equals(downloadFile.getDownloadType()) ? this.collectionServiceURL
					: this.dataObjectServiceURL;
			final String serviceURL = UriComponentsBuilder.fromHttpUrl(basisURL).path("/{dme-archive-path}/download")
					.buildAndExpand(downloadFile.getDestinationPath()).encode().toUri().toURL().toExternalForm();
			log.info("download service url " + serviceURL);
			HpcDownloadRequestDTO dto = new HpcDownloadRequestDTO();
			if (downloadFile.getSearchType() != null && downloadFile.getSearchType().equals("async")) {
				HpcGlobusDownloadDestination destination = new HpcGlobusDownloadDestination();
				HpcFileLocation location = new HpcFileLocation();
				location.setFileContainerId(downloadFile.getEndPointName());
				location.setFileId(downloadFile.getEndPointLocation());
				destination.setDestinationLocation(location);
				dto.setGlobusDownloadDestination(destination);
			} else if (downloadFile.getSearchType() != null && downloadFile.getSearchType().equals("s3")) {
				HpcS3DownloadDestination destination = new HpcS3DownloadDestination();
				HpcFileLocation location = new HpcFileLocation();
				location.setFileContainerId(downloadFile.getBucketName());
				location.setFileId(downloadFile.getS3Path());
				destination.setDestinationLocation(location);
				HpcS3Account account = new HpcS3Account();
				account.setAccessKey(downloadFile.getAccessKey());
				account.setSecretKey(downloadFile.getSecretKey());
				account.setRegion(downloadFile.getRegion());
				destination.setAccount(account);
				dto.setS3DownloadDestination(destination);
			} else if (downloadFile.getSearchType() != null && downloadFile.getSearchType().equals("drive")) {
				String accessToken = (String) session.getAttribute("accessToken");
				HpcGoogleDriveDownloadDestination destination = new HpcGoogleDriveDownloadDestination();
				HpcFileLocation location = new HpcFileLocation();
				location.setFileContainerId("MyDrive");
				location.setFileId(downloadFile.getDrivePath().trim());
				destination.setDestinationLocation(location);
				destination.setAccessToken(accessToken);
				dto.setGoogleDriveDownloadDestination(destination);
			}
			final String downloadTaskType = "collection".equals(downloadFile.getDownloadType())
					? HpcDownloadTaskType.COLLECTION.name()
					: HpcDownloadTaskType.DATA_OBJECT.name();
			result = DoeClientUtil.downloadDataFile(authToken, serviceURL, dto, downloadTaskType, sslCertPath,
					sslCertPassword);

			String taskId = result.getMessage();
			// store the task ID in DB if logged on user exists
			if (loggedOnUser != null) {

				if (taskId != null && taskId.indexOf("Download request is not successful:") != -1) {
					result.setMessage(taskId);
				} else {
					taskManagerService.saveTransfer(taskId, "Download", downloadFile.getDownloadType(), name,
							getLoggedOnUserInfo());
					String transferType = downloadFile.getSearchType().equals("async") ? "Globus" : "S3";
					// store the auditing info
					AuditingModel audit = new AuditingModel();
					audit.setName(loggedOnUser);
					audit.setOperation("Download");
					audit.setStartTime(new Date());
					audit.setTransferType(transferType);
					audit.setPath(downloadFile.getDestinationPath());
					audit.setTaskId(taskId);
					auditingService.saveAuditInfo(audit);
					result.setMessage("Asynchronous download request is submitted successfully! Task ID: " + taskId);
				}

			}
			return result;
		} catch (DoeWebException e) {
			result.setMessage("Download request is not successful: " + e.getMessage());
			return result;
		} catch (Exception e) {
			log.error("Error in download request" + e.getMessage());
			result.setMessage("Download request is not successful");
			return result;
		}
	}
}
