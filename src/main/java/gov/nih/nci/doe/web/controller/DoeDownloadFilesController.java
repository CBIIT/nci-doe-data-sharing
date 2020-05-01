package gov.nih.nci.doe.web.controller;


import java.util.StringTokenizer;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.client.RestClientException;

import com.fasterxml.jackson.annotation.JsonView;

import gov.nih.nci.doe.web.model.AjaxResponseBody;
import gov.nih.nci.doe.web.model.DoeDownloadDatafile;
import gov.nih.nci.doe.web.model.Views;
import gov.nih.nci.doe.web.service.TaskManagerService;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.domain.datatransfer.HpcFileLocation;
import gov.nih.nci.hpc.domain.datatransfer.HpcGlobusDownloadDestination;
import gov.nih.nci.hpc.domain.datatransfer.HpcS3Account;
import gov.nih.nci.hpc.domain.datatransfer.HpcS3DownloadDestination;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectDownloadRequestDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcBulkDataObjectDownloadResponseDTO;



@Controller
@EnableAutoConfiguration
@RequestMapping("/downloadfiles")
public class DoeDownloadFilesController extends AbstractDoeController {
	@Value("${gov.nih.nci.hpc.server.v2.download}")
	private String downloadServiceURL;
	
	@Value("${gov.nih.nci.hpc.web.server}")
	private String webServerName;
	
	 @Autowired
	 TaskManagerService taskManagerService;


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
	@RequestMapping(value = "/download", method = RequestMethod.POST)
	@ResponseBody
	public AjaxResponseBody download(@RequestBody @Valid DoeDownloadDatafile downloadFile, 
	    HttpSession session, HttpServletRequest request, HttpServletResponse response) {
		AjaxResponseBody result = new AjaxResponseBody();
		try {
			String authToken = null;
			String loggedOnUser = getLoggedOnUserInfo();
			if(loggedOnUser != null && !StringUtils.isEmpty(loggedOnUser) && !StringUtils.isBlank(loggedOnUser)) {
				 authToken = (String) session.getAttribute("writeAccessUserToken");
			} else {
				 authToken = (String) session.getAttribute("hpcUserToken");
			}
			if (authToken == null) {
				result.setMessage("Invalid user session, expired. Please login again.");
				return result;
			}

			HpcBulkDataObjectDownloadRequestDTO dto = new HpcBulkDataObjectDownloadRequestDTO();
			String selectedPathsStr = String.join("  ", downloadFile.getSelectedPaths());
			String downloadType = downloadFile.getDownloadType();
			if (selectedPathsStr.isEmpty()) {
				result.setMessage("Data file list is missing!");
			} else {
				selectedPathsStr = selectedPathsStr.substring(1, selectedPathsStr.length() - 1);
				StringTokenizer tokens = new StringTokenizer(selectedPathsStr, ",");
				while (tokens.hasMoreTokens()) {
					if(downloadType.equals("datafiles"))
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
			}  else if (downloadFile.getSearchType() != null && downloadFile.getSearchType().equals("s3")) {
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
			}

			try {
				HpcBulkDataObjectDownloadResponseDTO downloadDTO = null;
				downloadDTO = DoeClientUtil
					.downloadFiles(authToken, downloadServiceURL, dto, sslCertPath, sslCertPassword);
				if (downloadDTO != null) {
					String taskId = downloadDTO.getTaskId();
					result.setMessage("Download request successful. Task Id: " +taskId);
					 if(loggedOnUser != null) {
				     taskManagerService.saveTransfer(taskId,"Download","Bulk Download Files",downloadType, getLoggedOnUserInfo());
					 }
				}
				return result; 
			} catch (Exception e) {
				result.setMessage("Download request is not successful: " + e.getMessage());
				return result;
			}
		} catch (HttpStatusCodeException e) {
			result.setMessage("Download request is not successful: " + e.getMessage());
			return result;
		} catch (RestClientException e) {
			result.setMessage("Download request is not successful: " + e.getMessage());
			return result;
		} catch (Exception e) {
			result.setMessage("Download request is not successful: " + e.getMessage());
			return result;
		}
	}
}
