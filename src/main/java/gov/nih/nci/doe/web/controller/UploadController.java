package gov.nih.nci.doe.web.controller;

import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.lang.StringUtils;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.UploadCollectionModel;
import gov.nih.nci.doe.web.service.DoeAuthorizationService;
import gov.nih.nci.doe.web.util.MiscUtil;

@CrossOrigin
@Controller
@EnableAutoConfiguration
@RequestMapping("/upload")
public class UploadController extends AbstractDoeController {

	@GetMapping
	public ResponseEntity<?> home(HttpSession session, @RequestHeader HttpHeaders headers,
			UploadCollectionModel uploadCollectionModel) throws Exception {

		log.info("upload controller");
		String user = getLoggedOnUserInfo();
		if (StringUtils.isEmpty(user)) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		session.setAttribute("basePathSelected", basePath);
		session.removeAttribute("GlobusEndpoint");
		session.removeAttribute("includeCriteria");
		session.removeAttribute("GlobusEndpointPath");
		session.removeAttribute("GlobusEndpointFiles");
		session.removeAttribute("GlobusEndpointFolders");
		session.setAttribute("datafilePath", uploadCollectionModel.getDataSetPath());
		session.setAttribute("programPath", uploadCollectionModel.getProgramPath());
		session.setAttribute("studyPath", uploadCollectionModel.getStudyPath());
		session.setAttribute("uploadPath", uploadCollectionModel.getUploadPath());
		session.setAttribute("actionType", uploadCollectionModel.getAction());
		session.removeAttribute("fileIds");
		session.removeAttribute("folderIds");
		session.removeAttribute("accessToken");
		session.removeAttribute("authorized");
		session.removeAttribute("authorizedGC");

		if (uploadCollectionModel.getUploadType() != null
				&& uploadCollectionModel.getUploadType().equalsIgnoreCase("assetBulkUpload")) {
			session.setAttribute("bulkUploadCollection", "Asset");
		} else {
			session.removeAttribute("bulkUploadCollection");
		}

		if (uploadCollectionModel.getAction() != null
				&& uploadCollectionModel.getAction().equalsIgnoreCase(DoeAuthorizationService.GOOGLE_DRIVE_TYPE)) {

			String returnURL = this.webServerName + "/addbulk";
			try {
				return new ResponseEntity<>(
						doeAuthorizationService.authorize(returnURL, DoeAuthorizationService.ResourceType.GOOGLEDRIVE),
						HttpStatus.OK);
			} catch (Exception e) {
				throw new DoeWebException("Failed to redirect to Google for authorization: " + e.getMessage());
			}

		} else if (uploadCollectionModel.getAction() != null
				&& uploadCollectionModel.getAction().equalsIgnoreCase(DoeAuthorizationService.GOOGLE_CLOUD_TYPE)) {

			String returnURL = this.webServerName + "/addbulk";
			try {
				return new ResponseEntity<>(
						doeAuthorizationService.authorize(returnURL, DoeAuthorizationService.ResourceType.GOOGLECLOUD),
						HttpStatus.OK);
			} catch (Exception e) {
				throw new DoeWebException("Failed to redirect to Google for authorization: " + e.getMessage());
			}

		} else {
			try {
				final String percentEncodedReturnURL = MiscUtil.performUrlEncoding(this.webServerName + "/addbulk");

				return new ResponseEntity<>(
						"https://app.globus.org/file-manager?method=GET&action=" + percentEncodedReturnURL,
						HttpStatus.OK);
			} catch (Exception e) {
				throw new DoeWebException("Failed to redirect to Globus: " + e.getMessage());
			}

		}
	}
}
