package gov.nih.nci.doe.web.controller;

import java.io.IOException;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.domain.InferencingTask;
import gov.nih.nci.doe.web.model.DoeSearch;

import gov.nih.nci.doe.web.model.PermissionsModel;
import gov.nih.nci.doe.web.model.SearchList;
import gov.nih.nci.doe.web.service.DoeAuthorizationService;

/**
 *
 * DOE Home Controller
 *
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/")
public class HomeController extends AbstractDoeController {

	@Value("${gov.nih.nci.hpc.server.collection}")
	private String serviceURL;

	@GetMapping
	public String homePage(HttpSession session, HttpServletRequest request) {

		log.info("home page");
		return "home/homeTab";

	}

	@GetMapping(value = "/searchTab")
	public String getSearchTab(Model model, HttpSession session, HttpServletRequest request,
			@RequestParam(value = "dme_data_id", required = false) String dmeDataId,
			@RequestParam(value = "doi", required = false) String doi,
			@RequestParam(value = "returnToSearch", required = false) String returnToSearch,
			@RequestParam(value = "keyWord", required = false) String keyWord) throws DoeWebException {

		log.info("Search tab");
		try {
			if (StringUtils.isNotEmpty(dmeDataId)) {
				model.addAttribute("dmeDataId", dmeDataId);
			}

			if (StringUtils.isNotEmpty(doi)) {
				model.addAttribute("doi", doi);
			}

			if (StringUtils.isNotEmpty(returnToSearch)) {
				String query = (String) session.getAttribute("searchQuery");
				log.info("searchQuery search tab" + query);
				model.addAttribute("searchQuery", query);
				model.addAttribute("returnToSearch", "true");
			}

			if (StringUtils.isNotEmpty(keyWord)) {
				model.addAttribute("keyWord", keyWord);
			}
			constructAllSearchCriteriaList(session, model);

			return "search/searchTab";
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			throw new DoeWebException("Failed to render search Tab " + e.getMessage());
		}
	}

	@GetMapping(value = "/statusTab")
	public String getTasksTab(Model model, @RequestParam(value = "isPred", required = false) String isPred,
			HttpSession session, HttpServletRequest request) throws DoeWebException {

		log.info("tasks Tab");
		String user = getLoggedOnUserInfo();
		if (StringUtils.isEmpty(user)) {
			return "redirect:/loginTab";
		}

		List<InferencingTask> getAllInferencingTasks = inferencingTaskService.getAllTaskByUserId(user);
		if (CollectionUtils.isNotEmpty(getAllInferencingTasks)) {
			model.addAttribute("showModelAnalysisTab", true);
			if (StringUtils.isNotEmpty(isPred)) {
				model.addAttribute("redirectToPredTab", true);
			}
		}

		return "status/statusTab";
	}

	@GetMapping(value = "/siteFeedback")
	public String getSiteFeedback(Model model, HttpSession session, HttpServletRequest request) {
		model.addAttribute("siteKey", siteKey);
		return "siteFeedbackTab";
	}

	@GetMapping(value = "/aboutTab")
	public String getAboutTab(HttpSession session, HttpServletRequest request) {
		return "about/aboutTab";
	}

	@GetMapping(value = "/downloadTab")
	public String getDownload(Model model, HttpSession session, HttpServletRequest request,
			@RequestParam(value = "selectedPaths", required = false) String selectedPaths,
			@RequestParam(value = "code", required = false) String code,
			@RequestParam(value = "endpoint_id", required = false) String endPointName,
			@RequestParam(value = "downloadAsyncType", required = false) String downloadAsyncType,
			@RequestParam(value = "fileName", required = false) String fileName,
			@RequestParam(value = "returnToSearch", required = false) String returnToSearch,
			@RequestParam(value = "returnToStatus", required = false) String returnToStatus,
			@RequestParam(value = "assetIdentifier", required = false) String assetIdentifier,
			@RequestParam(value = "fileSize", required = false) String fileSize) throws DoeWebException {

		log.info("get download tab details");

		String user = getLoggedOnUserInfo();
		if (StringUtils.isEmpty(user)) {
			return "redirect:/loginTab";
		}
		model.addAttribute("selectedPathsString", selectedPaths);
		model.addAttribute("downloadAsyncType", downloadAsyncType);
		model.addAttribute("fileName", fileName);
		model.addAttribute("clientId", clientId);
		String identifier = (String) session.getAttribute("assetIdentifier");
		String returnSearch = (String) session.getAttribute("returnToSearch");

		if (StringUtils.isNotEmpty(assetIdentifier)) {
			model.addAttribute("assetIdentifier", assetIdentifier);
		} else if (StringUtils.isNotEmpty(identifier)) {
			model.addAttribute("assetIdentifier", identifier);
		}

		if (StringUtils.isNotEmpty(returnToSearch)) {
			model.addAttribute("returnToSearch", returnToSearch);
		} else if (StringUtils.isNotEmpty(returnSearch)) {
			model.addAttribute("returnToSearch", returnSearch);
		}
		if (StringUtils.isNotEmpty(selectedPaths)) {
			session.setAttribute("selectedPathsString", selectedPaths);
		}

		if (StringUtils.isNotEmpty(downloadAsyncType)) {
			session.setAttribute("downloadAsyncType", downloadAsyncType);
		}

		if (StringUtils.isNotEmpty(returnToStatus)) {
			model.addAttribute("returnToStatus", returnToStatus);
		} else if (session.getAttribute("returnToStatus") != null) {
			model.addAttribute("returnToStatus", session.getAttribute("returnToStatus"));
		}

		if (StringUtils.isNotEmpty(fileName)) {
			session.setAttribute("fileName", fileName);
		}

		if (StringUtils.isNotEmpty(fileSize)) {
			model.addAttribute("fileSize", fileSize);
		}

		selectedPaths = (String) session.getAttribute("selectedPathsString");
		downloadAsyncType = (String) session.getAttribute("downloadAsyncType");
		fileName = (String) session.getAttribute("fileName");

		if (code != null) {
			log.info("return Authorization code from google for download tab" + code);
			code = request.getParameter("code");
			if (code != null) {
				// Return from Google Drive Authorization
				String actionType = (String) session.getAttribute("actionType");
				final String returnURL = this.webServerName + "/downloadTab";
				if (actionType != null && actionType.equalsIgnoreCase(DoeAuthorizationService.GOOGLE_DRIVE_TYPE)) {
					try {

						String accessToken = doeAuthorizationService.getToken(code, returnURL,
								DoeAuthorizationService.ResourceType.GOOGLEDRIVE);
						log.info("access token for download tab" + accessToken);
						session.setAttribute("accessToken", accessToken);
						model.addAttribute("accessToken", accessToken);
						model.addAttribute("asyncSearchType", "drive");
						model.addAttribute("transferType", "drive");
						model.addAttribute("authorized", "true");
					} catch (Exception e) {
						throw new DoeWebException("Failed to redirect to Google for authorization: " + e.getMessage());
					}
				} else if (actionType != null
						&& actionType.equalsIgnoreCase(DoeAuthorizationService.GOOGLE_CLOUD_TYPE)) {
					try {

						String refreshTokenDetailsGoogleCloud = doeAuthorizationService.getRefreshToken(code, returnURL,
								DoeAuthorizationService.ResourceType.GOOGLECLOUD);
						session.setAttribute("refreshTokenDetailsGoogleCloud", refreshTokenDetailsGoogleCloud);
						model.addAttribute("authorizedGC", "true");
						model.addAttribute("asyncSearchType", "cloud");
						model.addAttribute("transferType", "cloud");
						model.addAttribute("refreshTokenDetailsGoogleCloud", refreshTokenDetailsGoogleCloud);
					} catch (Exception e) {
						throw new DoeWebException("Failed to redirect to Google for authorization: " + e.getMessage());
					}
				}
			}
		} else if (endPointName != null) {
			// This is return from Globus site
			model.addAttribute("endPointName", endPointName);
			String endPointLocation = request.getParameter("path");
			model.addAttribute("endPointLocation", endPointLocation);
			model.addAttribute("asyncSearchType", "async");
			model.addAttribute("transferType", "async");
		}

		model.addAttribute("selectedPathsString", selectedPaths);
		model.addAttribute("downloadAsyncType", downloadAsyncType);
		model.addAttribute("fileName", fileName);
		return "download/downloadTab";
	}

	@GetMapping(value = "/metaDataPermissionsList")
	public ResponseEntity<?> getPermissionsList(HttpSession session) {
		return getMetaDataPermissionsList(session, null);
	}

	@PostMapping(value = "/metaDataPermissionsList")
	@ResponseBody
	public String savePermissionsList(HttpSession session, @RequestHeader HttpHeaders headers,
			@RequestParam(value = "collectionId") String collectionId, @RequestParam(value = "path") String path,
			@RequestParam(value = "selectedPermissions[]", required = false) String[] selectedPermissions)
			throws DoeWebException {
		log.info("save metadata permissions");
		return saveMetaDataPermissionsList(collectionId, path, selectedPermissions);
	}

	@GetMapping(value = "/getPermissionByCollectionId")
	public ResponseEntity<?> getPermissionsByCollectionId(HttpSession session, @RequestHeader HttpHeaders headers,
			@RequestParam(value = "collectionId") String collectionId) {
		log.info("get meta data permissions list");
		return getMetadataPermissionsByCollectionId(collectionId);
	}

	@GetMapping(value = "/updateAccessGroupMetaData")
	public ResponseEntity<?> saveAccessGroup(HttpSession session, @RequestHeader HttpHeaders headers,
			PermissionsModel permissionGroups) throws DoeWebException {
		log.info("save access Groups");
		return saveCollectionAccessGroup(session, permissionGroups);

	}

	@GetMapping(value = "/getAccessgroups", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<?> getAccessgroups(@RequestParam(value = "selectedPath") String selectedPath,
			@RequestParam(value = "levelName") String levelName, HttpSession session, HttpServletRequest request,
			HttpServletResponse response) throws DoeWebException {
		log.info("get collection access groups");
		return getCollectionAccessGroups(session, selectedPath, levelName);
	}

	@GetMapping(value = "/getFilterList")
	public ResponseEntity<?> getFilterList(HttpSession session, @RequestHeader HttpHeaders headers, DoeSearch search)
			throws DoeWebException, IOException {

		log.info("get filtered list" + search);
		SearchList list = constructFilterCriteria(session, search);

		return new ResponseEntity<>(list, HttpStatus.OK);

	}
}
