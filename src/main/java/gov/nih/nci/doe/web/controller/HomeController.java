package gov.nih.nci.doe.web.controller;

import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

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
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.DoeSearch;
import gov.nih.nci.doe.web.model.DoeUsersModel;
import gov.nih.nci.doe.web.model.PermissionsModel;

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
	public String index(HttpSession session, HttpServletRequest request) {

		log.info("home page");
		return "home";

	}

	/**
	 * @param headers
	 * @return
	 */
	@GetMapping(value = "user-info")
	public ResponseEntity<?> getUserInfo(HttpSession session, @RequestHeader HttpHeaders headers,
			@RequestParam(value = "emailAddr") String emailAddr) {
		log.info("getting user info with email address " + emailAddr);
		try {
			DoeUsersModel user = authService.getUserInfo(emailAddr);
			return new ResponseEntity<>(user, headers, HttpStatus.OK);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			return new ResponseEntity<>(null, headers, HttpStatus.SERVICE_UNAVAILABLE);
		}
	}

	@PostMapping(value = "user-info")
	public ResponseEntity<?> updateUserInfo(@RequestBody DoeUsersModel doeModel, @RequestHeader HttpHeaders headers) {
		log.info("update user info for user " + doeModel.getEmailAddrr());
		try {
			if (doeModel.getEmailAddrr() != null) {
				authService.saveUserInfo(doeModel);
			}
			return new ResponseEntity<>("SUCCESS", HttpStatus.OK);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			return new ResponseEntity<>(e.getMessage(), HttpStatus.SERVICE_UNAVAILABLE);
		}

	}

	@GetMapping(value = "/searchTab")
	public String getSearchTab(Model model, HttpSession session, HttpServletRequest request,
			@RequestParam(value = "dme_data_id", required = false) String dmeDataId,
			@RequestParam(value = "doi", required = false) String doi,
			@RequestParam(value = "returnToSearch", required = false) String returnToSearch) throws DoeWebException {

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

		constructSearchCriteriaList(session, model);

		return "searchTab";
	}

	@GetMapping(value = "/tasksTab")
	public String getTasksTab(HttpSession session, HttpServletRequest request) {
		return "tasksTab";
	}

	@GetMapping(value = "/loginTab")
	public String getLoginTab(Model model, @RequestParam(value = "token", required = false) String token,
			@RequestParam(value = "email", required = false) String email) throws DoeWebException {

		try {
			if (StringUtils.isNotEmpty(token) && StringUtils.isNotEmpty(email)) {
				String status = authenticateService.confirmRegistration(token, email);
				if ("SUCCESS".equalsIgnoreCase(status)) {
					model.addAttribute("successMsg", "Thank you for registering. You may now log in.");
				}
			}
		} catch (Exception e) {
			throw new DoeWebException("Failed to send registration email" + e.getMessage());
		}

		return "loginTab";
	}

	@GetMapping(value = "/myaccount")
	public String getMyAccount(HttpSession session, HttpServletRequest request) {
		return "myAccount";
	}

	@GetMapping(value = "/resetPassword")
	public String getResetPassword(HttpSession session, HttpServletRequest request) {
		return "resetPassword";
	}

	@GetMapping(value = "/aboutTab")
	public String getAboutTab(HttpSession session, HttpServletRequest request) {
		return "aboutTab";
	}

	@GetMapping(value = "/assetDetails")
	public String getAssetDetailsTab(Model model, HttpSession session, HttpServletRequest request,
			@RequestParam(value = "dme_data_id", required = false) String dmeDataId,
			@RequestParam(value = "returnToSearch", required = false) String returnToSearch,
			@RequestParam(value = "assetIdentifier", required = false) String assetIdentifier) throws DoeWebException {

		log.info("get asset details");
		getAssetDetails(session, dmeDataId, returnToSearch, assetIdentifier, model);
		return "assetDetails";
	}

	@GetMapping(value = "/downloadTab")
	public String getDownload(Model model, HttpSession session, HttpServletRequest request,
			@RequestParam(value = "selectedPaths", required = false) String selectedPaths,
			@RequestParam(value = "code", required = false) String code,
			@RequestParam(value = "endpoint_id", required = false) String endPointName,
			@RequestParam(value = "downloadAsyncType", required = false) String downloadAsyncType,
			@RequestParam(value = "fileName", required = false) String fileName,
			@RequestParam(value = "returnToSearch", required = false) String returnToSearch,
			@RequestParam(value = "assetIdentifier", required = false) String assetIdentifier) throws DoeWebException {

		log.info("get download tab details");
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

		if (StringUtils.isNotEmpty(fileName)) {
			session.setAttribute("fileName", fileName);
		}

		if (code != null) {
			log.info("return Authorization code from google for download tab" + code);
			code = request.getParameter("code");
			if (code != null) {
				// Return from Google Drive Authorization
				selectedPaths = (String) session.getAttribute("selectedPathsString");
				downloadAsyncType = (String) session.getAttribute("downloadAsyncType");
				fileName = (String) session.getAttribute("fileName");
				final String returnURL = this.webServerName + "/downloadTab";
				try {
					String accessToken = doeAuthorizationService.getToken(code, returnURL);
					log.info("access token for download tab" + accessToken);
					session.setAttribute("accessToken", accessToken);
					model.addAttribute("accessToken", accessToken);
				} catch (Exception e) {
					throw new DoeWebException("Failed to redirect to Google for authorization: " + e.getMessage());
				}
				model.addAttribute("asyncSearchType", "drive");
				model.addAttribute("transferType", "drive");
				model.addAttribute("authorized", "true");
				model.addAttribute("selectedPathsString", selectedPaths);
				model.addAttribute("downloadAsyncType", downloadAsyncType);
				model.addAttribute("fileName", fileName);
			}
		} else if (endPointName != null) {
			// This is return from Globus site
			selectedPaths = (String) session.getAttribute("selectedPathsString");
			downloadAsyncType = (String) session.getAttribute("downloadAsyncType");
			fileName = (String) session.getAttribute("fileName");
			model.addAttribute("endPointName", endPointName);
			String endPointLocation = request.getParameter("path");
			model.addAttribute("endPointLocation", endPointLocation);
			model.addAttribute("asyncSearchType", "async");
			model.addAttribute("transferType", "async");
			model.addAttribute("selectedPathsString", selectedPaths);
			model.addAttribute("downloadAsyncType", downloadAsyncType);
			model.addAttribute("fileName", fileName);
		}
		return "downloadTab";
	}

	@GetMapping(value = "/metaDataPermissionsList")
	public ResponseEntity<?> getPermissionsList() {
		return getMetaDataPermissionsList();
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
		return getCollectionAccessGroups(selectedPath, levelName);
	}

	@GetMapping(value = "/getFilterList")
	public ResponseEntity<?> getFilterList(HttpSession session, @RequestHeader HttpHeaders headers, DoeSearch search)
			throws DoeWebException {

		log.info("get filtered list" + search);
		Set<String> list = constructFilterCriteria(session, search);

		return new ResponseEntity<>(list, HttpStatus.OK);

	}
}
