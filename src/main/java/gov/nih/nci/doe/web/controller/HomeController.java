package gov.nih.nci.doe.web.controller;

import java.io.InputStream;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.ws.rs.core.Response;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.cxf.jaxrs.client.WebClient;
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
import org.springframework.web.util.UriComponentsBuilder;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.MappingJsonFactory;
import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.DoeSearch;
import gov.nih.nci.doe.web.model.DoeUsersModel;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.model.PermissionsModel;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementModelDTO;
import gov.nih.nci.hpc.dto.datasearch.HpcCompoundMetadataQueryDTO;

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
			@RequestParam(value = "returnToSearch", required = false) String returnToSearch) {

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

	@SuppressWarnings("unchecked")
	@GetMapping(value = "/assetDetails")
	public String getAssetDetailsTab(Model model, HttpSession session, HttpServletRequest request,
			@RequestParam(value = "dme_data_id", required = false) String dmeDataId,
			@RequestParam(value = "returnToSearch", required = false) String returnToSearch,
			@RequestParam(value = "assetIdentifier", required = false) String assetIdentifier) throws DoeWebException {

		log.info("get asset details for dme_data_id: " + dmeDataId);
		String authToken = (String) session.getAttribute("hpcUserToken");
		log.info("authToken: " + authToken);

		String user = getLoggedOnUserInfo();
		log.info("asset details for user: " + user);

		List<KeyValueBean> loggedOnUserPermissions = (List<KeyValueBean>) getMetaDataPermissionsList().getBody();
		DoeSearch search = new DoeSearch();

		if (StringUtils.isNotEmpty(dmeDataId)) {
			String[] attrNames = { "collection_type", "dme_data_id" };
			String[] attrValues = { "Asset", dmeDataId };
			search.setAttrName(attrNames);
			search.setAttrValue(attrValues);
		} else if (StringUtils.isNotEmpty(assetIdentifier)) {

			String[] attrNames = { "collection_type", "asset_identifier" };
			String[] attrValues = { "Asset", assetIdentifier };
			search.setAttrName(attrNames);
			search.setAttrValue(attrValues);
		}
		String[] levelValues = { "ANY", "Asset" };
		boolean[] isExcludeParentMetadata = { false, false };
		String[] rowIds = { "1", "2" };
		String[] operators = { "LIKE", "LIKE" };
		boolean[] iskeyWordSearch = { true, false };
		boolean[] isAdvancedSearch = { false, false };

		search.setLevel(levelValues);
		search.setIsExcludeParentMetadata(isExcludeParentMetadata);
		search.setRowId(rowIds);
		search.setOperator(operators);
		search.setIskeyWordSearch(iskeyWordSearch);
		search.setIsAdvancedSearch(isAdvancedSearch);

		List<String> systemAttrs = (List<String>) session.getAttribute("systemAttrs");
		if (CollectionUtils.isEmpty(systemAttrs)) {
			HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");
			if (modelDTO == null) {
				modelDTO = DoeClientUtil.getDOCModel(authToken, hpcModelURL, sslCertPath, sslCertPassword);
				session.setAttribute("userDOCModel", modelDTO);
			}

			systemAttrs = modelDTO.getCollectionSystemGeneratedMetadataAttributeNames();
			List<String> dataObjectsystemAttrs = modelDTO.getDataObjectSystemGeneratedMetadataAttributeNames();
			systemAttrs.addAll(dataObjectsystemAttrs);
			systemAttrs.add("collection_type");
			systemAttrs.add("access_group");
			session.setAttribute("systemAttrs", systemAttrs);
		}

		try {
			HpcCompoundMetadataQueryDTO compoundQuery = constructCriteria(search);
			compoundQuery.setDetailedResponse(true);
			log.info("search compund query" + compoundQuery);
			serviceURL = compoundDataObjectSearchServiceURL;

			UriComponentsBuilder ucBuilder = UriComponentsBuilder.fromHttpUrl(compoundDataObjectSearchServiceURL);

			ucBuilder.queryParam("returnParent", Boolean.TRUE);
			final String requestURL = ucBuilder.build().encode().toUri().toURL().toExternalForm();

			WebClient client = DoeClientUtil.getWebClient(requestURL, sslCertPath, sslCertPassword);
			client.header("Authorization", "Bearer " + authToken);
			Response restResponse = client.invoke("POST", compoundQuery);
			if (restResponse.getStatus() == 200) {
				HpcCompoundMetadataQueryDTO compoundQuerySession = (HpcCompoundMetadataQueryDTO) session
						.getAttribute("compoundQuery");

				if (compoundQuerySession == null) {
					session.setAttribute("compoundQuery", compoundQuery);
				}

				MappingJsonFactory factory = new MappingJsonFactory();
				JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
				HpcCollectionListDTO collections = parser.readValueAs(HpcCollectionListDTO.class);
				HpcCollectionDTO collection = collections.getCollections().get(0);

				String studyName = getAttributeValue("study_name",
						collection.getMetadataEntries().getParentMetadataEntries(), "Study");

				String progName = getAttributeValue("program_name",
						collection.getMetadataEntries().getParentMetadataEntries(), "Program");

				String assetPermission = getPermissionRole(user, collection.getCollection().getCollectionId(),
						loggedOnUserPermissions);

				String accessGrp = getAttributeValue("access_group",
						collection.getMetadataEntries().getSelfMetadataEntries(), "Asset");

				String assetName = getAttributeValue("asset_name",
						collection.getMetadataEntries().getSelfMetadataEntries(), "Asset");

				List<KeyValueBean> selfMetadata = getUserMetadata(
						collection.getMetadataEntries().getSelfMetadataEntries(), "Asset", systemAttrs);

				if (StringUtils.isNotEmpty(returnToSearch)) {
					model.addAttribute("returnToSearch", true);
				}

				model.addAttribute("accessGrp", accessGrp);
				model.addAttribute("assetName", assetName);
				model.addAttribute("assetMetadata", selfMetadata);
				model.addAttribute("studyName", studyName);
				model.addAttribute("progName", progName);
				model.addAttribute("assetPath", collection.getCollection().getCollectionName());
				model.addAttribute("assetPermission", assetPermission);
			} else {
				throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
			}
		} catch (Exception e) {
			throw new DoeWebException(e.getMessage());
		}

		return "assetDetails";
	}

	@GetMapping(value = "/downloadTab")
	public String getDownload(Model model, HttpSession session, HttpServletRequest request,
			@RequestParam(value = "selectedPaths", required = false) String selectedPaths,
			@RequestParam(value = "code", required = false) String code,
			@RequestParam(value = "endpoint_id", required = false) String endPointName,
			@RequestParam(value = "downloadAsyncType", required = false) String downloadAsyncType,
			@RequestParam(value = "fileName", required = false) String fileName) throws DoeWebException {

		log.info("get download tab details");
		model.addAttribute("selectedPathsString", selectedPaths);
		model.addAttribute("downloadAsyncType", downloadAsyncType);
		model.addAttribute("fileName", fileName);
		model.addAttribute("clientId", clientId);
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
}
