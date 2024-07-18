package gov.nih.nci.doe.web.controller;

import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.multipart.MultipartFile;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.AuditingModel;
import gov.nih.nci.doe.web.model.DoeDatafileModel;
import gov.nih.nci.doe.web.service.DoeAuthorizationService;
import gov.nih.nci.doe.web.service.TaskManagerService;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.doe.web.util.DoeExcelUtil;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcDirectoryScanRegistrationItemDTO;
import gov.nih.nci.hpc.domain.metadata.HpcBulkMetadataEntries;
import gov.nih.nci.hpc.domain.metadata.HpcBulkMetadataEntry;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;

/**
 *
 * Add data file controller.
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/addbulk")
public class DoeCreateBulkDatafileController extends DoeCreateCollectionDataFileController {

	@Value("${gov.nih.nci.hpc.server.dataObject}")
	private String serviceURL;
	@Value("${gov.nih.nci.hpc.server.collection}")
	private String collectionServiceURL;

	@Autowired
	TaskManagerService taskManagerService;

	@GetMapping
	public String home(Model model, HttpSession session, HttpServletRequest request) throws Exception {

		log.info("load upload page");

		String user = getLoggedOnUserInfo();
		if (StringUtils.isEmpty(user)) {
			return "redirect:/loginTab";
		}
		String code = request.getParameter("code");
		String action = (String) session.getAttribute("actionType");
		model.addAttribute("clientId", clientId);
		if (code != null) {
			if (StringUtils.isNotEmpty(action) && action.equalsIgnoreCase(DoeAuthorizationService.GOOGLE_DRIVE_TYPE)) {
				// Return from Google Drive Authorization
				final String returnURL = this.webServerName + "/addbulk";
				model.addAttribute("uploadAsyncType", DoeAuthorizationService.GOOGLE_DRIVE_TYPE);
				try {
					String accessToken = doeAuthorizationService.getToken(code, returnURL,
							DoeAuthorizationService.ResourceType.GOOGLEDRIVE);

					session.setAttribute("accessToken", accessToken);
					model.addAttribute("accessToken", accessToken);
					model.addAttribute("authorized", "true");

				} catch (Exception e) {

					model.addAttribute("error", "Failed to redirect to Google for authorization: " + e.getMessage());
					log.error("Failed to redirect to Google for authorization: " + e.getMessage());
					throw new DoeWebException("Failed to redirect to Google for authorization.");
				}

			} else if (StringUtils.isNotEmpty(action)
					&& action.equalsIgnoreCase(DoeAuthorizationService.GOOGLE_CLOUD_TYPE)) {
				// Return from Google cloud Authorization
				final String returnURL = this.webServerName + "/addbulk";
				model.addAttribute("uploadAsyncType", DoeAuthorizationService.GOOGLE_CLOUD_TYPE);
				try {
					String refreshTokenDetailsGoogleCloud = doeAuthorizationService.getRefreshToken(code, returnURL,
							DoeAuthorizationService.ResourceType.GOOGLECLOUD);

					session.setAttribute("refreshTokenDetailsGoogleCloud", refreshTokenDetailsGoogleCloud);
					model.addAttribute("refreshTokenDetailsGoogleCloud", refreshTokenDetailsGoogleCloud);
					model.addAttribute("authorizedGC", "true");

				} catch (Exception e) {
					model.addAttribute("error", "Failed to redirect to Google for authorization: " + e.getMessage());
					log.error("Failed to redirect to Google for authorization: " + e.getMessage());
					throw new DoeWebException("Failed to redirect to Google for authorization.");
				}

			}
		}

		if (request.getParameterNames().hasMoreElements()) {
			setInputParameters(request, session, model);
		} else {
			clearSessionAttrs(session);
		}

		model.addAttribute("basePathSelected", basePath);
		return "upload";
	}

	/**
	 * Post operation to update metadata
	 * 
	 * @param hpcDatafile
	 * @param model
	 * @param bindingResult
	 * @param session
	 * @param request
	 * @param response
	 * @param redirectAttributes
	 * @return
	 * @throws DoeWebException
	 */
	@SuppressWarnings("unchecked")
	@PostMapping
	@ResponseBody
	public String createDatafile(@Valid DoeDatafileModel doeDataFileModel,
			@RequestParam(value = "doeMetadataFile", required = false) MultipartFile doeMetadataFile, Model model,
			@RequestParam(value = "isFormBulkAssetUpload", required = false) Boolean isFormBulkAssetUpload,
			HttpSession session, HttpServletRequest request, HttpServletResponse response) throws DoeWebException {

		log.info("bulk upload and isFormBulkAssetUpload : " + isFormBulkAssetUpload);
		/*
		 * when isFormBulkAssetUpload is null, then the upload is for bulk data files/
		 * sub collections
		 */
		String user = getLoggedOnUserInfo();
		if (StringUtils.isEmpty(user)) {
			return "Not Authorized";
		}
		String authToken = (String) session.getAttribute("writeAccessUserToken");

		String dataFilePath = request.getParameter("bulkDatafilePath");
		String bulkType = request.getParameter("uploadType");

		if (dataFilePath != null) {
			doeDataFileModel.setPath(dataFilePath.trim());
		}
		if (doeDataFileModel.getPath() == null || doeDataFileModel.getPath().trim().length() == 0)
			return "Invalid Data file path";

		doeDataFileModel.setPath(doeDataFileModel.getPath().trim());
		String accessGroups = null;

		HpcBulkMetadataEntries formBulkMetadataEntries = null;
		HpcBulkDataObjectRegistrationRequestDTO registrationDTO = null;
		HpcBulkMetadataEntries entries = null;

		// Validate parent path
		try {
			if (StringUtils.isNotEmpty(dataFilePath)) {
				HpcCollectionListDTO parentCollectionDto = DoeClientUtil.getCollection(authToken, collectionServiceURL,
						dataFilePath, true);
				Boolean isValidPermissions = verifyCollectionPermissions(dataFilePath, parentCollectionDto);
				if (Boolean.FALSE.equals(isValidPermissions)) {
					return "Insufficient privileges to register data files.";
				}
			}

			if (request.getParameterNames().hasMoreElements()) {
				setInputParameters(request, session, model);
			}

			List<String> existingGroups = accessGroupsService.getGroupsByCollectionPath(dataFilePath);

			if (CollectionUtils.isNotEmpty(existingGroups)) {
				accessGroups = String.join(",", existingGroups);
			}

			// when bulk metadata file is provided
			HashMap<HpcBulkMetadataEntries, Map<String, String>> doeBulkMetadataEntries = DoeExcelUtil
					.parseBulkMatadataEntries(doeMetadataFile, accessGroups, doeDataFileModel.getPath().trim());

			if (doeBulkMetadataEntries != null) {
				entries = doeBulkMetadataEntries.keySet().stream().findFirst().get();
				Map<String, String> assetIdentifierMapping = doeBulkMetadataEntries.get(entries);
				registrationDTO = constructV2BulkRequest(request, session, doeDataFileModel.getPath().trim(),
						assetIdentifierMapping);

			} else {
				registrationDTO = constructV2BulkRequest(request, session, doeDataFileModel.getPath().trim(), null);

			}

			// when asset upload is done by form
			if (Boolean.TRUE.equals(isFormBulkAssetUpload)) {
				formBulkMetadataEntries = constructRequest(request, session, accessGroups,
						doeDataFileModel.getPath().trim(), bulkType);

				if (formBulkMetadataEntries != null) {
					for (HpcDirectoryScanRegistrationItemDTO itemDTO : registrationDTO
							.getDirectoryScanRegistrationItems())
						itemDTO.setBulkMetadataEntries(formBulkMetadataEntries);
				}
			} else {
				for (HpcDirectoryScanRegistrationItemDTO itemDTO : registrationDTO
						.getDirectoryScanRegistrationItems()) {
					if (entries != null) {
						itemDTO.setBulkMetadataEntries(entries);
					}
				}
			}

			if (registrationDTO.getDataObjectRegistrationItems() != null
					&& !registrationDTO.getDataObjectRegistrationItems().isEmpty()) {
				for (HpcDataObjectRegistrationItemDTO dto : registrationDTO.getDataObjectRegistrationItems()) {
					if (entries != null && !entries.getPathsMetadataEntries().isEmpty()) {
						for (HpcBulkMetadataEntry bulkMeta : entries.getPathsMetadataEntries()) {
							if (dto.getPath().equals(bulkMeta.getPath()))
								dto.getDataObjectMetadataEntries().addAll(bulkMeta.getPathMetadataEntries());
						}
					}

					if (formBulkMetadataEntries != null
							&& !formBulkMetadataEntries.getPathsMetadataEntries().isEmpty()) {
						for (HpcBulkMetadataEntry bulkMeta : formBulkMetadataEntries.getPathsMetadataEntries()) {
							if (dto.getPath().equals(bulkMeta.getPath()))
								dto.getDataObjectMetadataEntries().addAll(bulkMeta.getPathMetadataEntries());
						}
					}
				}
			}

			if (CollectionUtils.isEmpty(registrationDTO.getDataObjectRegistrationItems())
					&& CollectionUtils.isEmpty(registrationDTO.getDirectoryScanRegistrationItems()))
				throw new DoeWebException("No input file(s) / folder(s) are selected");

			HpcBulkDataObjectRegistrationResponseDTO responseDTO = DoeClientUtil.registerBulkDatafiles(authToken,
					bulkRegistrationURL, registrationDTO);

			if (responseDTO != null) {

				String taskId = responseDTO.getTaskId();
				String name = null;
				Set<String> pathsList = new HashSet<String>();
				if (isFormBulkAssetUpload == null) {
					// if isFormBulkAssetUpload is null, the upload is for data files/ sub folders.
					name = doeDataFileModel.getPath().substring(doeDataFileModel.getPath().lastIndexOf('/') + 1);
					// get the paths for new collection registration and save in modac
					pathsList = (Set<String>) session.getAttribute("pathsList");
				} else if (CollectionUtils.isNotEmpty(registrationDTO.getDirectoryScanRegistrationItems())) {
					// upload is for bulk assets
					for (HpcDirectoryScanRegistrationItemDTO item : registrationDTO
							.getDirectoryScanRegistrationItems()) {
						if (item.getBulkMetadataEntries() != null && CollectionUtils
								.isNotEmpty(item.getBulkMetadataEntries().getPathsMetadataEntries())) {
							Set<String> itemPaths = item.getBulkMetadataEntries().getPathsMetadataEntries().stream()
									.map(e -> e.getPath()).collect(Collectors.toSet());
							pathsList.addAll(itemPaths);

						}
					}

				}

				if (CollectionUtils.isNotEmpty(pathsList)) {

					// add the collection paths to the permissions table
					// the collection Id will be null for now
					// when the scheduler runs, it updates the collection Id when the collection is
					// created
					pathsList.stream().forEach(f -> {

						// save collection permissions in MoDaC DB

						metaDataPermissionService.savePermissionsList(user, null, null, f);
					});

				}

				clearSessionAttrs(session);

				taskManagerService.saveTransfer(taskId, "Upload", null, name, user,
						CollectionUtils.isNotEmpty(pathsList) ? String.join(",", pathsList)
								: doeDataFileModel.getPath());

				// store the auditing info
				AuditingModel audit = new AuditingModel();
				audit.setName(user);
				audit.setOperation("Upload");
				audit.setStartTime(new Date());
				audit.setTransferType(bulkType);
				audit.setPath(doeDataFileModel.getPath());
				audit.setTaskId(taskId);
				auditingService.saveAuditInfo(audit);

				return "Your bulk data file registration request has the following task ID: <a href='/tasksTab'>"
						+ taskId + "</a>";

			}

			return "Error in registration";
		} catch (Exception e) {
			log.error("Failed to bulk register data files due to: " + e);
			return e.getMessage();
		}
	}

	@SuppressWarnings("unchecked")
	private HpcBulkMetadataEntries constructRequest(HttpServletRequest request, HttpSession session, String accessGrp,
			String collectionPath, String bulkType) throws DoeWebException {

		log.info("construct request for bulk asset upload through form: " + collectionPath + " and bulk type is: "
				+ bulkType);
		HpcBulkMetadataEntries entryList = new HpcBulkMetadataEntries();

		List<String> globusEndpointFolders = (List<String>) session.getAttribute("GlobusEndpointFolders");
		List<String> googleDriveFolderIds = (List<String>) session.getAttribute("folderIds");
		String s3Path = (String) request.getParameter("s3Path");
		String gcPath = (String) request.getParameter("gcPath");
		gcPath = (gcPath != null ? gcPath.trim() : null);
		List<HpcBulkMetadataEntry> pathMetadataEntries = new ArrayList<HpcBulkMetadataEntry>();
		String assetIdentifier = request.getParameter("zAttrStr_asset_identifier");
		List<String> folderNames = new ArrayList<String>();

		if ("globus".equalsIgnoreCase(bulkType) && CollectionUtils.isNotEmpty(globusEndpointFolders)) {
			folderNames = globusEndpointFolders;
		} else if ("drive".equalsIgnoreCase(bulkType) && CollectionUtils.isNotEmpty(googleDriveFolderIds)) {

			folderNames = googleDriveFolderIds;
		} else if ("S3".equalsIgnoreCase(bulkType) && s3Path != null) {
			folderNames.add(s3Path);

		} else if ("cloud".equalsIgnoreCase(bulkType) && gcPath != null) {
			folderNames.add(gcPath);
		}

		if (CollectionUtils.isNotEmpty(folderNames)) {
			for (String folderName : folderNames) {
				pathMetadataEntries = getAssetMetadataAttributes(pathMetadataEntries, assetIdentifier, request,
						collectionPath, folderName, accessGrp);
			}
		}
		entryList.getPathsMetadataEntries().addAll(pathMetadataEntries);
		return entryList;
	}

	private List<HpcBulkMetadataEntry> getAssetMetadataAttributes(List<HpcBulkMetadataEntry> pathMetadataEntries,
			String assetIdentifier, HttpServletRequest request, String collectionPath, String folderName,
			String accessGrp) throws DoeWebException {

		String assetType = request.getParameter("assetType");
		String collectionType = request.getParameter("collection_type");
		Enumeration<String> params = request.getParameterNames();
		HpcBulkMetadataEntry metadataEntry = new HpcBulkMetadataEntry();
		metadataEntry.setPath(collectionPath + "/" + assetIdentifier);
		List<HpcMetadataEntry> entries = new ArrayList<HpcMetadataEntry>();

		if (StringUtils.isNotEmpty(assetType)) {
			HpcMetadataEntry entryAssetType = new HpcMetadataEntry();
			entryAssetType.setAttribute("asset_type");
			entryAssetType.setValue(assetType);
			entries.add(entryAssetType);
		}

		if (StringUtils.isNotEmpty(collectionType)) {
			HpcMetadataEntry entryCollectionType = new HpcMetadataEntry();
			entryCollectionType.setAttribute("collection_type");
			entryCollectionType.setValue(collectionType);
			entries.add(entryCollectionType);
		}

		if (StringUtils.isNotEmpty(accessGrp)) {

			HpcMetadataEntry entryAccessGrp = new HpcMetadataEntry();
			entryAccessGrp.setAttribute("access_group");
			entryAccessGrp.setValue(accessGrp);
			entries.add(entryAccessGrp);
		}

		while (params.hasMoreElements()) {
			String paramName = params.nextElement();
			if (paramName.startsWith("zAttrStr_")) {

				HpcMetadataEntry entry1 = new HpcMetadataEntry();
				String attrName = paramName.substring("zAttrStr_".length());
				String[] attrValue = request.getParameterValues(paramName);
				entry1.setAttribute(attrName);
				entry1.setValue(attrValue[0].trim());
				if (StringUtils.isNotEmpty(entry1.getValue())) {
					entries.add(entry1);
				}

			} else if (paramName.startsWith("_addAttrName")) {
				HpcMetadataEntry userEntry = new HpcMetadataEntry();
				String attrId = paramName.substring("_addAttrName".length());
				String[] attrName = request.getParameterValues(paramName);
				String[] attrValue = request.getParameterValues("_addAttrValue" + attrId);
				if (attrName.length > 0 && !attrName[0].isEmpty())
					userEntry.setAttribute(attrName[0]);
				else
					throw new DoeWebException("Invalid metadata attribute name. Empty value is not valid!");
				if (attrValue.length > 0 && !attrValue[0].isEmpty())
					userEntry.setValue(attrValue[0].trim());
				else
					throw new DoeWebException("Invalid metadata attribute value. Empty value is not valid!");
				entries.add(userEntry);
			}
		}
		metadataEntry.getPathMetadataEntries().addAll(entries);
		pathMetadataEntries.add(metadataEntry);
		return pathMetadataEntries;
	}
}
