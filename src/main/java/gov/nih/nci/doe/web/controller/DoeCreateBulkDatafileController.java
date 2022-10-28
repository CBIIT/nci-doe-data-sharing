package gov.nih.nci.doe.web.controller;

import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

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
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementModelDTO;

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
	@Value("${gov.nih.nci.hpc.server.v2.bulkregistration}")
	private String bulkRegistrationURL;

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
					String refreshTokenDetailsGoogleCloud = doeAuthorizationService.getToken(code, returnURL,
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
			@RequestParam(value = "isFormBulkUpload", required = false) Boolean isFormBulkUpload, HttpSession session,
			HttpServletRequest request, HttpServletResponse response) throws DoeWebException {

		log.info("bulk upload and isFormBulkUpload : " + isFormBulkUpload);
		String user = getLoggedOnUserInfo();
		if (StringUtils.isEmpty(user)) {
			return "Not Authorized";
		}
		String authToken = (String) session.getAttribute("writeAccessUserToken");

		String dataFilePath = request.getParameter("bulkDatafilePath");

		if (dataFilePath != null) {
			doeDataFileModel.setPath(dataFilePath.trim());
		}
		if (doeDataFileModel.getPath() == null || doeDataFileModel.getPath().trim().length() == 0)
			return "Invalid Data file path";

		doeDataFileModel.setPath(doeDataFileModel.getPath().trim());
		String accessGroups = null;
		Set<String> pathsList = new HashSet<String>();
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

				for (HpcDirectoryScanRegistrationItemDTO itemDTO : registrationDTO
						.getDirectoryScanRegistrationItems()) {
					itemDTO.setBulkMetadataEntries(entries);
				}
			} else {
				registrationDTO = constructV2BulkRequest(request, session, doeDataFileModel.getPath().trim(), null);
			}

			// when asset upload upload is done by form
			if (Boolean.TRUE.equals(isFormBulkUpload)) {
				formBulkMetadataEntries = constructRequest(request, session, accessGroups,
						doeDataFileModel.getPath().trim());

				if (formBulkMetadataEntries != null) {
					for (HpcDirectoryScanRegistrationItemDTO itemDTO : registrationDTO
							.getDirectoryScanRegistrationItems())
						itemDTO.setBulkMetadataEntries(formBulkMetadataEntries);
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

			String bulkType = request.getParameter("uploadType");

			if (CollectionUtils.isEmpty(registrationDTO.getDataObjectRegistrationItems())
					&& CollectionUtils.isEmpty(registrationDTO.getDirectoryScanRegistrationItems()))
				throw new DoeWebException("No input file(s) / folder(s) are selected");
			Set<String> basePaths = (Set<String>) session.getAttribute("basePaths");

			if (basePaths == null || basePaths.isEmpty()) {
				HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");
				if (modelDTO == null) {
					modelDTO = DoeClientUtil.getDOCModel(authToken, hpcModelURL);
					session.setAttribute("userDOCModel", modelDTO);
				}
				String userId = (String) session.getAttribute("hpcUserId");
				DoeClientUtil.populateBasePaths(session, modelDTO, authToken, userId, serviceURL);
				basePaths = (Set<String>) session.getAttribute("basePaths");
			}

			HpcBulkDataObjectRegistrationResponseDTO responseDTO = DoeClientUtil.registerBulkDatafiles(authToken,
					bulkRegistrationURL, registrationDTO);

			if (responseDTO != null) {

				String taskId = responseDTO.getTaskId();
				String name = null;
				if (isFormBulkUpload == null) {
					// if isFormBulkUpload is null, the upload is for data files.
					name = doeDataFileModel.getPath().substring(doeDataFileModel.getPath().lastIndexOf('/') + 1);
				}

				// get the paths for new collection registration and save in modac

				if (CollectionUtils.isNotEmpty(registrationDTO.getDirectoryScanRegistrationItems())) {
					for (HpcDirectoryScanRegistrationItemDTO item : registrationDTO
							.getDirectoryScanRegistrationItems()) {
						item.getBulkMetadataEntries().getPathsMetadataEntries().stream()
								.forEach(e -> pathsList.add(e.getPath()));
					}
				}

				if (CollectionUtils.isNotEmpty(pathsList)) {
					pathsList.stream().forEach(f -> {
						mailService.sendCollectionRegistationFailure(user, f, null, taskId);
					});

				}

				clearSessionAttrs(session);

				taskManagerService.saveTransfer(taskId, "Upload", null, name, user);

				// store the auditing info
				AuditingModel audit = new AuditingModel();
				audit.setName(user);
				audit.setOperation("Upload");
				audit.setStartTime(new Date());
				audit.setTransferType(bulkType);
				audit.setPath(doeDataFileModel.getPath());
				audit.setTaskId(taskId);
				auditingService.saveAuditInfo(audit);

				return "Your bulk data file registration request has the following task ID: " + taskId;

			}

			return "Error in registration";
		} catch (Exception e) {
			log.error("Failed to bulk register data files due to: " + e);
			return e.getMessage();
		}
	}

	@SuppressWarnings("unchecked")
	private HpcBulkMetadataEntries constructRequest(HttpServletRequest request, HttpSession session, String accessGrp,
			String collectionPath) throws DoeWebException {

		log.info("construct request for bulk asset upload through form: " + collectionPath);
		HpcBulkMetadataEntries entryList = new HpcBulkMetadataEntries();
		List<HpcBulkMetadataEntry> pathMetadataEntries = new ArrayList<HpcBulkMetadataEntry>();
		List<String> globusEndpointFolders = (List<String>) session.getAttribute("GlobusEndpointFolders");
		String assetType = request.getParameter("assetType");
		String collectionType = request.getParameter("collection_type");
		String assetGroupIdentifier = request.getParameter("assetGroupIdentifier");
		int index = 1;
		if (CollectionUtils.isNotEmpty(globusEndpointFolders)) {
			for (String folderName : globusEndpointFolders) {

				String assetIdentifier = assetGroupIdentifier + "_" + index;
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
				HpcMetadataEntry entry = new HpcMetadataEntry();
				entry.setAttribute("asset_identifier");
				entry.setValue(assetIdentifier);
				entries.add(entry);

				HpcMetadataEntry entryName = new HpcMetadataEntry();
				entryName.setAttribute("asset_name");
				entryName.setValue(folderName);
				entries.add(entryName);

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
				index++;
			}
		}

		entryList.getPathsMetadataEntries().addAll(pathMetadataEntries);
		return entryList;
	}
}
