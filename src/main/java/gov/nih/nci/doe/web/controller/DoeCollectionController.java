package gov.nih.nci.doe.web.controller;

import java.util.Date;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.AuditingModel;
import gov.nih.nci.doe.web.model.DoeCollectionModel;
import gov.nih.nci.doe.web.model.EditCollectionsModel;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionRegistrationDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationRequestDTO;

/**
 *
 * Collection controller. Gets selected collection details. Updates collection
 * metadata.
 * 
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/collection")
public class DoeCollectionController extends DoeCreateCollectionDataFileController {

	@Value("${gov.nih.nci.hpc.server.collection}")
	private String serviceURL;

	/**
	 * Update collection
	 *
	 * @param hpcCollection
	 * @param model
	 * @param bindingResult
	 * @param session
	 * @param request
	 * @param response
	 * @param redirectAttributes
	 * @return
	 */
	@PostMapping
	public @ResponseBody String updateCollection(@Valid DoeCollectionModel doeCollection, HttpSession session,
			HttpServletRequest request, HttpServletResponse response) throws DoeWebException {

		String authToken = (String) session.getAttribute("writeAccessUserToken");
		String loggedOnUser = getLoggedOnUserInfo();
		String[] path = request.getParameterValues("path");
		String isDataObject = request.getParameter("isDataObject");

		if (path[0] != null) {
			doeCollection.setPath(path[0].trim());
		}

		if (doeCollection.getPath() == null || doeCollection.getPath().trim().length() == 0) {
			return "Invalid collection path";
		}

		if (isDataObject != null && isDataObject.equalsIgnoreCase("true")) {
			HpcDataObjectRegistrationRequestDTO registrationDTO = constructDataRequest(request);
			boolean updated = DoeClientUtil.updateDatafile(authToken, dataObjectAsyncServiceURL, registrationDTO,
					doeCollection.getPath());
			if (updated) {
				session.removeAttribute("selectedUsers");
				return "The metadata was successfully updated.";

			}

		} else {
			HpcCollectionRegistrationDTO registrationDTO = constructRequest(request, null, Boolean.TRUE);

			Integer restResponse = DoeClientUtil.updateCollection(authToken, serviceURL, registrationDTO,
					doeCollection.getPath());
			if (restResponse == 200 || restResponse == 201) {
				session.removeAttribute("selectedUsers");
				// store the auditing info
				AuditingModel audit = new AuditingModel();
				audit.setName(loggedOnUser);
				audit.setOperation("Edit Meta Data");
				audit.setStartTime(new Date());
				audit.setPath(doeCollection.getPath());
				auditingService.saveAuditInfo(audit);
				return "The metadata was successfully updated.";
			}
		}

		return "ERROR";

	}

	@SuppressWarnings("unchecked")
	@GetMapping(value = "/canEdit", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<?> isEditpermissions(@RequestParam(value = "selectedPath") String selectedPath,
			@RequestParam(value = "collectionType", required = false) String collectionType,
			@RequestParam(value = "verifyAtAssetLevel", required = false) Boolean verifyAtAssetLevel,
			HttpSession session, HttpServletRequest request, HttpServletResponse response) throws DoeWebException {

		log.info("edit permissions for path: " + selectedPath);
		String user = getLoggedOnUserInfo();
		if (StringUtils.isEmpty(user)) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}
		String authToken = (String) session.getAttribute("writeAccessUserToken");
		try {
			HpcCollectionListDTO collectionDto = DoeClientUtil.getCollection(authToken, serviceURL, selectedPath, true);
			if (Boolean.TRUE.equals(verifyAtAssetLevel)) {

				return new ResponseEntity<>(verifyCollectionPermissions(selectedPath, collectionDto), HttpStatus.OK);

			} else {
				EditCollectionsModel editPermModel = new EditCollectionsModel();

				List<KeyValueBean> loggedOnUserPermissions = (List<KeyValueBean>) getMetaDataPermissionsList(null)
						.getBody();
				String permissionRole = getPermissionRoleByCollectionPath(user, selectedPath, loggedOnUserPermissions);
				editPermModel.setPermissionRole(permissionRole);
				editPermModel.setCollectionPath(selectedPath);
				editPermModel.setCollectionType(collectionType);

				if (collectionDto != null && collectionDto.getCollections() != null
						&& !CollectionUtils.isEmpty(collectionDto.getCollections())) {
					HpcCollectionDTO collection = collectionDto.getCollections().get(0);
					editPermModel.setCollectionId(collection.getCollection().getCollectionId());
					String collectionName = selectedPath.substring(selectedPath.lastIndexOf('/') + 1);
					editPermModel.setCollectionName(collectionName);
				}

				return new ResponseEntity<>(editPermModel, HttpStatus.OK);

			}

		} catch (Exception e) {
			log.error("Failed to verify permissions on the collection");
			throw new DoeWebException(e);
		}
	}
}
