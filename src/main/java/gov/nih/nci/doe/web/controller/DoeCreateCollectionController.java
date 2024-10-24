package gov.nih.nci.doe.web.controller;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.AuditingModel;
import gov.nih.nci.doe.web.model.DoeCollectionModel;
import gov.nih.nci.doe.web.model.DoeMetadataAttrEntry;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionRegistrationDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementModelDTO;

/**
 *
 * Collection controller. Gets selected collection details. Updates collection
 * metadata.
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/addCollection")
public class DoeCreateCollectionController extends DoeCreateCollectionDataFileController {

	@GetMapping(value = "/parentAccessGroup", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<List<KeyValueBean>> populateCollectionTypes(HttpSession session,
			@RequestParam(value = "parent") String parent, Model model) throws DoeWebException {

		log.info("collection types for: " + parent);
		List<KeyValueBean> results = new ArrayList<KeyValueBean>();

		List<String> accessGrpList = accessGroupsService.getGroupsByCollectionPath(parent);
		if (CollectionUtils.isNotEmpty(accessGrpList)) {
			results.add(new KeyValueBean("parentAccessGroup", String.join(",", accessGrpList)));
		} else {
			// if no access groups exist, the collection access group is public
			results.add(new KeyValueBean("parentAccessGroup", "public"));
		}

		return new ResponseEntity<>(results, HttpStatus.OK);
	}

	/**
	 * Create collection
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

	@GetMapping
	public ResponseEntity<List<DoeMetadataAttrEntry>> getCollectionAttributes(
			@RequestParam(value = "selectedPath", required = false) String selectedPath,
			@RequestParam(value = "collectionType") String collectionType,
			@RequestParam(required = false) String[] controllerValue, @RequestParam(required = false) Boolean refresh,
			@RequestParam(required = false) String[] controllerAttribute, HttpSession session,
			HttpServletRequest request, HttpServletResponse response) {

		log.info("get collection attributes" + selectedPath + " ,collectionType:  " + collectionType
				+ ", controllerValue:" + controllerValue + ", refresh:" + refresh);
		List<DoeMetadataAttrEntry> metadataEntries = new ArrayList<DoeMetadataAttrEntry>();
		List<DoeMetadataAttrEntry> cachedEntries = new ArrayList<DoeMetadataAttrEntry>();
		try {
			if (StringUtils.isNotEmpty(selectedPath)) {
				log.info("selected path");
				if (refresh == null) {
					refresh = true;
				}
				if (Boolean.FALSE.equals(refresh)) {
					log.info("get collection attributes for edit");
					List<KeyValueBean> entryList = getUserMetaDataAttributesByPath(selectedPath, collectionType,
							"false", session);

					for (KeyValueBean k : entryList) {
						log.info("get cahced entry list:" + k.getKey() + " ,value:" + k.getValue());
						DoeMetadataAttrEntry entry = new DoeMetadataAttrEntry();
						entry.setAttrName(k.getKey());
						entry.setAttrValue(k.getValue());
						cachedEntries.add(entry);
					}
				}
				log.info("cached list: " + cachedEntries);
			}
			metadataEntries = populateFormAttributes(request, session, basePath, collectionType, controllerAttribute,
					controllerValue, refresh, cachedEntries);

			return new ResponseEntity<>(metadataEntries, HttpStatus.OK);

		} catch (Exception e) {
			log.debug(e.getMessage());
		}

		return new ResponseEntity<>(metadataEntries, HttpStatus.OK);

	}

	@GetMapping(value = "/getControlAttributesList")
	public ResponseEntity<List<DoeMetadataAttrEntry>> getControllerAttributesList(
			@RequestParam(value = "collectionType") String collectionType,
			@RequestParam(value = "controllerAttribute") String controllerAttribute, HttpSession session,
			HttpServletRequest request, HttpServletResponse response) {

		log.info("get control attributes for" + controllerAttribute);
		List<DoeMetadataAttrEntry> controlAttrList = new ArrayList<DoeMetadataAttrEntry>();

		try {
			controlAttrList = getControlAttributes(request, session, basePath, collectionType, controllerAttribute);

			return new ResponseEntity<>(controlAttrList, HttpStatus.OK);

		} catch (Exception e) {
			log.debug(e.getMessage());
		}

		return new ResponseEntity<>(controlAttrList, HttpStatus.OK);

	}

	/**
	 * Create collection
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
	@ResponseBody
	public String createCollection(@Valid DoeCollectionModel doeCollection, HttpSession session,
			HttpServletRequest request, HttpServletResponse response) throws DoeWebException {

		log.info("create collection");
		String user = getLoggedOnUserInfo();
		if (StringUtils.isEmpty(user)) {
			return "Not Authorized";
		}
		String authToken = (String) session.getAttribute("writeAccessUserToken");
		String[] path = request.getParameterValues("path");
		if (path[0] != null) {
			doeCollection.setPath(path[0].trim());
		}

		HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");
		if (modelDTO == null) {
			modelDTO = DoeClientUtil.getDOCModel(authToken, hpcModelURL);
			session.setAttribute("userDOCModel", modelDTO);
		}

		// Validate parent path
		String parentPath = null;
		try {
			if (doeCollection.getPath().lastIndexOf('/') != -1)
				parentPath = doeCollection.getPath().substring(0, doeCollection.getPath().lastIndexOf('/'));
			else
				parentPath = doeCollection.getPath();
			if (!parentPath.isEmpty()) {
				if (!parentPath.equalsIgnoreCase(basePath)) {
					HpcCollectionListDTO parentCollectionDto = DoeClientUtil.getCollection(authToken, serviceURL,
							parentPath, true);
					Boolean isValidPermissions = verifyCollectionPermissions(session, parentPath, parentCollectionDto);
					if (Boolean.FALSE.equals(isValidPermissions)) {
						return "Insufficient privileges to create collection";
					}
				}
			} else {
				return "Invalid parent in Collection Path";
			}

		} catch (DoeWebException e) {
			log.debug("Invalid parent in Collection Path:" + e.getMessage());
		}

		HpcCollectionRegistrationDTO registrationDTO = null;

		registrationDTO = constructRequest(request, doeCollection, Boolean.FALSE);
		log.info("collection registrationDto: " + registrationDTO);

		// Validate Collection path
		try {
			HpcCollectionListDTO collection = DoeClientUtil.getCollection(authToken, serviceURL,
					doeCollection.getPath(), false, true);
			if (collection != null && collection.getCollections() != null
					&& CollectionUtils.isNotEmpty(collection.getCollections())) {
				return "Collection already exists: " + doeCollection.getPath();
			}
		} catch (DoeWebException e) {
			log.debug("Error in validating collection path" + e.getMessage());
		}
		try {
			Integer restResponse = DoeClientUtil.updateCollection(authToken, serviceURL, registrationDTO,
					doeCollection.getPath());

			if (restResponse == 200 || restResponse == 201) {

				// after collection is created, store the permissions.
				String[] progList = request.getParameterValues("metaDataPermissionsList");
				log.info("selected permissions" + progList);
				HpcCollectionListDTO collections = DoeClientUtil.getCollection(authToken, serviceURL,
						doeCollection.getPath(), false);

				if (collections != null && collections.getCollections() != null
						&& !CollectionUtils.isEmpty(collections.getCollections())) {

					HpcCollectionDTO collection = collections.getCollections().get(0);

					// save collection permissions in MoDaC DB

					metaDataPermissionService.savePermissionsList(user,
							progList != null ? String.join(",", progList) : null,
							collection.getCollection().getCollectionId(), doeCollection.getPath());

					// store the access_group metadata in MoDaC DB
					HpcMetadataEntry selectedEntry = collection.getMetadataEntries().getSelfMetadataEntries().stream()
							.filter(e -> e.getAttribute().equalsIgnoreCase("access_group")).findAny().orElse(null);
					if (selectedEntry != null) {
						accessGroupsService.saveAccessGroups(collection.getCollection().getCollectionId(),
								doeCollection.getPath(), selectedEntry.getValue(), user);
					}

					// store the auditing info
					AuditingModel audit = new AuditingModel();
					audit.setName(user);
					audit.setOperation("register collection");
					audit.setStartTime(new Date());
					audit.setPath(doeCollection.getPath());
					auditingService.saveAuditInfo(audit);
				}

				// remove the session attribute

				session.removeAttribute("validValuesList");

				return "Collection is created!";
			}
		} catch (DoeWebException e) {
			throw new DoeWebException("Failed to create collection due to: " + e.getMessage());
		}
		return null;
	}

}
