package gov.nih.nci.doe.web.controller;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Value;
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
import gov.nih.nci.doe.web.model.DoeCollectionModel;
import gov.nih.nci.doe.web.model.DoeMetadataAttrEntry;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataValidationRule;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionRegistrationDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementModelDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementRulesDTO;

/**
 *
 * Collection controller. Gets selected collection details. Updates collection
 * metadata.
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/addCollection")
public class DoeCreateCollectionController extends DoeCreateCollectionDataFileController {
	@Value("${gov.nih.nci.hpc.server.collection}")
	private String serviceURL;
	@Value("${gov.nih.nci.hpc.server.model}")
	private String hpcModelURL;
	@Value("${dme.archive.naming.forbidden.chararacters}")
	private String forbiddenCharacters;

	@GetMapping(value = "/collectionTypes", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<List<KeyValueBean>> populateCollectionTypes(HttpSession session,
			@RequestParam(value = "parent") String parent, Model model) throws DoeWebException {

		String authToken = (String) session.getAttribute("writeAccessUserToken");

		HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");
		if (modelDTO == null) {
			modelDTO = DoeClientUtil.getDOCModel(authToken, hpcModelURL, sslCertPath, sslCertPassword);
			session.setAttribute("userDOCModel", modelDTO);
		}

		HpcDataManagementRulesDTO basePathRules = DoeClientUtil.getBasePathManagementRules(modelDTO, basePath);
		String collectionType = null;
		Set<String> collectionTypesSet = new TreeSet<String>(String.CASE_INSENSITIVE_ORDER);
		List<KeyValueBean> results = new ArrayList<>();
		HpcCollectionListDTO collections = null;
		HpcCollectionDTO collection = null;

		if (parent != null && "basePath".equalsIgnoreCase(parent)) {
			parent = basePath;
		}
		if (parent != null && !parent.isEmpty()) {
			collections = DoeClientUtil.getCollection(authToken, serviceURL, parent, false, sslCertPath,
					sslCertPassword);
			if (collections != null && collections.getCollections() != null
					&& !CollectionUtils.isEmpty(collections.getCollections())) {
				collection = collections.getCollections().get(0);
			}
		}
		if (basePathRules != null) {
			List<HpcMetadataValidationRule> rules = basePathRules.getCollectionMetadataValidationRules();
			// Parent name is given
			if (parent != null) {
				if (collection != null) {
					if (collection.getMetadataEntries() == null) {
						if (basePathRules.getDataHierarchy() != null)
							collectionTypesSet.add(basePathRules.getDataHierarchy().getCollectionType());
						else
							collectionTypesSet.add("Folder");
					} else {
						for (HpcMetadataEntry entry : collection.getMetadataEntries().getSelfMetadataEntries()) {
							if (entry.getAttribute().equals("collection_type")) {
								collectionType = entry.getValue();
								break;
							}
						}
					}
				} else {
					for (String type : getCollectionTypes(rules))
						collectionTypesSet.add(type);
				}

				if (collectionType != null) {
					List<String> subCollections = getSubCollectionTypes(collectionType,
							basePathRules.getDataHierarchy());
					if ((subCollections == null || subCollections.isEmpty()) && !rules.isEmpty())
						throw new DoeWebException("Adding a sub collection is not allowed with: " + parent);
					for (String type : subCollections)
						collectionTypesSet.add(type);
				}
			}

			if (collectionType == null && collectionTypesSet.isEmpty()) {
				for (HpcMetadataValidationRule validationrule : rules) {
					if (validationrule.getMandatory() && validationrule.getAttribute().equals("collection_type")) {
						for (String type : validationrule.getValidValues())
							collectionTypesSet.add(type);
					}
				}
			}
		}
		final String collectionTypeVal = collectionType;
		HpcMetadataEntry entry = (collection != null && collection.getMetadataEntries() != null)
				? collection.getMetadataEntries().getSelfMetadataEntries().stream()
						.filter(e -> e.getAttribute().equalsIgnoreCase("access_group")).findAny().orElse(null)
				: null;
		collectionTypesSet.stream().forEach(e -> results.add(new KeyValueBean(e, collectionTypeVal)));
		results.add(new KeyValueBean("parentAccessGroup", entry != null ? entry.getValue() : ""));

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
			@RequestParam(value = "selectedPath") String selectedPath,
			@RequestParam(value = "collectionType") String collectionType,
			@RequestParam(required = false) String assetType, @RequestParam(required = false) Boolean refresh,
			HttpSession session, HttpServletRequest request, HttpServletResponse response) {

		log.info("get collection attributes" + selectedPath + " ,collectionType:  " + collectionType + ", assetType:"
				+ assetType + ", refresh:" + refresh);
		List<DoeMetadataAttrEntry> metadataEntries = new ArrayList<DoeMetadataAttrEntry>();
		List<DoeMetadataAttrEntry> cachedEntries = new ArrayList<DoeMetadataAttrEntry>();
		try {
			if (selectedPath != null) {
				log.info("selected path");
				if (refresh == null) {
					refresh = true;
				}
				if (refresh != null && !refresh) {
					log.info("get collection attributes for edit");
					List<KeyValueBean> entryList = getUserMetaDataAttributesByPath(selectedPath, collectionType,
							"false", session, request, response);

					for (KeyValueBean k : entryList) {
						log.info("get cahced entry list:" + k.getKey() + " ,value:" + k.getValue());
						DoeMetadataAttrEntry entry = new DoeMetadataAttrEntry();
						entry.setAttrName(k.getKey());
						entry.setAttrValue(k.getValue());
						cachedEntries.add(entry);
					}
				}
				log.info("cached list: " + cachedEntries);
				metadataEntries = populateFormAttributes(request, session, basePath, collectionType, assetType, refresh,
						cachedEntries);

				return new ResponseEntity<>(metadataEntries, HttpStatus.OK);
			}

		} catch (Exception e) {
			log.debug(e.getMessage());
		}

		return new ResponseEntity<>(metadataEntries, HttpStatus.OK);

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

		String authToken = (String) session.getAttribute("writeAccessUserToken");
		String[] path = request.getParameterValues("path");
		if (path[0] != null) {
			doeCollection.setPath(path[0].trim());
		}

		HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");
		if (modelDTO == null) {
			modelDTO = DoeClientUtil.getDOCModel(authToken, hpcModelURL, sslCertPath, sslCertPassword);
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
							parentPath, true, sslCertPath, sslCertPassword);
					Boolean isValidPermissions = verifyCollectionPermissions(parentPath, parentCollectionDto);
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

		try {
			registrationDTO = constructRequest(request, session, doeCollection);
			log.info("collection registrationDto: " + registrationDTO);
		} catch (DoeWebException e) {
			log.debug("Error in construct request" + e.getMessage());
		}

		// Validate Collection path
		try {
			HpcCollectionListDTO collection = DoeClientUtil.getCollection(authToken, serviceURL,
					doeCollection.getPath(), false, true, sslCertPath, sslCertPassword);
			if (collection != null && collection.getCollections() != null
					&& CollectionUtils.isNotEmpty(collection.getCollections())) {
				return "Collection already exists: " + doeCollection.getPath();
			}
		} catch (DoeWebException e) {
			log.debug("Error in validating collection path" + e.getMessage());
		}
		try {
			Integer restResponse = DoeClientUtil.updateCollection(authToken, serviceURL, registrationDTO,
					doeCollection.getPath(), sslCertPath, sslCertPassword);
			if (restResponse == 200 || restResponse == 201) {
				// after collection is created, store the permissions.
				String progList = request.getParameter("metaDataPermissionsList");
				log.info("selected permissions" + progList);
				HpcCollectionListDTO collections = DoeClientUtil.getCollection(authToken, serviceURL,
						doeCollection.getPath(), false, sslCertPath, sslCertPassword);
				if (collections != null && collections.getCollections() != null
						&& !CollectionUtils.isEmpty(collections.getCollections())) {
					HpcCollectionDTO collection = collections.getCollections().get(0);
					metaDataPermissionService.savePermissionsList(getLoggedOnUserInfo(), progList,
							collection.getCollection().getCollectionId(), doeCollection.getPath());

					// store the access_group metadata in MoDaC DB
					HpcMetadataEntry selectedEntry = collection.getMetadataEntries().getSelfMetadataEntries().stream()
							.filter(e -> e.getAttribute().equalsIgnoreCase("access_group")).findAny().orElse(null);
					if (selectedEntry != null) {
						accessGroupsService.saveAccessGroups(collection.getCollection().getCollectionId(),
								doeCollection.getPath(), selectedEntry.getValue(), getLoggedOnUserInfo());
					}
				}

				return "Collection is created!";
			}
		} catch (Exception e) {
			log.debug("Error in update collection" + e.getMessage());
		}
		return null;
	}

}
