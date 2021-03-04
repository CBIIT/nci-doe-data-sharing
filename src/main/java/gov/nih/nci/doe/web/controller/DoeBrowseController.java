package gov.nih.nci.doe.web.controller;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.DoeBrowserEntry;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.domain.datamanagement.HpcCollection;
import gov.nih.nci.hpc.domain.datamanagement.HpcCollectionListingEntry;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementModelDTO;

/**
 * MoDaC browse
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/browse")
public class DoeBrowseController extends AbstractDoeController {

	@Value("${gov.nih.nci.hpc.server.collection}")
	private String collectionURL;

	@Value("${gov.nih.nci.hpc.server.model}")
	private String hpcModelURL;

	@Value("${gov.nih.nci.hpc.server.dataObject}")
	private String serviceURL;

	// The logger instance.
	private final Logger logger = LoggerFactory.getLogger(this.getClass().getName());

	@GetMapping(value = "/metaData", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<?> getUserMetaDataByPath(@RequestParam(value = "selectedPath") String selectedPath,
			@RequestParam(value = "levelName") String levelName,
			@RequestParam(value = "isDataObject") String isDataObject, HttpSession session, HttpServletRequest request,
			HttpServletResponse response) throws DoeWebException {

		List<KeyValueBean> entryList = getUserMetaDataAttributesByPath(selectedPath, levelName, isDataObject, session,
				request, response);
		return new ResponseEntity<>(entryList, HttpStatus.OK);

	}

	@GetMapping(value = "/getAccessgroups", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<?> getAccessgroups(@RequestParam(value = "selectedPath") String selectedPath,
			@RequestParam(value = "levelName") String levelName, HttpSession session, HttpServletRequest request,
			HttpServletResponse response) throws DoeWebException {

		String authToken = (String) session.getAttribute("hpcUserToken");
		List<KeyValueBean> entryList = new ArrayList<KeyValueBean>();

		HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");
		if (modelDTO == null) {
			modelDTO = DoeClientUtil.getDOCModel(authToken, hpcModelURL, sslCertPath, sslCertPassword);
			session.setAttribute("userDOCModel", modelDTO);
		}

		try {
			if (selectedPath != null) {
				List<String> accessGrpList = accessGroupsService.getGroupsByCollectionPath(selectedPath);
				if (CollectionUtils.isNotEmpty(accessGrpList)) {
					entryList.add(new KeyValueBean("selectedEntry", String.join(",", accessGrpList)));
				}
				if (selectedPath.lastIndexOf('/') != -1
						&& (levelName.equalsIgnoreCase("Asset") || levelName.equalsIgnoreCase("Study"))) {
					String parentPath = selectedPath.substring(0, selectedPath.lastIndexOf('/'));
					List<String> parentGrpAccessGrpList = accessGroupsService.getGroupsByCollectionPath(parentPath);
					if (CollectionUtils.isNotEmpty(parentGrpAccessGrpList)) {
						entryList.add(new KeyValueBean("parentAccessGroups", String.join(",", parentGrpAccessGrpList)));
					}
				}
				// Get collection
				/*
				 * HpcCollectionListDTO collections = DoeClientUtil.getCollection(authToken,
				 * collectionURL, selectedPath, true, sslCertPath, sslCertPassword); if
				 * (collections != null && collections.getCollections() != null &&
				 * !collections.getCollections().isEmpty()) { HpcCollectionDTO collection =
				 * collections.getCollections().get(0); HpcMetadataEntry selectedEntry =
				 * collection.getMetadataEntries().getSelfMetadataEntries().stream() .filter(e
				 * -> e.getAttribute().equalsIgnoreCase("access_group")).findAny().orElse(null);
				 * if (selectedEntry != null) { entryList.add(new KeyValueBean("selectedEntry",
				 * selectedEntry.getValue())); }
				 * 
				 * HpcMetadataEntry programEntry =
				 * collection.getMetadataEntries().getParentMetadataEntries().stream() .filter(e
				 * -> e.getAttribute().equalsIgnoreCase("access_group") &&
				 * e.getLevelLabel().equalsIgnoreCase("program")) .findAny().orElse(null); if
				 * (programEntry != null) { entryList.add(new
				 * KeyValueBean("programLevelAccessGroups", programEntry.getValue())); }
				 * 
				 * HpcMetadataEntry studyEntry =
				 * collection.getMetadataEntries().getParentMetadataEntries().stream() .filter(e
				 * -> e.getAttribute().equalsIgnoreCase("access_group") &&
				 * e.getLevelLabel().equalsIgnoreCase("study")) .findAny().orElse(null); if
				 * (studyEntry != null) { entryList.add(new
				 * KeyValueBean("studyLevelAccessGroups", studyEntry.getValue())); } }
				 */
			}
		} catch (Exception e) {
			String errMsg = "Failed to get metadata: " + e.getMessage();
			logger.error(errMsg, e);
		}
		return new ResponseEntity<>(entryList, HttpStatus.OK);
	}

	@GetMapping(value = "/collection", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<?> browseCollection(@RequestParam(value = "selectedPath") String selectedPath,
			@RequestParam(required = false) String refreshNode, HttpSession session, HttpServletRequest request,
			HttpServletResponse response) {

		String authToken = (String) session.getAttribute("hpcUserToken");
		DoeBrowserEntry browserEntry = (DoeBrowserEntry) session.getAttribute("browserEntry");
		List<KeyValueBean> results = new ArrayList<>();
		boolean getChildren = false;
		boolean refresh = false;

		if (!StringUtils.isEmpty(refreshNode)) {
			getChildren = true;
			refresh = true;
		}

		try {
			if (selectedPath != null) {
				browserEntry = getTreeNodes(selectedPath.trim(), browserEntry, authToken, getChildren, true, refresh);

				browserEntry = trimPath(browserEntry, browserEntry.getName());
				String name = browserEntry.getName().substring(browserEntry.getName().lastIndexOf('/') + 1);
				browserEntry.setName(name);
				List<DoeBrowserEntry> children = browserEntry.getChildren();

				children.stream().forEach(e -> {
					if (e.getFullPath() != null && StringUtils.isNotEmpty(e.getFullPath().trim())) {
						results.add(new KeyValueBean(e.getFullPath(), e.getName()));
					}
				});
			}
		} catch (Exception e) {
			String errMsg = "Failed to browse: " + e.getMessage();
			logger.error(errMsg, e);
		}
		return new ResponseEntity<>(results, HttpStatus.OK);
	}

	/**
	 * GET operation on Browse. Invoked under the following conditions: - Builds
	 * initial tree (/browse?base) - When the refresh screen button is clicked
	 * (/browse?refresh) - When a bookmark is selected
	 * (/browse?refresh&path=/some_path/some_collection_or_file) - When the browse
	 * icon is clicked from the details page
	 * (/browse?refresh=1&path=/some_path/some_collection)
	 *
	 * @param q
	 * @param model
	 * @param bindingResult
	 * @param session
	 * @param request
	 * @return
	 */
	@GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<?> get(HttpSession session, HttpServletRequest request) {

		// Verify User session
		String authToken = (String) session.getAttribute("hpcUserToken");
		if (authToken == null) {
			return null;
		}

		List<KeyValueBean> results = new ArrayList<>();
		String path = basePath;

		// If browser tree nodes are cached, return cached data. If not, query
		// browser tree nodes based on the base path and cache it.
		try {
			if (path != null) {
				path = path.trim();

				DoeBrowserEntry browserEntry = (DoeBrowserEntry) session.getAttribute("browserEntry");
				if (browserEntry == null) {
					browserEntry = new DoeBrowserEntry();
					browserEntry.setCollection(true);
					browserEntry.setFullPath(path);
					browserEntry.setId(path);
					browserEntry.setName(path);
					browserEntry = getTreeNodes(path, browserEntry, authToken, false, true, false);
					if (request.getParameter("base") == null)
						browserEntry = addPathEntries(path, browserEntry);
					browserEntry = trimPath(browserEntry, browserEntry.getName());
					session.setAttribute("browserEntry", browserEntry);
				}

				if (browserEntry != null) {
					List<DoeBrowserEntry> children = browserEntry.getChildren();
					children.stream().forEach(e -> results.add(new KeyValueBean(e.getFullPath(), e.getName())));
				}
			}

			return new ResponseEntity<>(results, HttpStatus.OK);
		} catch (Exception e) {
			String errMsg = "Failed to get tree. Reason: " + e.getMessage();
			logger.error(errMsg, e);

		}

		return new ResponseEntity<>(results, HttpStatus.NO_CONTENT);
	}

	private DoeBrowserEntry addPathEntries(String path, DoeBrowserEntry browserEntry) {
		if (path.indexOf("/") != -1) {
			String[] paths = path.split("/");
			for (int i = paths.length - 2; i >= 0; i--) {
				if (paths[i].isEmpty())
					continue;
				browserEntry = addPathEntry(path, paths[i], browserEntry);
			}
		}
		return browserEntry;
	}

	private DoeBrowserEntry addPathEntry(String fullPath, String path, DoeBrowserEntry childEntry) {
		DoeBrowserEntry entry = new DoeBrowserEntry();
		String entryPath = fullPath.substring(0, (fullPath.indexOf('/' + path) + ('/' + path).length()));
		entry.setCollection(true);
		entry.setId(entryPath);
		entry.setFullPath(entryPath);
		entry.setPopulated(true);
		entry.setName(path);
		entry.getChildren().add(childEntry);
		return entry;
	}

	private DoeBrowserEntry getSelectedEntry(String path, DoeBrowserEntry browserEntry) {
		if (browserEntry == null)
			return null;

		if (browserEntry.getFullPath() != null && browserEntry.getFullPath().equals(path))
			return browserEntry;

		for (DoeBrowserEntry childEntry : browserEntry.getChildren()) {
			if (childEntry.getFullPath() != null && childEntry.getFullPath().equals(path))
				return childEntry;
			else {
				// Drill down childEntry, but only if this child entry
				// happens to be an ancestor of the path we are looking for
				// Else we proceed to next childEntry
				if (childEntry.getFullPath() != null && path.contains(childEntry.getFullPath())) {
					DoeBrowserEntry entry = getSelectedEntry(path, childEntry);
					if (entry != null)
						return entry;
				}
			}
		}
		return null;
	}

	/**
	 * Get child Tree nodes for selected tree node and merge it with cached nodes
	 *
	 * @param path
	 * @param browserEntry
	 * @param authToken
	 * @param model
	 * @param getChildren
	 * @param refresh
	 * @return
	 */
	private DoeBrowserEntry getTreeNodes(String path, DoeBrowserEntry browserEntry, String authToken,
			boolean getChildren, boolean partial, boolean refresh) throws DoeWebException {

		path = path.trim();
		DoeBrowserEntry selectedEntry = getSelectedEntry(path, browserEntry);
		if (refresh && selectedEntry != null) {
			selectedEntry.setPopulated(false);
		}

		if (selectedEntry != null && selectedEntry.isPopulated())
			return partial ? selectedEntry : browserEntry;
		if (selectedEntry != null && selectedEntry.getChildren() != null)
			selectedEntry.getChildren().clear();
		if (selectedEntry == null) {
			selectedEntry = new DoeBrowserEntry();
			selectedEntry.setName(path);
		}
		// If partial is true or refresh is true, then it means we need to
		// retrieve info on the selectedEntry also along with it's child list
		// Else, we only get the child list, since we already have the
		// info about the selectedEntry.
		HpcCollectionListDTO collections = DoeClientUtil.getCollection(authToken, collectionURL, path,
				partial || refresh ? false : true, partial || refresh, sslCertPath, sslCertPassword);

		for (HpcCollectionDTO collectionDTO : collections.getCollections()) {
			HpcCollection collection = collectionDTO.getCollection();

			if (collection.getAbsolutePath() != null) {
				selectedEntry.setFullPath(collection.getAbsolutePath());
				selectedEntry.setId(collection.getAbsolutePath());
				selectedEntry.setName(collection.getCollectionName());
			}

			// This will ensure that the next time we access this path
			// we dont read again from DB, unless an explicit refresh
			// request has been made
			selectedEntry.setPopulated(true);

			selectedEntry.setCollection(true);
			for (HpcCollectionListingEntry listEntry : collection.getSubCollections()) {
				DoeBrowserEntry listChildEntry = new DoeBrowserEntry();
				listChildEntry.setCollection(true);
				listChildEntry.setFullPath(listEntry.getPath());
				listChildEntry.setId(listEntry.getPath());
				listChildEntry.setName(listEntry.getPath());
				listChildEntry.setPopulated(false);
				if (getChildren)
					listChildEntry = getTreeNodes(listEntry.getPath(), listChildEntry, authToken, false, partial,
							false);
				else {
					DoeBrowserEntry emptyEntry = new DoeBrowserEntry();
					emptyEntry.setName("");
					listChildEntry.getChildren().add(emptyEntry);
				}
				selectedEntry.getChildren().add(listChildEntry);
			}
			for (HpcCollectionListingEntry listEntry : collection.getDataObjects()) {
				selectedEntry.setCollection(true);
				DoeBrowserEntry listChildEntry = new DoeBrowserEntry();
				listChildEntry.setCollection(false);
				listChildEntry.setFullPath(listEntry.getPath());
				listChildEntry.setId(listEntry.getPath());
				listChildEntry.setName(listEntry.getPath());
				listChildEntry.setPopulated(true);
				selectedEntry.getChildren().add(listChildEntry);
			}
			if (selectedEntry.getChildren() == null || selectedEntry.getChildren().isEmpty()) {
				DoeBrowserEntry listChildEntry = new DoeBrowserEntry();
				listChildEntry.setCollection(false);
				listChildEntry.setFullPath(" ");
				listChildEntry.setId(" ");
				listChildEntry.setName(" ");
				listChildEntry.setPopulated(true);
				selectedEntry.getChildren().add(listChildEntry);
			}
		}

		return partial ? selectedEntry : browserEntry;
	}

	private DoeBrowserEntry trimPath(DoeBrowserEntry entry, String parentPath) {
		for (DoeBrowserEntry child : entry.getChildren()) {
			String childPath = child.getFullPath();
			if (childPath == null || childPath.equals(""))
				continue;
			if (childPath.indexOf(parentPath) != -1) {
				String childName = childPath.substring(childPath.indexOf(parentPath) + parentPath.length() + 1);
				child.setName(childName);
			}
			trimPath(child, childPath);
		}
		return entry;
	}

}
