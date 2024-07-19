package gov.nih.nci.doe.web.controller;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

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
import gov.nih.nci.doe.web.model.DoeCollectionEntry;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.domain.datamanagement.HpcCollection;
import gov.nih.nci.hpc.domain.datamanagement.HpcCollectionListingEntry;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;

/**
 * MoDaC browse
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("")
public class RetrieveMetadataController extends AbstractDoeController {

	@Value("${gov.nih.nci.hpc.server.collection}")
	private String collectionURL;

	// The logger instance.
	private final Logger logger = LoggerFactory.getLogger(this.getClass().getName());

	@GetMapping(value = "/metaData", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<?> getUserMetaDataByPath(@RequestParam(value = "selectedPath") String selectedPath,
			@RequestParam(value = "levelName") String levelName,
			@RequestParam(value = "isDataObject") String isDataObject, HttpSession session, HttpServletRequest request,
			HttpServletResponse response) throws DoeWebException {

		List<KeyValueBean> entryList = getUserMetaDataAttributesByPath(selectedPath, levelName, isDataObject, session);
		return new ResponseEntity<>(entryList, HttpStatus.OK);

	}

	@GetMapping(value = "/collectionList", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<?> getCollectionNamesList(@RequestParam(value = "selectedPath") String selectedPath,
			@RequestParam(required = false) String refreshNode, HttpSession session, HttpServletRequest request,
			HttpServletResponse response) {

		String authToken = (String) session.getAttribute("hpcUserToken");
		DoeCollectionEntry browserEntry = (DoeCollectionEntry) session.getAttribute("browserEntry");
		List<KeyValueBean> results = new ArrayList<>();
		boolean refresh = false;

		if (!StringUtils.isEmpty(refreshNode)) {
			refresh = true;
		}

		try {
			if (selectedPath != null) {
				browserEntry = getTreeNodes(selectedPath.trim(), browserEntry, authToken, true, refresh);

				browserEntry = trimPath(browserEntry, browserEntry.getName());
				String name = browserEntry.getName().substring(browserEntry.getName().lastIndexOf('/') + 1);
				browserEntry.setName(name);
				List<DoeCollectionEntry> children = browserEntry.getChildren();

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

	@GetMapping(value = "/programList", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<?> getProgamNamesList(HttpSession session, HttpServletRequest request) {

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

				DoeCollectionEntry browserEntry = (DoeCollectionEntry) session.getAttribute("browserEntry");
				if (browserEntry == null) {
					browserEntry = new DoeCollectionEntry();

					browserEntry.setFullPath(path);

					browserEntry.setName(path);
					browserEntry = getTreeNodes(path, browserEntry, authToken, true, false);
					browserEntry = trimPath(browserEntry, browserEntry.getName());
					session.setAttribute("browserEntry", browserEntry);
				}

				if (browserEntry != null) {
					List<DoeCollectionEntry> children = browserEntry.getChildren();
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

	private DoeCollectionEntry getSelectedEntry(String path, DoeCollectionEntry browserEntry) {
		if (browserEntry == null)
			return null;

		if (browserEntry.getFullPath() != null && browserEntry.getFullPath().equals(path))
			return browserEntry;

		for (DoeCollectionEntry childEntry : browserEntry.getChildren()) {
			if (childEntry.getFullPath() != null && childEntry.getFullPath().equals(path))
				return childEntry;
			else {
				// Drill down childEntry, but only if this child entry
				// happens to be an ancestor of the path we are looking for
				// Else we proceed to next childEntry
				if (childEntry.getFullPath() != null && path.contains(childEntry.getFullPath())) {
					DoeCollectionEntry entry = getSelectedEntry(path, childEntry);
					if (entry != null)
						return entry;
				}
			}
		}
		return null;
	}

	private DoeCollectionEntry getTreeNodes(String path, DoeCollectionEntry browserEntry, String authToken, boolean partial,
			boolean refresh) throws DoeWebException {

		path = path.trim();
		DoeCollectionEntry selectedEntry = getSelectedEntry(path, browserEntry);
		if (refresh && selectedEntry != null) {
			selectedEntry.setPopulated(false);
		}

		if (selectedEntry != null && selectedEntry.isPopulated())
			return partial ? selectedEntry : browserEntry;
		if (selectedEntry != null && selectedEntry.getChildren() != null)
			selectedEntry.getChildren().clear();
		if (selectedEntry == null) {
			selectedEntry = new DoeCollectionEntry();
			selectedEntry.setName(path);
		}
		// If partial is true or refresh is true, then it means we need to
		// retrieve info on the selectedEntry also along with it's child list
		// Else, we only get the child list, since we already have the
		// info about the selectedEntry.
		HpcCollectionListDTO collections = DoeClientUtil.getCollection(authToken, collectionURL, path,
				partial || refresh ? false : true, partial || refresh);

		for (HpcCollectionDTO collectionDTO : collections.getCollections()) {
			HpcCollection collection = collectionDTO.getCollection();

			if (collection.getAbsolutePath() != null) {
				selectedEntry.setFullPath(collection.getAbsolutePath());
				selectedEntry.setName(collection.getCollectionName());
			}

			// This will ensure that the next time we access this path
			// we dont read again from DB, unless an explicit refresh
			// request has been made
			selectedEntry.setPopulated(true);

			for (HpcCollectionListingEntry listEntry : collection.getSubCollections()) {
				DoeCollectionEntry listChildEntry = new DoeCollectionEntry();
				listChildEntry.setFullPath(listEntry.getPath());
				listChildEntry.setName(listEntry.getPath());
				listChildEntry.setPopulated(false);

				DoeCollectionEntry emptyEntry = new DoeCollectionEntry();
				emptyEntry.setName("");
				listChildEntry.getChildren().add(emptyEntry);

				selectedEntry.getChildren().add(listChildEntry);
			}

			if (selectedEntry.getChildren() == null || selectedEntry.getChildren().isEmpty()) {
				DoeCollectionEntry listChildEntry = new DoeCollectionEntry();
				listChildEntry.setFullPath(" ");
				listChildEntry.setName(" ");
				listChildEntry.setPopulated(true);
				selectedEntry.getChildren().add(listChildEntry);
			}
		}

		return partial ? selectedEntry : browserEntry;
	}

	private DoeCollectionEntry trimPath(DoeCollectionEntry entry, String parentPath) {
		for (DoeCollectionEntry child : entry.getChildren()) {
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
