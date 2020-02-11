/**
 * HpcCollectionController.java
 *
 * Copyright SVG, Inc.
 * Copyright Leidos Biomedical Research, Inc
 * 
 * Distributed under the OSI-approved BSD 3-Clause License.
 * See https://ncisvn.nci.nih.gov/svn/HPC_Data_Management/branches/hpc-prototype-dev/LICENSE.txt for details.
 */
package gov.nih.nci.doe.web.controller;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.DoeCollectionModel;
import gov.nih.nci.doe.web.model.DoeMetadataAttrEntry;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.domain.datamanagement.HpcDataHierarchy;
import gov.nih.nci.hpc.domain.datamanagement.HpcDirectoryScanPathMap;
import gov.nih.nci.hpc.domain.datatransfer.HpcFileLocation;
import gov.nih.nci.hpc.domain.datatransfer.HpcPatternType;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataValidationRule;
import gov.nih.nci.hpc.dto.datamanagement.HpcBulkDataObjectRegistrationRequestDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionRegistrationDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementModelDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementRulesDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectRegistrationItemDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDirectoryScanRegistrationItemDTO;

/**
 * <p>
 * 
 * 
 * </p>
 */

@EnableAutoConfiguration
public abstract class DoeCreateCollectionDataFileController extends AbstractDoeController {
	@Value("${gov.nih.nci.hpc.server.collection}")
	private String serviceURL;
	@Value("${gov.nih.nci.hpc.server.model}")
	private String hpcModelURL;
	@Value("${gov.nih.nci.hpc.server.collection.acl.user}")
	private String collectionAclURL;

	

	protected void clearSessionAttrs(HttpSession session) {
		session.removeAttribute("datafilePath");
		session.removeAttribute("collection_type");
		session.removeAttribute("basePathSelected");
		session.removeAttribute("GlobusEndpoint");
		session.removeAttribute("GlobusEndpointPath");
		session.removeAttribute("GlobusEndpointFiles");
		session.removeAttribute("GlobusEndpointFolders");
		session.removeAttribute("parentCollection");
		session.removeAttribute("metadataEntries");
		session.removeAttribute("userMetadataEntries");
		session.removeAttribute("parent");
		session.removeAttribute("includeCriteria");
		session.removeAttribute("excludeCriteria");
		session.removeAttribute("dryRun");
	}

	@SuppressWarnings("unchecked")
	protected void populateBasePaths(HttpServletRequest request, HttpSession session, String path)
			throws DoeWebException {
		String authToken = (String) session.getAttribute("hpcUserToken");
		Set<String> basePaths = (Set<String>) session.getAttribute("basePaths");
		String userId = (String) session.getAttribute("hpcUserId");
		if (basePaths == null || basePaths.isEmpty()) {
			HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");
			if (modelDTO == null) {
				modelDTO = DoeClientUtil.getDOCModel(authToken, hpcModelURL, sslCertPath, sslCertPassword);
				session.setAttribute("userDOCModel", modelDTO);
			}
			DoeClientUtil.populateBasePaths(session, modelDTO, authToken, userId, collectionAclURL, sslCertPath,
					sslCertPassword);
			basePaths = (Set<String>) session.getAttribute("basePaths");
		}

		String selectedBasePath = DoeClientUtil.getBasePath(request);
		if (selectedBasePath == null)
			selectedBasePath = (String) session.getAttribute("basePathSelected");
		else
			session.setAttribute("basePathSelected", selectedBasePath);

		setCollectionPath(request, path);
	}

	@SuppressWarnings("unchecked")
	protected void setInputParameters(HttpServletRequest request, HttpSession session, String path,
			String parent, String source, boolean refresh) {
		String endPoint = request.getParameter("endpoint_id");
		String globusPath = request.getParameter("path");
		List<String> fileNames = new ArrayList<String>();
		List<String> folderNames = new ArrayList<String>();
		Enumeration<String> names = request.getParameterNames();
		while (names.hasMoreElements()) {
			String paramName = names.nextElement();
			if (paramName.startsWith("file"))
				fileNames.add(request.getParameter(paramName));
			else if (paramName.startsWith("folder"))
				folderNames.add(request.getParameter(paramName));
		}
		if (endPoint == null)
			endPoint = (String) session.getAttribute("GlobusEndpoint");
		else
			session.setAttribute("GlobusEndpoint", endPoint);

		

		if (refresh || globusPath == null)
			globusPath = (String) session.getAttribute("GlobusEndpointPath");
		else
			session.setAttribute("GlobusEndpointPath", globusPath);


		if (fileNames.isEmpty())
			fileNames = (List<String>) session.getAttribute("GlobusEndpointFiles");
		else
			session.setAttribute("GlobusEndpointFiles", fileNames);

		if (folderNames.isEmpty())
			folderNames = (List<String>) session.getAttribute("GlobusEndpointFolders");
		else
			session.setAttribute("GlobusEndpointFolders", folderNames);

		setCriteria(request, session);


	}

	protected void setCriteria(HttpServletRequest request, HttpSession session)
	{
		String includeCriteria = request.getParameter("includeCriteria");
		String excludeCriteria = request.getParameter("excludeCriteria");
		String dryRun = request.getParameter("dryrun");

		if (includeCriteria == null)
			includeCriteria = (String) session.getAttribute("includeCriteria");
		else
			session.setAttribute("includeCriteria", includeCriteria);

		if (excludeCriteria == null)
			excludeCriteria = (String) session.getAttribute("excludeCriteria");
		else
			session.setAttribute("excludeCriteria", excludeCriteria);	

		if (dryRun == null)
			dryRun = (String) session.getAttribute("dryRun");
		else
			session.setAttribute("dryRun", dryRun);
	
	}
	
	@SuppressWarnings("unchecked")
	protected HpcBulkDataObjectRegistrationRequestDTO constructBulkRequest(HttpServletRequest request,
			HttpSession session, String path) {
		HpcBulkDataObjectRegistrationRequestDTO dto = new HpcBulkDataObjectRegistrationRequestDTO();
		String datafilePath = (String) session.getAttribute("datafilePath");
		String globusEndpoint = (String) session.getAttribute("GlobusEndpoint");
		//String selectedBasePath = (String) session.getAttribute("basePathSelected");
		String globusEndpointPath = (String) session.getAttribute("GlobusEndpointPath");
		String includeCriteria = (String) session.getAttribute("includeCriteria");
		String excludeCriteria = (String) session.getAttribute("excludeCriteria");
		String dryRun = (String) request.getParameter("dryrun");
		String criteriaType = (String)request.getParameter("criteriaType");
		List<String> globusEndpointFiles = (List<String>) session.getAttribute("GlobusEndpointFiles");
		List<String> globusEndpointFolders = (List<String>) session.getAttribute("GlobusEndpointFolders");

		if (globusEndpointFiles != null) {
			List<HpcDataObjectRegistrationItemDTO> files = new ArrayList<HpcDataObjectRegistrationItemDTO>();
			for (String fileName : globusEndpointFiles) {
				HpcDataObjectRegistrationItemDTO file = new HpcDataObjectRegistrationItemDTO();
				HpcFileLocation source = new HpcFileLocation();
				source.setFileContainerId(globusEndpoint);
				source.setFileId(globusEndpointPath + fileName);
				file.setSource(source);
				file.setCreateParentCollections(true);
				file.setPath(path + "/" + fileName);
				System.out.println(path + "/" + fileName);
				// file.getParentCollectionMetadataEntries().addAll(metadataEntries);
				files.add(file);
			}
			dto.getDataObjectRegistrationItems().addAll(files);
		}

		List<String> include = new ArrayList<String>();
		if(includeCriteria != null && !includeCriteria.isEmpty())
		{
			StringTokenizer tokens = new StringTokenizer(includeCriteria, "\r\n");
			while(tokens.hasMoreTokens())
				include.add(tokens.nextToken());
		}
		
		List<String> exclude = new ArrayList<String>();
		if(excludeCriteria != null && !excludeCriteria.isEmpty())
		{
			StringTokenizer tokens = new StringTokenizer(excludeCriteria, "\r\n");
			while(tokens.hasMoreTokens())
				exclude.add(tokens.nextToken());
		}

		if (globusEndpointFolders != null) {
			List<HpcDirectoryScanRegistrationItemDTO> folders = new ArrayList<HpcDirectoryScanRegistrationItemDTO>();
			for (String folderName : globusEndpointFolders) {
				HpcDirectoryScanRegistrationItemDTO folder = new HpcDirectoryScanRegistrationItemDTO();
				HpcFileLocation source = new HpcFileLocation();
				source.setFileContainerId(globusEndpoint);
				String fromPath = globusEndpointPath.endsWith("/") ?  globusEndpointPath + folderName : globusEndpointPath + "/" + folderName;
				String toPath = "/" + folderName;
				source.setFileId(fromPath);
				folder.setBasePath(datafilePath);
				folder.setScanDirectoryLocation(source);
				folders.add(folder);
				if(!fromPath.equals(toPath)) {
					HpcDirectoryScanPathMap pathDTO = new HpcDirectoryScanPathMap();
					pathDTO.setFromPath(fromPath);
					pathDTO.setToPath(toPath);
					folder.setPathMap(pathDTO);
				}
				if(criteriaType != null && criteriaType.equals("Simple"))
					folder.setPatternType(HpcPatternType.SIMPLE);
				else
					folder.setPatternType(HpcPatternType.REGEX);
				if(exclude.size() > 0)
					folder.getExcludePatterns().addAll(exclude);
				if(include.size() > 0)
					folder.getIncludePatterns().addAll(include);
			}
			dto.getDirectoryScanRegistrationItems().addAll(folders);
		}
		dto.setDryRun(dryRun != null && dryRun.equals("on"));
		return dto;
	}

	protected List<HpcMetadataEntry> getMetadataEntries(HttpServletRequest request, HttpSession session, String path) {
		Enumeration<String> params = request.getParameterNames();
		List<HpcMetadataEntry> metadataEntries = new ArrayList<>();

		while (params.hasMoreElements()) {
			String paramName = params.nextElement();
			if (paramName.startsWith("zAttrStr_")) {
				HpcMetadataEntry entry = new HpcMetadataEntry();
				String attrName = paramName.substring("zAttrStr_".length());
				String[] attrValue = request.getParameterValues(paramName);
				entry.setAttribute(attrName);
				entry.setValue(attrValue[0]);
				metadataEntries.add(entry);
			} else if (paramName.startsWith("_addAttrName")) {
				HpcMetadataEntry entry = new HpcMetadataEntry();
				String attrId = paramName.substring("_addAttrName".length());
				String[] attrName = request.getParameterValues(paramName);
				String[] attrValue = request.getParameterValues("_addAttrValue" + attrId);
				if (attrName.length > 0 && !attrName[0].isEmpty())
					entry.setAttribute(attrName[0]);
				else
					throw new DoeWebException("Invalid metadata attribute name. Empty value is not valid!");
				if (attrValue.length > 0 && !attrValue[0].isEmpty())
					entry.setValue(attrValue[0]);
				else
					throw new DoeWebException("Invalid metadata attribute value. Empty value is not valid!");
				metadataEntries.add(entry);
			}
		}
		return metadataEntries;
	}



	protected void checkParent(String parent, HttpSession session) {
		String authToken = (String) session.getAttribute("hpcUserToken");
		if (parent != null && !parent.isEmpty()) {
			HpcCollectionListDTO collections = DoeClientUtil.getCollection(authToken, serviceURL, parent, false,
					sslCertPath, sslCertPassword);
			if (collections != null && collections.getCollections() != null && collections.getCollections().size() > 0)
				session.setAttribute("parentCollection", collections.getCollections().get(0));
		}

	}

	protected List<String> getCollectionTypes(List<HpcMetadataValidationRule> rules) {
		List<String> collectionTypesSet = new ArrayList<String>();
		for (HpcMetadataValidationRule rule : rules) {
			if (rule.getMandatory() && rule.getAttribute().equals("collection_type"))
				collectionTypesSet.addAll(rule.getValidValues());
		}
		return collectionTypesSet;
	}

	protected List<String> getSubCollectionTypes(String collectionType, HpcDataHierarchy dataHierarchy) {
		List<String> types = new ArrayList<String>();
		if (dataHierarchy == null || dataHierarchy.getSubCollectionsHierarchies() == null)
			return types;
		if (dataHierarchy.getCollectionType().equals(collectionType)) {
			List<HpcDataHierarchy> subs = dataHierarchy.getSubCollectionsHierarchies();
			for (HpcDataHierarchy sub : subs)
				types.add(sub.getCollectionType());
		} else {
			List<HpcDataHierarchy> subs = dataHierarchy.getSubCollectionsHierarchies();
			for (HpcDataHierarchy sub : subs)
				return getSubCollectionTypes(collectionType, sub);
		}

		return types;
	}
	
	private boolean isDataObjectContainer(String collectionType, HpcDataHierarchy dataHierarchy) {
		if (dataHierarchy == null)
			return true;
		if (dataHierarchy.getCollectionType().equals(collectionType))
			return dataHierarchy.getIsDataObjectContainer();
		else {
			List<HpcDataHierarchy> subs = dataHierarchy.getSubCollectionsHierarchies();
			for (HpcDataHierarchy sub : subs)
				return isDataObjectContainer(collectionType, sub);
		}
		return false;
	}

	@SuppressWarnings("unchecked")
	protected void populateFormAttributes(HttpServletRequest request, HttpSession session,
			 String basePath, String collectionType, boolean refresh, boolean datafile) {
		String authToken = (String) session.getAttribute("hpcUserToken");

		HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");
		if (modelDTO == null) {
			modelDTO = DoeClientUtil.getDOCModel(authToken, hpcModelURL, sslCertPath, sslCertPassword);
			session.setAttribute("userDOCModel", modelDTO);
		}
		List<HpcMetadataValidationRule> rules = null;
		HpcDataManagementRulesDTO basePathRules = DoeClientUtil.getBasePathManagementRules(modelDTO, basePath);
		if (basePathRules != null) {
			HpcDataHierarchy dataHierarchy = basePathRules.getDataHierarchy();
			if(dataHierarchy != null)
				
			if (datafile) {
				if(!refresh && !isDataObjectContainer(collectionType, dataHierarchy))
					throw new DoeWebException("Adding a data file is not allowed under collection type: " + collectionType);
				rules = basePathRules.getDataObjectMetadataValidationRules();
			}
			else
				rules = basePathRules.getCollectionMetadataValidationRules();
		}

		HpcCollectionDTO collectionDTO = (HpcCollectionDTO) session.getAttribute("parentCollection");
		List<DoeMetadataAttrEntry> cachedEntries = (List<DoeMetadataAttrEntry>) session.getAttribute("metadataEntries");
		List<DoeMetadataAttrEntry> metadataEntries = new ArrayList<DoeMetadataAttrEntry>();
		List<DoeMetadataAttrEntry> userMetadataEntries = new ArrayList<DoeMetadataAttrEntry>();
		List<String> attributeNames = new ArrayList<String>();
		if (rules != null && !rules.isEmpty()) {
			for (HpcMetadataValidationRule rule : rules) {
				if ((rule.getMandatory()
						&& (rule.getCollectionTypes().contains(collectionType) || rule.getCollectionTypes().isEmpty()))
						&& !rule.getAttribute().equals("collection_type")) {
					DoeMetadataAttrEntry entry = new DoeMetadataAttrEntry();
					entry.setAttrName(rule.getAttribute());
					attributeNames.add(rule.getAttribute());
					entry.setAttrValue(getFormAttributeValue(request, "zAttrStr_" + rule.getAttribute(), cachedEntries, "zAttrStr_"));
					if (entry.getAttrValue() == null) {
						if (!refresh)
							entry.setAttrValue(getCollectionAttrValue(collectionDTO, rule.getAttribute()));
						else
							entry.setAttrValue(rule.getDefaultValue());
					}
					if (rule.getValidValues() != null && !rule.getValidValues().isEmpty()) {
						List<String> validValues = new ArrayList<String>();
						for (String value : rule.getValidValues())
							validValues.add(value);
						entry.setValidValues(validValues);
					}
					entry.setDescription(rule.getDescription());
					metadataEntries.add(entry);
				}
			}
		}

		// Handle custom attributes. If refresh, ignore them
		if (!refresh) {
			Enumeration<String> params = request.getParameterNames();
			while (params.hasMoreElements()) {
				String paramName = params.nextElement();
				if (paramName.startsWith("_addAttrName")) {
					DoeMetadataAttrEntry entry = new DoeMetadataAttrEntry();
					String[] attrName = request.getParameterValues(paramName);
					String attrId = paramName.substring("_addAttrName".length());
					String attrValue = getFormAttributeValue(request, "_addAttrValue" + attrId, cachedEntries, "_addAttrValue"); 
					if (attrName.length > 0 && !attrName[0].isEmpty())
						entry.setAttrName(attrName[0]);
					entry.setAttrValue(attrValue);
					userMetadataEntries.add(entry);
				}
			}
		}
	
		session.setAttribute("metadataEntries", metadataEntries);
		session.setAttribute("userMetadataEntries", userMetadataEntries);

	}

	private String getCollectionAttrValue(HpcCollectionDTO collectionDTO, String attrName) {
		if (collectionDTO == null || collectionDTO.getMetadataEntries() == null
				|| collectionDTO.getMetadataEntries().getSelfMetadataEntries() == null)
			return null;

		for (HpcMetadataEntry entry : collectionDTO.getMetadataEntries().getSelfMetadataEntries()) {
			if (entry.getAttribute().equals(attrName))
				return entry.getValue();
		}
		return null;
	}

	private String getFormAttributeValue(HttpServletRequest request, String attributeName,
			List<DoeMetadataAttrEntry> cachedEntries, String prefix) {
		String[] attrValue = request.getParameterValues(attributeName);
		if (attrValue != null)
			return attrValue[0];
		else {
			if (cachedEntries == null || cachedEntries.size() == 0)
				return null;
			for (DoeMetadataAttrEntry entry : cachedEntries) {
				if (attributeName.equals(prefix + entry.getAttrName()))
					return entry.getAttrValue();
			}
		}
		return null;
	}

	protected void setCollectionPath(HttpServletRequest request, String parentPath) {
		String path = request.getParameter("path");
		if (path != null && !path.isEmpty()) {			
			return;
		}

		if (parentPath == null || parentPath.isEmpty()) {
			String[] basePathValues = request.getParameterValues("basePath");
			String basePath = null;
			if (basePathValues == null || basePathValues.length == 0)
				basePath = (String) request.getAttribute("basePath");
			else
				basePath = basePathValues[0];
		} 
	}

	protected HpcCollectionRegistrationDTO constructRequest(HttpServletRequest request, HttpSession session, DoeCollectionModel doeCollection) throws DoeWebException {
		Enumeration<String> params = request.getParameterNames();
		HpcCollectionRegistrationDTO dto = new HpcCollectionRegistrationDTO();
		List<HpcMetadataEntry> metadataEntries = new ArrayList<>();
		List<DoeMetadataAttrEntry> selfMetadataEntries = new ArrayList<>();
		while (params.hasMoreElements()) {
			String paramName = params.nextElement();
			if (paramName.startsWith("zAttrStr_")) {
				HpcMetadataEntry entry = new HpcMetadataEntry();
				DoeMetadataAttrEntry attrEntry = new DoeMetadataAttrEntry();
				String attrName = paramName.substring("zAttrStr_".length());
				String[] attrValue = request.getParameterValues(paramName);
				entry.setAttribute(attrName);
				entry.setValue(attrValue[0]);
				metadataEntries.add(entry);
				attrEntry.setAttrName(attrName);
				attrEntry.setAttrValue(attrValue[0]);
				attrEntry.setSystemAttr(false);
				selfMetadataEntries.add(attrEntry);
			} else if (paramName.startsWith("_addAttrName")) {
				HpcMetadataEntry entry = new HpcMetadataEntry();
				String attrId = paramName.substring("_addAttrName".length());
				String[] attrName = request.getParameterValues(paramName);
				String[] attrValue = request.getParameterValues("_addAttrValue" + attrId);
				if (attrName.length > 0 && !attrName[0].isEmpty())
					entry.setAttribute(attrName[0]);
				else
					throw new DoeWebException("Invalid metadata attribute name. Empty value is not valid!");
				if (attrValue.length > 0 && !attrValue[0].isEmpty())
					entry.setValue(attrValue[0]);
				else
					throw new DoeWebException("Invalid metadata attribute value. Empty value is not valid!");
				metadataEntries.add(entry);
				DoeMetadataAttrEntry attrEntry = new DoeMetadataAttrEntry();
				attrEntry.setAttrName(attrName[0]);
				attrEntry.setAttrValue(attrValue[0]);
				attrEntry.setSystemAttr(false);
				selfMetadataEntries.add(attrEntry);
			} else if(paramName.startsWith("path")) {
				String[] path = request.getParameterValues(paramName);
				doeCollection.setPath(path[0]);
			}
		}
		if (doeCollection != null)
			doeCollection.setSelfMetadataEntries(selfMetadataEntries);
		dto.getMetadataEntries().addAll(metadataEntries);
		return dto;
	}

}
