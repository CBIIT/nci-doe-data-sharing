/**
 * DoeCollectionController.java
 *
 * Copyright SVG, Inc.
 * Copyright Leidos Biomedical Research, Inc
 * 
 * Distributed under the OSI-approved BSD 3-Clause License.
 * See https://ncisvn.nci.nih.gov/svn/HPC_Data_Management/branches/hpc-prototype-dev/LICENSE.txt for details.
 */
package gov.nih.nci.doe.web.controller;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.apache.commons.codec.binary.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.ui.Model;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.DoeCollectionModel;
import gov.nih.nci.doe.web.model.DoeMetadataAttrEntry;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.domain.datamanagement.HpcDataHierarchy;
import gov.nih.nci.hpc.domain.datamanagement.HpcDirectoryScanPathMap;
import gov.nih.nci.hpc.domain.datatransfer.HpcFileLocation;
import gov.nih.nci.hpc.domain.datatransfer.HpcGlobusScanDirectory;
import gov.nih.nci.hpc.domain.datatransfer.HpcGlobusUploadSource;
import gov.nih.nci.hpc.domain.datatransfer.HpcS3Account;
import gov.nih.nci.hpc.domain.datatransfer.HpcS3ScanDirectory;
import gov.nih.nci.hpc.domain.datatransfer.HpcS3UploadSource;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataValidationRule;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionRegistrationDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementModelDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementRulesDTO;

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
	protected void setInputParameters(HttpServletRequest request, HttpSession session,Model model) {
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

			model.addAttribute("endpoint_id", endPoint);

			if (globusPath == null)
				globusPath = (String) session.getAttribute("GlobusEndpointPath");
			else
				session.setAttribute("GlobusEndpointPath", globusPath);

			model.addAttribute("endpoint_path", globusPath);

			if (fileNames.isEmpty())
				fileNames = (List<String>) session.getAttribute("GlobusEndpointFiles");
			else
				session.setAttribute("GlobusEndpointFiles", fileNames);

			if (folderNames.isEmpty())
				folderNames = (List<String>) session.getAttribute("GlobusEndpointFolders");
			else
				session.setAttribute("GlobusEndpointFolders", folderNames);

			if (endPoint != null)
				model.addAttribute("async", true);

			if (fileNames != null && !fileNames.isEmpty())
				model.addAttribute("fileNames", fileNames);

			if (folderNames != null && !folderNames.isEmpty())
				model.addAttribute("folderNames", folderNames);
			
			model.addAttribute("datafilePath",session.getAttribute("datafilePath"));
			model.addAttribute("institutePath",session.getAttribute("institutePath"));
			model.addAttribute("studyPath",session.getAttribute("studyPath"));

	}

	
	@SuppressWarnings("unchecked")
	protected gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO constructV2BulkRequest(HttpServletRequest request,
			HttpSession session, String path) {
		gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO dto = new gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO();
		
		String datafilePath = (String) session.getAttribute("datafilePath");
		String globusEndpoint = (String) session.getAttribute("GlobusEndpoint");
		String globusEndpointPath = (String) session.getAttribute("GlobusEndpointPath");
		List<String> globusEndpointFiles = (List<String>) session.getAttribute("GlobusEndpointFiles");
		List<String> globusEndpointFolders = (List<String>) session.getAttribute("GlobusEndpointFolders");

		
		String bulkType = (String)request.getParameter("uploadType");
		String bucketName = (String)request.getParameter("bucketName");
		String s3Path = (String)request.getParameter("s3Path");
		String accessKey = (String)request.getParameter("accessKey");
		String secretKey = (String)request.getParameter("secretKey");
		String region = (String)request.getParameter("region");
		String s3File = (String)request.getParameter("s3File");
		boolean isS3File = s3File != null && s3File.equals("on");

		
		if (StringUtils.equals(bulkType, "globus") && globusEndpointFiles != null) {
			List<gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO> files = new ArrayList<gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO>();
			for (String fileName : globusEndpointFiles) {
				gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO file = new gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO();
				HpcFileLocation source = new HpcFileLocation();
				source.setFileContainerId(globusEndpoint);
				source.setFileId(globusEndpointPath + fileName);
				HpcGlobusUploadSource globusSource = new HpcGlobusUploadSource();
				globusSource.setSourceLocation(source);
				file.setGlobusUploadSource(globusSource);
				file.setCreateParentCollections(true);
				file.setPath(path + "/" + fileName);
				System.out.println(path + "/" + fileName);
				files.add(file);
			}
			dto.getDataObjectRegistrationItems().addAll(files);
		}

		if (StringUtils.equals(bulkType, "globus") && globusEndpointFolders != null) {
			List<gov.nih.nci.hpc.dto.datamanagement.v2.HpcDirectoryScanRegistrationItemDTO> folders = new ArrayList<gov.nih.nci.hpc.dto.datamanagement.v2.HpcDirectoryScanRegistrationItemDTO>();
			for (String folderName : globusEndpointFolders) {
				gov.nih.nci.hpc.dto.datamanagement.v2.HpcDirectoryScanRegistrationItemDTO folder = new gov.nih.nci.hpc.dto.datamanagement.v2.HpcDirectoryScanRegistrationItemDTO();
				HpcFileLocation source = new HpcFileLocation();
				source.setFileContainerId(globusEndpoint);
				String fromPath = globusEndpointPath.endsWith("/") ?  globusEndpointPath + folderName : globusEndpointPath + "/" + folderName;
				String toPath = "/" + folderName;
				source.setFileId(fromPath);
				folder.setBasePath(datafilePath);
				HpcGlobusScanDirectory globusDirectory = new HpcGlobusScanDirectory();
				globusDirectory.setDirectoryLocation(source);
				folder.setGlobusScanDirectory(globusDirectory);
				folders.add(folder);
				if(!fromPath.equals(toPath)) {
					HpcDirectoryScanPathMap pathDTO = new HpcDirectoryScanPathMap();
					pathDTO.setFromPath(fromPath);
					pathDTO.setToPath(toPath);
					folder.setPathMap(pathDTO);
				}
			}
			dto.getDirectoryScanRegistrationItems().addAll(folders);
		}
		if (StringUtils.equals(bulkType, "s3") && s3Path != null && isS3File) {
			List<gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO> files = new ArrayList<gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO>();
			gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO file = new gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO();
			HpcFileLocation source = new HpcFileLocation();
			source.setFileContainerId(bucketName);
			source.setFileId(s3Path);
			HpcS3UploadSource s3UploadSource = new HpcS3UploadSource();
			HpcS3Account s3Account = new HpcS3Account();
			s3Account.setAccessKey(accessKey);
			s3Account.setSecretKey(secretKey);
			s3Account.setRegion(region);
			s3UploadSource.setAccount(s3Account);
			s3UploadSource.setSourceLocation(source);
			file.setS3UploadSource(s3UploadSource);
			file.setCreateParentCollections(true);
			Path s3FilePath = Paths.get(s3Path);
			file.setPath(path + "/" + s3FilePath.getFileName());
			System.out.println(path + "/" + s3FilePath.getFileName());
			files.add(file);
			dto.getDataObjectRegistrationItems().addAll(files);
		}
		else if (StringUtils.equals(bulkType, "s3") && s3Path != null) {
			List<gov.nih.nci.hpc.dto.datamanagement.v2.HpcDirectoryScanRegistrationItemDTO> folders = new ArrayList<gov.nih.nci.hpc.dto.datamanagement.v2.HpcDirectoryScanRegistrationItemDTO>();
			gov.nih.nci.hpc.dto.datamanagement.v2.HpcDirectoryScanRegistrationItemDTO folder = new gov.nih.nci.hpc.dto.datamanagement.v2.HpcDirectoryScanRegistrationItemDTO();
			HpcFileLocation source = new HpcFileLocation();
			source.setFileContainerId(bucketName);
			source.setFileId(s3Path);
			folder.setBasePath(path);
			HpcS3ScanDirectory s3Directory = new HpcS3ScanDirectory();
			s3Directory.setDirectoryLocation(source);
			HpcS3Account s3Account = new HpcS3Account();
			s3Account.setAccessKey(accessKey);
			s3Account.setSecretKey(secretKey);
			s3Account.setRegion(region);
			s3Directory.setAccount(s3Account);
			folder.setS3ScanDirectory(s3Directory);
			folders.add(folder);

			dto.getDirectoryScanRegistrationItems().addAll(folders);
		}
		
		
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
