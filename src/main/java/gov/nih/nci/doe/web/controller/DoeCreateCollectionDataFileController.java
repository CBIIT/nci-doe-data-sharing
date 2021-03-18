package gov.nih.nci.doe.web.controller;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.List;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.ui.Model;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.domain.LookUp;
import gov.nih.nci.doe.web.model.DoeCollectionModel;
import gov.nih.nci.doe.web.model.DoeMetadataAttrEntry;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.doe.web.util.LambdaUtils;
import gov.nih.nci.hpc.domain.datamanagement.HpcDataHierarchy;
import gov.nih.nci.hpc.domain.datamanagement.HpcDirectoryScanPathMap;
import gov.nih.nci.hpc.domain.datatransfer.HpcFileLocation;
import gov.nih.nci.hpc.domain.datatransfer.HpcS3Account;
import gov.nih.nci.hpc.domain.datatransfer.HpcS3ScanDirectory;
import gov.nih.nci.hpc.domain.datatransfer.HpcScanDirectory;
import gov.nih.nci.hpc.domain.datatransfer.HpcStreamingUploadSource;
import gov.nih.nci.hpc.domain.datatransfer.HpcUploadSource;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataValidationRule;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionRegistrationDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementModelDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementRulesDTO;
import gov.nih.nci.hpc.domain.datatransfer.HpcGoogleDriveScanDirectory;

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
		session.removeAttribute("parent");
		session.removeAttribute("institutePath");
		session.removeAttribute("studyPath");
		session.removeAttribute("fileIds");
		session.removeAttribute("folderIds");
		session.removeAttribute("accessToken");
		session.removeAttribute("authorized");
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
	protected void setInputParameters(HttpServletRequest request, HttpSession session, Model model) {
		String endPoint = request.getParameter("endpoint_id");
		String globusPath = request.getParameter("path");
		List<String> fileNames = new ArrayList<String>();
		List<String> folderNames = new ArrayList<String>();
		List<String> fileIds = new ArrayList<String>();
		List<String> folderIds = new ArrayList<String>();
		Enumeration<String> names = request.getParameterNames();
		String accessToken = (String) session.getAttribute("accessToken");

		while (names.hasMoreElements()) {
			String paramName = names.nextElement();
			if (paramName.startsWith("fileNames") && request.getParameterValues(paramName) != null)
				fileNames.addAll(Arrays.asList(request.getParameterValues(paramName)));
			else if (paramName.startsWith("folderNames") && request.getParameterValues(paramName) != null)
				folderNames.addAll(Arrays.asList(request.getParameterValues(paramName)));
			else if (paramName.startsWith("fileIds") && request.getParameterValues(paramName) != null)
				fileIds.addAll(Arrays.asList(request.getParameterValues(paramName)));
			else if (paramName.startsWith("folderIds") && request.getParameterValues(paramName) != null)
				folderIds.addAll(Arrays.asList(request.getParameterValues(paramName)));
			else if (paramName.startsWith("file"))
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

		if (fileIds.isEmpty())
			fileIds = (List<String>) session.getAttribute("fileIds");
		else
			session.setAttribute("fileIds", fileIds);

		if (folderIds.isEmpty())
			folderIds = (List<String>) session.getAttribute("folderIds");
		else
			session.setAttribute("folderIds", folderIds);

		if (endPoint != null)
			model.addAttribute("async", true);

		if (fileNames != null && !fileNames.isEmpty())
			model.addAttribute("fileNames", fileNames);

		if (folderNames != null && !folderNames.isEmpty())
			model.addAttribute("folderNames", folderNames);

		if (fileIds != null && !fileIds.isEmpty())
			model.addAttribute("fileIds", fileIds);

		if (folderIds != null && !folderIds.isEmpty())
			model.addAttribute("folderIds", folderIds);

		if (accessToken != null) {
			model.addAttribute("accessToken", accessToken);
			model.addAttribute("authorized", "true");
		}

		model.addAttribute("datafilePath", session.getAttribute("datafilePath"));
		model.addAttribute("institutePath", session.getAttribute("institutePath"));
		model.addAttribute("studyPath", session.getAttribute("studyPath"));

	}

	@SuppressWarnings("unchecked")
	protected gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO constructV2BulkRequest(
			HttpServletRequest request, HttpSession session, String path) {
		gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO dto = new gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO();

		String datafilePath = (String) session.getAttribute("datafilePath");
		String globusEndpoint = (String) session.getAttribute("GlobusEndpoint");
		String globusEndpointPath = (String) session.getAttribute("GlobusEndpointPath");
		List<String> globusEndpointFiles = (List<String>) session.getAttribute("GlobusEndpointFiles");
		List<String> globusEndpointFolders = (List<String>) session.getAttribute("GlobusEndpointFolders");
		List<String> googleDriveFileIds = (List<String>) session.getAttribute("fileIds");
		List<String> googleDriveFolderIds = (List<String>) session.getAttribute("folderIds");
		String accessToken = (String) session.getAttribute("accessToken");

		String bulkType = (String) request.getParameter("uploadType");
		String bucketName = (String) request.getParameter("bucketName");
		String s3Path = (String) request.getParameter("s3Path");
		String accessKey = (String) request.getParameter("accessKey");
		String secretKey = (String) request.getParameter("secretKey");
		String region = (String) request.getParameter("region");
		String s3File = (String) request.getParameter("s3File");
		boolean isS3File = s3File != null && s3File.equals("on");

		if (StringUtils.equals(bulkType, "globus") && globusEndpointFiles != null) {
			List<gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO> files = new ArrayList<gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO>();
			for (String fileName : globusEndpointFiles) {
				gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO file = new gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO();
				HpcFileLocation source = new HpcFileLocation();
				source.setFileContainerId(globusEndpoint);
				source.setFileId(globusEndpointPath + fileName);
				HpcUploadSource globusSource = new HpcUploadSource();
				globusSource.setSourceLocation(source);
				file.setGlobusUploadSource(globusSource);
				file.setCreateParentCollections(true);
				file.setPath(path + "/" + fileName);
				log.info(path + "/" + fileName);
				files.add(file);
			}
			dto.getDataObjectRegistrationItems().addAll(files);
		} else if (StringUtils.equals(bulkType, "drive") && googleDriveFileIds != null) {
			List<gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO> files = new ArrayList<gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO>();
			for (String fileId : googleDriveFileIds) {
				gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO file = new gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO();
				HpcFileLocation source = new HpcFileLocation();
				source.setFileContainerId("MyDrive");
				source.setFileId(fileId);
				Path filePath = Paths.get(globusEndpointFiles.get(googleDriveFileIds.indexOf(fileId)));
				String fileName = filePath.getFileName().toString();
				HpcStreamingUploadSource googleDriveSource = new HpcStreamingUploadSource();
				googleDriveSource.setSourceLocation(source);
				googleDriveSource.setAccessToken(accessToken);
				file.setGoogleDriveUploadSource(googleDriveSource);
				file.setCreateParentCollections(true);
				file.setPath(path + "/" + fileName);
				log.info(path + "/" + fileName);
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
				String fromPath = globusEndpointPath.endsWith("/") ? globusEndpointPath + folderName
						: globusEndpointPath + "/" + folderName;
				String toPath = "/" + folderName;
				source.setFileId(fromPath);
				folder.setBasePath(datafilePath);
				HpcScanDirectory globusDirectory = new HpcScanDirectory();
				globusDirectory.setDirectoryLocation(source);
				folder.setGlobusScanDirectory(globusDirectory);
				folders.add(folder);
				if (!fromPath.equals(toPath)) {
					HpcDirectoryScanPathMap pathDTO = new HpcDirectoryScanPathMap();
					pathDTO.setFromPath(fromPath);
					pathDTO.setToPath(toPath);
					folder.setPathMap(pathDTO);
				}
			}
			dto.getDirectoryScanRegistrationItems().addAll(folders);
		}

		if (StringUtils.equals(bulkType, "drive") && googleDriveFolderIds != null) {
			List<gov.nih.nci.hpc.dto.datamanagement.v2.HpcDirectoryScanRegistrationItemDTO> folders = new ArrayList<gov.nih.nci.hpc.dto.datamanagement.v2.HpcDirectoryScanRegistrationItemDTO>();
			for (String folderId : googleDriveFolderIds) {
				gov.nih.nci.hpc.dto.datamanagement.v2.HpcDirectoryScanRegistrationItemDTO folder = new gov.nih.nci.hpc.dto.datamanagement.v2.HpcDirectoryScanRegistrationItemDTO();
				HpcFileLocation source = new HpcFileLocation();
				source.setFileContainerId("MyDrive");
				Path folderPath = Paths.get(globusEndpointFolders.get(googleDriveFolderIds.indexOf(folderId)));
				String folderName = folderPath.getFileName().toString();
				String fromPath = "/" + folderPath.toString();
				String toPath = "/" + folderName;
				source.setFileId(folderId);
				folder.setBasePath(datafilePath);
				HpcGoogleDriveScanDirectory googleDriveDirectory = new HpcGoogleDriveScanDirectory();
				googleDriveDirectory.setDirectoryLocation(source);
				googleDriveDirectory.setAccessToken(accessToken);
				folder.setGoogleDriveScanDirectory(googleDriveDirectory);
				folders.add(folder);
				if (!fromPath.equals(toPath)) {
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
			HpcStreamingUploadSource s3UploadSource = new HpcStreamingUploadSource();
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
		} else if (StringUtils.equals(bulkType, "s3") && s3Path != null) {
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

	protected List<HpcMetadataEntry> getMetadataEntries(HttpServletRequest request, HttpSession session, String path)
			throws DoeWebException {
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

	protected List<DoeMetadataAttrEntry> populateFormAttributes(HttpServletRequest request, HttpSession session,
			String basePath, String collectionType, String assetType, boolean refresh,
			List<DoeMetadataAttrEntry> cachedEntries) throws DoeWebException {
		String authToken = (String) session.getAttribute("writeAccessUserToken");

		HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");
		if (modelDTO == null) {
			modelDTO = DoeClientUtil.getDOCModel(authToken, hpcModelURL, sslCertPath, sslCertPassword);
			session.setAttribute("userDOCModel", modelDTO);
		}
		List<HpcMetadataValidationRule> rules = null;
		HpcDataManagementRulesDTO basePathRules = DoeClientUtil.getBasePathManagementRules(modelDTO, basePath);
		if (basePathRules != null) {
			HpcDataHierarchy dataHierarchy = basePathRules.getDataHierarchy();
			if (dataHierarchy != null) {
				rules = basePathRules.getCollectionMetadataValidationRules();

			}
		}

		HpcCollectionDTO collectionDTO = (HpcCollectionDTO) session.getAttribute("parentCollection");
		List<DoeMetadataAttrEntry> metadataEntries = new ArrayList<DoeMetadataAttrEntry>();
		List<String> attributeNames = new ArrayList<String>();
		log.info("base path rules:" + rules);

		if (rules != null && !rules.isEmpty()) {
			for (HpcMetadataValidationRule rule : rules) {
				if ((rule.getCollectionTypes().contains(collectionType) || rule.getCollectionTypes().isEmpty())
						&& !rule.getAttribute().equalsIgnoreCase("collection_type")) {
					log.info("get HpcMetadataValidationRule:" + rule);
					Boolean isValid = true;
					Boolean isConditonalMetaData = false;
					if (StringUtils.isNotEmpty(rule.getControllerValue())
							&& StringUtils.isNotEmpty(rule.getControllerAttribute())
							&& rule.getControllerValue().equalsIgnoreCase(assetType)) {
						isValid = true;
						isConditonalMetaData = true;
					} else if (StringUtils.isNotEmpty(rule.getControllerValue())
							&& StringUtils.isNotEmpty(rule.getControllerAttribute())
							&& !rule.getControllerValue().equalsIgnoreCase(assetType)) {
						isValid = false;
					}
					if (Boolean.TRUE.equals(isValid)) {
						DoeMetadataAttrEntry entry = new DoeMetadataAttrEntry();
						entry.setAttrName(rule.getAttribute());
						attributeNames.add(rule.getAttribute());
						entry.setAttrValue(getFormAttributeValue(request, "zAttrStr_" + rule.getAttribute(),
								cachedEntries, "zAttrStr_"));
						if (entry.getAttrValue() == null) {
							if (refresh)
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
						if (Boolean.TRUE.equals(isConditonalMetaData)) {
							entry.setMandatory(Boolean.TRUE);
						} else {
							entry.setMandatory(rule.getMandatory());
						}
						LookUp val = lookUpService.getLookUpByLevelAndName(collectionType, rule.getAttribute());
						if (val != null) {
							entry.setDisplayName(val.getDisplayName());
							entry.setIsEditable(val.getIsEditable());
						} else {
							entry.setDisplayName(rule.getAttribute());
							entry.setIsEditable(true);
						}
						metadataEntries.add(entry);
					}
				}
			}
		}

		// Handle custom attributes. If refresh, ignore them
		if (!refresh) {
			// add cached entries which are not included in rules
			List<String> attrNames = LambdaUtils.map(cachedEntries, DoeMetadataAttrEntry::getAttrName);
			List<String> ruleAttrNames = LambdaUtils.map(rules, HpcMetadataValidationRule::getAttribute);
			List<String> userDefinedAttrNames = LambdaUtils.filter(attrNames, (String n) -> !ruleAttrNames.contains(n));

			log.info("user defined attrnames :" + userDefinedAttrNames);
			for (DoeMetadataAttrEntry x : cachedEntries) {
				LookUp lookUpVal = lookUpService.getLookUpByLevelAndName(collectionType, x.getAttrName());
				if (userDefinedAttrNames.contains(x.getAttrName())) {
					if (lookUpVal != null) {
						x.setDisplayName(lookUpVal.getDisplayName());
						x.setIsEditable(lookUpVal.getIsEditable());
					} else {
						x.setDisplayName(x.getAttrName());
						x.setIsEditable(true);
					}
					metadataEntries.add(x);
				}
			}
		}
		session.setAttribute("metadataEntries", metadataEntries);
		return metadataEntries;

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

	protected HpcCollectionRegistrationDTO constructRequest(HttpServletRequest request,
			DoeCollectionModel doeCollection) throws DoeWebException {
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
				if ("zAttrStr_access_group".equalsIgnoreCase(paramName)) {
					entry.setValue(String.join(",", attrValue));
				} else {
					entry.setValue(attrValue[0]);
				}
				if (StringUtils.isNotEmpty(entry.getValue())) {
					metadataEntries.add(entry);
				}

				attrEntry.setAttrName(attrName);
				if ("zAttrStr_access_group".equalsIgnoreCase(paramName)) {
					attrEntry.setAttrValue(String.join(",", attrValue));
				} else {
					attrEntry.setAttrValue(attrValue[0]);
				}
				attrEntry.setSystemAttr(false);
				if (StringUtils.isNotEmpty(entry.getValue())) {
					selfMetadataEntries.add(attrEntry);
				}
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
			} else if (paramName.startsWith("path")) {
				String[] path = request.getParameterValues(paramName);
				doeCollection.setPath(path[0]);
			}
		}
		if (doeCollection != null)
			doeCollection.setSelfMetadataEntries(selfMetadataEntries);
		dto.getMetadataEntries().addAll(metadataEntries);
		return dto;
	}

	@SuppressWarnings("unchecked")
	public Boolean verifyCollectionPermissions(String parentPath, HpcCollectionListDTO parentCollectionDto) {
		if (!parentPath.equalsIgnoreCase(basePath) && parentCollectionDto != null
				&& parentCollectionDto.getCollections() != null
				&& !CollectionUtils.isEmpty(parentCollectionDto.getCollections())) {
			HpcCollectionDTO collection = parentCollectionDto.getCollections().get(0);
			List<KeyValueBean> loggedOnUserPermissions = (List<KeyValueBean>) getMetaDataPermissionsList().getBody();
			String role = getPermissionRole(getLoggedOnUserInfo(), collection.getCollection().getCollectionId(),
					loggedOnUserPermissions);
			if (StringUtils.isNotEmpty(role)
					&& (role.equalsIgnoreCase("Owner") || role.equalsIgnoreCase("Group User"))) {
				return true;
			}
		}
		return false;
	}

}