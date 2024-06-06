package gov.nih.nci.doe.web.controller;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.ui.Model;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.domain.LookUp;
import gov.nih.nci.doe.web.model.DoeCollectionModel;
import gov.nih.nci.doe.web.model.DoeMetadataAttrEntry;
import gov.nih.nci.doe.web.model.DoeSearch;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.service.DoeAuthorizationService;
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
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationRequestDTO;
import gov.nih.nci.hpc.domain.datatransfer.HpcGoogleScanDirectory;
import gov.nih.nci.hpc.domain.datatransfer.HpcPatternType;

@EnableAutoConfiguration
public abstract class DoeCreateCollectionDataFileController extends AbstractDoeController {

	protected void clearSessionAttrs(HttpSession session) {
		session.removeAttribute("datafilePath");
		session.removeAttribute("collection_type");
		session.removeAttribute("basePathSelected");
		session.removeAttribute("includeCriteria");
		session.removeAttribute("GlobusEndpoint");
		session.removeAttribute("GlobusEndpointPath");
		session.removeAttribute("GlobusEndpointFiles");
		session.removeAttribute("GlobusEndpointFolders");
		session.removeAttribute("parentCollection");
		session.removeAttribute("metadataEntries");
		session.removeAttribute("parent");
		session.removeAttribute("institutePath");
		session.removeAttribute("studyPath");
		session.removeAttribute("uploadPath");
		session.removeAttribute("fileIds");
		session.removeAttribute("folderIds");
		session.removeAttribute("accessToken");
		session.removeAttribute("authorized");
		session.removeAttribute("actionType");
		session.removeAttribute("refreshTokenDetailsGoogleCloud");
		session.removeAttribute("authorizedGC");
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
		String refreshTokenDetailsGoogleCloud = (String) session.getAttribute("refreshTokenDetailsGoogleCloud");

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

		if (refreshTokenDetailsGoogleCloud != null) {
			model.addAttribute("refreshTokenDetailsGoogleCloud", refreshTokenDetailsGoogleCloud);
			model.addAttribute("authorizedGC", "true");
		}
		model.addAttribute("datafilePath", session.getAttribute("datafilePath"));
		model.addAttribute("institutePath", session.getAttribute("institutePath"));
		model.addAttribute("studyPath", session.getAttribute("studyPath"));
		model.addAttribute("bulkUploadCollection", session.getAttribute("bulkUploadCollection"));
		model.addAttribute("uploadPath", session.getAttribute("uploadPath"));

	}

	@SuppressWarnings("unchecked")
	protected gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO constructV2BulkRequest(
			HttpServletRequest request, HttpSession session, String path, Map<String, String> assetIdentifierMapping) {
		gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO dto = new gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO();

		String datafilePath = (String) session.getAttribute("uploadPath");

		if (datafilePath == null) {
			datafilePath = (String) session.getAttribute("studyPath");
		}

		if (datafilePath == null) {
			datafilePath = path;
		}

		String includeCriteria = request.getParameter("includeCriteria");
		String globusEndpoint = (String) session.getAttribute("GlobusEndpoint");
		String globusEndpointPath = (String) session.getAttribute("GlobusEndpointPath");
		List<String> globusEndpointFiles = (List<String>) session.getAttribute("GlobusEndpointFiles");
		List<String> globusEndpointFolders = (List<String>) session.getAttribute("GlobusEndpointFolders");
		List<String> googleDriveFileIds = (List<String>) session.getAttribute("fileIds");
		List<String> googleDriveFolderIds = (List<String>) session.getAttribute("folderIds");
		String accessToken = (String) session.getAttribute("accessToken");
		String refreshTokenDetailsGoogleCloud = (String) session.getAttribute("refreshTokenDetailsGoogleCloud");

		String bulkType = (String) request.getParameter("uploadType");
		String bucketName = (String) request.getParameter("bucketName");
		String s3Path = (String) request.getParameter("s3Path");
		String accessKey = (String) request.getParameter("accessKey");
		String secretKey = (String) request.getParameter("secretKey");
		String region = (String) request.getParameter("region");
		String gcbucketName = (String) request.getParameter("gcbucketName");
		String gcPath = (String) request.getParameter("gcPath");
		gcPath = (gcPath != null ? gcPath.trim() : null);
		String s3File = (String) request.getParameter("s3File");
		boolean isS3File = s3File != null && s3File.equals("on");
		String gcFile = (String) request.getParameter("gcFile");
		boolean isGcFile = gcFile != null && gcFile.equals("on");

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
		} else if (StringUtils.equals(bulkType, DoeAuthorizationService.GOOGLE_DRIVE_TYPE)
				&& googleDriveFileIds != null) {
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
		} else if (StringUtils.equals(bulkType, DoeAuthorizationService.GOOGLE_CLOUD_TYPE) && gcPath != null) {
			// Upload File From Google Cloud Storage
			List<gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO> files = new ArrayList<gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO>();
			gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO file = new gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO();
			HpcFileLocation source = new HpcFileLocation();
			source.setFileContainerId(gcbucketName);
			source.setFileId(gcPath);
			HpcStreamingUploadSource googleCloudSource = new HpcStreamingUploadSource();
			googleCloudSource.setSourceLocation(source);
			googleCloudSource.setAccessToken(refreshTokenDetailsGoogleCloud);
			file.setGoogleCloudStorageUploadSource(googleCloudSource);
			Path gcFilePath = Paths.get(gcPath);
			file.setPath(path + "/" + gcFilePath.getFileName());
			files.add(file);

			dto.getDataObjectRegistrationItems().addAll(files);
		} else if (StringUtils.equals(bulkType, DoeAuthorizationService.GOOGLE_CLOUD_TYPE) && gcPath != null
				&& !isGcFile) {
			// Upload Directory/Folder from Google Cloud Storage
			List<gov.nih.nci.hpc.dto.datamanagement.v2.HpcDirectoryScanRegistrationItemDTO> folders = new ArrayList<gov.nih.nci.hpc.dto.datamanagement.v2.HpcDirectoryScanRegistrationItemDTO>();
			gov.nih.nci.hpc.dto.datamanagement.v2.HpcDirectoryScanRegistrationItemDTO folder = new gov.nih.nci.hpc.dto.datamanagement.v2.HpcDirectoryScanRegistrationItemDTO();
			HpcFileLocation source = new HpcFileLocation();
			source.setFileContainerId(gcbucketName);
			source.setFileId(gcPath);
			HpcGoogleScanDirectory googleCloudSource = new HpcGoogleScanDirectory();
			googleCloudSource.setDirectoryLocation(source);
			googleCloudSource.setAccessToken(refreshTokenDetailsGoogleCloud);
			folder.setGoogleCloudStorageScanDirectory(googleCloudSource);
			folder.setBasePath(datafilePath);
			HpcDirectoryScanPathMap pathDTO = new HpcDirectoryScanPathMap();
			pathDTO.setFromPath(gcPath);
			// Extract the last subdirectory. If there are no subdirectories, FromPath and
			// ToPath will be the same
			String tempPath = gcPath;
			if (gcPath.endsWith("/"))
				tempPath = gcPath.substring(0, gcPath.length() - 1);
			String gcToPath = tempPath.substring(tempPath.lastIndexOf("/") + 1, tempPath.length());
			pathDTO.setToPath(gcToPath);
			folder.setPathMap(pathDTO);
			folders.add(folder);

			dto.getDirectoryScanRegistrationItems().addAll(folders);
			/* set path for storing in MoDaC */
			List<String> googleCloudFolderIds = new ArrayList<>();
			googleCloudFolderIds.add(gcFile);
			constructPath(path, googleCloudFolderIds, session);

		}
		List<String> include = new ArrayList<String>();
		if (includeCriteria != null && !includeCriteria.isEmpty()) {
			StringTokenizer tokens = new StringTokenizer(includeCriteria, "\r\n");
			while (tokens.hasMoreTokens())
				include.add(tokens.nextToken());
		}

		if (StringUtils.equals(bulkType, "globus") && globusEndpointFolders != null) {
			String assetGroupIdentifier = request.getParameter("assetGroupIdentifier");
			int index = 1;
			List<gov.nih.nci.hpc.dto.datamanagement.v2.HpcDirectoryScanRegistrationItemDTO> folders = new ArrayList<gov.nih.nci.hpc.dto.datamanagement.v2.HpcDirectoryScanRegistrationItemDTO>();
			for (String folderName : globusEndpointFolders) {
				gov.nih.nci.hpc.dto.datamanagement.v2.HpcDirectoryScanRegistrationItemDTO folder = new gov.nih.nci.hpc.dto.datamanagement.v2.HpcDirectoryScanRegistrationItemDTO();
				HpcFileLocation source = new HpcFileLocation();
				source.setFileContainerId(globusEndpoint);
				// the to Path is the destination Path. If bulk asset upload, set the toPath
				// from asset_identifer.
				String fromPath = globusEndpointPath.endsWith("/") ? globusEndpointPath + folderName
						: globusEndpointPath + "/" + folderName;
				String toPath = "";
				if (StringUtils.isNoneEmpty(assetGroupIdentifier)) {
					toPath = "/" + assetGroupIdentifier + "_" + index;
				} else if (assetIdentifierMapping != null) {
					String assetIdentifier = assetIdentifierMapping.get(folderName);
					if (assetIdentifier != null && !assetIdentifier.equals(folderName)) {
						toPath = "/" + assetIdentifier;
					} else {
						toPath = "/" + folderName;
					}
				} else {
					toPath = "/" + folderName;
				}

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
				if (include.size() > 0) {
					folder.getIncludePatterns().addAll(include);
					folder.setPatternType(HpcPatternType.SIMPLE);
				}
				index++;

			}
			dto.getDirectoryScanRegistrationItems().addAll(folders);
			/* set path for storing in MoDaC */
			constructPath(path, globusEndpointFolders, session);

		}

		if (StringUtils.equals(bulkType, DoeAuthorizationService.GOOGLE_DRIVE_TYPE) && googleDriveFolderIds != null) {
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
				HpcGoogleScanDirectory googleDriveDirectory = new HpcGoogleScanDirectory();
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
			/* set path for storing in MoDaC */
			constructPath(path, googleDriveFolderIds, session);
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
			folder.setBasePath(datafilePath);

			HpcS3ScanDirectory s3Directory = new HpcS3ScanDirectory();
			s3Directory.setDirectoryLocation(source);
			HpcS3Account s3Account = new HpcS3Account();
			s3Account.setAccessKey(accessKey);
			s3Account.setSecretKey(secretKey);
			s3Account.setRegion(region);
			s3Directory.setAccount(s3Account);
			folder.setS3ScanDirectory(s3Directory);
			folders.add(folder);
			String fromPath = "";
			String toPath = "";
			if (s3Path.equals("/")) {
				fromPath = "/";
				toPath = "/";
			} else {
				Path folderPath = Paths.get(s3Path);
				String folderName = folderPath.getFileName().toString();
				fromPath = "/" + s3Path;
				toPath = "/" + folderName;
			}

			if (!fromPath.equals(toPath)) {
				HpcDirectoryScanPathMap pathDTO = new HpcDirectoryScanPathMap();
				pathDTO.setFromPath(fromPath);
				pathDTO.setToPath(toPath);
				folder.setPathMap(pathDTO);
			}
			if (include.size() > 0) {
				folder.getIncludePatterns().addAll(include);
				folder.setPatternType(HpcPatternType.SIMPLE);
			}

			dto.getDirectoryScanRegistrationItems().addAll(folders);

			List<String> S3FolderIds = new ArrayList<>();
			S3FolderIds.add(s3Path);
			/* set path for storing in MoDaC */
			constructPath(path, S3FolderIds, session);

		}

		return dto;

	}

	private void constructPath(String path, List<String> folderIds, HttpSession session) {

		Set<String> pathsList = new HashSet<String>();
		for (String folderName : folderIds) {
			pathsList.add(path + "/" + folderName);

		}
		session.setAttribute("pathsList", pathsList);

	}

	protected List<String> getCollectionTypes(List<HpcMetadataValidationRule> rules) {

		log.info("get collection Types: " + rules);
		List<String> collectionTypesSet = new ArrayList<String>();
		for (HpcMetadataValidationRule rule : rules) {
			if (rule.getMandatory() && rule.getAttribute().equals("collection_type"))
				collectionTypesSet.addAll(rule.getValidValues());
		}
		return collectionTypesSet;
	}

	protected List<String> getSubCollectionTypes(String collectionType, HpcDataHierarchy dataHierarchy) {

		log.info("get sub collection types" + collectionType + " dataHierarchy : " + dataHierarchy);
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

	protected List<DoeMetadataAttrEntry> getControlAttributes(HttpServletRequest request, HttpSession session,
			String basePath, String collectionType, String controllerAttribute) throws DoeWebException {

		String authToken = (String) session.getAttribute("writeAccessUserToken");
		List<DoeMetadataAttrEntry> controlAttributeEntries = new ArrayList<DoeMetadataAttrEntry>();

		HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");
		if (modelDTO == null) {
			modelDTO = DoeClientUtil.getDOCModel(authToken, hpcModelURL);
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

		log.info("base path rules:" + rules);

		List<HpcMetadataValidationRule> controllerAttrRules = rules.stream()
				.filter(e -> e.getCollectionTypes().contains(collectionType) && (e.getControllerAttribute() != null
						&& e.getControllerAttribute().equalsIgnoreCase(controllerAttribute)))
				.collect(Collectors.toList());

		controlAttributeEntries = getMetadataEntries(controllerAttrRules, null, collectionType, request, session, null);

		return controlAttributeEntries;
	}

	protected List<DoeMetadataAttrEntry> populateFormAttributes(HttpServletRequest request, HttpSession session,
			String basePath, String collectionType, String[] controllerAttribute, String[] controllerValue,
			Boolean refresh, List<DoeMetadataAttrEntry> cachedEntries) throws DoeWebException {
		String authToken = (String) session.getAttribute("writeAccessUserToken");

		List<String> controllerAttrList = controllerAttribute == null ? new ArrayList<String>()
				: Arrays.asList(controllerAttribute);
		List<String> controllerValuesList = controllerValue == null ? new ArrayList<String>()
				: Arrays.asList(controllerValue);

		HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");
		if (modelDTO == null) {
			modelDTO = DoeClientUtil.getDOCModel(authToken, hpcModelURL);
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
		log.info("base path rules:" + rules);
		List<HpcMetadataValidationRule> filteredRules = rules.stream()
				.filter(e -> e.getCollectionTypes().contains(collectionType)).collect(Collectors.toList());

		List<DoeMetadataAttrEntry> metadataEntries = new ArrayList<DoeMetadataAttrEntry>();

		log.info("base path rules for collection type:" + collectionType + " are: " + filteredRules);

		if (CollectionUtils.isEmpty(controllerValuesList) && CollectionUtils.isEmpty(controllerAttrList)
				&& Boolean.FALSE.equals(refresh) && CollectionUtils.isNotEmpty(cachedEntries)) {
			List<HpcMetadataValidationRule> controllerAttrRules = rules.stream().filter(
					e -> e.getCollectionTypes().contains(collectionType) && (e.getControllerAttribute() != null))
					.collect(Collectors.toList());
			if (CollectionUtils.isNotEmpty(controllerAttrRules)) {
				List<String> controllerAttrNames = LambdaUtils.map(controllerAttrRules,
						HpcMetadataValidationRule::getControllerAttribute);
				cachedEntries.stream().forEach(e -> {
					if (controllerAttrNames.contains(e.getAttrName())) {
						controllerAttrList.add(e.getAttrName());
						controllerValuesList.add(e.getAttrValue());
					}
				});
			}
		}
		/*
		 * If the rule has controller value/attribute exists, filter the rules by
		 * controllerAttribute/controllerAttribute
		 * 
		 */

		List<HpcMetadataValidationRule> filteredControllerRules = rules.stream()
				.filter(e -> e.getCollectionTypes().contains(collectionType)
						&& (e.getControllerAttribute() == null
								|| controllerAttrList.contains(e.getControllerAttribute()))
						&& (e.getControllerValue() == null || controllerValuesList.contains(e.getControllerValue())))
				.collect(Collectors.toList());

		metadataEntries = getMetadataEntries(filteredControllerRules, filteredRules, collectionType, request, session,
				cachedEntries);

		// Handle custom attributes. If refresh, ignore them
		if (Boolean.FALSE.equals(refresh)) {
			// add cached entries which are not included in rules
			List<String> attrNames = LambdaUtils.map(cachedEntries, DoeMetadataAttrEntry::getAttrName);
			List<String> ruleAttrNames = LambdaUtils.map(filteredRules, HpcMetadataValidationRule::getAttribute);
			List<String> userDefinedAttrNames = LambdaUtils.filter(attrNames, (String n) -> !ruleAttrNames.contains(n));

			log.info("user defined attrnames :" + userDefinedAttrNames);
			for (DoeMetadataAttrEntry x : cachedEntries) {
				LookUp lookUpVal = lookUpService.getLookUpByLevelAndName(collectionType, x.getAttrName());
				if (userDefinedAttrNames.contains(x.getAttrName())) {
					if (lookUpVal != null) {
						x.setDisplayName(lookUpVal.getDisplayName());
						x.setIsEditable(lookUpVal.getIsEditable());
						x.setDisplayOrder(lookUpVal.getDisplayOrder());
						x.setIsVisible(lookUpVal.getIsVisible());
						x.setIsVisibleOnUplaodPage(lookUpVal.getIsVisibleOnUplaodPage());
						x.setIsVisibleForReviewCommiteeMember(lookUpVal.getIsVisibleForReviewCommiteeMember());
					} else {
						x.setDisplayName(x.getAttrName());
						x.setIsEditable(true);
					}
					metadataEntries.add(x);
				}
			}
		}

		Collections.sort(metadataEntries,
				Comparator
						.comparing(DoeMetadataAttrEntry::getDisplayOrder,
								Comparator.nullsLast(Comparator.naturalOrder()))
						.thenComparing(DoeMetadataAttrEntry::getAttrName));

		return metadataEntries;

	}

	@SuppressWarnings("unchecked")
	private List<DoeMetadataAttrEntry> getMetadataEntries(List<HpcMetadataValidationRule> metadataRules,
			List<HpcMetadataValidationRule> filteredRules, String collectionType, HttpServletRequest request,
			HttpSession session, List<DoeMetadataAttrEntry> cachedEntries) throws DoeWebException {

		List<DoeMetadataAttrEntry> metadataEntries = new ArrayList<DoeMetadataAttrEntry>();

		if (metadataRules != null && !metadataRules.isEmpty()) {
			for (HpcMetadataValidationRule rule : metadataRules) {

				log.info("get HpcMetadataValidationRule:" + rule);
				DoeMetadataAttrEntry entry = new DoeMetadataAttrEntry();

				if (CollectionUtils.isNotEmpty(filteredRules)) {
					// find out if this rule is a controller attribute for any other rule
					HpcMetadataValidationRule isControllerForOtherAttr = filteredRules.stream()
							.filter(e -> rule.getAttribute().equalsIgnoreCase(e.getControllerAttribute())).findAny()
							.orElse(null);

					if (isControllerForOtherAttr != null) {
						entry.setControllerAttribute(Boolean.TRUE);
					} else {
						entry.setControllerAttribute(Boolean.FALSE);
					}
				}

				entry.setAttrName(rule.getAttribute());
				entry.setAttrValue(
						getFormAttributeValue(request, "zAttrStr_" + rule.getAttribute(), cachedEntries, "zAttrStr_"));
				if (entry.getAttrValue() == null) {
					entry.setAttrValue(rule.getDefaultValue());
				}
				if (rule.getValidValues() != null && !rule.getValidValues().isEmpty()) {
					List<KeyValueBean> validValues = new ArrayList<KeyValueBean>();
					rule.getValidValues().stream().forEach(e -> validValues.add(new KeyValueBean(e, e)));
					entry.setValidValues(validValues);
				}

				if (rule.getAttribute().equalsIgnoreCase("applicable_model_paths")) {

					/*
					 * get this info from dme search api
					 */
					List<KeyValueBean> validValues = (List<KeyValueBean>) session.getAttribute("validValuesList");
					if (CollectionUtils.isEmpty(validValues)) {
						validValues = getAllApplicableModelPaths(session);
					}
					entry.setValidValues(validValues);
				}
				entry.setDescription(rule.getDescription());

				if (StringUtils.isNotEmpty(rule.getDefaultValue())) {
					entry.setMandatory(Boolean.FALSE);
				} else if (StringUtils.isNotEmpty(rule.getControllerAttribute())) {
					entry.setMandatory(Boolean.TRUE);
				} else {
					entry.setMandatory(rule.getMandatory());
				}

				entry.setControllerAttrName(rule.getControllerAttribute());
				entry.setControllerAttrValue(rule.getControllerValue());

				if (StringUtils.isNotEmpty(rule.getDefaultValue())) {
					entry.setDefaultValue(rule.getDefaultValue());
				}
				LookUp val = lookUpService.getLookUpByLevelAndName(collectionType, rule.getAttribute());
				if (val != null) {
					entry.setDisplayName(val.getDisplayName());
					entry.setIsEditable(val.getIsEditable());
					entry.setDisplayOrder(val.getDisplayOrder());
					entry.setIsVisible(val.getIsVisible());
					entry.setIsVisibleOnUplaodPage(val.getIsVisibleOnUplaodPage());
					entry.setIsVisibleForReviewCommiteeMember(val.getIsVisibleForReviewCommiteeMember());
				} else {
					entry.setDisplayName(rule.getAttribute());
					entry.setIsEditable(true);
				}
				metadataEntries.add(entry);

			}
		}
		return metadataEntries;

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

	protected HpcDataObjectRegistrationRequestDTO constructDataRequest(HttpServletRequest request)
			throws DoeWebException {
		Enumeration<String> params = request.getParameterNames();
		HpcDataObjectRegistrationRequestDTO dto = new HpcDataObjectRegistrationRequestDTO();
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
		dto.getMetadataEntries().addAll(metadataEntries);
		return dto;
	}

	protected HpcCollectionRegistrationDTO constructRequest(HttpServletRequest request,
			DoeCollectionModel doeCollection, Boolean isEditCollection) throws DoeWebException {
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
				if (attrValue != null && attrValue.length > 1) {
					String combinedAttrVal = Stream.of(attrValue).filter(s -> s != null && !s.isEmpty())
							.collect(Collectors.joining(","));
					entry.setValue(combinedAttrVal);
				} else {
					entry.setValue(attrValue[0].trim());
				}

				if (Boolean.TRUE.equals(isEditCollection)) {
					metadataEntries.add(entry);
				} else if (Boolean.FALSE.equals(isEditCollection) && StringUtils.isNotEmpty(entry.getValue())) {
					metadataEntries.add(entry);

				}

				// when creating/editing collection of type reference dataset with applicable
				// models,
				// add/update the hidden metadata applicable_model_identifiers

				if (StringUtils.isNotEmpty(attrName) && attrValue != null && attrValue.length > 0
						&& attrName.equalsIgnoreCase("applicable_model_paths")) {

					HpcMetadataEntry hiddenMetadataForApplicableModels = new HpcMetadataEntry();
					hiddenMetadataForApplicableModels.setAttribute("applicable_model_identifiers");
					DoeMetadataAttrEntry hiddenAttrEntryForApplicableModels = new DoeMetadataAttrEntry();
					hiddenAttrEntryForApplicableModels.setAttrName("applicable_model_identifiers");

					List<String> attrNamesDisplay = new ArrayList<String>();

					Stream.of(attrValue).forEach(e -> {
						if (e != null && !e.isEmpty()) {
							String identifier = e.substring(e.lastIndexOf('/') + 1);
							attrNamesDisplay.add(identifier);
						}
					});
					if (CollectionUtils.isNotEmpty(attrNamesDisplay)) {
						hiddenMetadataForApplicableModels.setValue(String.join(", ", attrNamesDisplay));
						hiddenAttrEntryForApplicableModels.setAttrValue(String.join(", ", attrNamesDisplay));
						metadataEntries.add(hiddenMetadataForApplicableModels);
						hiddenAttrEntryForApplicableModels.setSystemAttr(false);
						selfMetadataEntries.add(hiddenAttrEntryForApplicableModels);
					}

				}

				attrEntry.setAttrName(attrName);
				if (attrValue != null && attrValue.length > 1) {

					String combinedAttrVal = Stream.of(attrValue).filter(s -> s != null && !s.isEmpty())
							.collect(Collectors.joining(","));
					attrEntry.setAttrValue(combinedAttrVal);

				} else {
					attrEntry.setAttrValue(attrValue[0].trim());
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
					entry.setValue(attrValue[0].trim());
				else
					throw new DoeWebException("Invalid metadata attribute value. Empty value is not valid!");
				metadataEntries.add(entry);
				DoeMetadataAttrEntry attrEntry = new DoeMetadataAttrEntry();
				attrEntry.setAttrName(attrName[0]);
				attrEntry.setAttrValue(attrValue[0].trim());
				attrEntry.setSystemAttr(false);
				selfMetadataEntries.add(attrEntry);
			} else if (paramName.startsWith("path") && doeCollection != null) {
				String[] path = request.getParameterValues(paramName);
				doeCollection.setPath(path[0]);
			}
		}
		if (doeCollection != null) {
			doeCollection.setSelfMetadataEntries(selfMetadataEntries);
		}

		dto.getMetadataEntries().addAll(metadataEntries);
		return dto;
	}

	@SuppressWarnings("unchecked")
	public Boolean verifyCollectionPermissions(String parentPath, HpcCollectionListDTO parentCollectionDto) {

		log.info("verify collection permissions for : " + parentPath);
		Integer parentCollectionId = null;
		if (!parentPath.equalsIgnoreCase(basePath) && parentCollectionDto != null
				&& parentCollectionDto.getCollections() != null
				&& !CollectionUtils.isEmpty(parentCollectionDto.getCollections())) {
			HpcCollectionDTO collection = parentCollectionDto.getCollections().get(0);
			List<KeyValueBean> loggedOnUserPermissions = (List<KeyValueBean>) getMetaDataPermissionsList(null)
					.getBody();

			/**
			 * when the parentPath collection is lower than Asset level, loop through the
			 * parentCollectionDto parent metadata entries to get the asset level
			 * permissions. Currently, when registering folder subcollections and uploading
			 * files to subcollections, the permissions are retrieved from asset level.
			 * 
			 */

			HpcMetadataEntry assetCollection = collection.getMetadataEntries().getParentMetadataEntries().stream()
					.filter(e -> e.getAttribute().equalsIgnoreCase("collection_type")
							&& e.getLevelLabel().equalsIgnoreCase("Asset"))
					.findAny().orElse(null);

			if (assetCollection != null) {
				parentCollectionId = assetCollection.getCollectionId();
			} else {
				parentCollectionId = collection.getCollection().getCollectionId();
			}

			String role = getPermissionRole(getLoggedOnUserInfo(), parentCollectionId, loggedOnUserPermissions);
			if (StringUtils.isNotEmpty(role)
					&& (role.equalsIgnoreCase("Owner") || role.equalsIgnoreCase("Group User"))) {
				return true;
			}
		}
		return false;
	}

	public List<KeyValueBean> getAllApplicableModelPaths(HttpSession session) throws DoeWebException {
		/*
		 * get all model paths with is_model_deployed metadata attr true
		 */

		log.info("get all models with model deployed true");
		DoeSearch search = new DoeSearch();
		String[] attrNames = { "collection_type", "asset_type", "is_model_deployed" };
		String[] attrValues = { "Asset", "Model", "Yes" };
		String[] levelValues = { "Asset", "Asset", "Asset" };
		boolean[] isExcludeParentMetadata = { false, false, false };
		String[] rowIds = { "1", "2", "3" };
		String[] operators = { "EQUAL", "EQUAL", "EQUAL" };

		search.setAttrName(attrNames);
		search.setAttrValue(attrValues);
		search.setLevel(levelValues);
		search.setIsExcludeParentMetadata(isExcludeParentMetadata);
		search.setRowId(rowIds);
		search.setOperator(operators);

		return getPathsForSearch(search, session);

	}
}