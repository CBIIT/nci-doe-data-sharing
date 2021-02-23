package gov.nih.nci.doe.web.controller;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.ws.rs.core.Response;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.cxf.jaxrs.client.WebClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.context.request.WebRequest;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.domain.MetaDataPermissions;
import gov.nih.nci.doe.web.model.DoeResponse;
import gov.nih.nci.doe.web.model.DoeUsersModel;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.service.AuditingService;
import gov.nih.nci.doe.web.service.AuthenticateService;
import gov.nih.nci.doe.web.service.DoeAuthorizationService;
import gov.nih.nci.doe.web.service.LookUpService;
import gov.nih.nci.doe.web.service.MailService;
import gov.nih.nci.doe.web.service.MetaDataPermissionsService;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementModelDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectListDTO;

public abstract class AbstractDoeController {

	@Value("${gov.nih.nci.hpc.ssl.cert}")
	protected String sslCertPath;
	@Value("${gov.nih.nci.hpc.ssl.cert.password}")
	protected String sslCertPassword;

	@Value("${gov.nih.nci.hpc.server.model}")
	private String hpcModelURL;

	@Value("${gov.nih.nci.hpc.server.dataObject}")
	private String dataObjectServiceURL;

	@Autowired
	public AuthenticateService authenticateService;

	@Autowired
	MetaDataPermissionsService metaDataPermissionService;

	@Autowired
	public AuditingService auditingService;

	@Autowired
	LookUpService lookUpService;

	@Autowired
	MailService mailService;

	@Autowired
	AuthenticateService authService;

	@Value("${doe.basePath}")
	String basePath;

	@Value("${doe.downtime.message}")
	String downtimeMessage;

	@Value("${gov.nih.nci.hpc.server.collection}")
	private String serviceURL;

	@Value("${doe.show.api-docs:false}")
	boolean showApiDocs;

	@Autowired
	DoeAuthorizationService doeAuthorizationService;

	@Value("${gov.nih.nci.hpc.web.server}")
	String webServerName;

	@Value("${gov.nih.nci.hpc.drive.clientid}")
	String clientId;
	@Value("${gov.nih.nci.hpc.drive.clientsecret}")
	String clientSecret;

	protected Logger log = LoggerFactory.getLogger(this.getClass());

	@ExceptionHandler({ Exception.class })
	public @ResponseBody DoeResponse handleUncaughtException(Exception ex, WebRequest request,
			HttpServletResponse response) {
		log.error("Converting Uncaught exception to RestResponse : " + ex.getMessage());

		response.setHeader("Content-Type", "application/json");
		response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		return new DoeResponse("Error occurred", "Invalid input or system error");

	}

	@ExceptionHandler({ DoeWebException.class })
	public @ResponseBody DoeResponse handleDoeWebException(DoeWebException ex, WebRequest request,
			HttpServletResponse response) {
		log.error("Converting DoeWeb exception to RestResponse : " + ex.getMessage());
		if (ex.getStatusCode() != null) {
			response.setStatus(ex.getStatusCode());
		} else {
			response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		}
		response.setHeader("Content-Type", "application/json");
		return new DoeResponse("Error occurred", ex.getMessage());

	}

	@ModelAttribute("loggedOnUser")
	public String getLoggedOnUserInfo() {
		Authentication auth = SecurityContextHolder.getContext().getAuthentication();
		Boolean isAnonymousUSer = auth.getAuthorities().stream()
				.anyMatch(o -> o.getAuthority().equals("ROLE_ANONYMOUS"));
		if (auth.isAuthenticated() && Boolean.FALSE.equals(isAnonymousUSer)) {
			return auth.getName().trim();
		}
		return null;
	}

	@ModelAttribute("downtimeMessage")
	public String getDowntimeMessage() {

		if (StringUtils.isNotEmpty(downtimeMessage) && StringUtils.isNotBlank(downtimeMessage)) {
			return downtimeMessage;
		}
		return null;
	}

	@ModelAttribute("showApiDocs")
	public boolean getShowApiDocs() {
		return showApiDocs;
	}

	@ModelAttribute("firstName")
	public String getLoggedOnUserFirstName() {
		String emailAddr = getLoggedOnUserInfo();
		if (StringUtils.isNotEmpty(emailAddr)) {
			DoeUsersModel user = authService.getUserInfo(emailAddr);
			if (user != null) {
				return user.getFirstName();
			}
		}
		return null;
	}

	@ModelAttribute("isUploader")
	public Boolean getIsUploader() {
		String emailAddr = getLoggedOnUserInfo();
		if (!StringUtils.isEmpty(emailAddr)) {
			DoeUsersModel user = authenticateService.getUserInfo(emailAddr);
			if (user.getIsWrite() != null && user.getIsWrite()) {
				return true;
			}
		}

		return false;
	}

	public List<KeyValueBean> getUserMetadata(List<HpcMetadataEntry> list, String levelName, List<String> systemAttrs) {

		List<KeyValueBean> entryList = new ArrayList<KeyValueBean>();

		for (HpcMetadataEntry entry : list) {
			if (systemAttrs != null && !systemAttrs.contains(entry.getAttribute())
					&& levelName.equalsIgnoreCase(entry.getLevelLabel())) {
				String attrName = lookUpService.getDisplayName(levelName, entry.getAttribute());
				KeyValueBean k = null;
				if (!StringUtils.isEmpty(attrName)) {
					k = new KeyValueBean(entry.getAttribute(), attrName, entry.getValue());
				} else {
					k = new KeyValueBean(entry.getAttribute(), entry.getAttribute(), entry.getValue());
				}

				entryList.add(k);
			}

		}

		return entryList;
	}

	public String getAttributeValue(String attrName, List<HpcMetadataEntry> list, String levelName) {
		if (list == null)
			return null;

		HpcMetadataEntry entry = list.stream().filter(
				e -> e.getAttribute().equalsIgnoreCase(attrName) && levelName.equalsIgnoreCase(e.getLevelLabel()))
				.findAny().orElse(null);
		if (entry != null) {
			return entry.getValue();
		}
		return null;
	}

	public String getPermissionRole(String user, Integer collectionId, List<KeyValueBean> loggedOnUserPermissions) {

		if (!StringUtils.isEmpty(user)) {
			List<String> loggedOnUserPermList = new ArrayList<String>();
			loggedOnUserPermissions.stream().forEach(e -> loggedOnUserPermList.add(e.getKey()));

			if (!CollectionUtils.isEmpty(loggedOnUserPermList)) {
				List<MetaDataPermissions> permissionList = metaDataPermissionService
						.getAllMetaDataPermissionsByCollectionId(collectionId);
				Boolean isOwner = permissionList.stream()
						.anyMatch(o -> (user.equalsIgnoreCase(o.getUserGroupId()) && o.getIsOwner()));
				Boolean isGroupUser = permissionList.stream()
						.anyMatch(o -> (loggedOnUserPermList.contains(o.getUserGroupId()) && o.getIsGroup()));
				if (Boolean.TRUE.equals(isOwner)) {
					return "Owner";
				} else if (Boolean.TRUE.equals(isGroupUser)) {
					return "Group User";
				}
			}
		}
		return "No Permissions";
	}

	public void downloadToUrl(String urlStr, int bufferSize, String fileName, HttpServletResponse response)
			throws DoeWebException {
		try {
			WebClient client = DoeClientUtil.getWebClient(urlStr, null, null);
			Response restResponse = client.invoke("GET", null);
			response.setContentType("application/octet-stream");
			response.setHeader("Content-Disposition", "attachment; filename=" + fileName);
			IOUtils.copy((InputStream) restResponse.getEntity(), response.getOutputStream());
		} catch (IOException e) {
			throw new DoeWebException(e);
		}
	}

	public ResponseEntity<?> getMetaDataPermissionsList() {
		log.info("get meta data permissions list");
		String loggedOnUser = getLoggedOnUserInfo();
		List<KeyValueBean> keyValueBeanResults = new ArrayList<>();
		if (!StringUtils.isEmpty(loggedOnUser)) {
			DoeUsersModel user = authenticateService.getUserInfo(loggedOnUser);
			if (user != null && !StringUtils.isEmpty(user.getProgramName())) {
				List<String> progList = Arrays.asList(user.getProgramName().split(","));
				progList.stream().forEach(e -> keyValueBeanResults.add(new KeyValueBean(e, e)));
			}
		}
		return new ResponseEntity<>(keyValueBeanResults, null, HttpStatus.OK);
	}

	public List<KeyValueBean> getUserMetaDataAttributesByPath(String selectedPath, String levelName,
			String isDataObject, HttpSession session, HttpServletRequest request, HttpServletResponse response)
			throws DoeWebException {

		log.info("getUserMetaDataAttributesByPath");
		String authToken = (String) session.getAttribute("writeAccessUserToken");
		List<KeyValueBean> entryList = new ArrayList<KeyValueBean>();

		HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");
		if (modelDTO == null) {
			modelDTO = DoeClientUtil.getDOCModel(authToken, hpcModelURL, sslCertPath, sslCertPassword);
			session.setAttribute("userDOCModel", modelDTO);
		}

		List<String> systemAttrs = modelDTO.getCollectionSystemGeneratedMetadataAttributeNames();
		List<String> dataObjectsystemAttrs = modelDTO.getDataObjectSystemGeneratedMetadataAttributeNames();
		systemAttrs.addAll(dataObjectsystemAttrs);
		systemAttrs.add("collection_type");
		systemAttrs.add("access_group");
		session.setAttribute("systemAttrs", systemAttrs);

		try {
			if (selectedPath != null) {
				if (StringUtils.isNotEmpty(isDataObject) && isDataObject.equalsIgnoreCase("false")) {
					// Get collection
					HpcCollectionListDTO collections = DoeClientUtil.getCollection(authToken, serviceURL, selectedPath,
							false, sslCertPath, sslCertPassword);
					if (collections != null && collections.getCollections() != null
							&& !collections.getCollections().isEmpty()) {
						HpcCollectionDTO collection = collections.getCollections().get(0);
						for (HpcMetadataEntry entry : collection.getMetadataEntries().getSelfMetadataEntries()) {
							if (systemAttrs != null && !systemAttrs.contains(entry.getAttribute())) {
								String attrName = lookUpService.getDisplayName(levelName, entry.getAttribute());
								KeyValueBean k = null;
								if (!StringUtils.isEmpty(attrName)) {
									k = new KeyValueBean(entry.getAttribute(), attrName, entry.getValue());
								} else {
									k = new KeyValueBean(entry.getAttribute(), entry.getAttribute(), entry.getValue());
								}

								entryList.add(k);
							}

						}
					}

				} else {
					HpcDataObjectListDTO datafiles = DoeClientUtil.getDatafiles(authToken, dataObjectServiceURL,
							selectedPath, false, true, sslCertPath, sslCertPassword);
					if (datafiles != null && datafiles.getDataObjects() != null
							&& !datafiles.getDataObjects().isEmpty()) {
						HpcDataObjectDTO dataFile = datafiles.getDataObjects().get(0);
						for (HpcMetadataEntry entry : dataFile.getMetadataEntries().getSelfMetadataEntries()) {
							if (systemAttrs != null && !systemAttrs.contains(entry.getAttribute())) {
								String attrName = lookUpService.getDisplayName(levelName, entry.getAttribute());
								KeyValueBean k = null;
								if (!StringUtils.isEmpty(attrName)) {
									k = new KeyValueBean(entry.getAttribute(), attrName, entry.getValue());
								} else {
									k = new KeyValueBean(entry.getAttribute(), entry.getAttribute(), entry.getValue());
								}

								entryList.add(k);
							}

						}

					}
				}
			}
		} catch (Exception e) {
			String errMsg = "Failed to get metadata: " + e.getMessage();
			log.error(errMsg, e);
		}
		log.info("entry list data :" + entryList);
		return entryList;
	}
}