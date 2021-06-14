package gov.nih.nci.doe.web.controller;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

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
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.context.request.WebRequest;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.MappingJsonFactory;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.domain.LookUp;
import gov.nih.nci.doe.web.domain.MetaDataPermissions;
import gov.nih.nci.doe.web.model.AuditingModel;
import gov.nih.nci.doe.web.model.DoeResponse;
import gov.nih.nci.doe.web.model.DoeSearch;
import gov.nih.nci.doe.web.model.DoeUsersModel;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.model.PermissionsModel;
import gov.nih.nci.doe.web.service.AccessGroupsService;
import gov.nih.nci.doe.web.service.AuditingService;
import gov.nih.nci.doe.web.service.AuthenticateService;
import gov.nih.nci.doe.web.service.DoeAuthorizationService;
import gov.nih.nci.doe.web.service.LookUpService;
import gov.nih.nci.doe.web.service.MailService;
import gov.nih.nci.doe.web.service.MetaDataPermissionsService;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.domain.metadata.HpcCompoundMetadataQuery;
import gov.nih.nci.hpc.domain.metadata.HpcCompoundMetadataQueryOperator;
import gov.nih.nci.hpc.domain.metadata.HpcCompoundMetadataQueryType;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataQuery;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataQueryAttributeMatch;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataQueryLevelFilter;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataQueryOperator;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionRegistrationDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementModelDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectDTO;
import gov.nih.nci.hpc.dto.datasearch.HpcCompoundMetadataQueryDTO;

public abstract class AbstractDoeController {

	@Value("${gov.nih.nci.hpc.ssl.cert}")
	protected String sslCertPath;
	@Value("${gov.nih.nci.hpc.ssl.cert.password}")
	protected String sslCertPassword;

	@Value("${gov.nih.nci.hpc.server.model}")
	public String hpcModelURL;

	@Value("${gov.nih.nci.hpc.server.v2.dataObject}")
	public String dataObjectAsyncServiceURL;

	@Value("${gov.nih.nci.hpc.server.search.dataobject.compound}")
	public String compoundDataObjectSearchServiceURL;

	@Value("${gov.nih.nci.hpc.web.server}")
	public String webUrl;

	@Value("${doe.search.results.pageSize}")
	private int pageSize;

	@Autowired
	public AuthenticateService authenticateService;

	@Autowired
	MetaDataPermissionsService metaDataPermissionService;

	@Autowired
	AccessGroupsService accessGroupsService;

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

	@Value("${download.buffer.size}")
	Integer bufferSize;

	@Autowired
	DoeAuthorizationService doeAuthorizationService;

	@Value("${gov.nih.nci.hpc.web.server}")
	String webServerName;

	@Value("${gov.nih.nci.hpc.drive.clientid}")
	String clientId;
	@Value("${gov.nih.nci.hpc.drive.clientsecret}")
	String clientSecret;

	@Value("${gov.nih.nci.hpc.server.search.collection.compound}")
	String compoundCollectionSearchServiceURL;

	protected Logger log = LoggerFactory.getLogger(this.getClass());

	@ExceptionHandler({ Exception.class })
	public @ResponseBody DoeResponse handleUncaughtException(Exception ex, WebRequest request,
			HttpServletResponse response) {
		log.error("Converting Uncaught exception to RestResponse : " + ex.getMessage(), ex);

		response.setHeader("Content-Type", "application/json");
		response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		return new DoeResponse("Error occurred", "Invalid input or system error");

	}

	@ExceptionHandler({ DoeWebException.class })
	public @ResponseBody DoeResponse handleDoeWebException(DoeWebException ex, WebRequest request,
			HttpServletResponse response) {
		log.error("Converting DoeWeb exception to RestResponse : " + ex.getMessage(), ex);
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
				// this is a temporary fix to escape json.stringify error with single and double
				// quotes
				String updatedString = entry.getValue().replaceAll("[\"']", "");
				if (!StringUtils.isEmpty(attrName)) {

					k = new KeyValueBean(entry.getAttribute(), attrName, updatedString);
				} else {
					k = new KeyValueBean(entry.getAttribute(), entry.getAttribute(), updatedString);
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
			// this is a temporary fix to escape json.stringify error with single and double
			// quotes
			return entry.getValue().replaceAll("[\"']", "");
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
						.anyMatch(o -> (o.getUser() != null && user.equalsIgnoreCase(o.getUser().getEmailAddrr())));
				Boolean isGroupUser = permissionList.stream().anyMatch(
						o -> (o.getGroup() != null && loggedOnUserPermList.contains(o.getGroup().getGroupName())));
				if (Boolean.TRUE.equals(isOwner)) {
					return "Owner";
				} else if (Boolean.TRUE.equals(isGroupUser)) {
					return "Group User";
				}
			}
		}
		return "No Permissions";
	}

	public void downloadToUrl(String urlStr, String fileName, HttpServletResponse response) throws DoeWebException {
		try {
			WebClient client = DoeClientUtil.getWebClient(urlStr, null, null);
			Response restResponse = client.invoke("GET", null);
			response.setContentType("application/octet-stream");
			response.setHeader("Content-Disposition", "attachment; filename=" + fileName);
			IOUtils.copy((InputStream) restResponse.getEntity(), response.getOutputStream(), bufferSize);
		} catch (IOException e) {
			throw new DoeWebException(e);
		}
	}

	public List<KeyValueBean> getUserMetaDataAttributesByPath(String selectedPath, String levelName,
			String isDataObject, HttpSession session) throws DoeWebException {

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
							if (!systemAttrs.contains(entry.getAttribute())) {
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
					HpcDataObjectDTO datafiles = DoeClientUtil.getDatafiles(authToken, dataObjectAsyncServiceURL,
							selectedPath, false, true, sslCertPath, sslCertPassword);
					if (datafiles != null && datafiles.getDataObject() != null) {
						for (HpcMetadataEntry entry : datafiles.getMetadataEntries().getSelfMetadataEntries()
								.getUserMetadataEntries()) {
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
		} catch (Exception e) {
			String errMsg = "Failed to get metadata: " + e.getMessage();
			log.error(errMsg, e);
		}
		log.info("entry list data :" + entryList);
		return entryList;
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

	public String saveMetaDataPermissionsList(String collectionId, String path, String[] selectedPermissions)
			throws DoeWebException {

		log.info("get meta data permissions list for collectionId: " + collectionId);

		try {
			String loggedOnUser = getLoggedOnUserInfo();

			// get the new permissions list
			List<String> newSelectedPermissionList = selectedPermissions == null ? new ArrayList<String>()
					: Arrays.asList(selectedPermissions);
			List<MetaDataPermissions> existingPermissionsList = metaDataPermissionService
					.getAllGroupMetaDataPermissionsByCollectionId(Integer.valueOf(collectionId));

			// get the existing permissions list
			List<String> oldPermissionsList = existingPermissionsList.stream().filter(e -> e.getGroup() != null)
					.map(s -> s.getGroup().getGroupName()).collect(Collectors.toList());

			if (CollectionUtils.isEmpty(oldPermissionsList) && !CollectionUtils.isEmpty(newSelectedPermissionList)
					&& StringUtils.isNotBlank(collectionId)) {

				// when there are no existing permissions set on the selection and the new
				// permissions set is not empty
				metaDataPermissionService.savePermissionsList(loggedOnUser, String.join(",", newSelectedPermissionList),
						Integer.valueOf(collectionId), path);
			} else {
				List<String> deletedPermissions = new ArrayList<String>();
				List<String> addedPermissions = new ArrayList<String>();

				// get deleted and added permissions list
				deletedPermissions = oldPermissionsList.stream().filter(e -> !newSelectedPermissionList.contains(e))
						.filter(value -> value != null).collect(Collectors.toList());

				addedPermissions = newSelectedPermissionList.stream().filter(e -> !oldPermissionsList.contains(e))
						.collect(Collectors.toList());

				metaDataPermissionService.deletePermissionsList(loggedOnUser, deletedPermissions,
						Integer.valueOf(collectionId));
				metaDataPermissionService.savePermissionsList(loggedOnUser, String.join(",", addedPermissions),
						Integer.valueOf(collectionId), path);

			}
			return "SUCCESS";
		} catch (Exception e) {
			throw new DoeWebException("Failed to save permissions " + e.getMessage());
		}
	}

	public ResponseEntity<?> saveCollectionAccessGroup(HttpSession session, PermissionsModel permissionGroups)
			throws DoeWebException {

		log.info("get meta data permissions list for " + permissionGroups.getPath());
		String loggedOnUser = getLoggedOnUserInfo();
		String authToken = (String) session.getAttribute("writeAccessUserToken");

		HpcCollectionRegistrationDTO dto = new HpcCollectionRegistrationDTO();
		// construct dto with attribute access_group
		List<HpcMetadataEntry> metadataEntries = new ArrayList<>();
		HpcMetadataEntry entry = new HpcMetadataEntry();
		entry.setAttribute("access_group");
		entry.setValue(permissionGroups.getSelectedAccessGroups());
		metadataEntries.add(entry);
		dto.getMetadataEntries().addAll(metadataEntries);

		Integer restResponse = DoeClientUtil.updateCollection(authToken, serviceURL, dto, permissionGroups.getPath(),
				sslCertPath, sslCertPassword);
		log.info("rest response for update collection:" + restResponse);

		if (restResponse == 200 || restResponse == 201) {
			// store the auditing info
			try {
				AuditingModel audit = new AuditingModel();
				audit.setName(loggedOnUser);
				audit.setOperation("Edit Meta Data");
				audit.setStartTime(new Date());
				audit.setPath(permissionGroups.getPath());
				auditingService.saveAuditInfo(audit);

				// update in MoDaC DB also

				List<String> existingGroups = accessGroupsService.getGroupsByCollectionPath(permissionGroups.getPath());
				List<String> newAccessGroups = Arrays.asList(permissionGroups.getSelectedAccessGroups().split(","));

				// get the added and deleted groups
				List<String> deletedgroups = existingGroups.stream().filter(e -> !newAccessGroups.contains(e))
						.filter(value -> value != null).collect(Collectors.toList());

				List<String> addedGroups = newAccessGroups.stream().filter(e -> !existingGroups.contains(e))
						.collect(Collectors.toList());

				accessGroupsService.updateAccessGroups(permissionGroups.getPath(),
						permissionGroups.getSelectedCollectionId(), addedGroups, deletedgroups);

			} catch (Exception e) {
				throw new DoeWebException(e.getMessage());
			}
		}

		return new ResponseEntity<>("SUCCESS", HttpStatus.OK);
	}

	public ResponseEntity<?> getMetadataPermissionsByCollectionId(String collectionId) {

		log.info("get meta data permissions list by collection id " + collectionId);
		List<KeyValueBean> keyValueBeanResults = new ArrayList<>();

		List<MetaDataPermissions> permissionsList = metaDataPermissionService
				.getAllGroupMetaDataPermissionsByCollectionId(Integer.valueOf(collectionId));

		if (CollectionUtils.isNotEmpty(permissionsList)) {
			permissionsList.stream().filter(e -> e.getGroup() != null).forEach(e -> keyValueBeanResults
					.add(new KeyValueBean(e.getGroup().getGroupName(), e.getGroup().getGroupName())));
		}
		return new ResponseEntity<>(keyValueBeanResults, null, HttpStatus.OK);
	}

	public ResponseEntity<?> getCollectionAccessGroups(String selectedPath, String levelName) throws DoeWebException {

		log.info("get Access Groups for path: " + selectedPath);
		List<KeyValueBean> entryList = new ArrayList<KeyValueBean>();

		try {
			if (selectedPath != null) {
				List<String> accessGrpList = accessGroupsService.getGroupsByCollectionPath(selectedPath);
				if (CollectionUtils.isNotEmpty(accessGrpList)) {
					entryList.add(new KeyValueBean("selectedEntry", String.join(",", accessGrpList)));
				} else {
					// if no access groups exist, the collection access group is public
					entryList.add(new KeyValueBean("selectedEntry", "public"));
				}

				// get parent access groups also if the collection level is Asset or Study
				// based on parent access group, the collection level access groups can be
				// restricted. At Program level, there is no restriction.
				if (selectedPath.lastIndexOf('/') != -1
						&& (levelName.equalsIgnoreCase("Asset") || levelName.equalsIgnoreCase("Study"))) {
					String parentPath = selectedPath.substring(0, selectedPath.lastIndexOf('/'));
					List<String> parentGrpAccessGrpList = accessGroupsService.getGroupsByCollectionPath(parentPath);
					if (CollectionUtils.isNotEmpty(parentGrpAccessGrpList)) {
						entryList.add(new KeyValueBean("parentAccessGroups", String.join(",", parentGrpAccessGrpList)));
					} else {
						entryList.add(new KeyValueBean("parentAccessGroups", "public"));
					}
				}
			}
		} catch (Exception e) {
			throw new DoeWebException(e.getMessage());
		}
		return new ResponseEntity<>(entryList, HttpStatus.OK);
	}

	public HpcCompoundMetadataQueryDTO constructCriteria(DoeSearch search, String levelName) {
		HpcCompoundMetadataQueryDTO dto = new HpcCompoundMetadataQueryDTO();
		dto.setTotalCount(true);
		HpcCompoundMetadataQuery query = buildSimpleSearch(search, levelName);
		dto.setCompoundQuery(query);
		dto.setDetailedResponse(search.isDetailed());
		dto.setCompoundQueryType(HpcCompoundMetadataQueryType.COLLECTION);
		dto.setPage(search.getPageNumber());
		dto.setPageSize(pageSize);
		return dto;
	}

	@SuppressWarnings("unchecked")
	private HpcCompoundMetadataQuery buildSimpleSearch(DoeSearch search, String levelName) {

		HpcCompoundMetadataQuery query = new HpcCompoundMetadataQuery();
		query.setOperator(HpcCompoundMetadataQueryOperator.AND);
		Map<String, HpcMetadataQuery> queriesMap = getQueries(search);

		Map<String, List<HpcMetadataQuery>> attrNamesMap = new HashMap<String, List<HpcMetadataQuery>>();

		//List<HpcMetadataQuery> queries = new ArrayList<HpcMetadataQuery>();
		Iterator<String> iter = queriesMap.keySet().iterator();
		

		while (iter.hasNext()) {
			String iterVal = iter.next();
			String attrName = queriesMap.get(iterVal).getAttribute();
			if (!attrNamesMap.containsKey(attrName)) {
				List<HpcMetadataQuery> attrNamesList = new ArrayList<HpcMetadataQuery>();
				attrNamesList.add(queriesMap.get(iterVal));
				attrNamesMap.put(attrName, attrNamesList);
			} else {
				List<HpcMetadataQuery> list = attrNamesMap.get(attrName);
				list.add(queriesMap.get(iterVal));
				attrNamesMap.put(attrName, list);
			}
			//queries.add(queriesMap.get(iterVal));
		}
		Iterator<String> iter1 = attrNamesMap.keySet().iterator();
		while (iter1.hasNext()) {
			String iterVal = iter1.next();
			List<HpcMetadataQuery> attrNameslist = attrNamesMap.get(iterVal);
			HpcCompoundMetadataQuery q = new HpcCompoundMetadataQuery();
			q.setOperator(HpcCompoundMetadataQueryOperator.OR);
			q.getQueries().addAll(attrNameslist);
			query.getCompoundQueries().add(q);
		}

		//query.getQueries().addAll(queries);

		// add criteria for access group public and other prog names for logged on user.
		List<KeyValueBean> loggedOnUserPermissions = (List<KeyValueBean>) getMetaDataPermissionsList().getBody();

		HpcCompoundMetadataQuery query1 = new HpcCompoundMetadataQuery();
		query1.setOperator(HpcCompoundMetadataQueryOperator.OR);
		List<HpcMetadataQuery> queries1 = new ArrayList<HpcMetadataQuery>();

		// perform OR operation of public access and logged on users access groups
		HpcMetadataQuery q = new HpcMetadataQuery();
		HpcMetadataQueryLevelFilter levelFilter = new HpcMetadataQueryLevelFilter();
		levelFilter.setLabel(levelName);
		levelFilter.setOperator(HpcMetadataQueryOperator.EQUAL);
		q.setLevelFilter(levelFilter);
		q.setAttribute("access_group");
		q.setValue("public");
		q.setOperator(HpcMetadataQueryOperator.EQUAL);
		queries1.add(q);

		for (KeyValueBean x : loggedOnUserPermissions) {
			HpcMetadataQuery q1 = new HpcMetadataQuery();
			HpcMetadataQueryLevelFilter levelFilter1 = new HpcMetadataQueryLevelFilter();
			levelFilter1.setLabel(levelName);
			levelFilter1.setOperator(HpcMetadataQueryOperator.EQUAL);
			q1.setAttribute("access_group");
			q1.setValue("%" + x.getValue() + "%");
			q1.setLevelFilter(levelFilter1);
			q1.setOperator(HpcMetadataQueryOperator.LIKE);
			queries1.add(q1);
		}

		query1.getQueries().addAll(queries1);

		// perform and operation of query and query1
		HpcCompoundMetadataQuery query2 = new HpcCompoundMetadataQuery();
		query2.setOperator(HpcCompoundMetadataQueryOperator.AND);
		query2.getCompoundQueries().add(query1);
		query2.getCompoundQueries().add(query);

		return query2;

	}

	public Map<String, HpcMetadataQuery> getQueries(DoeSearch search) {
		Map<String, HpcMetadataQuery> queries = new HashMap<String, HpcMetadataQuery>();

		for (int i = 0; i < search.getAttrName().length; i++) {
			String rowId = search.getRowId()[i];
			String attrName = search.getAttrName()[i];
			String attrValue = search.getAttrValue()[i];
			String operator = search.getOperator()[i];
			String level = null;
			boolean selfMetadata = search.getIsExcludeParentMetadata()[i];

			LookUp val = lookUpService.getLookUpByDisplayName(attrName);

			if (val != null) {
				level = val.getLevelName();
			} else {
				level = search.getLevel()[i];
			}

			if (!attrValue.isEmpty()) {
				HpcMetadataQuery criteria = new HpcMetadataQuery();
				if (StringUtils.isEmpty(attrName) || StringUtils.isBlank(attrName)
						|| "ANY".equalsIgnoreCase(attrName)) {
					criteria.setAttributeMatch(HpcMetadataQueryAttributeMatch.ANY);
				} else {
					if (val != null) {
						criteria.setAttribute(val.getAttrName());
					} else {
						criteria.setAttribute(attrName);
					}

				}
				criteria.setValue(attrValue);
				criteria.setOperator(HpcMetadataQueryOperator.fromValue(operator));
				// If its a timestamp operator, specify the format
				if (operator.startsWith("TIMESTAMP_GREATER")) {
					criteria.setValue(attrValue.concat(" 00:00:00").replace("/", "-"));
					criteria.setFormat("MM-DD-YYYY HH24:MI:SS");
				}
				if (operator.startsWith("TIMESTAMP_LESS")) {
					criteria.setValue(attrValue.concat(" 23:59:59").replace("/", "-"));
					criteria.setFormat("MM-DD-YYYY HH24:MI:SS");
				}
				if (level != null) {
					HpcMetadataQueryLevelFilter levelFilter = new HpcMetadataQueryLevelFilter();
					if (selfMetadata) {
						levelFilter.setLevel(1);
						levelFilter.setOperator(HpcMetadataQueryOperator.EQUAL);
					} else if (level.equals("ANY")) {
						levelFilter.setLevel(1);
						levelFilter.setOperator(HpcMetadataQueryOperator.NUM_GREATER_OR_EQUAL);
					} else {
						if (level.equals("Data file") || level.equals("DataObject"))
							levelFilter.setLevel(1);
						else
							levelFilter.setLabel(level);
						levelFilter.setOperator(HpcMetadataQueryOperator.EQUAL);
					}

					criteria.setLevelFilter(levelFilter);
				}
				queries.put(rowId, criteria);
			}
		}
		return queries;
	}

	@SuppressWarnings("unchecked")
	public void getAssetDetails(HttpSession session, String dmeDataId, String returnToSearch, String assetIdentifier,
			Model model) throws DoeWebException {

		log.info("get Asset detials for dmeDataId: " + dmeDataId + " is returnToSearch: " + returnToSearch
				+ " and assetIdentifier is : " + assetIdentifier);
		String authToken = (String) session.getAttribute("hpcUserToken");
		log.info("authToken: " + authToken);
		String user = getLoggedOnUserInfo();
		log.info("asset details for user: " + user);
		List<KeyValueBean> loggedOnUserPermissions = (List<KeyValueBean>) getMetaDataPermissionsList().getBody();
		DoeSearch search = new DoeSearch();

		if (StringUtils.isNotEmpty(dmeDataId)) {
			String[] attrNames = { "collection_type", "dme_data_id" };
			String[] attrValues = { "Asset", dmeDataId };
			search.setAttrName(attrNames);
			search.setAttrValue(attrValues);
		} else if (StringUtils.isNotEmpty(assetIdentifier)) {

			String[] attrNames = { "collection_type", "asset_identifier" };
			String[] attrValues = { "Asset", assetIdentifier };
			search.setAttrName(attrNames);
			search.setAttrValue(attrValues);
		}
		String[] levelValues = { "ANY", "Asset" };
		boolean[] isExcludeParentMetadata = { false, false };
		String[] rowIds = { "1", "2" };
		String[] operators = { "LIKE", "LIKE" };
		boolean[] iskeyWordSearch = { true, false };

		search.setLevel(levelValues);
		search.setIsExcludeParentMetadata(isExcludeParentMetadata);
		search.setRowId(rowIds);
		search.setOperator(operators);
		search.setIskeyWordSearch(iskeyWordSearch);

		List<String> systemAttrs = (List<String>) session.getAttribute("systemAttrs");
		if (CollectionUtils.isEmpty(systemAttrs)) {
			HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");
			if (modelDTO == null) {
				modelDTO = DoeClientUtil.getDOCModel(authToken, hpcModelURL, sslCertPath, sslCertPassword);
				session.setAttribute("userDOCModel", modelDTO);
			}

			systemAttrs = modelDTO.getCollectionSystemGeneratedMetadataAttributeNames();
			List<String> dataObjectsystemAttrs = modelDTO.getDataObjectSystemGeneratedMetadataAttributeNames();
			systemAttrs.addAll(dataObjectsystemAttrs);
			systemAttrs.add("collection_type");
			systemAttrs.add("access_group");
			session.setAttribute("systemAttrs", systemAttrs);
		}

		try {
			HpcCompoundMetadataQueryDTO compoundQuery = constructCriteria(search, "Asset");
			compoundQuery.setDetailedResponse(true);
			log.info("search compund query" + compoundQuery);

			Response restResponse = DoeClientUtil.getDataObjectQuery(authToken, compoundDataObjectSearchServiceURL,
					true, sslCertPath, sslCertPassword, compoundQuery);

			if (restResponse.getStatus() == 200) {
				HpcCompoundMetadataQueryDTO compoundQuerySession = (HpcCompoundMetadataQueryDTO) session
						.getAttribute("compoundQuery");

				if (compoundQuerySession == null) {
					session.setAttribute("compoundQuery", compoundQuery);
				}

				MappingJsonFactory factory = new MappingJsonFactory();
				JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
				HpcCollectionListDTO collections = parser.readValueAs(HpcCollectionListDTO.class);
				HpcCollectionDTO collection = collections.getCollections().get(0);

				String studyName = getAttributeValue("study_name",
						collection.getMetadataEntries().getParentMetadataEntries(), "Study");

				String progName = getAttributeValue("program_name",
						collection.getMetadataEntries().getParentMetadataEntries(), "Program");

				String assetPermission = getPermissionRole(user, collection.getCollection().getCollectionId(),
						loggedOnUserPermissions);

				String accessGrp = getAttributeValue("access_group",
						collection.getMetadataEntries().getSelfMetadataEntries(), "Asset");

				String assetName = getAttributeValue("asset_name",
						collection.getMetadataEntries().getSelfMetadataEntries(), "Asset");

				String asset_Identifier = getAttributeValue("asset_identifier",
						collection.getMetadataEntries().getSelfMetadataEntries(), "Asset");

				String dme_Data_Id = getAttributeValue("dme_data_id",
						collection.getMetadataEntries().getSelfMetadataEntries(), "Asset");

				List<KeyValueBean> selfMetadata = getUserMetadata(
						collection.getMetadataEntries().getSelfMetadataEntries(), "Asset", systemAttrs);

				if (StringUtils.isNotEmpty(returnToSearch)) {
					model.addAttribute("returnToSearch", true);
				}

				model.addAttribute("dme_Data_Id", dme_Data_Id);
				model.addAttribute("asset_Identifier", asset_Identifier);
				model.addAttribute("accessGrp", accessGrp);
				model.addAttribute("assetName", assetName);
				model.addAttribute("assetMetadata", selfMetadata);
				model.addAttribute("studyName", studyName);
				model.addAttribute("progName", progName);
				model.addAttribute("assetPath", collection.getCollection().getCollectionName());
				model.addAttribute("assetPermission", assetPermission);
				model.addAttribute("assetLink", webUrl + "/assetDetails?dme_data_id=" + dme_Data_Id);
			} else {
				throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
			}
		} catch (Exception e) {
			throw new DoeWebException(e.getMessage());
		}

	}

	public void constructSearchCriteriaList(HttpSession session, Model model) throws DoeWebException {
		Map<String, Set<String>> browseList = new LinkedHashMap<String, Set<String>>();
		List<LookUp> results = lookUpService.getAllDisplayNames();

		for (LookUp val : results) {
			DoeSearch search = new DoeSearch();

			String[] attrNames = { "collection_type" };
			String[] attrValues = new String[3];
			attrValues[0] = val.getLevelName();
			String[] levelValues = new String[3];
			levelValues[0] = val.getLevelName();

			String[] rowIds = { "1" };
			String[] operators = { "EQUAL" };
			boolean[] isExcludeParentMetadata = { true };
			search.setLevel(levelValues);
			search.setAttrName(attrNames);
			search.setAttrValue(attrValues);

			search.setRowId(rowIds);
			search.setOperator(operators);
			search.setIsExcludeParentMetadata(isExcludeParentMetadata);
			search.setDetailed(true);

			Set<String> list = retrieveSearchList(session, search, val.getAttrName(), levelValues[0]);
			browseList.put(val.getDisplayName(), list);

		}
		model.addAttribute("browseList", browseList);
	}

	public Set<String> retrieveSearchList(HttpSession session, DoeSearch search, String attributeName, String levelName)
			throws DoeWebException {

		Set<String> list = new HashSet<>();
		String authToken = (String) session.getAttribute("hpcUserToken");
		log.info("authToken: " + authToken);

		try {
			HpcCompoundMetadataQueryDTO compoundQuery = constructCriteria(search, levelName);

			Response restResponse = DoeClientUtil.getCollectionSearchQuery(authToken,
					compoundCollectionSearchServiceURL, sslCertPath, sslCertPassword, compoundQuery);

			if (restResponse.getStatus() == 200) {
				MappingJsonFactory factory = new MappingJsonFactory();
				JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
				HpcCollectionListDTO collections = parser.readValueAs(HpcCollectionListDTO.class);
				List<HpcCollectionDTO> results = collections.getCollections();

				results.stream().flatMap(g -> g.getMetadataEntries().getSelfMetadataEntries().stream()).forEach(f -> {
					if (f.getAttribute().equalsIgnoreCase(attributeName)) {
						list.add(f.getValue());
					}
				});

				/*results.stream().flatMap(g -> g.getMetadataEntries().getParentMetadataEntries().stream()).forEach(f -> {
					if (f.getAttribute().equalsIgnoreCase(attributeName)) {
						list.add(f.getValue());
					}
				});*/

			}
		} catch (Exception e) {
			log.error("Failed to get search list");
			throw new DoeWebException(e);
		}
		return list;
	}

	public Set<String> constructFilterCriteria(HttpSession session, DoeSearch search) throws DoeWebException {
		String level = null;
		String attrName = null;
		LookUp value = lookUpService.getLookUpByDisplayName(search.getSearchName());
		if (value != null) {
			level = value.getLevelName();
			attrName = value.getAttrName();
		}

		int len = search.getRowId().length;

		String[] newRowId = Arrays.copyOf(search.getRowId(), len + 1);
		String[] newAttrNames = Arrays.copyOf(search.getAttrName(), len + 1);
		String[] newAttrValues = Arrays.copyOf(search.getAttrValue(), len + 1);
		String[] newLevelValues = new String[len + 1];
		boolean[] newIsExcludeParentMetadata = Arrays.copyOf(search.getIsExcludeParentMetadata(), len + 1);
		String[] newOperators = Arrays.copyOf(search.getOperator(), len + 1);

		newRowId[len] = String.valueOf(len + 1);
		newAttrNames[len] = "collection_type";
		newAttrValues[len] = level;
		newLevelValues[len] = level;
		newOperators[len] = "EQUAL";
		newIsExcludeParentMetadata[len] = false;

		search.setRowId(newRowId);
		search.setAttrName(newAttrNames);
		search.setAttrValue(newAttrValues);
		search.setLevel(newLevelValues);
		search.setIsExcludeParentMetadata(newIsExcludeParentMetadata);
		search.setOperator(newOperators);

		search.setDetailed(true);

		Set<String> list = retrieveSearchList(session, search, attrName, level);

		return list;
	}
}
