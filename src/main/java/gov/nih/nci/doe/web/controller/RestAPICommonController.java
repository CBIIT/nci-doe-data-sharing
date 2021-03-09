package gov.nih.nci.doe.web.controller;

import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.util.UriComponentsBuilder;
import org.apache.commons.io.IOUtils;

import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import springfox.documentation.annotations.ApiIgnore;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.MappingJsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;

import org.springframework.http.MediaType;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.AuditingModel;
import gov.nih.nci.doe.web.model.DoeUsersModel;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.service.TaskManagerService;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.domain.metadata.HpcCompoundMetadataQuery;
import gov.nih.nci.hpc.domain.metadata.HpcCompoundMetadataQueryOperator;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataQuery;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataQueryLevelFilter;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataQueryOperator;
import gov.nih.nci.hpc.dto.datamanagement.HpcBulkDataObjectDownloadResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDownloadResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionRegistrationDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectDownloadResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDownloadRequestDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectDTO;
import gov.nih.nci.hpc.dto.datasearch.HpcCompoundMetadataQueryDTO;

import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;
import javax.ws.rs.core.Response;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.cxf.jaxrs.client.WebClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

/**
 * Rest API Common Controller.
 *
 * @author <a href="mailto:mounica.ganta@nih.gov">Mounica Ganta</a>
 *
 */
@CrossOrigin
@RestController
@RequestMapping(value = "/api")
public class RestAPICommonController extends AbstractDoeController {

	@Value("${gov.nih.nci.hpc.server.dataObject}")
	private String dataObjectServiceURL;

	@Value("${gov.nih.nci.hpc.server.v2.download}")
	private String bulkDownloadUrl;

	@Value("${gov.nih.nci.hpc.server.collection}")
	private String serviceURL;

	@Value("${gov.nih.nci.hpc.server.v2.collection}")
	private String collectionUrl;

	@Value("${gov.nih.nci.hpc.server.search.collection.compound}")
	private String compoundCollectionSearchServiceURL;

	@Value("${gov.nih.nci.hpc.server.search.dataobject.compound}")
	private String compoundDataObjectSearchServiceURL;

	@Value("${gov.nih.nci.hpc.server.v2.bulkregistration}")
	private String bulkRegistrationURL;

	@Autowired
	TaskManagerService taskManagerService;

	private static final String TOKEN_SUBJECT = "MoDaCAuthenticationToken";

	@Value("${doe.userid.token.claim}")
	private String userIdTokenClaim;

	@Value("${doe.jwt.secret.key}")
	private String jwtSecretkey;

	// The authentication token expiration period in minutes.
	@Value("${doe.service.security.authenticationTokenExpirationPeriod}")
	private int authenticationTokenExpirationPeriod = 0;

	/**
	 * Returns the preassigned Url.
	 *
	 * @param path The path. @return The REST service response. @throws
	 */
	@PostMapping(value = "/dataObject/**/generateDownloadRequestURL")
	public ResponseEntity<?> generateDownloadRequestURL(@RequestHeader HttpHeaders headers, HttpServletRequest request,
			@ApiIgnore HttpSession session, HttpServletResponse response) throws DoeWebException, IOException {

		log.info("generateDownloadRequestURL");

		// getting path param from request URI.
		String path = request.getRequestURI().split(request.getContextPath() + "/dataObject/")[1];
		Integer index = path.lastIndexOf('/');
		path = path.substring(0, index);
		log.info("download sync:");
		log.info("Headers: {}", headers);
		log.info("pathName: " + path);

		// getting write access token for download request URL
		String authToken = (String) session.getAttribute("hpcUserToken");
		log.info("authToken: " + authToken);
		if (authToken == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		// verifying MoDaC authentication.
		String doeLogin = (String) session.getAttribute("doeLogin");
		log.info("doeLogin: " + doeLogin);
		if (doeLogin == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		Boolean isPermissions = false;
		String parentPath = null;
		if (path.lastIndexOf('/') != -1) {
			parentPath = path.substring(0, path.lastIndexOf('/'));
		}
		HpcCollectionListDTO collectionDto = DoeClientUtil.getCollection(authToken, serviceURL, parentPath, true,
				sslCertPath, sslCertPassword);

		HpcCollectionDTO result = collectionDto.getCollections().get(0);
		String accessGrp = getAttributeValue("access_group", result.getMetadataEntries().getSelfMetadataEntries());

		// verify group or owner permissions on the collection path
		if (StringUtils.isNotEmpty(accessGrp) && ("public".equalsIgnoreCase(accessGrp)
				|| Boolean.TRUE.equals(hasCollectionPermissions(doeLogin, parentPath, collectionDto)))) {
			isPermissions = true;
		}

		if (Boolean.TRUE.equals(isPermissions)) {
			final String requestUrl = UriComponentsBuilder.fromHttpUrl(this.dataObjectServiceURL)
					.path("/{dme-archive-path}/generateDownloadRequestURL").buildAndExpand(path).encode().toUri()
					.toURL().toExternalForm();

			final HpcDownloadRequestDTO dto = new HpcDownloadRequestDTO();
			dto.setGenerateDownloadRequestURL(true);

			WebClient client = DoeClientUtil.getWebClient(requestUrl, sslCertPath, sslCertPassword);
			client.header("Authorization", "Bearer " + authToken);

			Response restResponse = client.invoke("POST", dto);
			log.info("rest response:" + restResponse.getStatus());
			if (restResponse.getStatus() == 200) {
				MappingJsonFactory factory = new MappingJsonFactory();
				JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
				HpcDataObjectDownloadResponseDTO dataObject = parser
						.readValueAs(HpcDataObjectDownloadResponseDTO.class);
				ObjectMapper mapper = new ObjectMapper();
				mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
				return new ResponseEntity<>(mapper.writeValueAsString(dataObject), HttpStatus.OK);
			}
		}

		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);

	}

	/**
	 * 
	 * download data objects or collection
	 * 
	 * @param headers
	 * @param request
	 * @param session
	 * @param response
	 * @param downloadRequest
	 * @return
	 * @throws DoeWebException
	 * @throws MalformedURLException
	 */
	@PostMapping(value = "/v2/download")
	public ResponseEntity<?> downloadDataObjectsOrCollections(@RequestHeader HttpHeaders headers,
			HttpServletRequest request, @ApiIgnore HttpSession session, HttpServletResponse response,
			@RequestBody @Valid gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectDownloadRequestDTO downloadRequest)
			throws DoeWebException, MalformedURLException {

		log.info("download collection:");
		log.info("Headers: {}", headers);

		String authToken = (String) session.getAttribute("hpcUserToken");
		log.info("authToken: " + authToken);
		if (authToken == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		// verifying MoDaC authentication.
		String doeLogin = (String) session.getAttribute("doeLogin");
		log.info("doeLogin: " + doeLogin);
		if (doeLogin == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}
		String requestURL;
		UriComponentsBuilder ucBuilder = UriComponentsBuilder.fromHttpUrl(bulkDownloadUrl);

		requestURL = ucBuilder.build().encode().toUri().toURL().toExternalForm();
		WebClient client = DoeClientUtil.getWebClient(requestURL, sslCertPath, sslCertPassword);
		client.header("Authorization", "Bearer " + authToken);
		Response restResponse = client.invoke("POST", downloadRequest);

		log.info("rest response:" + restResponse.getStatus());
		if (restResponse.getStatus() == 200) {
			HpcBulkDataObjectDownloadResponseDTO downloadDTO = (HpcBulkDataObjectDownloadResponseDTO) DoeClientUtil
					.getObject(restResponse, HpcBulkDataObjectDownloadResponseDTO.class);
			try {
				taskManagerService.saveTransfer(downloadDTO.getTaskId(), "Download", "async", "datafiles", doeLogin);
				// store the auditing info
				AuditingModel audit = new AuditingModel();
				audit.setName(doeLogin);
				audit.setOperation("Download");
				audit.setStartTime(new Date());
				audit.setTransferType("async");
				audit.setTaskId(downloadDTO.getTaskId());
				auditingService.saveAuditInfo(audit);
			} catch (Exception e) {
				log.error("error in save transfer" + e.getMessage());
			}
			return new ResponseEntity<>(downloadDTO, HttpStatus.OK);
		}

		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);

	}

	/**
	 * collection download
	 * 
	 * @param downloadRequest
	 */
	@PostMapping(value = "/v2/collection/**/download")
	public ResponseEntity<?> collectionDownload(@RequestHeader HttpHeaders headers, HttpServletRequest request,
			@ApiIgnore HttpSession session, HttpServletResponse response,
			@RequestBody @Valid gov.nih.nci.hpc.dto.datamanagement.v2.HpcDownloadRequestDTO downloadRequest)
			throws DoeWebException, MalformedURLException {

		log.info("download collection:");
		String path = request.getRequestURI().split(request.getContextPath() + "/v2/collection/")[1];
		Integer index = path.lastIndexOf('/');
		path = path.substring(0, index);

		log.info("Headers: {}", headers);
		log.info("pathName: " + path);

		String authToken = (String) session.getAttribute("hpcUserToken");
		log.info("authToken: " + authToken);
		if (authToken == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		// verifying MoDaC authentication.
		String doeLogin = (String) session.getAttribute("doeLogin");
		log.info("doeLogin: " + doeLogin);
		if (doeLogin == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}
		Boolean isPermissions = false;

		HpcCollectionListDTO collectionDto = DoeClientUtil.getCollection(authToken, serviceURL, path, true, sslCertPath,
				sslCertPassword);

		HpcCollectionDTO result = collectionDto.getCollections().get(0);
		String accessGrp = getAttributeValue("access_group", result.getMetadataEntries().getSelfMetadataEntries());

		if (StringUtils.isNotEmpty(accessGrp) && ("public".equalsIgnoreCase(accessGrp)
				|| Boolean.TRUE.equals(hasCollectionPermissions(doeLogin, path, collectionDto)))) {
			isPermissions = true;
		}

		if (Boolean.TRUE.equals(isPermissions)) {
			final String requestUrl = UriComponentsBuilder.fromHttpUrl(this.collectionUrl)
					.path("/{dme-archive-path}/download").buildAndExpand(path).encode().toUri().toURL()
					.toExternalForm();

			WebClient client = DoeClientUtil.getWebClient(requestUrl, sslCertPath, sslCertPassword);
			client.header("Authorization", "Bearer " + authToken);

			Response restResponse = client.invoke("POST", downloadRequest);
			log.info("rest response:" + restResponse.getStatus());
			if (restResponse.getStatus() == 200) {
				HpcCollectionDownloadResponseDTO downloadDTO = (HpcCollectionDownloadResponseDTO) DoeClientUtil
						.getObject(restResponse, HpcCollectionDownloadResponseDTO.class);
				String name = path.substring(path.lastIndexOf('/') + 1);
				try {
					taskManagerService.saveTransfer(downloadDTO.getTaskId(), "Download", "async", name, doeLogin);
					// store the auditing info
					AuditingModel audit = new AuditingModel();
					audit.setName(doeLogin);
					audit.setOperation("Download");
					audit.setStartTime(new Date());
					audit.setPath(path);
					audit.setTransferType("async");
					audit.setTaskId(downloadDTO.getTaskId());
					auditingService.saveAuditInfo(audit);
				} catch (Exception e) {
					log.error("error in save transfer" + e.getMessage());
				}
				return new ResponseEntity<>(downloadDTO, HttpStatus.OK);
			}
		}
		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);
	}

	/**
	 * asynchronousDownload
	 * 
	 * @throws IOException
	 * 
	 * 
	 */
	@PostMapping(value = "/dataObject/**/download")
	public ResponseEntity<?> synchronousDownload(@RequestHeader HttpHeaders headers, HttpServletRequest request,
			@ApiIgnore HttpSession session, HttpServletResponse response)
			throws DoeWebException, MalformedURLException {

		log.info("download async:");
		String path = request.getRequestURI().split(request.getContextPath() + "/dataObject/")[1];
		Integer index = path.lastIndexOf('/');
		path = path.substring(0, index);
		log.info("Headers: {}", headers);
		log.info("pathName: " + path);

		String authToken = (String) session.getAttribute("hpcUserToken");
		log.info("authToken: " + authToken);
		if (authToken == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		// verifying MoDaC authentication.
		String doeLogin = (String) session.getAttribute("doeLogin");
		log.info("doeLogin: " + doeLogin);
		if (doeLogin == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}
		Boolean isPermissions = false;
		String parentPath = null;
		if (path.lastIndexOf('/') != -1) {
			parentPath = path.substring(0, path.lastIndexOf('/'));
		}

		HpcCollectionListDTO collectionDto = DoeClientUtil.getCollection(authToken, serviceURL, parentPath, true,
				sslCertPath, sslCertPassword);

		HpcCollectionDTO result = collectionDto.getCollections().get(0);
		String accessGrp = getAttributeValue("access_group", result.getMetadataEntries().getSelfMetadataEntries());

		if (StringUtils.isNotEmpty(accessGrp) && ("public".equalsIgnoreCase(accessGrp)
				|| Boolean.TRUE.equals(hasCollectionPermissions(doeLogin, parentPath, collectionDto)))) {
			isPermissions = true;
		}

		if (Boolean.TRUE.equals(isPermissions)) {
			final String requestUrl = UriComponentsBuilder.fromHttpUrl(this.dataObjectServiceURL)
					.path("/{dme-archive-path}/download").buildAndExpand(path).encode().toUri().toURL()
					.toExternalForm();

			final HpcDownloadRequestDTO dto = new HpcDownloadRequestDTO();
			dto.setGenerateDownloadRequestURL(true);

			WebClient client = DoeClientUtil.getWebClient(requestUrl, sslCertPath, sslCertPassword);
			client.header("Authorization", "Bearer " + authToken);

			Response restResponse = client.invoke("POST", dto);
			log.info("rest response:" + restResponse.getStatus());
			if (restResponse.getStatus() == 200) {
				HpcDataObjectDownloadResponseDTO downloadDTO = (HpcDataObjectDownloadResponseDTO) DoeClientUtil
						.getObject(restResponse, HpcDataObjectDownloadResponseDTO.class);
				downloadToUrl(downloadDTO.getDownloadRequestURL(), 1000000, "test", response);
				return new ResponseEntity<>(HttpStatus.OK);
			}
		}
		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);
	}

	/**
	 * synchronous download
	 * 
	 * @param downloadRequest
	 * @throws DoeWebException
	 * @throws IOException
	 * @throws Exception
	 */
	@PostMapping(value = "/v2/dataObject/**/download")
	public ResponseEntity<?> asynchronousDownload(@RequestHeader HttpHeaders headers, @ApiIgnore HttpSession session,
			HttpServletResponse response, HttpServletRequest request,
			@RequestBody @Valid gov.nih.nci.hpc.dto.datamanagement.v2.HpcDownloadRequestDTO downloadRequest)
			throws DoeWebException, IOException {

		log.info("download async:" + downloadRequest);
		log.info("Headers: {}", headers);

		String path = request.getRequestURI().split(request.getContextPath() + "/v2/dataObject/")[1];
		Integer index = path.lastIndexOf('/');
		path = path.substring(0, index);

		log.info("pathName: " + path);

		String authToken = (String) session.getAttribute("hpcUserToken");
		log.info("authToken: " + authToken);
		if (authToken == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		// verifying MoDaC authentication.
		String doeLogin = (String) session.getAttribute("doeLogin");
		log.info("doeLogin: " + doeLogin);
		if (doeLogin == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}
		String parentPath = null;
		if (path.lastIndexOf('/') != -1) {
			parentPath = path.substring(0, path.lastIndexOf('/'));
		}

		Boolean isPermissions = false;
		HpcCollectionListDTO collectionDto = DoeClientUtil.getCollection(authToken, serviceURL, parentPath, true,
				sslCertPath, sslCertPassword);

		HpcCollectionDTO result = collectionDto.getCollections().get(0);
		String accessGrp = getAttributeValue("access_group", result.getMetadataEntries().getSelfMetadataEntries());

		if (StringUtils.isNotEmpty(accessGrp) && ("public".equalsIgnoreCase(accessGrp)
				|| Boolean.TRUE.equals(hasCollectionPermissions(doeLogin, parentPath, collectionDto)))) {
			isPermissions = true;
		}

		if (Boolean.TRUE.equals(isPermissions)) {
			final String requestUrl = UriComponentsBuilder.fromHttpUrl(this.dataObjectAsyncServiceURL)
					.path("/{dme-archive-path}/download").buildAndExpand(path).encode().toUri().toURL()
					.toExternalForm();

			WebClient client = DoeClientUtil.getWebClient(requestUrl, sslCertPath, sslCertPassword);
			client.header("Authorization", "Bearer " + authToken);

			Response restResponse = client.invoke("POST", downloadRequest);
			log.info("rest response:" + restResponse.getStatus());
			if (restResponse.getStatus() == 200) {
				Object value = restResponse.getMetadata().getFirst("content-type");
				if ("application/octet-stream".equalsIgnoreCase(value.toString())) {
					
					log.info("response content type is application/octet-stream");
					response.setContentType("application/octet-stream");
					response.setHeader("Content-Disposition", "attachment; filename=" + "test");
					IOUtils.copy((InputStream) restResponse.getEntity(), response.getOutputStream());
					return new ResponseEntity<>(HttpStatus.OK);

				} else {
					HpcDataObjectDownloadResponseDTO downloadDTO = (HpcDataObjectDownloadResponseDTO) DoeClientUtil
							.getObject(restResponse, HpcDataObjectDownloadResponseDTO.class);

					String dataObjectName = path.substring(path.lastIndexOf('/') + 1);

					try {
						taskManagerService.saveTransfer(downloadDTO.getTaskId(), "Download", "data_object",
								dataObjectName, doeLogin);
						// store the auditing info
						AuditingModel audit = new AuditingModel();
						audit.setName(doeLogin);
						audit.setOperation("Download");
						audit.setStartTime(new Date());
						audit.setPath(path);
						audit.setTransferType("async");
						audit.setTaskId(downloadDTO.getTaskId());
						auditingService.saveAuditInfo(audit);
					} catch (Exception e) {
						log.error("error in save transfer" + e.getMessage());
					}
					return new ResponseEntity<>(downloadDTO, HttpStatus.OK);
				}

			}
		}
		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);
	}

	/**
	 * get data object
	 * 
	 * @param includeAcl
	 * 
	 */
	@GetMapping(value = "/v2/dataObject/**", produces = { MediaType.APPLICATION_JSON_VALUE,
			MediaType.APPLICATION_OCTET_STREAM_VALUE })
	public ResponseEntity<?> getDataObject(@RequestHeader HttpHeaders headers, @ApiIgnore HttpSession session,
			HttpServletResponse response, HttpServletRequest request,
			@RequestParam(required = false) Boolean includeAcl) throws DoeWebException {

		log.info("get dataobject:");
		log.info("Headers: {}", headers);
		String path = request.getRequestURI().split(request.getContextPath() + "/v2/dataObject/")[1];
		log.info("pathName: " + path);
		log.info("query params: includeAcl: " + includeAcl);

		String authToken = (String) session.getAttribute("hpcUserToken");
		log.info("authToken: " + authToken);

		if (authToken == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		Boolean isPermissions = false;
		String doeLogin = (String) session.getAttribute("doeLogin");
		log.info("doeLogin: " + doeLogin);
		String parentPath = null;
		parentPath = path.substring(0, path.lastIndexOf('/'));

		if (!parentPath.isEmpty()) {
			HpcCollectionListDTO collectionDto = DoeClientUtil.getCollection(authToken, serviceURL, parentPath, true,
					sslCertPath, sslCertPassword);

			HpcCollectionDTO result = collectionDto.getCollections().get(0);
			String accessGrp = getAttributeValue("access_group", result.getMetadataEntries().getSelfMetadataEntries());

			if (StringUtils.isNotEmpty(accessGrp) && ("public".equalsIgnoreCase(accessGrp)
					|| Boolean.TRUE.equals(hasCollectionPermissions(doeLogin, path, collectionDto)))) {
				isPermissions = true;
			}

			if (Boolean.TRUE.equals(isPermissions)) {

				HpcDataObjectDTO dataObjectList = DoeClientUtil.getDatafiles(authToken, dataObjectAsyncServiceURL, path,
						true, includeAcl, sslCertPath, sslCertPassword);
				if (dataObjectList != null) {
					return new ResponseEntity<>(dataObjectList, HttpStatus.OK);
				}

			}
		}
		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);
	}

	/**
	 * get collection
	 * 
	 * @param includeAcl
	 * @param list
	 */
	@GetMapping(value = "/collection/**", produces = { MediaType.APPLICATION_JSON_VALUE,
			MediaType.APPLICATION_OCTET_STREAM_VALUE })
	public ResponseEntity<?> getCollection(@RequestHeader HttpHeaders headers, @ApiIgnore HttpSession session,
			HttpServletResponse response, HttpServletRequest request, @RequestParam(required = false) Boolean list,
			@RequestParam(required = false) Boolean includeAcl) throws DoeWebException {

		log.info("download async:");
		log.info("Headers: {}", headers);
		log.info("query params: includeAcl: " + includeAcl + ", list: " + list);
		String path = request.getRequestURI().split(request.getContextPath() + "/collection/")[1];
		log.info("pathName: " + path);

		String authToken = (String) session.getAttribute("hpcUserToken");
		log.info("authToken: " + authToken);

		if (authToken == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		Boolean isPermissions = false;
		String doeLogin = (String) session.getAttribute("doeLogin");
		log.info("doeLogin: " + doeLogin);
		HpcCollectionListDTO collectionDto = DoeClientUtil.getCollection(authToken, serviceURL, path, false, list,
				includeAcl, sslCertPath, sslCertPassword);

		HpcCollectionDTO result = collectionDto.getCollections().get(0);
		String accessGrp = getAttributeValue("access_group", result.getMetadataEntries().getSelfMetadataEntries());

		if (StringUtils.isNotEmpty(accessGrp) && ("public".equalsIgnoreCase(accessGrp)
				|| Boolean.TRUE.equals(hasCollectionPermissions(doeLogin, path, collectionDto)))) {
			isPermissions = true;
		}

		if (Boolean.TRUE.equals(isPermissions) && CollectionUtils.isNotEmpty(collectionDto.getCollections())) {
			return new ResponseEntity<>(collectionDto.getCollections(), HttpStatus.OK);

		}
		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);

	}

	/**
	 * register collection
	 * 
	 * @param collectionRegistration
	 */
	@PutMapping(value = "/collection/**")
	public Integer registerCollection(@RequestHeader HttpHeaders headers, @ApiIgnore HttpSession session,
			HttpServletResponse response, HttpServletRequest request,
			@RequestBody @Valid HpcCollectionRegistrationDTO collectionRegistration) throws DoeWebException {

		log.info("register collection: " + collectionRegistration);
		log.info("Headers: {}", headers);
		String path = request.getRequestURI().split(request.getContextPath() + "/collection/")[1];
		log.info("pathName: " + path);

		String authToken = (String) session.getAttribute("writeAccessUserToken");
		log.info("authToken: " + authToken);

		if (authToken == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		// verifying MoDaC authentication.
		String doeLogin = (String) session.getAttribute("doeLogin");
		log.info("doeLogin: " + doeLogin);
		if (doeLogin == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		if (Boolean.TRUE.equals(isUploader(doeLogin))) {
			String parentPath = null;
			if (path.lastIndexOf('/') != -1) {
				parentPath = path.substring(0, path.lastIndexOf('/'));
			} else {
				parentPath = path;
			}
			if (!parentPath.isEmpty()) {
				if (!parentPath.equalsIgnoreCase(basePath)) {
					HpcCollectionListDTO parentCollectionDto = DoeClientUtil.getCollection(authToken, serviceURL,
							parentPath, true, sslCertPath, sslCertPassword);
					Boolean isValidPermissions = hasCollectionPermissions(doeLogin, parentPath, parentCollectionDto);
					if (Boolean.FALSE.equals(isValidPermissions)) {
						throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);
					}
				}
			} else {
				throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);
			}

			Integer responseStatus = DoeClientUtil.updateCollection(authToken, serviceURL, collectionRegistration, path,
					sslCertPath, sslCertPassword);
			if (responseStatus == 200 || responseStatus == 201) {
				// after collection is created, store the permissions.
				String progList = request.getParameter("metaDataPermissionsList");
				log.info("selected permissions" + progList);
				HpcCollectionListDTO collections = DoeClientUtil.getCollection(authToken, serviceURL, path, false,
						sslCertPath, sslCertPassword);
				if (collections != null && collections.getCollections() != null
						&& !CollectionUtils.isEmpty(collections.getCollections())) {
					HpcCollectionDTO collection = collections.getCollections().get(0);
					try {
						metaDataPermissionService.savePermissionsList(doeLogin, progList,
								collection.getCollection().getCollectionId(), path);
						// store the auditing info
						AuditingModel audit = new AuditingModel();
						audit.setName(doeLogin);
						audit.setOperation("register collection");
						audit.setStartTime(new Date());
						audit.setPath(path);
						audit.setTransferType("async");
						auditingService.saveAuditInfo(audit);
					} catch (Exception e) {
						log.error("error in save permissions list" + e.getMessage());
					}
				}

				return responseStatus;
			}
		}
		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);

	}

	/**
	 * register data object
	 * 
	 * @param doeDataFile
	 * @param dataObjectRegistration
	 * 
	 */
	@PutMapping(value = "/v2/dataObject/**")
	public Integer registerDataObject(@RequestHeader HttpHeaders headers, @ApiIgnore HttpSession session,
			HttpServletResponse response, HttpServletRequest request,
			@RequestPart("dataObjectRegistration") @Valid gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationRequestDTO dataObjectRegistration,
			@RequestBody(required = false) @Valid MultipartFile doeDataFile) throws DoeWebException {

		log.info("register data files: " + dataObjectRegistration);
		log.info("multipart file: " + doeDataFile);
		log.info("Headers: {}", headers);
		String path = request.getRequestURI().split(request.getContextPath() + "/v2/dataObject/")[1];
		log.info("pathName: " + path);

		String authToken = (String) session.getAttribute("writeAccessUserToken");
		log.info("authToken: " + authToken);

		if (authToken == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		// verifying MoDaC authentication.
		String doeLogin = (String) session.getAttribute("doeLogin");
		log.info("doeLogin: " + doeLogin);
		if (doeLogin == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}
		if (StringUtils.isNotEmpty(doeLogin) && Boolean.TRUE.equals(isUploader(doeLogin))) {
			String parentPath = null;
			parentPath = path.substring(0, path.lastIndexOf('/'));

			if (!parentPath.isEmpty()) {
				HpcCollectionListDTO parentCollectionDto = DoeClientUtil.getCollection(authToken, serviceURL,
						parentPath, true, sslCertPath, sslCertPassword);
				Boolean isValidPermissions = hasCollectionPermissions(doeLogin, parentPath, parentCollectionDto);
				if (Boolean.FALSE.equals(isValidPermissions)) {
					throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);
				}
			}

			Integer restResponse = DoeClientUtil.registerDatafile(authToken, doeDataFile, dataObjectAsyncServiceURL,
					dataObjectRegistration, path, sslCertPath, sslCertPassword);
			if (restResponse == 200 || restResponse == 201) {
				// store the auditing info
				AuditingModel audit = new AuditingModel();
				audit.setName(doeLogin);
				audit.setOperation("Upload Single File");
				audit.setStartTime(new Date());
				audit.setPath(path);
				auditingService.saveAuditInfo(audit);

				return restResponse;
			}
		}

		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);
	}

	/**
	 * search collections
	 * 
	 * @param headers
	 * @param session
	 * @param response
	 * @param request
	 * @param compoundMetadataQuery
	 * @return
	 * @throws DoeWebException
	 * @throws IOException
	 */
	@PostMapping(value = "/collection/query", consumes = { MediaType.APPLICATION_XML_VALUE,
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_XML_VALUE,
					MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<?> queryCollections(@RequestHeader HttpHeaders headers, @ApiIgnore HttpSession session,
			HttpServletResponse response, HttpServletRequest request,
			@RequestBody @Valid HpcCompoundMetadataQueryDTO compoundMetadataQuery) throws DoeWebException, IOException {

		log.info("search collection query: " + compoundMetadataQuery);
		String authToken = (String) session.getAttribute("hpcUserToken");

		log.info("authToken: " + authToken);

		if (authToken == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		String doeLogin = (String) session.getAttribute("doeLogin");
		log.info("doeLogin: " + doeLogin);

		HpcCompoundMetadataQuery query2 = getCompoundQuery(doeLogin, compoundMetadataQuery);
		compoundMetadataQuery.setCompoundQuery(query2);
		compoundMetadataQuery.setDetailedResponse(true);

		Response restResponse = DoeClientUtil.getCollectionSearchQuery(authToken, compoundCollectionSearchServiceURL,
				sslCertPath, sslCertPassword, compoundMetadataQuery);

		if (restResponse.getStatus() == 200) {
			MappingJsonFactory factory = new MappingJsonFactory();
			JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
			return new ResponseEntity<>(parser.readValueAs(HpcCollectionListDTO.class), HttpStatus.OK);

		}
		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);

	}

	/**
	 * search dataobject
	 * 
	 * @param headers
	 * @param session
	 * @param response
	 * @param request
	 * @param returnParent
	 * @param compoundMetadataQuery
	 * @return
	 * @throws DoeWebException
	 * @throws IOException
	 */
	@PostMapping(value = "/dataObject/query", consumes = { MediaType.APPLICATION_XML_VALUE,
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_XML_VALUE,
					MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<?> queryDataObjects(@RequestHeader HttpHeaders headers, @ApiIgnore HttpSession session,
			HttpServletResponse response, HttpServletRequest request,
			@RequestParam(required = false) Boolean returnParent,
			@RequestBody @Valid HpcCompoundMetadataQueryDTO compoundMetadataQuery) throws DoeWebException, IOException {

		log.info("search data object query: " + compoundMetadataQuery);
		String authToken = (String) session.getAttribute("hpcUserToken");
		log.info("authToken: " + authToken);

		if (authToken == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		String doeLogin = (String) session.getAttribute("doeLogin");
		log.info("doeLogin: " + doeLogin);

		if (Boolean.TRUE.equals(returnParent)) {
			HpcCompoundMetadataQuery query2 = getCompoundQuery(doeLogin, compoundMetadataQuery);
			compoundMetadataQuery.setCompoundQuery(query2);
		}
		compoundMetadataQuery.setDetailedResponse(true);

		Response restResponse = DoeClientUtil.getDataObjectQuery(authToken, compoundDataObjectSearchServiceURL,
				returnParent, sslCertPath, sslCertPassword, compoundMetadataQuery);

		if (restResponse.getStatus() == 200 || restResponse.getStatus() == 201) {
			MappingJsonFactory factory = new MappingJsonFactory();
			JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
			if (Boolean.TRUE.equals(returnParent)) {
				HpcCollectionListDTO dataObjects = parser.readValueAs(HpcCollectionListDTO.class);
				return new ResponseEntity<>(dataObjects, HttpStatus.OK);
			} else {
				HpcDataObjectListDTO dataObjects = parser.readValueAs(HpcDataObjectListDTO.class);
				return new ResponseEntity<>(dataObjects, HttpStatus.OK);
			}
		}
		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);
	}

	/**
	 * create modac token
	 * 
	 * @param headers
	 * @param session
	 * @param response
	 * @param request
	 * @return token
	 * @throws DoeWebException
	 */

	@GetMapping(value = "/authenticate")
	public ResponseEntity<?> authenticate(@RequestHeader HttpHeaders headers, @ApiIgnore HttpSession session,
			HttpServletResponse response, HttpServletRequest request) throws DoeWebException {

		log.info("create modac authentication token");
		String doeLogin = (String) session.getAttribute("doeLogin");
		log.info("doeLogin: " + doeLogin);

		// Prepare the Claims Map.
		Map<String, Object> claims = new HashMap<>();
		if (StringUtils.isNotEmpty(doeLogin)) {
			claims.put(userIdTokenClaim, doeLogin);
			// Calculate the expiration date.
			Calendar tokenExpiration = Calendar.getInstance();
			tokenExpiration.add(Calendar.MINUTE, authenticationTokenExpirationPeriod);
			// construct JWT token
			String token = Jwts.builder().setSubject(TOKEN_SUBJECT).setClaims(claims)
					.setExpiration(tokenExpiration.getTime()).signWith(SignatureAlgorithm.HS256, jwtSecretkey)
					.compact();

			if (StringUtils.isNotEmpty(token)) {
				return new ResponseEntity<>(token, HttpStatus.OK);
			}
		}

		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);

	}

	private Boolean hasCollectionPermissions(String loggedOnUser, String parentPath,
			HpcCollectionListDTO parentCollectionDto) {

		log.info("has collection permssions for " + loggedOnUser + " path: " + parentPath);
		List<KeyValueBean> keyValueBeanResults = new ArrayList<>();
		if (!StringUtils.isEmpty(loggedOnUser)) {
			// get logged on user prog list to keyvaluebean list
			DoeUsersModel user = authenticateService.getUserInfo(loggedOnUser);
			if (user != null && !StringUtils.isEmpty(user.getProgramName())) {
				List<String> progList = Arrays.asList(user.getProgramName().split(","));
				progList.stream().forEach(e -> keyValueBeanResults.add(new KeyValueBean(e, e)));
			}
		}

		// verify collection path permissions
		if (!basePath.equalsIgnoreCase(parentPath) && parentCollectionDto != null
				&& parentCollectionDto.getCollections() != null
				&& !CollectionUtils.isEmpty(parentCollectionDto.getCollections())) {
			HpcCollectionDTO collection = parentCollectionDto.getCollections().get(0);
			String role = getPermissionRole(loggedOnUser, collection.getCollection().getCollectionId(),
					keyValueBeanResults);
			if (StringUtils.isNotEmpty(role)
					&& (role.equalsIgnoreCase("Owner") || role.equalsIgnoreCase("Group User"))) {
				return true;
			}
		}
		return false;

	}

	private Boolean isUploader(String emailAddr) {
		log.info("is uploader for : " + emailAddr);
		if (!StringUtils.isEmpty(emailAddr)) {
			DoeUsersModel user = authenticateService.getUserInfo(emailAddr);
			if (user.getIsWrite() != null && user.getIsWrite()) {
				return true;
			}
		}

		return false;
	}

	private String getAttributeValue(String attrName, List<HpcMetadataEntry> list) {
		if (list == null)
			return null;

		HpcMetadataEntry entry = list.stream().filter(e -> e.getAttribute().equalsIgnoreCase(attrName)).findAny()
				.orElse(null);
		if (entry != null) {
			return entry.getValue();
		}
		return null;
	}

	public HpcCompoundMetadataQuery getCompoundQuery(String doeLogin,
			HpcCompoundMetadataQueryDTO compoundMetadataQuery) {

		// get compound query for additional access groups.
		HpcCompoundMetadataQuery query = compoundMetadataQuery.getCompoundQuery();
		List<KeyValueBean> loggedOnUserPermissions = new ArrayList<>();
		if (StringUtils.isNotEmpty(doeLogin)) {
			DoeUsersModel user = authenticateService.getUserInfo(doeLogin);
			if (user != null && !StringUtils.isEmpty(user.getProgramName())) {
				List<String> progList = Arrays.asList(user.getProgramName().split(","));
				progList.stream().forEach(e -> loggedOnUserPermissions.add(new KeyValueBean(e, e)));
			}
		}

		HpcCompoundMetadataQuery query1 = new HpcCompoundMetadataQuery();
		query1.setOperator(HpcCompoundMetadataQueryOperator.OR);
		List<HpcMetadataQuery> queries1 = new ArrayList<HpcMetadataQuery>();

		// perform OR operation of public access and logged on users access groups
		HpcMetadataQuery q = new HpcMetadataQuery();
		HpcMetadataQueryLevelFilter levelFilter = new HpcMetadataQueryLevelFilter();
		levelFilter.setLabel("Asset");
		levelFilter.setOperator(HpcMetadataQueryOperator.EQUAL);
		q.setLevelFilter(levelFilter);
		q.setAttribute("access_group");
		q.setValue("public");
		q.setOperator(HpcMetadataQueryOperator.EQUAL);
		queries1.add(q);

		for (KeyValueBean x : loggedOnUserPermissions) {
			HpcMetadataQuery q1 = new HpcMetadataQuery();
			HpcMetadataQueryLevelFilter levelFilter1 = new HpcMetadataQueryLevelFilter();
			levelFilter1.setLabel("Asset");
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
}
