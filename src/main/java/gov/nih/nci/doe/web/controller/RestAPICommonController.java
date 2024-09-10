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

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.MappingJsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;

import org.springframework.http.MediaType;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.domain.InferencingTask;
import gov.nih.nci.doe.web.model.Path;
import gov.nih.nci.doe.web.model.ReferenceDataset;
import gov.nih.nci.doe.web.model.AuditingModel;
import gov.nih.nci.doe.web.model.DoeUsersModel;
import gov.nih.nci.doe.web.model.EvaluationResponse;
import gov.nih.nci.doe.web.model.InferencingTaskModel;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.model.ModelPath;
import gov.nih.nci.doe.web.model.ModelStatus;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.domain.datamanagement.HpcCollectionListingEntry;
import gov.nih.nci.hpc.domain.metadata.HpcCompoundMetadataQuery;
import gov.nih.nci.hpc.domain.metadata.HpcCompoundMetadataQueryOperator;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataQuery;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataQueryLevelFilter;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataQueryOperator;
import gov.nih.nci.hpc.dto.datamanagement.HpcBulkDataObjectDownloadResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDownloadResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDownloadStatusDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionRegistrationDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectDownloadResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectDownloadStatusDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectListDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationStatusDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcDirectoryScanRegistrationItemDTO;
import gov.nih.nci.hpc.dto.datasearch.HpcCompoundMetadataQueryDTO;

import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;
import javax.ws.rs.core.Response;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.cxf.jaxrs.client.WebClient;
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

	@Value("${gov.nih.nci.hpc.server.download}")
	private String queryServiceURL;

	private static final String TOKEN_SUBJECT = "MoDaCAuthenticationToken";

	@Value("${doe.userid.token.claim}")
	private String userIdTokenClaim;

	@Value("${doe.jwt.secret.key}")
	private String jwtSecretkey;

	// The authentication token expiration period in minutes.
	@Value("${doe.service.security.authenticationTokenExpirationPeriod}")
	private int authenticationTokenExpirationPeriod = 0;

	SimpleDateFormat format = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");

	/**
	 * Returns the preassigned Url.
	 *
	 * @param path The path. @return The REST service response. @throws
	 */
	@PostMapping(value = "/dataObject/**/generateDownloadRequestURL")
	public ResponseEntity<?> generateDownloadRequestURL(@RequestHeader HttpHeaders headers, HttpServletRequest request,
			HttpSession session, HttpServletResponse response) throws DoeWebException, IOException {

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
		HpcCollectionListDTO collectionDto = DoeClientUtil.getCollection(authToken, serviceURL, parentPath, true);

		HpcCollectionDTO result = collectionDto.getCollections().get(0);
		String accessGrp = getAttributeValue("access_group", result.getMetadataEntries().getSelfMetadataEntries());

		// verify group or owner permissions on the collection path
		if ("public".equalsIgnoreCase(accessGrp)
				|| Boolean.TRUE.equals(hasCollectionPermissions(session, doeLogin, parentPath, collectionDto))) {
			isPermissions = true;
		}

		if (Boolean.TRUE.equals(isPermissions)) {
			Response restResponse = DoeClientUtil.getPreSignedUrl(authToken, dataObjectServiceURL, path);
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
			HttpServletRequest request, HttpSession session, HttpServletResponse response,
			@RequestBody @Valid gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectDownloadRequestDTO downloadRequest)
			throws DoeWebException {

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

		Response restResponse = DoeClientUtil.downloadDataObjectsOrCollections(authToken, bulkDownloadUrl,
				downloadRequest);

		log.info("rest response:" + restResponse.getStatus());
		if (restResponse.getStatus() == 200) {
			HpcBulkDataObjectDownloadResponseDTO downloadDTO = (HpcBulkDataObjectDownloadResponseDTO) DoeClientUtil
					.getObject(restResponse, HpcBulkDataObjectDownloadResponseDTO.class);
			try {
				taskManagerService.saveTransfer(downloadDTO.getTaskId(), "Download", "async", "datafiles", doeLogin,
						null);
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
	 * @throws IOException
	 */
	@PostMapping(value = "/v2/collection/**/download")
	public ResponseEntity<?> collectionDownload(@RequestHeader HttpHeaders headers, HttpServletRequest request,
			HttpSession session, HttpServletResponse response,
			@RequestBody @Valid gov.nih.nci.hpc.dto.datamanagement.v2.HpcDownloadRequestDTO downloadRequest)
			throws DoeWebException {

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

		HpcCollectionListDTO collectionDto = DoeClientUtil.getCollection(authToken, serviceURL, path, true);

		HpcCollectionDTO result = collectionDto.getCollections().get(0);
		String accessGrp = getAttributeValue("access_group", result.getMetadataEntries().getSelfMetadataEntries());

		if ("public".equalsIgnoreCase(accessGrp)
				|| Boolean.TRUE.equals(hasCollectionPermissions(session, doeLogin, path, collectionDto))) {
			isPermissions = true;
		}

		if (Boolean.TRUE.equals(isPermissions)) {

			Response restResponse = DoeClientUtil.downloadCollection(authToken, collectionUrl, path, downloadRequest);
			log.info("rest response:" + restResponse.getStatus());
			if (restResponse.getStatus() == 200) {
				HpcCollectionDownloadResponseDTO downloadDTO = (HpcCollectionDownloadResponseDTO) DoeClientUtil
						.getObject(restResponse, HpcCollectionDownloadResponseDTO.class);
				String name = path.substring(path.lastIndexOf('/') + 1);
				try {
					taskManagerService.saveTransfer(downloadDTO.getTaskId(), "Download", "async", name, doeLogin, path);
					// store the auditing info
					AuditingModel audit = new AuditingModel();
					audit.setName(doeLogin);
					audit.setOperation("Download");
					audit.setStartTime(new Date());
					audit.setPath(path);
					audit.setTransferType("async");
					audit.setTaskId(downloadDTO.getTaskId());
					auditingService.saveAuditInfo(audit);

					return new ResponseEntity<>(downloadDTO, HttpStatus.OK);
				} catch (Exception e) {
					log.error("error in save transfer" + e.getMessage());
				}
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
	@PostMapping(value = "/dataObject/**/syncDownload")
	public ResponseEntity<?> synchronousDownload(@RequestHeader HttpHeaders headers, HttpServletRequest request,
			HttpSession session, HttpServletResponse response) throws DoeWebException, IOException {

		log.info("download async:");
		// getting path param from request URI.
		String path = request.getRequestURI().split(request.getContextPath() + "/dataObject/")[1];
		Integer index = path.lastIndexOf('/');
		path = path.substring(0, index);
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
		HpcCollectionListDTO collectionDto = DoeClientUtil.getCollection(authToken, serviceURL, parentPath, true);

		HpcCollectionDTO result = collectionDto.getCollections().get(0);
		String accessGrp = getAttributeValue("access_group", result.getMetadataEntries().getSelfMetadataEntries());

		// verify group or owner permissions on the collection path
		if ("public".equalsIgnoreCase(accessGrp)
				|| Boolean.TRUE.equals(hasCollectionPermissions(session, doeLogin, parentPath, collectionDto))) {
			isPermissions = true;
		}

		if (Boolean.TRUE.equals(isPermissions)) {

			Response restResponse = DoeClientUtil.getPreSignedUrl(authToken, dataObjectServiceURL, path);

			log.info("rest response:" + restResponse.getStatus());
			if (restResponse.getStatus() == 200) {
				MappingJsonFactory factory = new MappingJsonFactory();
				JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
				HpcDataObjectDownloadResponseDTO dataObject = parser
						.readValueAs(HpcDataObjectDownloadResponseDTO.class);
				downloadToUrl(dataObject.getDownloadRequestURL(), dataObject.getDestinationFile().getName(), response);
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
	public ResponseEntity<?> syncAndasynchronousDownload(@RequestHeader HttpHeaders headers, HttpSession session,
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

		String dataObjectName = path.substring(path.lastIndexOf('/') + 1);

		Boolean isPermissions = false;
		HpcCollectionListDTO collectionDto = DoeClientUtil.getCollection(authToken, serviceURL, parentPath, true);

		HpcCollectionDTO result = collectionDto.getCollections().get(0);
		String accessGrp = getAttributeValue("access_group", result.getMetadataEntries().getSelfMetadataEntries());

		if ("public".equalsIgnoreCase(accessGrp)
				|| Boolean.TRUE.equals(hasCollectionPermissions(session, doeLogin, parentPath, collectionDto))) {
			isPermissions = true;
		}

		if (Boolean.TRUE.equals(isPermissions)) {

			Response restResponse = DoeClientUtil.syncAndasynchronousDownload(authToken, dataObjectAsyncServiceURL,
					path, downloadRequest);
			log.info("rest response:" + restResponse.getStatus());
			if (restResponse.getStatus() == 200) {
				// verify the content type from restReponse. If the content is of type
				// application/octet-stream,
				// it is a sync download
				Object value = restResponse.getMetadata().getFirst("content-type");
				if ("application/octet-stream".equalsIgnoreCase(value.toString())) {

					log.info("response content type is application/octet-stream");
					response.setContentType("application/octet-stream");
					response.setHeader("Content-Disposition", "attachment; filename=" + dataObjectName);
					// default buffer size is 4k
					IOUtils.copy((InputStream) restResponse.getEntity(), response.getOutputStream(), bufferSize);
					return new ResponseEntity<>(HttpStatus.OK);

				} else {
					HpcDataObjectDownloadResponseDTO downloadDTO = (HpcDataObjectDownloadResponseDTO) DoeClientUtil
							.getObject(restResponse, HpcDataObjectDownloadResponseDTO.class);

					try {
						taskManagerService.saveTransfer(downloadDTO.getTaskId(), "Download", "data_object",
								dataObjectName, doeLogin, path);
						// store the auditing info
						AuditingModel audit = new AuditingModel();
						audit.setName(doeLogin);
						audit.setOperation("Download");
						audit.setStartTime(new Date());
						audit.setPath(path);
						audit.setTransferType("async");
						audit.setTaskId(downloadDTO.getTaskId());
						auditingService.saveAuditInfo(audit);

						return new ResponseEntity<>(downloadDTO, HttpStatus.OK);
					} catch (Exception e) {
						log.error("error in save transfer" + e.getMessage());
					}

				}

			}
		}
		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);
	}

	/**
	 * get data object
	 * 
	 * @param includeAcl
	 * @throws JsonProcessingException
	 * 
	 */
	@GetMapping(value = "/v2/dataObject/**", produces = { MediaType.APPLICATION_JSON_VALUE,
			MediaType.APPLICATION_OCTET_STREAM_VALUE })
	public ResponseEntity<?> getDataObject(@RequestHeader HttpHeaders headers, HttpSession session,
			HttpServletResponse response, HttpServletRequest request,
			@RequestParam(required = false) Boolean includeAcl,
			@RequestParam(required = false) Boolean excludeParentMetadata)
			throws DoeWebException, JsonProcessingException {

		log.info("get dataobject:");
		log.info("Headers: {}", headers);
		String path = request.getRequestURI().split(request.getContextPath() + "/v2/dataObject/")[1];
		log.info("pathName: " + path);
		log.info("query params: includeAcl: " + includeAcl);
		log.info("query params: excludeParentMetadata: " + excludeParentMetadata);

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
			HpcCollectionListDTO collectionDto = DoeClientUtil.getCollection(authToken, serviceURL, parentPath, true);

			HpcCollectionDTO result = collectionDto.getCollections().get(0);
			String accessGrp = getAttributeValue("access_group", result.getMetadataEntries().getSelfMetadataEntries());

			if ("public".equalsIgnoreCase(accessGrp)
					|| Boolean.TRUE.equals(hasCollectionPermissions(session, doeLogin, path, collectionDto))) {
				isPermissions = true;
			}

			if (Boolean.TRUE.equals(isPermissions)) {

				HpcDataObjectDTO dataObjectList = DoeClientUtil.getDatafiles(authToken, dataObjectAsyncServiceURL, path,
						includeAcl, excludeParentMetadata);
				if (dataObjectList != null) {
					ObjectMapper mapper = new ObjectMapper();
					mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
					mapper.setSerializationInclusion(JsonInclude.Include.NON_EMPTY);
					return new ResponseEntity<>(mapper.writeValueAsString(dataObjectList), HttpStatus.OK);

				}

			}
		}
		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);
	}

	/**
	 * @throws JsonProcessingException get collection
	 * 
	 * @param includeAcl @param list @throws
	 */
	@GetMapping(value = "/collection/**", produces = { MediaType.APPLICATION_JSON_VALUE,
			MediaType.APPLICATION_OCTET_STREAM_VALUE })
	public ResponseEntity<?> getCollection(@RequestHeader HttpHeaders headers, HttpSession session,
			HttpServletResponse response, HttpServletRequest request, @RequestParam(required = false) Boolean list,
			@RequestParam(required = false) Boolean includeAcl) throws DoeWebException, JsonProcessingException {

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
				includeAcl);

		HpcCollectionDTO result = collectionDto.getCollections().get(0);
		String accessGrp = getAttributeValue("access_group", result.getMetadataEntries().getSelfMetadataEntries());

		if ("public".equalsIgnoreCase(accessGrp)
				|| Boolean.TRUE.equals(hasCollectionPermissions(session, doeLogin, path, collectionDto))) {
			isPermissions = true;
		}

		if (Boolean.TRUE.equals(isPermissions) && CollectionUtils.isNotEmpty(collectionDto.getCollections())) {
			ObjectMapper mapper = new ObjectMapper();
			mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
			mapper.setSerializationInclusion(JsonInclude.Include.NON_EMPTY);
			return new ResponseEntity<>(mapper.writeValueAsString(collectionDto), HttpStatus.OK);

		}
		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);

	}

	/**
	 * register collection
	 * 
	 * @param collectionRegistration
	 */
	@PutMapping(value = "/collection/**")
	public Integer registerCollection(@RequestHeader HttpHeaders headers, HttpSession session,
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
				String basePathTrimmed = basePath.substring(basePath.indexOf('/') + 1, basePath.length());
				if (!parentPath.equalsIgnoreCase(basePathTrimmed)) {
					HpcCollectionListDTO parentCollectionDto = DoeClientUtil.getCollection(authToken, serviceURL,
							parentPath, true);
					Boolean isValidPermissions = hasCollectionPermissions(session, doeLogin, parentPath,
							parentCollectionDto);
					if (Boolean.FALSE.equals(isValidPermissions)) {
						throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);
					}
				}
			} else {
				throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);
			}

			Integer responseStatus = DoeClientUtil.updateCollection(authToken, serviceURL, collectionRegistration,
					path);
			log.info("response Status : " + responseStatus);
			if (responseStatus == 200 || responseStatus == 201) {

				HpcCollectionListDTO collections = DoeClientUtil.getCollection(authToken, serviceURL, path, false);
				if (collections != null && collections.getCollections() != null
						&& !CollectionUtils.isEmpty(collections.getCollections())) {
					HpcCollectionDTO collection = collections.getCollections().get(0);
					try {
						// save owner collection permissions in MoDaC DB
						metaDataPermissionService.savePermissionsList(doeLogin, null,
								collection.getCollection().getCollectionId(), path);

						// store the access_group metadata in MoDaC DB
						HpcMetadataEntry selectedEntry = collection.getMetadataEntries().getSelfMetadataEntries()
								.stream().filter(e -> e.getAttribute().equalsIgnoreCase("access_group")).findAny()
								.orElse(null);
						if (selectedEntry != null) {
							accessGroupsService.saveAccessGroups(collection.getCollection().getCollectionId(), path,
									selectedEntry.getValue(), doeLogin);
						}

						// store the auditing info
						AuditingModel audit = new AuditingModel();
						audit.setName(doeLogin);
						audit.setOperation("register collection");
						audit.setStartTime(new Date());
						audit.setPath(path);
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
	 * @param dataObject
	 * @param dataObjectRegistration
	 * 
	 */
	@PutMapping(value = "/v2/dataObject/**")
	public Integer registerDataObject(@RequestHeader HttpHeaders headers, HttpSession session,
			HttpServletResponse response, HttpServletRequest request,
			@RequestPart("dataObjectRegistration") @Valid gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationRequestDTO dataObjectRegistration,
			@RequestBody(required = false) @Valid MultipartFile dataObject) throws DoeWebException {

		log.info("register data files: " + dataObjectRegistration);
		log.info("multipart file: " + dataObject);
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
						parentPath, true);
				Boolean isValidPermissions = hasCollectionPermissions(session, doeLogin, parentPath,
						parentCollectionDto);
				if (Boolean.FALSE.equals(isValidPermissions)) {
					throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);
				}
			}

			Response restResponse = DoeClientUtil.registerDatafile(authToken, dataObject, dataObjectAsyncServiceURL,
					dataObjectRegistration, path);
			if (restResponse.getStatus() == 200 || restResponse.getStatus() == 201) {
				// store the auditing info
				AuditingModel audit = new AuditingModel();
				audit.setName(doeLogin);
				audit.setOperation("Upload Single File");
				audit.setStartTime(new Date());
				audit.setPath(path);
				auditingService.saveAuditInfo(audit);

				return restResponse.getStatus();
			}
		}

		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);
	}

	@PutMapping(value = "/v2/registration")
	public ResponseEntity<?> bulkRegistration(@RequestHeader HttpHeaders headers, HttpSession session,
			HttpServletResponse response, HttpServletRequest request,
			@RequestBody @Valid gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO bulkDataObjectRegistrationRequest)
			throws DoeWebException, JsonProcessingException {

		log.info("bulk registration: " + bulkDataObjectRegistrationRequest);
		log.info("Headers: {}", headers);

		String authToken = (String) session.getAttribute("writeAccessUserToken");
		log.info("authToken: " + authToken);

		if (authToken == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}
		Boolean isValidPermissions = false;
		// verifying MoDaC authentication.
		String doeLogin = (String) session.getAttribute("doeLogin");
		log.info("doeLogin: " + doeLogin);
		if (doeLogin == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		if (StringUtils.isNotEmpty(doeLogin) && Boolean.TRUE.equals(isUploader(doeLogin))) {

			List<HpcDirectoryScanRegistrationItemDTO> directoryScanRegistrationItems = bulkDataObjectRegistrationRequest
					.getDirectoryScanRegistrationItems();
			String path = CollectionUtils.isNotEmpty(directoryScanRegistrationItems)
					? directoryScanRegistrationItems.get(0).getBasePath()
					: "";

			if (StringUtils.isNotEmpty(path)) {
				HpcCollectionListDTO parentCollectionDto = DoeClientUtil.getCollection(authToken, serviceURL, path,
						true);
				isValidPermissions = hasCollectionPermissions(session, doeLogin, path, parentCollectionDto);
				if (Boolean.FALSE.equals(isValidPermissions)) {
					throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);
				}
			}

			// loop through dataObjectRegistrationItems
			List<HpcDataObjectRegistrationItemDTO> dataObjectRegistrationItems = bulkDataObjectRegistrationRequest
					.getDataObjectRegistrationItems();
			if (CollectionUtils.isNotEmpty(dataObjectRegistrationItems)) {
				for (HpcDataObjectRegistrationItemDTO item : dataObjectRegistrationItems) {
					String dataFilePath = item.getPath();
					String assetPath = dataFilePath.substring(0, dataFilePath.lastIndexOf('/'));
					if (Boolean.TRUE.equals(item.getCreateParentCollections())) {
						// this is for new collection registration, check permissions for the parent
						// path of the asset
						String parentPath = assetPath.substring(0, assetPath.lastIndexOf('/'));
						HpcCollectionListDTO parentCollectionDto = DoeClientUtil.getCollection(authToken, serviceURL,
								parentPath, true);
						isValidPermissions = hasCollectionPermissions(session, doeLogin, parentPath,
								parentCollectionDto);
					} else {
						// this is for data objects upload, check permissions at asset level
						HpcCollectionListDTO parentCollectionDto = DoeClientUtil.getCollection(authToken, serviceURL,
								assetPath, true);
						isValidPermissions = hasCollectionPermissions(session, doeLogin, assetPath,
								parentCollectionDto);
					}
					if (Boolean.FALSE.equals(isValidPermissions)) {
						throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);
					}
				}
			}

			if (Boolean.TRUE.equals(isValidPermissions)) {
				HpcBulkDataObjectRegistrationResponseDTO responseDTO = DoeClientUtil.registerBulkDatafiles(authToken,
						bulkRegistrationURL, bulkDataObjectRegistrationRequest);
				if (responseDTO != null) {
					String taskId = responseDTO.getTaskId();
					if (StringUtils.isNotEmpty(taskId)) {

						// get the paths for new collection registration and save in modac
						List<String> pathsList = new ArrayList<String>();

						if (CollectionUtils.isNotEmpty(dataObjectRegistrationItems)) {
							for (HpcDataObjectRegistrationItemDTO item : dataObjectRegistrationItems) {
								if (Boolean.TRUE.equals(item.getCreateParentCollections())) {
									item.getParentCollectionsBulkMetadataEntries().getPathsMetadataEntries().stream()
											.forEach(e -> pathsList.add(e.getPath()));
								}
							}
						}

						if (CollectionUtils.isNotEmpty(directoryScanRegistrationItems)) {
							for (HpcDirectoryScanRegistrationItemDTO item : directoryScanRegistrationItems) {
								item.getBulkMetadataEntries().getPathsMetadataEntries().stream()
										.forEach(e -> pathsList.add(e.getPath()));
							}
						}

						try {

							// save the task info
							taskManagerService.saveTransfer(taskId, "Upload", "async", null, doeLogin,
									CollectionUtils.isNotEmpty(pathsList) ? String.join(",", pathsList) : null);

							// store the auditing info
							AuditingModel audit = new AuditingModel();
							audit.setName(doeLogin);
							audit.setOperation("Upload");
							audit.setStartTime(new Date());
							audit.setTransferType("Bulk Registration");
							audit.setTaskId(taskId);
							auditingService.saveAuditInfo(audit);

						} catch (Exception e) {
							log.error("error in save" + e.getMessage());
						}
						if (CollectionUtils.isNotEmpty(pathsList)) {
							for (String collectionPath : pathsList) {
								try {
									HpcCollectionListDTO collections = DoeClientUtil.getCollection(authToken,
											serviceURL, collectionPath, false);
									if (collections != null && collections.getCollections() != null
											&& !CollectionUtils.isEmpty(collections.getCollections())) {
										HpcCollectionDTO collection = collections.getCollections().get(0);

										// save owner collection permissions in MoDaC DB
										metaDataPermissionService.savePermissionsList(doeLogin, null,
												collection.getCollection().getCollectionId(), collectionPath);

										// store the access_group metadata in MoDaC DB
										HpcMetadataEntry selectedEntry = collection.getMetadataEntries()
												.getSelfMetadataEntries().stream()
												.filter(e -> e.getAttribute().equalsIgnoreCase("access_group"))
												.findAny().orElse(null);
										if (selectedEntry != null
												&& !"public".equalsIgnoreCase(selectedEntry.getValue())) {
											accessGroupsService.saveAccessGroups(
													collection.getCollection().getCollectionId(), collectionPath,
													selectedEntry.getValue(), doeLogin);
										}
									}
								} catch (Exception e) {
									log.error("error in bulk registration" + e.getMessage());
									// send email to admin list if there is a failure in saving the collection to
									// modac database
									mailService.sendCollectionRegistationFailure(doeLogin, collectionPath, e, taskId);
								}
							}
						}
					}

					ObjectMapper mapper = new ObjectMapper();
					mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
					mapper.setSerializationInclusion(JsonInclude.Include.NON_EMPTY);
					return new ResponseEntity<>(mapper.writeValueAsString(responseDTO), HttpStatus.OK);

				}

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
	public ResponseEntity<?> queryCollections(@RequestHeader HttpHeaders headers, HttpSession session,
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
				compoundMetadataQuery);

		if (restResponse.getStatus() == 200 || restResponse.getStatus() == 201) {
			MappingJsonFactory factory = new MappingJsonFactory();
			JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
			ObjectMapper mapper = new ObjectMapper();
			mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
			mapper.setSerializationInclusion(JsonInclude.Include.NON_EMPTY);
			mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
			return new ResponseEntity<>(mapper.writeValueAsString(parser.readValueAs(HpcCollectionListDTO.class)),
					HttpStatus.OK);

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
	public ResponseEntity<?> queryDataObjects(@RequestHeader HttpHeaders headers, HttpSession session,
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
				returnParent, compoundMetadataQuery, null);

		if (restResponse.getStatus() == 200 || restResponse.getStatus() == 201) {
			MappingJsonFactory factory = new MappingJsonFactory();
			JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
			ObjectMapper mapper = new ObjectMapper();
			mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
			mapper.setSerializationInclusion(JsonInclude.Include.NON_EMPTY);

			if (Boolean.TRUE.equals(returnParent)) {
				HpcCollectionListDTO dataObjects = parser.readValueAs(HpcCollectionListDTO.class);
				return new ResponseEntity<>(mapper.writeValueAsString(dataObjects), HttpStatus.OK);
			} else {
				HpcDataObjectListDTO dataObjects = parser.readValueAs(HpcDataObjectListDTO.class);
				return new ResponseEntity<>(mapper.writeValueAsString(dataObjects), HttpStatus.OK);
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
	public ResponseEntity<?> authenticate(@RequestHeader HttpHeaders headers, HttpSession session,
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

	@GetMapping(value = "/v2/registration/**", produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<?> getDataObjectsRegistrationStatus(@RequestHeader HttpHeaders headers, HttpSession session,
			HttpServletResponse response, HttpServletRequest request) throws DoeWebException {

		log.info("get registration status by task Id.");
		log.info("Headers: {}", headers);
		String taskId = request.getRequestURI().split(request.getContextPath() + "/v2/registration/")[1];
		log.info("taskId: " + taskId);
		String doeLogin = (String) session.getAttribute("doeLogin");
		log.info("doeLogin: " + doeLogin);
		String authToken = (String) session.getAttribute("hpcUserToken");

		if (authToken == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		try {
			if (StringUtils.isNotEmpty(taskId) && StringUtils.isNotEmpty(doeLogin)) {
				UriComponentsBuilder ucBuilder1 = UriComponentsBuilder.fromHttpUrl(bulkRegistrationURL)
						.path("/{dme-archive-path}");
				final String serviceURL = ucBuilder1.buildAndExpand(taskId).encode().toUri().toURL().toExternalForm();

				WebClient client = DoeClientUtil.getWebClient(serviceURL);
				client.header("Authorization", "Bearer " + authToken);
				Response restResponse = client.invoke("GET", null);

				if (restResponse.getStatus() == 200) {
					ObjectMapper mapper = new ObjectMapper();
					mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
					mapper.setSerializationInclusion(JsonInclude.Include.NON_EMPTY);
					MappingJsonFactory factory = new MappingJsonFactory(mapper);
					JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
					HpcBulkDataObjectRegistrationStatusDTO dto = parser
							.readValueAs(HpcBulkDataObjectRegistrationStatusDTO.class);

					return new ResponseEntity<>(mapper.writeValueAsString(dto), HttpStatus.OK);
				}
			}
		} catch (Exception e) {
			log.error("Error in get data object registration status: " + e.getMessage());
		}
		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);
	}

	@GetMapping(value = "/collection/download/**", produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<?> getCollectionDownloadStatus(@RequestHeader HttpHeaders headers, HttpSession session,
			HttpServletResponse response, HttpServletRequest request) throws DoeWebException {

		log.info("get download status for collection by task Id.");
		log.info("Headers: {}", headers);
		String taskId = request.getRequestURI().split(request.getContextPath() + "/collection/download/")[1];
		log.info("taskId: " + taskId);
		String doeLogin = (String) session.getAttribute("doeLogin");
		log.info("doeLogin: " + doeLogin);
		String authToken = (String) session.getAttribute("hpcUserToken");

		if (authToken == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		try {
			if (StringUtils.isNotEmpty(taskId) && StringUtils.isNotEmpty(doeLogin)) {

				UriComponentsBuilder ucBuilder1 = UriComponentsBuilder.fromHttpUrl(serviceURL)
						.path("/download/{dme-archive-path}");
				final String serviceURL = ucBuilder1.buildAndExpand(taskId).encode().toUri().toURL().toExternalForm();

				WebClient client = DoeClientUtil.getWebClient(serviceURL);
				client.header("Authorization", "Bearer " + authToken);
				Response restResponse = client.invoke("GET", null);

				if (restResponse.getStatus() == 200) {
					ObjectMapper mapper = new ObjectMapper();
					mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
					mapper.setSerializationInclusion(JsonInclude.Include.NON_EMPTY);
					MappingJsonFactory factory = new MappingJsonFactory(mapper);
					JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
					HpcCollectionDownloadStatusDTO dto = parser.readValueAs(HpcCollectionDownloadStatusDTO.class);

					return new ResponseEntity<>(mapper.writeValueAsString(dto), HttpStatus.OK);
				}
			}
		} catch (Exception e) {
			log.error("Error in get collection download status: " + e.getMessage());
		}
		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);

	}

	@GetMapping(value = "/dataObject/download/**", produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<?> getDataObjectDownloadStatus(@RequestHeader HttpHeaders headers, HttpSession session,
			HttpServletResponse response, HttpServletRequest request) throws DoeWebException {

		log.info("get download status for data object by task Id.");
		log.info("Headers: {}", headers);
		String taskId = request.getRequestURI().split(request.getContextPath() + "/dataObject/download/")[1];
		log.info("taskId: " + taskId);
		String doeLogin = (String) session.getAttribute("doeLogin");
		log.info("doeLogin: " + doeLogin);
		String authToken = (String) session.getAttribute("hpcUserToken");

		if (authToken == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		try {
			if (StringUtils.isNotEmpty(taskId) && StringUtils.isNotEmpty(doeLogin)) {
				UriComponentsBuilder ucBuilder1 = UriComponentsBuilder.fromHttpUrl(dataObjectServiceURL)
						.path("/download/{dme-archive-path}");
				final String serviceURL = ucBuilder1.buildAndExpand(taskId).encode().toUri().toURL().toExternalForm();

				WebClient client = DoeClientUtil.getWebClient(serviceURL);
				client.header("Authorization", "Bearer " + authToken);
				Response restResponse = client.invoke("GET", null);

				if (restResponse.getStatus() == 200) {
					ObjectMapper mapper = new ObjectMapper();
					mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
					mapper.setSerializationInclusion(JsonInclude.Include.NON_EMPTY);
					MappingJsonFactory factory = new MappingJsonFactory(mapper);
					JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
					HpcDataObjectDownloadStatusDTO dto = parser.readValueAs(HpcDataObjectDownloadStatusDTO.class);

					return new ResponseEntity<>(mapper.writeValueAsString(dto), HttpStatus.OK);
				}
			}
		} catch (Exception e) {
			log.error("Error in get dataObject download status: " + e.getMessage());
		}
		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);

	}

	/**
	 * get data object or collection download status
	 * 
	 * @param headers
	 * @param session
	 * @param response
	 * @param request
	 * @return
	 * @throws DoeWebException
	 */
	@GetMapping(value = "/download/**", produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<?> getDataObjectsOrCollectionsDownloadStatus(@RequestHeader HttpHeaders headers,
			HttpSession session, HttpServletResponse response, HttpServletRequest request) throws DoeWebException {

		log.info("get status for data object or collection list by task Id.");
		log.info("Headers: {}", headers);
		String taskId = request.getRequestURI().split(request.getContextPath() + "/download/")[1];
		log.info("taskId: " + taskId);
		String doeLogin = (String) session.getAttribute("doeLogin");
		log.info("doeLogin: " + doeLogin);
		String authToken = (String) session.getAttribute("hpcUserToken");

		if (authToken == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		try {
			if (StringUtils.isNotEmpty(taskId) && StringUtils.isNotEmpty(doeLogin)) {
				UriComponentsBuilder ucBuilder1 = UriComponentsBuilder.fromHttpUrl(queryServiceURL)
						.path("/{dme-archive-path}");
				final String serviceURL = ucBuilder1.buildAndExpand(taskId).encode().toUri().toURL().toExternalForm();

				WebClient client = DoeClientUtil.getWebClient(serviceURL);
				client.header("Authorization", "Bearer " + authToken);
				Response restResponse = client.invoke("GET", null);

				if (restResponse.getStatus() == 200) {
					ObjectMapper mapper = new ObjectMapper();
					mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
					mapper.setSerializationInclusion(JsonInclude.Include.NON_EMPTY);
					MappingJsonFactory factory = new MappingJsonFactory(mapper);
					JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
					HpcCollectionDownloadStatusDTO dto = parser.readValueAs(HpcCollectionDownloadStatusDTO.class);

					return new ResponseEntity<>(mapper.writeValueAsString(dto), HttpStatus.OK);
				}
			}
		} catch (Exception e) {
			log.error("Error in get getDataObjects or collections download status: " + e.getMessage());
		}
		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);

	}

	/**
	 * 
	 * get evaluation status
	 * 
	 * @param headers
	 * @param session
	 * @param response
	 * @param request
	 * @return
	 * @throws DoeWebException
	 * @throws IOException
	 */

	@GetMapping(value = "/model/evaluate/**", produces = { MediaType.APPLICATION_JSON_VALUE,
			MediaType.APPLICATION_OCTET_STREAM_VALUE })
	public ResponseEntity<?> getStatusByTaskId(@RequestHeader HttpHeaders headers, HttpSession session,
			HttpServletResponse response, HttpServletRequest request) throws DoeWebException, JsonProcessingException {

		log.info("get model evaluate status by task Id.");
		log.info("Headers: {}", headers);
		String taskId = request.getRequestURI().split(request.getContextPath() + "/model/evaluate/")[1];
		log.info("taskId: " + taskId);
		String authToken = (String) session.getAttribute("hpcUserToken");

		if (authToken == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		String doeLogin = (String) session.getAttribute("doeLogin");
		log.info("doeLogin: " + doeLogin);

		if (doeLogin == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		try {
			if (StringUtils.isNotEmpty(taskId)) {
				ModelStatus responseDTO = new ModelStatus();
				InferencingTask task = inferencingTaskService.getInferenceByTaskId(taskId);
				responseDTO.setInputFile(task.getTestDataSetPath());
				responseDTO.setStartDate(format.format(task.getStartDate()));
				responseDTO.setOutcomeFile(task.getOutcomeFilePath());
				if (task != null) {
					if ("NOTSTARTED".equalsIgnoreCase(task.getStatus())) {
						responseDTO.setStatus("Not Started");
					} else if ("INPROGRESS".equalsIgnoreCase(task.getStatus())) {
						responseDTO.setStatus("In Progress");
						if (task.getDmeTaskId() != null) {
							UriComponentsBuilder ucBuilder1 = UriComponentsBuilder.fromHttpUrl(bulkRegistrationURL)
									.path("/{dme-archive-path}");
							final String serviceURL = ucBuilder1.buildAndExpand(task.getDmeTaskId()).encode().toUri()
									.toURL().toExternalForm();

							WebClient client = DoeClientUtil.getWebClient(serviceURL);
							client.header("Authorization", "Bearer " + authToken);
							Response restResponse = client.invoke("GET", null);

							if (restResponse.getStatus() == 200) {
								ObjectMapper mapper = new ObjectMapper();
								MappingJsonFactory factory = new MappingJsonFactory(mapper);
								JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
								HpcBulkDataObjectRegistrationStatusDTO dto = parser
										.readValueAs(HpcBulkDataObjectRegistrationStatusDTO.class);
								if (dto != null) {
									if (dto.getTask().getCompleted() != null) {
										task.setStatus("COMPLETED");
										task.setCompletedDate(dto.getTask().getCompleted().getTime());
										inferencingTaskService.save(task);

										responseDTO.setStatus("Completed");
										responseDTO.setCompletedDate(
												format.format(dto.getTask().getCompleted().getTime()));
										responseDTO.setResultPath(task.getResultPath());
									}
								}
							}
						}

					} else if ("COMPLETED".equalsIgnoreCase(task.getStatus())) {
						responseDTO.setStatus("Completed");
						responseDTO.setCompletedDate(format.format(task.getCompletedDate()));
						responseDTO.setResultPath(task.getResultPath());
					} else if ("FAILURE".equalsIgnoreCase(task.getStatus())) {
						responseDTO.setStatus("Failed");
						responseDTO.setFailureMsg(task.getErrorMessage());
					}

					ObjectMapper mapper = new ObjectMapper();
					mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
					mapper.setSerializationInclusion(JsonInclude.Include.NON_EMPTY);
					return new ResponseEntity<>(mapper.writeValueAsString(responseDTO), HttpStatus.OK);
				}
			}
		} catch (Exception e) {
			log.error("Error in get status by task Id" + e.getMessage());
		}

		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);

	}

	@PostMapping(value = "/models/evaluate/**", consumes = { MediaType.APPLICATION_XML_VALUE,
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_XML_VALUE,
					MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<?> performModelEvaluationForReferenceDatasets(@RequestHeader HttpHeaders headers,
			HttpSession session, HttpServletResponse response, HttpServletRequest request,
			@RequestBody @Valid ModelPath modelPaths) throws DoeWebException, IOException {

		log.info("model evaluate using reference datasets on one or more models.");
		log.info("Headers: {}", headers);
		String doeLogin = (String) session.getAttribute("doeLogin");
		log.info("doeLogin: " + doeLogin);
		String authToken = (String) session.getAttribute("hpcUserToken");
		String referenceDatasetPath = request.getRequestURI().split(request.getContextPath() + "/models/evaluate")[1];
		log.info("referenceDatasetPath: " + referenceDatasetPath);

		if (authToken == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		if (doeLogin == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		if (StringUtils.isNotEmpty(doeLogin) && Boolean.TRUE.equals(isUploader(doeLogin))
				&& CollectionUtils.isNotEmpty(modelPaths.getModelPaths())) {

			EvaluationResponse evaluation = new EvaluationResponse();
			List<Path> modelPathsList = modelPaths.getModelPaths();

			// perform model evaluation on a reference dataset against multiple models
			List<String> taskIdList = new ArrayList<String>();
			HpcCollectionListDTO collectionDto = DoeClientUtil.getCollection(authToken, serviceURL,
					referenceDatasetPath, true);
			HpcCollectionDTO result = collectionDto.getCollections().get(0);

			List<HpcCollectionListingEntry> dataObjectsList = result.getCollection().getDataObjects();

			String resultFileName = getAttributeValue("outcome_file_name",
					result.getMetadataEntries().getSelfMetadataEntries(), null);

			if (StringUtils.isEmpty(resultFileName)) {

				throw new DoeWebException("Outcome file name metadata not found in reference dataset provided.",
						HttpServletResponse.SC_BAD_REQUEST);
			}

			for (Path applicableModelName : modelPathsList) {
				try {
					InferencingTaskModel inference = new InferencingTaskModel();
					inference.setAssetPath(referenceDatasetPath);
					inference.setUserId(doeLogin);
					dataObjectsList.stream().forEach(e -> {
						String path = e.getPath();
						String name = path.substring(path.lastIndexOf('/') + 1);
						/*
						 * check for outcome file name and input dataset paths
						 */
						if (StringUtils.isNotEmpty(name) && name.contains(resultFileName)) {
							inference.setOutcomeFileName(name);
							inference.setOutcomeFilePath(path);

						} else {
							inference.setTestInputPath(path);
						}
					});

					if (StringUtils.isEmpty(inference.getOutcomeFilePath())) {
						throw new DoeWebException("Outcome file not found in reference dataset provided.",
								HttpServletResponse.SC_BAD_REQUEST);
					}
					if (StringUtils.isEmpty(inference.getTestInputPath())) {

						throw new DoeWebException("Reference dataset file not found for : " + referenceDatasetPath,
								HttpServletResponse.SC_BAD_REQUEST);
					}

					String taskId = performModelEvaluation(inference, referenceDatasetPath,
							applicableModelName.getPath(), session, authToken);
					// save the inferencing task

					inferencingTaskService.saveInferenceTask(inference);
					taskIdList.add(taskId);
				} catch (Exception e) {
					log.error("Exception in performing model analysis: " + e);
					throw new DoeWebException("Exception in performing model evaluation: " + e);
				}
			}
			evaluation.setMessage("Evaluate task(s) submitted.");
			evaluation.setTaskId(taskIdList);
			ObjectMapper mapper = new ObjectMapper();
			mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
			mapper.setSerializationInclusion(JsonInclude.Include.NON_EMPTY);
			return new ResponseEntity<>(mapper.writeValueAsString(evaluation), HttpStatus.OK);
		}

		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);

	}

	@PostMapping(value = "/model/evaluate/**")
	public ResponseEntity<?> performModelEvaluationForDatasets(@RequestHeader HttpHeaders headers, HttpSession session,
			HttpServletResponse response, HttpServletRequest request, @RequestParam(value = "type") String type,
			@RequestPart(name = "referenceDataset", required = false) @Valid ReferenceDataset referenceDataset,
			@RequestBody(required = false) @Valid MultipartFile inputFile,
			@RequestBody(required = false) @Valid MultipartFile outcomeFile) throws DoeWebException, IOException {

		log.info("model evaluate for input file");
		log.info("Headers: {}", headers);
		String doeLogin = (String) session.getAttribute("doeLogin");
		log.info("doeLogin: " + doeLogin);
		String modelPath = request.getRequestURI().split(request.getContextPath() + "/model/evaluate")[1];
		log.info("modelPath: " + modelPath);
		log.info("type of input" + type);
		String authToken = (String) session.getAttribute("hpcUserToken");
		log.info("authToken: " + authToken);

		if (authToken == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		if (doeLogin == null) {
			throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
		}

		if (StringUtils.isNotEmpty(doeLogin) && Boolean.TRUE.equals(isUploader(doeLogin))
				&& StringUtils.isNotEmpty(modelPath)) {

			EvaluationResponse evaluation = new EvaluationResponse();
			List<String> taskIdList = new ArrayList<String>();
			String trainedModelPath;

			// get the model path from the asset
			HpcCollectionListDTO assetDto = DoeClientUtil.getCollection(authToken, serviceURL, modelPath, true);
			HpcCollectionDTO assetResult = assetDto.getCollections().get(0);

			List<HpcCollectionListingEntry> dataObjectsList = assetResult.getCollection().getDataObjects();

			Optional<HpcCollectionListingEntry> entry = dataObjectsList.stream()
					.filter(e -> e != null && e.getPath().contains(".h5")).findFirst();

			if (entry != null && entry.isPresent()) {

				trainedModelPath = entry.get().getPath();

			} else {
				throw new DoeWebException("No trained model file found in the asset provided.",
						HttpServletResponse.SC_BAD_REQUEST);
			}

			if ("ReferenceDataset".equalsIgnoreCase(type) && referenceDataset != null
					&& CollectionUtils.isNotEmpty(referenceDataset.getReferenceDatasetPaths())) {

				// perform generate predictions using a model and one or more reference datasets

				List<Path> referenceDatasetPaths = referenceDataset.getReferenceDatasetPaths();

				for (Path referenceDatasetPath : referenceDatasetPaths) {

					InferencingTaskModel inferenceDataset = new InferencingTaskModel();

					String taskId = performGeneratePredictions(inferenceDataset, referenceDatasetPath.getPath(),
							session, authToken, modelPath);

					inferenceDataset.setUserId(doeLogin);
					inferenceDataset.setModelPath(trainedModelPath);
					inferencingTaskService.saveInferenceTask(inferenceDataset);
					taskIdList.add(taskId);
				}

			} else if (("GDCManifest".equalsIgnoreCase(type) || "UserInput".equalsIgnoreCase(type))
					&& inputFile != null) {
				InferencingTaskModel inference = new InferencingTaskModel();

				inference.setModelPath(trainedModelPath);

				// create a modac task Id
				String taskId = UUID.randomUUID().toString();

				// create a file name for y_pred file and append to the asset Path
				String resultPath = modelPath + "/y_pred_" + taskId + ".csv";

				// check if the file name is already used for inferencing for the same user and
				// same model path and is not in failed status
				if (Boolean.TRUE.equals(inferencingTaskService.checkifFileExistsForUser(doeLogin, modelPath,
						inputFile.getOriginalFilename()))) {

					throw new DoeWebException("Input file name already exists", HttpServletResponse.SC_BAD_REQUEST);
				}

				if (outcomeFile != null) {

					inference.setOutcomeFileName(outcomeFile.getOriginalFilename());
					Files.copy(outcomeFile.getInputStream(), Paths.get(uploadPath + outcomeFile.getOriginalFilename()),
							StandardCopyOption.REPLACE_EXISTING);
				}

				// save the inferencing task
				inference.setIsReferenceAsset(Boolean.FALSE);
				inference.setUploadFrom(type.equalsIgnoreCase("GDCManifest") ? "gdcData" : "inputFile");
				inference.setTaskId(taskId);
				inference.setResultPath(resultPath);
				inference.setAssetPath(modelPath);
				inference.setTestInputPath(modelPath + "/" + inputFile.getOriginalFilename());
				inference.setUserId(doeLogin);

				inferencingTaskService.saveInferenceTask(inference);

				// copy the test dataset file to mount
				Files.copy(inputFile.getInputStream(), Paths.get(uploadPath + inputFile.getOriginalFilename()),
						StandardCopyOption.REPLACE_EXISTING);
				taskIdList.add(taskId);

			} else {
				throw new DoeWebException("Invalid Request", HttpServletResponse.SC_BAD_REQUEST);
			}
			evaluation.setMessage("Evaluate task(s) submitted.");
			evaluation.setTaskId(taskIdList);
			ObjectMapper mapper = new ObjectMapper();
			mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
			mapper.setSerializationInclusion(JsonInclude.Include.NON_EMPTY);
			return new ResponseEntity<>(mapper.writeValueAsString(evaluation), HttpStatus.OK);

		}
		throw new DoeWebException("Invalid Permissions", HttpServletResponse.SC_BAD_REQUEST);

	}

	@SuppressWarnings("unchecked")
	private Boolean hasCollectionPermissions(HttpSession session, String loggedOnUser, String parentPath,
			HpcCollectionListDTO parentCollectionDto) {

		log.info("has collection permssions for " + loggedOnUser + " path: " + parentPath);
		Integer parentCollectionId = null;
		List<KeyValueBean> keyValueBeanResults = new ArrayList<>();
		if (!StringUtils.isEmpty(loggedOnUser)) {
			// get logged on user prog list to keyvaluebean list
			keyValueBeanResults = (List<KeyValueBean>) getMetaDataPermissionsList(session, loggedOnUser).getBody();

		}

		// verify collection path permissions
		if (!basePath.equalsIgnoreCase(parentPath) && parentCollectionDto != null
				&& parentCollectionDto.getCollections() != null
				&& !CollectionUtils.isEmpty(parentCollectionDto.getCollections())) {
			HpcCollectionDTO collection = parentCollectionDto.getCollections().get(0);

			HpcMetadataEntry assetCollection = collection.getMetadataEntries().getParentMetadataEntries().stream()
					.filter(e -> e.getAttribute().equalsIgnoreCase("collection_type")
							&& e.getLevelLabel().equalsIgnoreCase("Asset"))
					.findAny().orElse(null);

			if (assetCollection != null) {
				parentCollectionId = assetCollection.getCollectionId();
			} else {
				parentCollectionId = collection.getCollection().getCollectionId();
			}

			String role = getPermissionRole(loggedOnUser, parentCollectionId, keyValueBeanResults);
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
