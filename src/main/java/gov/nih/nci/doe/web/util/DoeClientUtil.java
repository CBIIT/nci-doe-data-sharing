package gov.nih.nci.doe.web.util;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.MappingJsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.introspect.AnnotationIntrospectorPair;
import com.fasterxml.jackson.databind.introspect.JacksonAnnotationIntrospector;
import com.fasterxml.jackson.databind.type.TypeFactory;
import com.fasterxml.jackson.module.jaxb.JaxbAnnotationIntrospector;
import com.fasterxml.jackson.annotation.JsonInclude;
import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.AjaxResponseBody;
import gov.nih.nci.doe.web.model.GoogleResponse;
import gov.nih.nci.hpc.domain.datamanagement.HpcPermission;
import gov.nih.nci.hpc.domain.datamanagement.HpcPermissionForCollection;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectDownloadRequestDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcBulkDataObjectDownloadResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationStatusDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDownloadResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDownloadStatusDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionRegistrationDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementModelDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementRulesDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectDownloadResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectDownloadStatusDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationRequestDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDocDataManagementRulesDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDownloadRetryRequestDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcDownloadRequestDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcRegistrationSummaryDTO;
import gov.nih.nci.hpc.dto.datasearch.HpcCompoundMetadataQueryDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDownloadSummaryDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcUserPermsForCollectionsDTO;
import gov.nih.nci.hpc.dto.error.HpcExceptionDTO;
import gov.nih.nci.hpc.dto.security.HpcAuthenticationResponseDTO;

import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;
import javax.net.ssl.TrustManager;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.ws.rs.core.Response;
import javax.xml.bind.DatatypeConverter;

import org.apache.commons.collections.CollectionUtils;
import org.apache.cxf.configuration.jsse.TLSClientParameters;
import org.apache.cxf.jaxrs.client.ClientConfiguration;
import org.apache.cxf.jaxrs.client.WebClient;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.ContentDisposition;
import org.apache.cxf.jaxrs.ext.multipart.MultipartBody;
import org.apache.cxf.transport.http.HTTPConduit;
import org.codehaus.jackson.jaxrs.JacksonJsonProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.MediaType;
import org.springframework.util.MultiValueMap;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.util.UriComponentsBuilder;

public class DoeClientUtil {

	private static final Logger log = LoggerFactory.getLogger(DoeClientUtil.class);

	private DoeClientUtil() {

	}

	public static WebClient getWebClient(String url) {

		log.debug("get web client for url " + url);
		WebClient client = WebClient.create(url, Collections.singletonList(new JacksonJsonProvider()));

		ClientConfiguration clientConfig = WebClient.getConfig(client);
		clientConfig.getRequestContext().put("support.type.as.multipart", "true");
		configureWebClientConduit(clientConfig);

		return client;
	}

	private static void configureWebClientConduit(ClientConfiguration clientConfig) {
		HTTPConduit conduit = clientConfig.getHttpConduit();

		TLSClientParameters tlsParams = conduit.getTlsClientParameters();
		if (null == tlsParams) {
			conduit.setTlsClientParameters(new TLSClientParameters());
			tlsParams = conduit.getTlsClientParameters();
		}
		tlsParams.setDisableCNCheck(true);
		tlsParams.setTrustManagers(new TrustManager[] { new TrustAllX509TrustManager() });

		conduit.getClient().setReceiveTimeout(60000000);
		conduit.getClient().setConnectionTimeout(60000000);
	}

	public static String getBasePath(String authToken, String serviceURL, String parent, String sslCertPath,
			String sslCertPassword, HpcDataManagementModelDTO modelDTO) throws DoeWebException {
		HpcCollectionListDTO collectionListDTO = DoeClientUtil.getCollection(authToken, serviceURL, parent, true);
		if (collectionListDTO != null && collectionListDTO.getCollections() != null) {
			HpcCollectionDTO collection = collectionListDTO.getCollections().get(0);
			String configurationId = null;
			if (collection != null && collection.getMetadataEntries() != null
					&& collection.getMetadataEntries().getSelfMetadataEntries() != null) {
				for (HpcMetadataEntry entry : collection.getMetadataEntries().getSelfMetadataEntries())
					if (entry.getAttribute().equals("configuration_id")) {
						configurationId = entry.getValue();
						break;
					}
			}
			if (modelDTO != null) {
				for (HpcDocDataManagementRulesDTO rulesDTO : modelDTO.getDocRules()) {
					for (HpcDataManagementRulesDTO rule : rulesDTO.getRules()) {
						if ((configurationId != null && rule.getId().equals(configurationId))
								|| rule.getBasePath().equals(parent))
							return rule.getBasePath();
					}
				}
			}
		}
		return null;

	}

	public static String getAuthenticationToken(String userId, String passwd, String hpcServerURL)
			throws DoeWebException {

		log.debug("get authentication token for " + userId);

		WebClient client = DoeClientUtil.getWebClient(hpcServerURL);
		String token = DatatypeConverter.printBase64Binary((userId + ":" + passwd).getBytes());
		client.header("Authorization", "Basic " + token);
		Response restResponse = client.get();
		try {

			if (restResponse.getStatus() != 200) {
				log.error("Authentication failed");
				String errorMessage = getErrorMessage(restResponse);
				throw new DoeWebException(errorMessage, restResponse.getStatus());
			}
			MappingJsonFactory factory = new MappingJsonFactory();
			JsonParser parser;
			parser = factory.createParser((InputStream) restResponse.getEntity());
			HpcAuthenticationResponseDTO dto = parser.readValueAs(HpcAuthenticationResponseDTO.class);
			return dto.getToken();
		} catch (Exception e) {
			throw new DoeWebException("Failed to get auth token: " + e.getMessage());
		}
	}

	public static String getAuthenticationTokenSso(String userId, String smSession, String hpcServerURL)
			throws DoeWebException {

		WebClient client = DoeClientUtil.getWebClient(hpcServerURL);
		Response restResponse = client.header("SM_USER", userId).header("NIHSMSESSION", smSession).get();
		try {

			if (restResponse.getStatus() != 200) {
				log.error("Authentication failed");
				String errorMessage = getErrorMessage(restResponse);
				throw new DoeWebException(errorMessage, restResponse.getStatus());
			}
			MappingJsonFactory factory = new MappingJsonFactory();
			JsonParser parser;
			parser = factory.createParser((InputStream) restResponse.getEntity());
			HpcAuthenticationResponseDTO dto = parser.readValueAs(HpcAuthenticationResponseDTO.class);
			return dto.getToken();
		} catch (Exception e) {
			throw new DoeWebException("Failed to get auth token: " + e.getMessage());
		}
	}

	public static List<HpcDataManagementRulesDTO> getUserDOCManagementRules(HpcDataManagementModelDTO docModelDto,
			String userDoc) {
		if (docModelDto == null || docModelDto.getDocRules() == null)
			return null;

		for (HpcDocDataManagementRulesDTO docDTO : docModelDto.getDocRules()) {
			if (docDTO.getDoc().equals(userDoc))
				return docDTO.getRules();
		}
		return null;
	}

	public static HpcDataManagementRulesDTO getBasePathManagementRules(HpcDataManagementModelDTO docModelDto,
			String basePath) {
		if (docModelDto == null || docModelDto.getDocRules() == null || basePath == null)
			return null;

		for (HpcDocDataManagementRulesDTO docDTO : docModelDto.getDocRules()) {
			for (HpcDataManagementRulesDTO rules : docDTO.getRules()) {
				if (rules.getBasePath().equals(basePath))
					return rules;
			}
		}
		return null;
	}

	public static HpcDataManagementModelDTO getDOCModel(String token, String hpcModelURL) throws DoeWebException {

		WebClient client = DoeClientUtil.getWebClient(hpcModelURL);
		client.header("Authorization", "Bearer " + token);

		Response restResponse = client.get();

		if (restResponse == null || restResponse.getStatus() != 200)
			return null;
		MappingJsonFactory factory = new MappingJsonFactory();
		JsonParser parser;
		try {
			parser = factory.createParser((InputStream) restResponse.getEntity());
		} catch (Exception e) {
			throw new DoeWebException("Failed to get DOC Model due to: " + e.getMessage());
		}
		try {
			return parser.readValueAs(HpcDataManagementModelDTO.class);
		} catch (Exception e) {
			throw new DoeWebException("Failed to get DOC Model due to: " + e.getMessage());
		}
	}

	public static List<String> getDOCs(String token, String hpcModelURL, String hpcCertPath, String hpcCertPassword,
			HttpSession session) throws DoeWebException {
		List<String> docs = new ArrayList<String>();
		HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");
		if (modelDTO == null) {
			modelDTO = DoeClientUtil.getDOCModel(token, hpcModelURL);

		}
		if (modelDTO != null) {
			session.setAttribute("userDOCModel", modelDTO);
			for (HpcDocDataManagementRulesDTO docDTO : modelDTO.getDocRules())
				docs.add(docDTO.getDoc());
		}
		return docs;
	}

	public static String getBasePath(HttpServletRequest request) {
		String[] basePathValues = request.getParameterValues("basePath");
		String basePath = null;
		if (basePathValues == null || basePathValues.length == 0)
			basePath = (String) request.getAttribute("basePath");
		else
			basePath = basePathValues[0];
		if (basePath != null && basePath.equals("_select_null"))
			return null;
		return basePath;
	}

	public static HpcCollectionListDTO getCollection(String token, String hpcCollectionlURL, String path, boolean list)
			throws DoeWebException {
		return getCollection(token, hpcCollectionlURL, path, false, list, false);
	}

	public static HpcCollectionListDTO getCollection(String token, String hpcCollectionlURL, String path,
			boolean children, boolean list) throws DoeWebException {
		return getCollection(token, hpcCollectionlURL, path, children, list, false);
	}

	public static HpcCollectionListDTO getCollection(String token, String hpcCollectionlURL, String path,
			Boolean children, Boolean list, Boolean includeAcl) throws DoeWebException {
		try {
			final UriComponentsBuilder ucBuilder = UriComponentsBuilder.fromHttpUrl(hpcCollectionlURL)
					.path("/{dme-archive-path}");
			if (children != null && children) {
				ucBuilder.pathSegment("children");
			}
			if (list != null) {
				ucBuilder.queryParam("list", Boolean.toString(list));
			}
			if (includeAcl != null) {
				ucBuilder.queryParam("includeAcl", Boolean.toString(includeAcl));
			}
			final String serviceURL = ucBuilder.buildAndExpand(path).encode().toUri().toURL().toExternalForm();

			WebClient client = DoeClientUtil.getWebClient(serviceURL);
			client.header("Authorization", "Bearer " + token);
			Response restResponse = client.invoke("GET", null);
			if (restResponse.getStatus() == 200) {
				ObjectMapper mapper = new ObjectMapper();
				AnnotationIntrospectorPair intr = new AnnotationIntrospectorPair(
						new JaxbAnnotationIntrospector(TypeFactory.defaultInstance()),
						new JacksonAnnotationIntrospector());
				mapper.setAnnotationIntrospector(intr);
				mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

				MappingJsonFactory factory = new MappingJsonFactory(mapper);
				JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());

				return parser.readValueAs(HpcCollectionListDTO.class);
			} else {
				log.error("Failed to get collection! No READ access!");
				String errorMessage = getErrorMessage(restResponse);
				throw new DoeWebException(errorMessage, restResponse.getStatus());
			}

		} catch (Exception e) {
			throw new DoeWebException(e.getMessage());
		}
	}

	public static HpcDataObjectDTO getDatafiles(String token, String hpcDatafileURL, String path, boolean list)
			throws DoeWebException {
		return getDatafiles(token, hpcDatafileURL, path, list, false);
	}

	public static HpcDataObjectDTO getDatafiles(String token, String hpcDatafileURL, String path, Boolean list,
			Boolean includeAcl) throws DoeWebException {
		try {

			final UriComponentsBuilder ucBuilder = UriComponentsBuilder.fromHttpUrl(hpcDatafileURL)
					.path("/{dme-archive-path}");
			if (list != null) {
				ucBuilder.queryParam("list", list);
			}

			if (includeAcl != null) {
				ucBuilder.queryParam("includeAcl", Boolean.toString(includeAcl));
			}
			final String url2Apply = ucBuilder.buildAndExpand(path).encode().toUri().toURL().toExternalForm();

			WebClient client = DoeClientUtil.getWebClient(url2Apply);
			client.header("Authorization", "Bearer " + token);

			Response restResponse = client.invoke("GET", null);
			if (restResponse.getStatus() == 200) {
				ObjectMapper mapper = new ObjectMapper();
				AnnotationIntrospectorPair intr = new AnnotationIntrospectorPair(
						new JaxbAnnotationIntrospector(TypeFactory.defaultInstance()),
						new JacksonAnnotationIntrospector());
				mapper.setAnnotationIntrospector(intr);
				mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

				// ACCEPT_SINGLE_VALUE_AS_ARRAY is a Feature that determines whether
				// it is acceptable to coerce non-array values to work
				// with Java collection types. If enabled,
				// collection deserializers will try to handle non-array values as if they had
				// "implicit" surrounding JSON array. This feature is meant to be used for
				// to work with packages that leave out JSON array in cases where there
				// is just a single element in array.
				mapper.configure(DeserializationFeature.ACCEPT_SINGLE_VALUE_AS_ARRAY, true);

				// Feature that determines whether it is acceptable to coerce single
				// value array (in JSON) values to the corresponding value type.
				// This is basically the opposite of the ACCEPT_SINGLE_VALUE_AS_ARRAY feature.
				// If more than one value is found in the array, a JsonMappingException is
				// thrown.
				mapper.configure(DeserializationFeature.UNWRAP_SINGLE_VALUE_ARRAYS, true);

				MappingJsonFactory factory = new MappingJsonFactory(mapper);
				JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());

				return parser.readValueAs(HpcDataObjectDTO.class);
			} else {
				log.error("File does not exist or you do not have READ access.");
				String errorMessage = getErrorMessage(restResponse);
				throw new DoeWebException(errorMessage, restResponse.getStatus());
			}

		} catch (Exception e) {
			throw new DoeWebException(e.getMessage());
		}
	}

	public static Response getDataObjectQuery(String token, String compoundDataObjectSearchServiceURL,
			Boolean returnParent, HpcCompoundMetadataQueryDTO compoundMetadataQuery)
			throws MalformedURLException, DoeWebException {

		log.info("get data objects query");
		try {
			UriComponentsBuilder ucBuilder = UriComponentsBuilder.fromHttpUrl(compoundDataObjectSearchServiceURL);
			ucBuilder.queryParam("returnParent", returnParent);
			String requestURL = ucBuilder.build().encode().toUri().toURL().toExternalForm();
			WebClient client = DoeClientUtil.getWebClient(requestURL);
			client.header("Authorization", "Bearer " + token);
			Response restResponse = client.invoke("POST", compoundMetadataQuery);
			log.info("response status for data object query" + restResponse);
			return restResponse;
		} catch (Exception e) {
			log.error("Failed to query data objects");
			throw new DoeWebException(e);
		}

	}

	public static Response getCollectionSearchQuery(String authToken, String compoundCollectionSearchServiceURL,
			HpcCompoundMetadataQueryDTO compoundMetadataQuery) throws MalformedURLException, DoeWebException {
		try {
			log.info("get collections for search");
			UriComponentsBuilder ucBuilder = UriComponentsBuilder.fromHttpUrl(compoundCollectionSearchServiceURL);
			ucBuilder.queryParam("returnParent", Boolean.TRUE);
			String requestURL;
			requestURL = ucBuilder.build().encode().toUri().toURL().toExternalForm();
			WebClient client = DoeClientUtil.getWebClient(requestURL);
			client.header("Authorization", "Bearer " + authToken);
			Response restResponse = client.invoke("POST", compoundMetadataQuery);
			if (restResponse.getStatus() == 201 || restResponse.getStatus() == 200 || restResponse.getStatus() == 204) {
				return restResponse;
			} else {
				log.error("Failed to query collection");
				String errorMessage = getErrorMessage(restResponse);
				throw new DoeWebException(errorMessage, restResponse.getStatus());
			}
		} catch (Exception e) {
			log.error("Failed to query collection");
			throw new DoeWebException(e);
		}

	}

	public static Boolean getResponseFromGoogleCaptcha(String secretKey, String response)
			throws IOException, DoeWebException {

		log.info("validate google catpcha token for response: " + response);
		try {
			UriComponentsBuilder ucBuilder = UriComponentsBuilder
					.fromHttpUrl("https://www.google.com/recaptcha/api/siteverify");
			ucBuilder.queryParam("secret", secretKey);
			ucBuilder.queryParam("response", response);
			String requestUrl = ucBuilder.build().encode().toUri().toURL().toExternalForm();
			WebClient client = DoeClientUtil.getWebClient(requestUrl);

			Response restResponse = client.invoke("POST", null);
			if (restResponse.getStatus() == 200) {

				MappingJsonFactory factory = new MappingJsonFactory();
				JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
				GoogleResponse googleResponse = parser.readValueAs(GoogleResponse.class);
				Boolean success = googleResponse.getSuccess();
				return success;
			} else {
				log.error("Failed to validate captcha");
				String errorMessage = getErrorMessage(restResponse);
				throw new DoeWebException(errorMessage, restResponse.getStatus());
			}
		} catch (Exception e) {
			log.error("Error in validating captcha.");
			throw new DoeWebException(e);
		}

	}

	public static boolean createCollection(String token, String hpcCollectionURL,
			HpcCollectionRegistrationDTO collectionDTO, String path) throws DoeWebException {
		try {
			HpcCollectionListDTO collection = getCollection(token, hpcCollectionURL, path, false);
			if (collection != null && collection.getCollectionPaths() != null
					&& !CollectionUtils.isEmpty(collection.getCollectionPaths()))
				throw new DoeWebException("Failed to create. Collection already exists: " + path);

			WebClient client = DoeClientUtil.getWebClient(UriComponentsBuilder.fromHttpUrl(hpcCollectionURL)
					.path("/{dme-archive-path}").buildAndExpand(path).encode().toUri().toURL().toExternalForm());
			client.header("Authorization", "Bearer " + token);

			Response restResponse = client.invoke("PUT", collectionDTO);
			if (restResponse.getStatus() == 201) {
				return true;
			} else {
				log.error("Failed to create collection");
				String errorMessage = getErrorMessage(restResponse);
				throw new DoeWebException(errorMessage, restResponse.getStatus());

			}
		} catch (Exception e) {
			throw new DoeWebException(e.getMessage());
		}
	}

	public static Integer updateCollection(String token, String hpcCollectionURL,
			HpcCollectionRegistrationDTO collectionDTO, String path) throws DoeWebException {
		try {
			WebClient client = DoeClientUtil.getWebClient(UriComponentsBuilder.fromHttpUrl(hpcCollectionURL)
					.path("/{dme-archive-path}").buildAndExpand(path).encode().toUri().toURL().toExternalForm());
			client.header("Authorization", "Bearer " + token);

			Response restResponse = client.invoke("PUT", collectionDTO);
			if (restResponse.getStatus() == 200 || restResponse.getStatus() == 201) {
				return restResponse.getStatus();
			} else {
				String errorMessage = getErrorMessage(restResponse);
				throw new DoeWebException(errorMessage, restResponse.getStatus());
			}
		} catch (Exception e) {
			throw new DoeWebException(e.getMessage());
		}
	}

	public static String deleteCollection(String token, String hpcCollectionURL, String collectionPath)
			throws DoeWebException {
		try {
			WebClient client = DoeClientUtil
					.getWebClient(UriComponentsBuilder.fromHttpUrl(hpcCollectionURL).path("/{dme-archive-path}")
							.buildAndExpand(collectionPath).encode().toUri().toURL().toExternalForm());
			client.header("Authorization", "Bearer " + token);

			Response restResponse = client.delete();
			if (restResponse.getStatus() == 200) {
				return "SUCCESS";
			} else {
				String errorMessage = getErrorMessage(restResponse);
				throw new DoeWebException(errorMessage, restResponse.getStatus());
			}
		} catch (Exception e) {
			throw new DoeWebException("Failed to delete collection due to: " + e.getMessage());
		}
	}

	public static Response registerDatafile(String token, MultipartFile hpcDatafile, String hpcDatafileURL,
			gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationRequestDTO datafileDTO, String path)
			throws DoeWebException {

		log.info("Register data file for path: " + path);
		try {
			try {
				HpcDataObjectDTO datafile = getDatafiles(token, hpcDatafileURL, path, false);
				if (datafile != null && datafile.getDataObject() != null)
					throw new DoeWebException("Failed to create. Data file already exists: " + path);
			} catch (DoeWebException e) {
				// Data file is not there!
				log.error("failed to get data file" + e);
			}
			ObjectMapper mapper1 = new ObjectMapper();
			mapper1.setSerializationInclusion(JsonInclude.Include.NON_NULL);
			String json = mapper1.writeValueAsString(datafileDTO);

			WebClient client = DoeClientUtil.getWebClient(UriComponentsBuilder.fromHttpUrl(hpcDatafileURL)
					.path("/{dme-archive-path}").buildAndExpand(path).encode().toUri().toURL().toExternalForm());

			client.type(MediaType.MULTIPART_FORM_DATA_VALUE).accept(MediaType.APPLICATION_JSON_VALUE);

			List<Attachment> atts = new LinkedList<Attachment>();
			atts.add(new org.apache.cxf.jaxrs.ext.multipart.Attachment("dataObjectRegistration", "application/json",
					json));

			if (hpcDatafile != null) {

				ContentDisposition cd2 = new ContentDisposition("attachment;filename=" + hpcDatafile.getName());
				atts.add(new org.apache.cxf.jaxrs.ext.multipart.Attachment("dataObject", hpcDatafile.getInputStream(),
						cd2));
			}

			client.header("Authorization", "Bearer " + token);

			Response restResponse = client.put(new MultipartBody(atts));
			if (restResponse.getStatus() == 201 || restResponse.getStatus() == 200) {
				log.info("response status" + restResponse.getStatus());
				return restResponse;
			} else {
				String errorMessage = getErrorMessage(restResponse);
				throw new DoeWebException(errorMessage, restResponse.getStatus());
			}
		} catch (Exception e) {
			throw new DoeWebException(e.getMessage());
		}
	}

	public static gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationResponseDTO registerBulkDatafiles(
			String token, String hpcDatafileURL,
			gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO datafileDTO)
			throws DoeWebException {
		try {
			WebClient client = DoeClientUtil.getWebClient(hpcDatafileURL);
			client.header("Authorization", "Bearer " + token);

			Response restResponse = client.invoke("PUT", datafileDTO);
			if (restResponse.getStatus() == 201 || restResponse.getStatus() == 200) {
				return (gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationResponseDTO) DoeClientUtil
						.getObject(restResponse,
								gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationResponseDTO.class);
			} else {
				log.error("Failed to bulk register data files");
				String errorMessage = getErrorMessage(restResponse);
				throw new DoeWebException(errorMessage, restResponse.getStatus());
			}
		} catch (Exception e) {
			throw new DoeWebException("Failed to bulk register data files due to: " + e.getMessage());
		}
	}

	public static boolean updateDatafile(String token, String hpcDatafileURL,
			HpcDataObjectRegistrationRequestDTO datafileDTO, String path) throws DoeWebException {
		try {
			WebClient client = DoeClientUtil.getWebClient(UriComponentsBuilder.fromHttpUrl(hpcDatafileURL)
					.path("/{dme-archive-path}").buildAndExpand(path).encode().toUri().toURL().toExternalForm());
			client.type(MediaType.MULTIPART_FORM_DATA_VALUE).accept(MediaType.APPLICATION_JSON_VALUE);
			List<Attachment> atts = new LinkedList<Attachment>();
			ObjectMapper mapper = new ObjectMapper();
			mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
			String json = mapper.writeValueAsString(datafileDTO);

			atts.add(new org.apache.cxf.jaxrs.ext.multipart.Attachment("dataObjectRegistration", "application/json",
					json));

			client.header("Authorization", "Bearer " + token);

			Response restResponse = client.put(new MultipartBody(atts));
			if (restResponse.getStatus() == 200 || restResponse.getStatus() == 201) {
				return true;
			} else {
				log.error("Failed to update data file");
				String errorMessage = getErrorMessage(restResponse);
				throw new DoeWebException(errorMessage, restResponse.getStatus());
			}
		} catch (Exception e) {
			throw new DoeWebException("Failed to update data file due to: " + e.getMessage());
		}
	}

	public static HpcUserPermsForCollectionsDTO getPermissionForCollections(String token, String hpcServiceURL,
			String userId, Object[] basePaths) throws DoeWebException {
		try {
			UriComponentsBuilder ucBuilder = UriComponentsBuilder.fromHttpUrl(hpcServiceURL).pathSegment(userId);
			if (null != basePaths && 0 < basePaths.length) {
				ucBuilder.queryParam("collectionPath", basePaths);
			}
			WebClient client = DoeClientUtil.getWebClient(ucBuilder.build().encode().toUri().toURL().toExternalForm());
			client.header("Authorization", "Bearer " + token);
			Response restResponse = client.get();
			HpcUserPermsForCollectionsDTO retVal = null;
			if (null != restResponse && 200 == restResponse.getStatus()) {
				retVal = new MappingJsonFactory().createParser((InputStream) restResponse.getEntity())
						.readValueAs(HpcUserPermsForCollectionsDTO.class);
			}
			return retVal;
		} catch (Exception e) {
			throw new DoeWebException("Failed to get permission due to: " + e.getMessage());
		}
	}

	public static String deleteDatafile(String token, String hpcDatafileURL, String path) throws DoeWebException {
		try {
			WebClient client = DoeClientUtil.getWebClient(UriComponentsBuilder.fromHttpUrl(hpcDatafileURL)
					.path("/{dme-archive-path}").buildAndExpand(path).encode().toUri().toURL().toExternalForm());
			client.header("Authorization", "Bearer " + token);

			Response restResponse = client.delete();
			if (restResponse.getStatus() == 200) {
				return "true";
			} else {
				String errorMessage = getErrorMessage(restResponse);
				throw new DoeWebException(errorMessage, restResponse.getStatus());
			}
		} catch (Exception e) {
			throw new DoeWebException(e.getMessage());
		}
	}

	public static HpcDownloadSummaryDTO getDownloadSummary(String token, String hpcQueryURL) throws DoeWebException {

		WebClient client = DoeClientUtil.getWebClient(hpcQueryURL);
		client.header("Authorization", "Bearer " + token);

		Response restResponse = client.get();

		if (restResponse == null || restResponse.getStatus() != 200)
			return null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			AnnotationIntrospectorPair intr = new AnnotationIntrospectorPair(
					new JaxbAnnotationIntrospector(TypeFactory.defaultInstance()), new JacksonAnnotationIntrospector());
			mapper.setAnnotationIntrospector(intr);
			mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

			MappingJsonFactory factory = new MappingJsonFactory(mapper);
			JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());

			return parser.readValueAs(HpcDownloadSummaryDTO.class);
		} catch (Exception e) {
			throw new DoeWebException("Failed to get download tasks list due to: " + e.getMessage());
		}
	}

	public static HpcRegistrationSummaryDTO getRegistrationSummary(String token, String hpcQueryURL,
			MultiValueMap<String, String> queryParamsMap) throws DoeWebException {
		try {
			WebClient client = DoeClientUtil.getWebClient(UriComponentsBuilder.fromHttpUrl(hpcQueryURL)
					.queryParams(queryParamsMap).build().encode().toUri().toURL().toExternalForm());
			client.header("Authorization", "Bearer " + token);
			Response restResponse = client.get();
			if (restResponse == null || restResponse.getStatus() != 200) {
				return null;
			}
			ObjectMapper mapper = new ObjectMapper();
			AnnotationIntrospectorPair intr = new AnnotationIntrospectorPair(
					new JaxbAnnotationIntrospector(TypeFactory.defaultInstance()), new JacksonAnnotationIntrospector());
			mapper.setAnnotationIntrospector(intr);
			mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
			mapper.configure(DeserializationFeature.ACCEPT_EMPTY_STRING_AS_NULL_OBJECT, true);
			MappingJsonFactory factory = new MappingJsonFactory(mapper);
			JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
			return parser.readValueAs(HpcRegistrationSummaryDTO.class);
		} catch (Exception e) {
			throw new DoeWebException("Failed to get registration tasks list due to: " + e.getMessage());
		}
	}

	public static Response syncAndasynchronousDownload(String authToken, String dataObjectAsyncServiceURL, String path,
			HpcDownloadRequestDTO downloadRequest) throws DoeWebException {
		try {
			final String requestUrl = UriComponentsBuilder.fromHttpUrl(dataObjectAsyncServiceURL)
					.path("/{dme-archive-path}/download").buildAndExpand(path).encode().toUri().toURL()
					.toExternalForm();

			WebClient client = DoeClientUtil.getWebClient(requestUrl);
			client.header("Authorization", "Bearer " + authToken);

			Response restResponse = client.invoke("POST", downloadRequest);
			log.info("rest response:" + restResponse.getStatus());
			if (restResponse.getStatus() == 200) {
				return restResponse;
			} else {
				String errorMessage = getErrorMessage(restResponse);
				throw new DoeWebException(errorMessage, restResponse.getStatus());
			}
		} catch (Exception e) {
			throw new DoeWebException(e.getMessage());
		}
	}

	public static Response downloadCollection(String authToken, String collectionUrl, String path,
			HpcDownloadRequestDTO downloadRequest) throws DoeWebException {
		try {
			final String requestUrl = UriComponentsBuilder.fromHttpUrl(collectionUrl)
					.path("/{dme-archive-path}/download").buildAndExpand(path).encode().toUri().toURL()
					.toExternalForm();

			WebClient client = DoeClientUtil.getWebClient(requestUrl);
			client.header("Authorization", "Bearer " + authToken);

			Response restResponse = client.invoke("POST", downloadRequest);
			log.info("rest response:" + restResponse.getStatus());
			if (restResponse.getStatus() == 200) {
				return restResponse;
			} else {
				String errorMessage = getErrorMessage(restResponse);
				throw new DoeWebException(errorMessage, restResponse.getStatus());
			}
		} catch (Exception e) {
			throw new DoeWebException(e.getMessage());
		}
	}

	public static Response downloadDataObjectsOrCollections(String authToken, String bulkDownloadUrl,
			HpcBulkDataObjectDownloadRequestDTO downloadRequest) throws DoeWebException {
		try {
			String requestURL;
			UriComponentsBuilder ucBuilder = UriComponentsBuilder.fromHttpUrl(bulkDownloadUrl);

			requestURL = ucBuilder.build().encode().toUri().toURL().toExternalForm();
			WebClient client = DoeClientUtil.getWebClient(requestURL);
			client.header("Authorization", "Bearer " + authToken);
			Response restResponse = client.invoke("POST", downloadRequest);
			log.info("rest response:" + restResponse.getStatus());
			if (restResponse.getStatus() == 200) {
				return restResponse;
			} else {
				String errorMessage = getErrorMessage(restResponse);
				throw new DoeWebException(errorMessage, restResponse.getStatus());
			}
		} catch (Exception e) {
			throw new DoeWebException(e.getMessage());
		}
	}

	public static Response getPreSignedUrl(String authToken, String dataObjectServiceURL, String path)
			throws DoeWebException {
		try {
			final String requestUrl = UriComponentsBuilder.fromHttpUrl(dataObjectServiceURL)
					.path("/{dme-archive-path}/generateDownloadRequestURL").buildAndExpand(path).encode().toUri()
					.toURL().toExternalForm();

			final gov.nih.nci.hpc.dto.datamanagement.HpcDownloadRequestDTO dto = new gov.nih.nci.hpc.dto.datamanagement.HpcDownloadRequestDTO();
			dto.setGenerateDownloadRequestURL(true);

			WebClient client = DoeClientUtil.getWebClient(requestUrl);
			client.header("Authorization", "Bearer " + authToken);

			Response restResponse = client.invoke("POST", dto);
			log.info("rest response:" + restResponse.getStatus());
			if (restResponse.getStatus() == 200) {
				return restResponse;
			} else {
				String errorMessage = getErrorMessage(restResponse);
				throw new DoeWebException(errorMessage, restResponse.getStatus());
			}
		} catch (Exception e) {
			throw new DoeWebException(e.getMessage());
		}
	}

	public static HpcBulkDataObjectDownloadResponseDTO downloadFiles(String token, String hpcQueryURL,
			HpcBulkDataObjectDownloadRequestDTO dto) throws DoeWebException {
		HpcBulkDataObjectDownloadResponseDTO response = null;
		try {
			log.debug("download files for path");
			WebClient client = DoeClientUtil.getWebClient(hpcQueryURL);
			client.header("Authorization", "Bearer " + token);
			Response restResponse = client.invoke("POST", dto);
			if (restResponse.getStatus() == 200) {
				ObjectMapper mapper = new ObjectMapper();
				AnnotationIntrospectorPair intr = new AnnotationIntrospectorPair(
						new JaxbAnnotationIntrospector(TypeFactory.defaultInstance()),
						new JacksonAnnotationIntrospector());
				mapper.setAnnotationIntrospector(intr);
				mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

				MappingJsonFactory factory = new MappingJsonFactory(mapper);
				JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
				response = parser.readValueAs(HpcBulkDataObjectDownloadResponseDTO.class);
			} else {
				String errorMessage = getErrorMessage(restResponse);
				throw new DoeWebException(errorMessage, restResponse.getStatus());
			}
		} catch (Exception e) {
			throw new DoeWebException(e.getMessage());
		}
		return response;
	}

	public static AjaxResponseBody downloadDataFile(String token, String serviceURL, HpcDownloadRequestDTO dto,
			String downloadType) throws IOException, DoeWebException {

		log.debug("download files for download type " + downloadType);
		AjaxResponseBody result = new AjaxResponseBody();
		WebClient client = DoeClientUtil.getWebClient(serviceURL);
		client.header("Authorization", "Bearer " + token);

		Response restResponse = client.invoke("POST", dto);
		if (restResponse.getStatus() == 200) {
			HpcDataObjectDownloadResponseDTO downloadDTO = (HpcDataObjectDownloadResponseDTO) DoeClientUtil
					.getObject(restResponse, HpcDataObjectDownloadResponseDTO.class);
			String taskId = "Unknown";
			if (downloadDTO != null)
				taskId = downloadDTO.getTaskId();
			result.setMessage(taskId);
			return result;
		} else {
			try {
				String errorMessage = getErrorMessage(restResponse);
				throw new DoeWebException(errorMessage, restResponse.getStatus());
			} catch (Exception e) {
				log.debug("Download request is not successful: " + e);
				result.setMessage("Download request is not successful: " + e.getMessage());
				return result;
			}
		}
	}

	public static HpcBulkDataObjectRegistrationStatusDTO getDataObjectRegistrationTask(String token, String hpcQueryURL,
			String taskId) throws DoeWebException {
		try {
			WebClient client = DoeClientUtil.getWebClient(UriComponentsBuilder.fromHttpUrl(hpcQueryURL)
					.pathSegment(taskId).build().encode().toUri().toURL().toExternalForm());
			client.header("Authorization", "Bearer " + token);
			Response restResponse = client.get();

			if (restResponse == null || restResponse.getStatus() != 200) {
				return null;
			}

			ObjectMapper mapper = new ObjectMapper();
			AnnotationIntrospectorPair intr = new AnnotationIntrospectorPair(
					new JaxbAnnotationIntrospector(TypeFactory.defaultInstance()), new JacksonAnnotationIntrospector());
			mapper.setAnnotationIntrospector(intr);
			mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
			mapper.configure(DeserializationFeature.ACCEPT_EMPTY_STRING_AS_NULL_OBJECT, true);
			MappingJsonFactory factory = new MappingJsonFactory(mapper);
			JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
			return parser.readValueAs(HpcBulkDataObjectRegistrationStatusDTO.class);

		} catch (Exception e) {
			throw new DoeWebException(e.getMessage());
		}
	}

	public static HpcDataObjectDownloadStatusDTO getDataObjectDownloadTask(String token, String hpcQueryURL)
			throws DoeWebException {

		log.debug("get data object download task");
		WebClient client = DoeClientUtil.getWebClient(hpcQueryURL);
		client.header("Authorization", "Bearer " + token);

		Response restResponse = client.get();

		if (restResponse == null || restResponse.getStatus() != 200)
			return null;

		ObjectMapper mapper = new ObjectMapper();
		AnnotationIntrospectorPair intr = new AnnotationIntrospectorPair(
				new JaxbAnnotationIntrospector(TypeFactory.defaultInstance()), new JacksonAnnotationIntrospector());
		mapper.setAnnotationIntrospector(intr);
		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		mapper.configure(DeserializationFeature.ACCEPT_EMPTY_STRING_AS_NULL_OBJECT, true);
		MappingJsonFactory factory = new MappingJsonFactory(mapper);
		JsonParser parser;
		try {
			parser = factory.createParser((InputStream) restResponse.getEntity());
		} catch (IllegalStateException | IOException e) {
			throw new DoeWebException("Failed to get data object download tasks details due to: " + e.getMessage());
		}
		try {
			return parser.readValueAs(HpcDataObjectDownloadStatusDTO.class);
		} catch (com.fasterxml.jackson.databind.JsonMappingException e) {
			throw new DoeWebException("Failed to get data object download tasks details due to: " + e.getMessage());
		} catch (JsonProcessingException e) {
			throw new DoeWebException("Failed to get data object download tasks details due to: " + e.getMessage());
		} catch (IOException e) {
			throw new DoeWebException("Failed to get data object download tasks details due to: " + e.getMessage());
		}
	}

	public static HpcCollectionDownloadStatusDTO getDataObjectsDownloadTask(String token, String hpcQueryURL)
			throws DoeWebException {

		log.debug("get data object downloads ");
		WebClient client = DoeClientUtil.getWebClient(hpcQueryURL);
		client.header("Authorization", "Bearer " + token);

		Response restResponse = client.get();

		if (restResponse == null || restResponse.getStatus() != 200)
			return null;

		ObjectMapper mapper = new ObjectMapper();
		AnnotationIntrospectorPair intr = new AnnotationIntrospectorPair(
				new JaxbAnnotationIntrospector(TypeFactory.defaultInstance()), new JacksonAnnotationIntrospector());
		mapper.setAnnotationIntrospector(intr);
		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		mapper.configure(DeserializationFeature.ACCEPT_EMPTY_STRING_AS_NULL_OBJECT, true);
		MappingJsonFactory factory = new MappingJsonFactory(mapper);
		JsonParser parser;
		try {
			parser = factory.createParser((InputStream) restResponse.getEntity());
		} catch (IllegalStateException | IOException e) {
			throw new DoeWebException("Failed to get data objects download tasks details due to: " + e.getMessage());
		}
		try {
			return parser.readValueAs(HpcCollectionDownloadStatusDTO.class);
		} catch (com.fasterxml.jackson.databind.JsonMappingException e) {
			throw new DoeWebException("Failed to get data objects download tasks details due to: " + e.getMessage());
		} catch (JsonProcessingException e) {
			throw new DoeWebException("Failed to get data objects download tasks details due to: " + e.getMessage());
		} catch (IOException e) {
			throw new DoeWebException("Failed to get data objects download tasks details due to: " + e.getMessage());
		}
	}

	public static <T> Object getObject(Response response, Class<T> objectClass) throws DoeWebException {
		ObjectMapper mapper = new ObjectMapper();
		AnnotationIntrospectorPair intr = new AnnotationIntrospectorPair(
				new JaxbAnnotationIntrospector(TypeFactory.defaultInstance()), new JacksonAnnotationIntrospector());
		mapper.setAnnotationIntrospector(intr);
		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

		MappingJsonFactory factory = new MappingJsonFactory(mapper);
		JsonParser parser;
		try {
			parser = factory.createParser((InputStream) response.getEntity());
			return parser.readValueAs(objectClass);
		} catch (Exception e) {
			throw new DoeWebException("Failed to parse object: " + e.getMessage());
		}
	}

	public static void populateBasePaths(HttpSession session, HpcDataManagementModelDTO modelDTO, String authToken,
			String userId, String collectionURL) throws DoeWebException {

		Set<String> basePaths = new TreeSet<String>(String.CASE_INSENSITIVE_ORDER);
		final List<String> docRulesBasePaths = new ArrayList<>();
		for (HpcDocDataManagementRulesDTO docRule : modelDTO.getDocRules()) {
			for (HpcDataManagementRulesDTO rule : docRule.getRules()) {
				docRulesBasePaths.add(rule.getBasePath());
			}
		}
		final HpcUserPermsForCollectionsDTO permissions = DoeClientUtil.getPermissionForCollections(authToken,
				collectionURL, userId, docRulesBasePaths.toArray());
		if (permissions != null) {
			for (HpcPermissionForCollection permission : permissions.getPermissionsForCollections()) {
				if (permission != null && permission.getPermission() != null
						&& (permission.getPermission().equals(HpcPermission.WRITE)
								|| permission.getPermission().equals(HpcPermission.OWN)))
					basePaths.add(permission.getCollectionPath());
			}
		}
		session.setAttribute("basePaths", basePaths);
	}

	public static HpcCollectionDownloadResponseDTO retryCollectionDownloadTask(String token, String retryUrl)
			throws DoeWebException, IOException {
		try {
			WebClient client = DoeClientUtil.getWebClient(retryUrl);
			client.header("Authorization", "Bearer " + token);

			HpcDownloadRetryRequestDTO requestDTO = new HpcDownloadRetryRequestDTO();
			requestDTO.setDestinationOverwrite(true);

			Response restResponse = client.invoke("POST", requestDTO);

			if (restResponse.getStatus() == 200) {
				ObjectMapper mapper = new ObjectMapper();
				AnnotationIntrospectorPair intr = new AnnotationIntrospectorPair(
						new JaxbAnnotationIntrospector(TypeFactory.defaultInstance()),
						new JacksonAnnotationIntrospector());
				mapper.setAnnotationIntrospector(intr);
				mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

				MappingJsonFactory factory = new MappingJsonFactory(mapper);
				JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
				return parser.readValueAs(HpcCollectionDownloadResponseDTO.class);

			} else {
				String errorMessage = getErrorMessage(restResponse);
				throw new DoeWebException(errorMessage, restResponse.getStatus());

			}

		} catch (DoeWebException e) {
			throw new DoeWebException(e.getMessage());
		}

	}

	public static HpcBulkDataObjectDownloadResponseDTO retryBulkDataObjectDownloadTask(String token, String retryUrl)
			throws DoeWebException, IOException {
		try {
			WebClient client = DoeClientUtil.getWebClient(retryUrl);
			client.header("Authorization", "Bearer " + token);

			HpcDownloadRetryRequestDTO requestDTO = new HpcDownloadRetryRequestDTO();
			requestDTO.setDestinationOverwrite(true);

			Response restResponse = client.invoke("POST", requestDTO);

			if (restResponse.getStatus() == 200) {
				ObjectMapper mapper = new ObjectMapper();
				AnnotationIntrospectorPair intr = new AnnotationIntrospectorPair(
						new JaxbAnnotationIntrospector(TypeFactory.defaultInstance()),
						new JacksonAnnotationIntrospector());
				mapper.setAnnotationIntrospector(intr);
				mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

				MappingJsonFactory factory = new MappingJsonFactory(mapper);
				JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
				return parser.readValueAs(HpcBulkDataObjectDownloadResponseDTO.class);

			} else {

				String errorMessage = getErrorMessage(restResponse);
				throw new DoeWebException(errorMessage, restResponse.getStatus());

			}

		} catch (DoeWebException e) {
			throw new DoeWebException(e.getMessage());
		}

	}

	public static HpcDataObjectDownloadResponseDTO retryDataObjectDownloadTask(String token, String retryUrl)
			throws DoeWebException, JsonParseException, IOException {
		try {

			WebClient client = DoeClientUtil.getWebClient(retryUrl);
			client.header("Authorization", "Bearer " + token);

			HpcDownloadRetryRequestDTO requestDTO = new HpcDownloadRetryRequestDTO();
			requestDTO.setDestinationOverwrite(true);

			Response restResponse = client.invoke("POST", requestDTO);

			if (restResponse.getStatus() == 200) {
				ObjectMapper mapper = new ObjectMapper();
				AnnotationIntrospectorPair intr = new AnnotationIntrospectorPair(
						new JaxbAnnotationIntrospector(TypeFactory.defaultInstance()),
						new JacksonAnnotationIntrospector());
				mapper.setAnnotationIntrospector(intr);
				mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

				MappingJsonFactory factory = new MappingJsonFactory(mapper);
				JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
				return parser.readValueAs(HpcDataObjectDownloadResponseDTO.class);

			} else {
				String errorMessage = getErrorMessage(restResponse);
				throw new DoeWebException(errorMessage, restResponse.getStatus());
			}

		} catch (DoeWebException e) {
			throw new DoeWebException(e.getMessage());
		}
	}

	private static Properties appProperties;

	private static void initApplicationProperties() throws DoeWebException {
		if (null == appProperties) {
			loadApplicationProperties();
		}
	}

	private static void loadApplicationProperties() throws DoeWebException {
		Properties theProperties = new Properties();
		try {
			theProperties.load(DoeClientUtil.class.getResourceAsStream("/application.properties"));
			if (null == appProperties) {
				appProperties = theProperties;
			} else {
				appProperties.clear();
				appProperties.putAll(theProperties);
			}
		} catch (IOException e) {
			throw new DoeWebException("Unable to load application properties!", e);
		}
	}

	public static String getErrorMessage(Response restResponse) throws IOException {
		ObjectMapper mapper = new ObjectMapper();
		AnnotationIntrospectorPair intr = new AnnotationIntrospectorPair(
				new JaxbAnnotationIntrospector(TypeFactory.defaultInstance()), new JacksonAnnotationIntrospector());
		mapper.setAnnotationIntrospector(intr);
		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

		MappingJsonFactory factory = new MappingJsonFactory(mapper);
		JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());

		HpcExceptionDTO exception = parser.readValueAs(HpcExceptionDTO.class);
		return exception.getMessage();
	}
}
