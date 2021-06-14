package gov.nih.nci.doe.web.controller;

import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpSession;
import javax.ws.rs.core.Response;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.cxf.jaxrs.client.WebClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.util.UriComponentsBuilder;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.MappingJsonFactory;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.constants.SystemAttributesList;
import gov.nih.nci.doe.web.model.DoeSearch;
import gov.nih.nci.doe.web.model.DoeDatafileSearchResultDetailed;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.domain.metadata.HpcCompoundMetadataQuery;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementModelDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectListDTO;
import gov.nih.nci.hpc.dto.datasearch.HpcCompoundMetadataQueryDTO;

/**
 *
 * Get Data Objects by Path Controller
 *
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/getDataObjects")
public class RetrieveDataObjectsController extends AbstractDoeController {

	@Value("${gov.nih.nci.hpc.server.search.dataobject.compound}")
	private String compoundDataObjectSearchServiceURL;


	/**
	 * The number of bytes in a kilobyte.
	 */
	public static final long ONE_KB = 1024;

	/**
	 * The number of bytes in a megabyte.
	 */
	public static final long ONE_MB = ONE_KB * ONE_KB;

	/**
	 * The number of bytes in a gigabyte.
	 */
	public static final long ONE_GB = ONE_KB * ONE_MB;

	@GetMapping
	public ResponseEntity<?> search(HttpSession session, @RequestHeader HttpHeaders headers, DoeSearch search,
			@RequestParam(value = "path") String path) throws DoeWebException {

		String authToken = (String) session.getAttribute("hpcUserToken");
		HpcCompoundMetadataQueryDTO compoundQuery = (HpcCompoundMetadataQueryDTO) session.getAttribute("compoundQuery");

		if (compoundQuery != null && compoundQuery.getCompoundQuery() != null
				&& !CollectionUtils.isEmpty(compoundQuery.getCompoundQuery().getCompoundQueries())) {

			List<HpcCompoundMetadataQuery> queries = compoundQuery.getCompoundQuery().getCompoundQueries();
			for (HpcCompoundMetadataQuery a : queries) {
				a.getQueries().stream().forEach(x -> x.setLevelFilter(null));
			}
		}

		List<String> systemAttrs = new ArrayList<>();

		HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");

		if (modelDTO != null) {
			systemAttrs = modelDTO.getDataObjectSystemGeneratedMetadataAttributeNames();
		} else {
			modelDTO = DoeClientUtil.getDOCModel(authToken, hpcModelURL, sslCertPath, sslCertPassword);
			session.setAttribute("userDOCModel", modelDTO);

			systemAttrs = modelDTO.getDataObjectSystemGeneratedMetadataAttributeNames();
		}

		List<DoeDatafileSearchResultDetailed> dataResults = new ArrayList<DoeDatafileSearchResultDetailed>();

		try {
			UriComponentsBuilder ucBuilder = UriComponentsBuilder.fromHttpUrl(compoundDataObjectSearchServiceURL);

			if (ucBuilder == null) {
				return null;
			}

			log.info("retrieve data objects compund query" + compoundQuery);
			ucBuilder.pathSegment(path.substring(1, path.length()));
			final String requestURL = ucBuilder.build().encode().toUri().toURL().toExternalForm();
			WebClient client = DoeClientUtil.getWebClient(requestURL, sslCertPath, sslCertPassword);
			client.header("Authorization", "Bearer " + authToken);
			Response restResponse = client.invoke("POST", compoundQuery);

			if (restResponse.getStatus() == 200) {
				session.setAttribute("compoundQuery", compoundQuery);
				dataResults = processDataObjectResponseResults(restResponse, systemAttrs);
				return new ResponseEntity<>(dataResults, HttpStatus.OK);
			} else if (restResponse.getStatus() == 204) {
				return new ResponseEntity<>(dataResults, HttpStatus.OK);
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);

		}
		return new ResponseEntity<>(dataResults, HttpStatus.NO_CONTENT);

	}

	private List<DoeDatafileSearchResultDetailed> processDataObjectResponseResults(Response restResponse,
			List<String> systemAttrs) throws IOException {

		List<DoeDatafileSearchResultDetailed> returnResults = new ArrayList<DoeDatafileSearchResultDetailed>();

		returnResults = processDataObjectResults(restResponse, systemAttrs);

		return returnResults;

	}

	private List<DoeDatafileSearchResultDetailed> processDataObjectResults(Response restResponse,
			List<String> systemAttrs) throws IOException {
		MappingJsonFactory factory = new MappingJsonFactory();
		JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
		HpcDataObjectListDTO dataObjects = parser.readValueAs(HpcDataObjectListDTO.class);
		SimpleDateFormat format = new SimpleDateFormat("MM/dd/yyyy hh:mm");
		List<HpcDataObjectDTO> searchResults = dataObjects.getDataObjects();
		List<DoeDatafileSearchResultDetailed> returnResults = new ArrayList<DoeDatafileSearchResultDetailed>();
		for (HpcDataObjectDTO result : searchResults) {
			DoeDatafileSearchResultDetailed returnResult = new DoeDatafileSearchResultDetailed();
			String name = result.getDataObject().getAbsolutePath()
					.substring(result.getDataObject().getAbsolutePath().lastIndexOf('/') + 1);
			returnResult.setName(name);
			returnResult.setPath(result.getDataObject().getAbsolutePath());
			returnResult.setDownload(result.getDataObject().getAbsolutePath());
			returnResult.setPermission(result.getDataObject().getAbsolutePath());
			returnResult.setCreatedOn(format.format(result.getDataObject().getCreatedAt().getTime()));
			returnResult.setSelfMetadata(
					getUserMetadata(result.getMetadataEntries().getSelfMetadataEntries(), "DataObject", systemAttrs));
			returnResult.setSystemMetadata(
					getSystemMetaData(result.getMetadataEntries().getSelfMetadataEntries(), "DataObject", systemAttrs));
			returnResult.setFileSize(addHumanReadableSize(getAttributeValue("source_file_size",
					result.getMetadataEntries().getSelfMetadataEntries(), "DataObject")));
			returnResult.setUuid(
					getAttributeValue("uuid", result.getMetadataEntries().getSelfMetadataEntries(), "DataObject"));
			returnResults.add(returnResult);
		}
		return returnResults;

	}

	private String addHumanReadableSize(String value) {
		if (StringUtils.isNotEmpty(value)) {
			return FileUtils.byteCountToDisplaySize(Long.parseLong(value));
		}

		return null;
	}

	public List<KeyValueBean> getSystemMetaData(List<HpcMetadataEntry> list, String levelName,
			List<String> systemAttrs) {

		List<KeyValueBean> entryList = new ArrayList<KeyValueBean>();

		for (HpcMetadataEntry entry : list) {
			if (systemAttrs != null && systemAttrs.contains(entry.getAttribute())
					&& SystemAttributesList.dataSystemAttributes.contains(entry.getAttribute())
					&& levelName.equalsIgnoreCase(entry.getLevelLabel())) {
				KeyValueBean k = null;
				k = new KeyValueBean(entry.getAttribute(), entry.getAttribute(), entry.getValue());
				entryList.add(k);
			}

		}

		return entryList;
	}
}
