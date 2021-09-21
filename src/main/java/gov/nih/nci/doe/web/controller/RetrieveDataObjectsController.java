package gov.nih.nci.doe.web.controller;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpSession;
import javax.ws.rs.core.Response;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.cxf.jaxrs.client.WebClient;
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
import gov.nih.nci.doe.web.model.DoeDatafileSearchResultDetailed;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.model.MoDaCPredictionsResults;
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
	public ResponseEntity<?> getDataObjects(HttpSession session, @RequestHeader HttpHeaders headers,
			@RequestParam(value = "path") String path) throws DoeWebException {

		log.info("get asset files under asset path: " + path);
		return getDataObjectsLevelFiles(session, headers, path, "Asset_Files");

	}

	@GetMapping("/getPredictionFiles")
	public ResponseEntity<?> getPredictionFiles(HttpSession session, @RequestHeader HttpHeaders headers,
			@RequestParam(value = "path") String path) throws DoeWebException {

		log.info("get model analysis files under asset path: " + path);
		return getDataObjectsLevelFiles(session, headers, path, "Prediction_Files");
	}

	private List<DoeDatafileSearchResultDetailed> processDataObjectResponseResults(Response restResponse,
			List<String> systemAttrs, String type) throws IOException {
		MappingJsonFactory factory = new MappingJsonFactory();
		JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
		HpcDataObjectListDTO dataObjects = parser.readValueAs(HpcDataObjectListDTO.class);

		List<HpcDataObjectDTO> searchResults = dataObjects.getDataObjects();
		List<DoeDatafileSearchResultDetailed> returnResults = new ArrayList<DoeDatafileSearchResultDetailed>();
		Map<String, List<DoeDatafileSearchResultDetailed>> foldersList = new HashMap<String, List<DoeDatafileSearchResultDetailed>>();
		for (HpcDataObjectDTO result : searchResults) {
			String name = result.getDataObject().getAbsolutePath()
					.substring(result.getDataObject().getAbsolutePath().lastIndexOf('/') + 1);
			List<HpcMetadataEntry> parentMetadaEntries = result.getMetadataEntries().getParentMetadataEntries();
			HpcMetadataEntry folderType = parentMetadaEntries.stream()
					.filter(e -> e.getAttribute().equals("collection_type") && e.getValue().equalsIgnoreCase("Folder"))
					.findAny().orElse(null);

			if (folderType != null) {
				DoeDatafileSearchResultDetailed returnResult = getAssetFile(result, true, systemAttrs);
				String folderPath = result.getDataObject().getAbsolutePath().substring(0,
						result.getDataObject().getAbsolutePath().lastIndexOf('/'));
				String folderName = folderPath.substring(folderPath.lastIndexOf("/") + 1);
				returnResult.setName(folderName);
				returnResult.setPath(folderPath);
				returnResult.setCollectionId(folderType.getCollectionId());
				List<DoeDatafileSearchResultDetailed> folderFiles;
				if (foldersList.containsKey(folderName)) {
					folderFiles = foldersList.get(folderName);
					DoeDatafileSearchResultDetailed returnResult1 = getAssetFile(result, false, systemAttrs);
					returnResult1.setName(name);
					returnResult1.setPath(result.getDataObject().getAbsolutePath());
					folderFiles.add(returnResult1);
				} else {
					folderFiles = new ArrayList<DoeDatafileSearchResultDetailed>();
					DoeDatafileSearchResultDetailed returnResult1 = getAssetFile(result, false, systemAttrs);
					returnResult1.setName(name);
					returnResult1.setPath(result.getDataObject().getAbsolutePath());
					folderFiles.add(returnResult1);
					foldersList.put(folderName, folderFiles);
				}
				returnResult.setIsFolder(true);
				returnResult.setFilesList(folderFiles);
				returnResults.add(returnResult);
			} else {

				String attr = getAttributeValue("generate_pred_username",
						result.getMetadataEntries().getSelfMetadataEntries(), "DataObject");
				if (StringUtils.isEmpty(attr)) {
					DoeDatafileSearchResultDetailed returnResult = getAssetFile(result, false, systemAttrs);
					returnResult.setPath(result.getDataObject().getAbsolutePath());
					returnResult.setName(name);
					returnResult.setIsFolder(false);
					returnResults.add(returnResult);

				}
			}
		}

		return returnResults;

	}

	private DoeDatafileSearchResultDetailed getAssetFile(HpcDataObjectDTO result, Boolean IsParentlevel,
			List<String> systemAttrs) {

		DoeDatafileSearchResultDetailed returnResult = new DoeDatafileSearchResultDetailed();

		if (Boolean.TRUE.equals(IsParentlevel)) {

			returnResult.setSelfMetadata(
					getUserMetadata(result.getMetadataEntries().getParentMetadataEntries(), "Folder", systemAttrs));
			returnResult.setSystemMetadata(
					getSystemMetaData(result.getMetadataEntries().getParentMetadataEntries(), "Folder", systemAttrs));

		} else {
			returnResult.setSelfMetadata(
					getUserMetadata(result.getMetadataEntries().getSelfMetadataEntries(), "DataObject", systemAttrs));
			returnResult.setSystemMetadata(
					getSystemMetaData(result.getMetadataEntries().getSelfMetadataEntries(), "DataObject", systemAttrs));
			returnResult.setFileSize(addHumanReadableSize(getAttributeValue("source_file_size",
					result.getMetadataEntries().getSelfMetadataEntries(), "DataObject")));
		}

		return returnResult;

	}

	private List<MoDaCPredictionsResults> processGeneratedPredDataObjects(Response restResponse,
			List<String> systemAttrs) throws IOException {
		MappingJsonFactory factory = new MappingJsonFactory();
		JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
		HpcDataObjectListDTO dataObjects = parser.readValueAs(HpcDataObjectListDTO.class);
		List<HpcDataObjectDTO> searchResults = dataObjects.getDataObjects();
		List<MoDaCPredictionsResults> returnResults = new ArrayList<MoDaCPredictionsResults>();
		for (HpcDataObjectDTO result : searchResults) {

			String user = getLoggedOnUserInfo();
			String attr = getAttributeValue("generate_pred_username",
					result.getMetadataEntries().getSelfMetadataEntries(), "DataObject");
			if (StringUtils.isNotEmpty(attr) && attr.equalsIgnoreCase(user)) {

				String modelAnanlysisTypeName = getAttributeValue("Model_Analysis_type_name",
						result.getMetadataEntries().getSelfMetadataEntries(), "DataObject");
				String name = result.getDataObject().getAbsolutePath()
						.substring(result.getDataObject().getAbsolutePath().lastIndexOf('/') + 1);

				if (!StringUtils.isEmpty(modelAnanlysisTypeName)) {

					if (modelAnanlysisTypeName.startsWith("input_dataset_")) {
						String taskId = modelAnanlysisTypeName.substring(
								modelAnanlysisTypeName.lastIndexOf("input_dataset_") + 14, modelAnanlysisTypeName.length());
						MoDaCPredictionsResults r = returnResults.stream()
								.filter(e -> ("y_pred_" + taskId).equalsIgnoreCase(e.getModelAnalysisPredName()))
								.findAny().orElse(null);
						if (r != null) {
							r.setInputDatasetName(name);
							r.setInputDatasetPath(result.getDataObject().getAbsolutePath());
							r.setModelAnalysisInputDatasetName(modelAnanlysisTypeName);
							r.setInputDatasetSelfMetadata(getUserMetadata(
									result.getMetadataEntries().getSelfMetadataEntries(), "DataObject", systemAttrs));
							r.setInputDatasetSystemMetadata(getSystemMetaData(
									result.getMetadataEntries().getSelfMetadataEntries(), "DataObject", systemAttrs));
						} else {
							MoDaCPredictionsResults returnResult = new MoDaCPredictionsResults();
							returnResult.setInputDatasetName(name);
							returnResult.setInputDatasetPath(result.getDataObject().getAbsolutePath());
							returnResult.setModelAnalysisInputDatasetName(modelAnanlysisTypeName);
							returnResult.setInputDatasetSelfMetadata(getUserMetadata(
									result.getMetadataEntries().getSelfMetadataEntries(), "DataObject", systemAttrs));
							returnResult.setInputDatasetSystemMetadata(getSystemMetaData(
									result.getMetadataEntries().getSelfMetadataEntries(), "DataObject", systemAttrs));
							returnResults.add(returnResult);
						}

					} else if (modelAnanlysisTypeName.startsWith("y_pred_")) {
						String taskId = modelAnanlysisTypeName.substring(modelAnanlysisTypeName.lastIndexOf("y_pred_") + 7,
								modelAnanlysisTypeName.length());
						MoDaCPredictionsResults r = returnResults.stream()
								.filter(e -> ("input_dataset_" + taskId).equalsIgnoreCase(e.getModelAnalysisInputDatasetName()))
								.findAny().orElse(null);
						if (r != null) {
							r.setPredictionsName(name);
							r.setPredictionsPath(result.getDataObject().getAbsolutePath());
							r.setModelAnalysisPredName(modelAnanlysisTypeName);
							r.setPredictionsSelfMetadata(getUserMetadata(
									result.getMetadataEntries().getSelfMetadataEntries(), "DataObject", systemAttrs));
							r.setPredictionsSystemMetadata(getSystemMetaData(
									result.getMetadataEntries().getSelfMetadataEntries(), "DataObject", systemAttrs));

						} else {
							MoDaCPredictionsResults returnResult = new MoDaCPredictionsResults();
							returnResult.setPredictionsName(name);
							returnResult.setPredictionsPath(result.getDataObject().getAbsolutePath());
							returnResult.setModelAnalysisPredName(modelAnanlysisTypeName);
							returnResult.setPredictionsSelfMetadata(getUserMetadata(
									result.getMetadataEntries().getSelfMetadataEntries(), "DataObject", systemAttrs));
							returnResult.setPredictionsSystemMetadata(getSystemMetaData(
									result.getMetadataEntries().getSelfMetadataEntries(), "DataObject", systemAttrs));
							returnResults.add(returnResult);
						}

					}
				}

			}

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

	public ResponseEntity<?> getDataObjectsLevelFiles(HttpSession session, HttpHeaders headers, String path,
			String type) throws DoeWebException {
		String authToken = (String) session.getAttribute("hpcUserToken");
		HpcCompoundMetadataQueryDTO compoundQuery = (HpcCompoundMetadataQueryDTO) session.getAttribute("compoundQuery");

		if (compoundQuery != null && compoundQuery.getCompoundQuery() != null
				&& !CollectionUtils.isEmpty(compoundQuery.getCompoundQuery().getCompoundQueries())) {

			List<HpcCompoundMetadataQuery> queries = compoundQuery.getCompoundQuery().getCompoundQueries();
			for (HpcCompoundMetadataQuery a : queries) {
				a.getQueries().stream().forEach(x -> x.setLevelFilter(null));
				a.getCompoundQueries().stream()
						.forEach(q -> q.getQueries().stream().forEach(x -> x.setLevelFilter(null)));
			}
		}

		List<String> systemAttrs = new ArrayList<>();

		HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");

		if (modelDTO != null) {
			systemAttrs = modelDTO.getDataObjectSystemGeneratedMetadataAttributeNames();
		} else {
			modelDTO = DoeClientUtil.getDOCModel(authToken, hpcModelURL);
			session.setAttribute("userDOCModel", modelDTO);

			systemAttrs = modelDTO.getDataObjectSystemGeneratedMetadataAttributeNames();
		}

		systemAttrs.add("collection_type");
		systemAttrs.add("dme_data_id");

		List<DoeDatafileSearchResultDetailed> dataResults = new ArrayList<DoeDatafileSearchResultDetailed>();
		List<MoDaCPredictionsResults> modelAnalysisFiles = new ArrayList<MoDaCPredictionsResults>();
		try {
			UriComponentsBuilder ucBuilder = UriComponentsBuilder.fromHttpUrl(compoundDataObjectSearchServiceURL);

			if (ucBuilder == null) {
				return null;
			}

			log.info("retrieve data objects compund query" + compoundQuery);
			ucBuilder.pathSegment(path.substring(1, path.length()));
			final String requestURL = ucBuilder.build().encode().toUri().toURL().toExternalForm();
			WebClient client = DoeClientUtil.getWebClient(requestURL);
			client.header("Authorization", "Bearer " + authToken);
			Response restResponse = client.invoke("POST", compoundQuery);

			if (restResponse.getStatus() == 200) {
				session.setAttribute("compoundQuery", compoundQuery);
				if (type.equalsIgnoreCase("Prediction_Files")) {

					modelAnalysisFiles = processGeneratedPredDataObjects(restResponse, systemAttrs);
					return new ResponseEntity<>(modelAnalysisFiles, HttpStatus.OK);
				} else {
					dataResults = processDataObjectResponseResults(restResponse, systemAttrs, type);
					return new ResponseEntity<>(dataResults, HttpStatus.OK);
				}

			} else if (restResponse.getStatus() == 204) {
				if (type.equalsIgnoreCase("Prediction_Files")) {
					return new ResponseEntity<>(modelAnalysisFiles, HttpStatus.OK);
				} else {
					return new ResponseEntity<>(dataResults, HttpStatus.OK);
				}
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);

		}
		return new ResponseEntity<>(dataResults, HttpStatus.NO_CONTENT);
	}
}
