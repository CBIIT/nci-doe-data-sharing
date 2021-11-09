package gov.nih.nci.doe.web.controller;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.ws.rs.core.Response;

import org.apache.commons.lang3.StringUtils;
import org.apache.cxf.jaxrs.client.WebClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.util.UriComponentsBuilder;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.MappingJsonFactory;

import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.doe.web.util.ExcelExportProc;
import gov.nih.nci.hpc.domain.metadata.HpcCompoundMetadataQuery;
import gov.nih.nci.hpc.domain.metadata.HpcCompoundMetadataQueryOperator;
import gov.nih.nci.hpc.domain.metadata.HpcCompoundMetadataQueryType;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataQuery;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataQueryAttributeMatch;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataQueryLevelFilter;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataQueryOperator;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectListDTO;
import gov.nih.nci.hpc.dto.datasearch.HpcCompoundMetadataQueryDTO;

@CrossOrigin
@Controller
@EnableAutoConfiguration
@RequestMapping("/export")
public class ExportController extends AbstractDoeController {

	@Value("${gov.nih.nci.hpc.server.collection}")
	private String collectionURL;

	@GetMapping
	public String exportMetadata(HttpSession session, HttpServletResponse response, HttpServletRequest request,
			@RequestParam(value = "assetIdentifier") String assetIdentifier,
			@RequestParam(value = "selectedPaths") String selectedPaths,
			@RequestParam(value = "isParent") String isParent) throws Exception {

		log.info("export file metadata for " + selectedPaths + " and is parent metadata: " + isParent);

		String authToken = (String) session.getAttribute("hpcUserToken");
		List<String> headers = new ArrayList<String>();

		HpcCompoundMetadataQueryDTO dataObjectCompoundQuery = new HpcCompoundMetadataQueryDTO();
		String[] paths = selectedPaths.split(",");
		List<String> pathsList = new ArrayList<>(Arrays.asList(paths));

		HpcCompoundMetadataQuery query = new HpcCompoundMetadataQuery();
		query.setOperator(HpcCompoundMetadataQueryOperator.OR);
		for (String path : pathsList) {
			HpcMetadataQueryLevelFilter levelFilter = new HpcMetadataQueryLevelFilter();
			String dataObjectName = path != null ? path.substring(path.lastIndexOf('/') + 1, path.length()) : null;
			HpcMetadataQuery q = new HpcMetadataQuery();
			q.setAttribute("archive_file_id");
			q.setValue('%' + dataObjectName);
			q.setOperator(HpcMetadataQueryOperator.LIKE);
			levelFilter.setLevel(1);
			levelFilter.setOperator(HpcMetadataQueryOperator.EQUAL);
			q.setLevelFilter(levelFilter);
			query.getQueries().add(q);
		}

		dataObjectCompoundQuery.setCompoundQuery(query);
		dataObjectCompoundQuery.setTotalCount(true);
		dataObjectCompoundQuery.setCompoundQueryType(HpcCompoundMetadataQueryType.DATA_OBJECT);
		dataObjectCompoundQuery.setPage(1);
		dataObjectCompoundQuery.setPageSize(500);
		dataObjectCompoundQuery.setDetailedResponse(true);

		UriComponentsBuilder ucBuilder = UriComponentsBuilder.fromHttpUrl(compoundDataObjectSearchServiceURL);

		if (ucBuilder == null) {
			return null;
		}

		ucBuilder.pathSegment(assetIdentifier.substring(1, assetIdentifier.length()));

		final String requestURL = ucBuilder.build().encode().toUri().toURL().toExternalForm();

		WebClient client = DoeClientUtil.getWebClient(requestURL);
		client.header("Authorization", "Bearer " + authToken);

		Response restResponse = client.invoke("POST", dataObjectCompoundQuery);

		if (StringUtils.isNotEmpty(selectedPaths)) {

			List<List<String>> rows = new ArrayList<>();

			if (restResponse.getStatus() == 200) {

				MappingJsonFactory factory = new MappingJsonFactory();
				JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
				HpcDataObjectListDTO dataObjects = parser.readValueAs(HpcDataObjectListDTO.class);
				List<HpcDataObjectDTO> searchResults = dataObjects.getDataObjects();

				for (HpcDataObjectDTO result : searchResults) {
					if (pathsList.contains(result.getDataObject().getAbsolutePath())) {

						List<String> r = new ArrayList<String>();
						for (String header : headers) {
							boolean found = false;
							for (HpcMetadataEntry entry : result.getMetadataEntries().getSelfMetadataEntries()) {
								if (header.equals("Dataobject" + "_" + entry.getAttribute())) {
									r.add(entry.getValue());
									found = true;
								}
							}

							if (!StringUtils.isEmpty(isParent) && isParent.equalsIgnoreCase("true")) {
								for (HpcMetadataEntry entry : result.getMetadataEntries().getParentMetadataEntries()) {
									if (header.equals(entry.getLevelLabel() + "_" + entry.getAttribute())) {
										r.add(entry.getValue());
										found = true;
									}
								}
							}
							if (!found)
								r.add("");
						}

						for (HpcMetadataEntry entry : result.getMetadataEntries().getSelfMetadataEntries()) {

							if (!headers.contains("Dataobject" + "_" + entry.getAttribute())) {
								headers.add("Dataobject" + "_" + entry.getAttribute());
								r.add(entry.getValue());
							}

						}

						if (!StringUtils.isEmpty(isParent) && isParent.equalsIgnoreCase("true")) {

							for (HpcMetadataEntry entry : result.getMetadataEntries().getParentMetadataEntries()) {

								if (!headers.contains(entry.getLevelLabel() + "_" + entry.getAttribute())) {
									headers.add(entry.getLevelLabel() + "_" + entry.getAttribute());
									r.add(entry.getValue());
								}

							}

						}

						rows.add(r);
					}

				}

				ExcelExportProc proc = new ExcelExportProc();
				proc.setReportName("Download Metadata Results");
				proc.setHeaders(headers);
				proc.setData(rows);
				proc.setFileName("download_metadata_");
				proc.setExtension(".xls");
				proc.setMimeType("application/vnd.ms-excel");
				proc.setFieldSeparator("\t");

				proc.doExport(request, response);
			}
		}
		return null;
	}
}
