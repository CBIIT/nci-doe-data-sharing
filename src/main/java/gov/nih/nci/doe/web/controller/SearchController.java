package gov.nih.nci.doe.web.controller;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.ws.rs.core.Response;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;

import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.MappingJsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.introspect.AnnotationIntrospectorPair;
import com.fasterxml.jackson.databind.introspect.JacksonAnnotationIntrospector;
import com.fasterxml.jackson.databind.type.TypeFactory;
import com.fasterxml.jackson.module.jaxb.JaxbAnnotationIntrospector;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.CollectionPermissions;
import gov.nih.nci.doe.web.model.DoeSearch;
import gov.nih.nci.doe.web.model.DoeSearchResult;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.doe.web.util.HibernateProxyTypeAdapter;
import gov.nih.nci.doe.web.util.MiscUtil;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datasearch.HpcCompoundMetadataQueryDTO;

/**
 *
 * Search Controller
 *
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/search")
public class SearchController extends AbstractDoeController {

	@Value("${asset.bulk.collections.size}")
	public String bulkCollectionSize;

	@GetMapping
	public ResponseEntity<?> search(HttpSession session, @RequestHeader HttpHeaders headers, HttpServletRequest request,
			DoeSearch search) throws DoeWebException {

		log.info("Search controller");
		String authToken = (String) session.getAttribute("hpcUserToken");

		List<DoeSearchResult> results = new ArrayList<>();

		try {
			HpcCompoundMetadataQueryDTO compoundQuery = constructCriteria(session, search);
			compoundQuery.setDetailedResponse(true);
			log.info("search compund query" + compoundQuery);

			Response restResponse = DoeClientUtil.getCollectionSearchQuery(authToken,
					compoundCollectionSearchServiceURL, compoundQuery);

			if (restResponse.getStatus() == 200) {
				session.setAttribute("compoundQuery", compoundQuery);
				GsonBuilder b = new GsonBuilder();
				b.registerTypeAdapterFactory(HibernateProxyTypeAdapter.FACTORY);
				Gson gson = b.create();
				String searchQuery = gson.toJson(search);
				session.setAttribute("searchQuery", searchQuery);
				log.info("Search query" + search);
				results = processCollectionResults(session, restResponse, search);
				return new ResponseEntity<>(results, HttpStatus.OK);

			} else if (restResponse.getStatus() == 204) {
				return new ResponseEntity<>(results, HttpStatus.OK);
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);

		}
		return new ResponseEntity<>(results, HttpStatus.NO_CONTENT);

	}

	private List<DoeSearchResult> processCollectionResults(HttpSession session, Response restResponse, DoeSearch search)
			throws IOException {

		log.info("process collection results for rendering the search results table");
		ObjectMapper mapper = new ObjectMapper();
		AnnotationIntrospectorPair intr = new AnnotationIntrospectorPair(
				new JaxbAnnotationIntrospector(TypeFactory.defaultInstance()), new JacksonAnnotationIntrospector());
		mapper.setAnnotationIntrospector(intr);
		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

		MappingJsonFactory factory = new MappingJsonFactory(mapper);
		JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
		HpcCollectionListDTO collections = parser.readValueAs(HpcCollectionListDTO.class);
		String user = getLoggedOnUserInfo();
		List<HpcCollectionDTO> searchResults = collections.getCollections();
		List<DoeSearchResult> returnResults = new ArrayList<DoeSearchResult>();
		HashMap<Integer, CollectionPermissions> permissionMap = new HashMap<Integer, CollectionPermissions>();

		if (StringUtils.isNotEmpty(user)) {
			List<String> loggedOnUsergrpList = getLoggedOnUserGroups(session, user);

			permissionMap = metaDataPermissionService.getAllMetadataPermissionsForLoggedOnUser(user,
					loggedOnUsergrpList);
		}

		for (HpcCollectionDTO result : searchResults) {
			DoeSearchResult returnResult = new DoeSearchResult();

			List<HpcMetadataEntry> selfMetadatEntries = result.getMetadataEntries().getSelfMetadataEntries();
			List<HpcMetadataEntry> parentMetadatEntries = result.getMetadataEntries().getParentMetadataEntries();

			/* setting parent metadata values */

			String studyPath = result.getCollection().getCollectionParentName();
			String programPath = studyPath.substring(0, studyPath.lastIndexOf('/'));
			Integer studyCollectionId = getCollectionId(parentMetadatEntries, "Study");
			Integer programCollectionId = getCollectionId(parentMetadatEntries, "Program");
			returnResult.setProgramName(getAttributeValue("program_name", parentMetadatEntries, "Program"));
			returnResult.setStudyName(getAttributeValue("study_name", parentMetadatEntries, "Study"));
			returnResult.setStudyCollectionId(studyCollectionId);
			returnResult.setProgramCollectionId(programCollectionId);

			/* setting asset metadata values */
			String collectionSize = getAttributeValue("collection_size", selfMetadatEntries, "Asset");
			returnResult.setIsBulkAsset(
					(collectionSize != null && Long.valueOf(collectionSize) > Long.valueOf(bulkCollectionSize))
							? Boolean.TRUE
							: Boolean.FALSE);

			String assetType = getAttributeValue("asset_type", selfMetadatEntries, "Asset");
			returnResult
					.setDisplayAssetSize(collectionSize != null ? MiscUtil.addHumanReadableSize(collectionSize) : "0");
			returnResult.setCollectionSize(collectionSize != null ? Long.valueOf(collectionSize) : 0);
			returnResult.setDataSetPath(result.getCollection().getCollectionName());
			returnResult.setDataSetCollectionId(result.getCollection().getCollectionId());
			returnResult.setDataSetName(getAttributeValue("asset_name", selfMetadatEntries, "Asset"));
			returnResult.setDataSetDescription(getAttributeValue("description", selfMetadatEntries, "Asset"));
			returnResult.setStudyPath(studyPath);
			returnResult.setProgramPath(programPath);

			returnResult.setDataSetdmeDataId(webServerName + "/searchTab?dme_data_id="
					+ getAttributeValue("dme_data_id", selfMetadatEntries, "Asset"));
			returnResult.setAssetType(assetType);
			returnResult.setDmeDataId(getAttributeValue("dme_data_id", selfMetadatEntries, "Asset"));

			/* setting edit permissions role from MoDaC DB */
			returnResult.setDataSetPermissionRole(
					getPermissionRoleForUser(result.getCollection().getCollectionId(), permissionMap));

			returnResult.setStudyPermissionRole(getPermissionRoleForUser(studyCollectionId, permissionMap));
			returnResult.setProgramPermissionRole(getPermissionRoleForUser(programCollectionId, permissionMap));
			returnResults.add(returnResult);

		}

		return returnResults;
	}

	private Integer getCollectionId(List<HpcMetadataEntry> list, String levelName) {
		if (list == null)
			return null;

		HpcMetadataEntry entry = list.stream().filter(e -> levelName.equalsIgnoreCase(e.getLevelLabel())).findAny()
				.orElse(null);
		if (entry != null) {
			return entry.getCollectionId();
		}
		return null;
	}

}
