package gov.nih.nci.doe.web.controller;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.ws.rs.core.Response;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.MappingJsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.introspect.AnnotationIntrospectorPair;
import com.fasterxml.jackson.databind.introspect.JacksonAnnotationIntrospector;
import com.fasterxml.jackson.databind.type.TypeFactory;
import com.fasterxml.jackson.module.jaxb.JaxbAnnotationIntrospector;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.domain.ModelInfo;
import gov.nih.nci.doe.web.domain.PredictionAccess;
import gov.nih.nci.doe.web.model.DoeSearch;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.doe.web.util.LambdaUtils;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementModelDTO;
import gov.nih.nci.hpc.dto.datasearch.HpcCompoundMetadataQueryDTO;

@Controller
@EnableAutoConfiguration
@RequestMapping("/")
public class AssetDetailsController extends AbstractDoeController {

	@GetMapping(value = "/assetDetails")
	public String getAssetDetailsTab(Model model, HttpSession session, HttpServletRequest request,
			@RequestParam(value = "dme_data_id", required = false) String dmeDataId,
			@RequestParam(value = "returnToSearch", required = false) String returnToSearch,
			@RequestParam(value = "assetIdentifier", required = false) String assetIdentifier) throws DoeWebException {

		log.info("get asset details");
		return getAssetDetails(session, dmeDataId, returnToSearch, assetIdentifier, model);

	}

	@SuppressWarnings("unchecked")
	public String getAssetDetails(HttpSession session, String dmeDataId, String returnToSearch, String assetIdentifier,
			Model model) throws DoeWebException {

		log.info("get Asset detials for dmeDataId: " + dmeDataId + " is returnToSearch: " + returnToSearch
				+ " and assetIdentifier is : " + assetIdentifier);
		String authToken = (String) session.getAttribute("hpcUserToken");
		log.info("authToken: " + authToken);
		String user = getLoggedOnUserInfo();
		log.info("asset details for user: " + user);
		List<KeyValueBean> loggedOnUserPermissions = (List<KeyValueBean>) getMetaDataPermissionsList(user).getBody();
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
		String[] levelValues = { "Asset", "Asset" };
		boolean[] isExcludeParentMetadata = { false, false };
		String[] rowIds = { "1", "2" };
		String[] operators = { "LIKE", "LIKE" };
		// boolean[] iskeyWordSearch = { true, false };

		search.setLevel(levelValues);
		search.setIsExcludeParentMetadata(isExcludeParentMetadata);
		search.setRowId(rowIds);
		search.setOperator(operators);
		// search.setIskeyWordSearch(iskeyWordSearch);

		List<String> systemAttrs = (List<String>) session.getAttribute("systemAttrs");
		if (CollectionUtils.isEmpty(systemAttrs)) {
			HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");
			if (modelDTO == null) {
				modelDTO = DoeClientUtil.getDOCModel(authToken, hpcModelURL);
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
			HpcCompoundMetadataQueryDTO compoundQuery = constructCriteria(search);
			compoundQuery.setDetailedResponse(true);
			log.info("search compund query" + compoundQuery);

			Response restResponse = DoeClientUtil.getCollectionSearchQuery(authToken,
					compoundCollectionSearchServiceURL, compoundQuery);

			if (restResponse.getStatus() == 200) {
				HpcCompoundMetadataQueryDTO compoundQuerySession = (HpcCompoundMetadataQueryDTO) session
						.getAttribute("compoundQuery");

				if (compoundQuerySession == null) {
					session.setAttribute("compoundQuery", compoundQuery);
				}

				ObjectMapper mapper = new ObjectMapper();
				AnnotationIntrospectorPair intr = new AnnotationIntrospectorPair(
						new JaxbAnnotationIntrospector(TypeFactory.defaultInstance()),
						new JacksonAnnotationIntrospector());
				mapper.setAnnotationIntrospector(intr);
				mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

				MappingJsonFactory factory = new MappingJsonFactory(mapper);
				JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
				HpcCollectionListDTO collections = parser.readValueAs(HpcCollectionListDTO.class);
				HpcCollectionDTO collection = collections.getCollections().get(0);

				String studyName = getAttributeValue("study_name",
						collection.getMetadataEntries().getParentMetadataEntries(), "Study");

				String progName = getAttributeValue("program_name",
						collection.getMetadataEntries().getParentMetadataEntries(), "Program");

				String assetPermission = getPermissionRole(user, collection.getCollection().getCollectionId(),
						loggedOnUserPermissions);

				/* get the latest access group from MoDaC database instead of the materialized view */
				List<String> accessGrpList = accessGroupsService
						.getGroupsByCollectionPath(collection.getCollection().getCollectionName());

				String assetName = getAttributeValue("asset_name",
						collection.getMetadataEntries().getSelfMetadataEntries(), "Asset");

				String asset_Identifier = getAttributeValue("asset_identifier",
						collection.getMetadataEntries().getSelfMetadataEntries(), "Asset");

				String dme_Data_Id = getAttributeValue("dme_data_id",
						collection.getMetadataEntries().getSelfMetadataEntries(), "Asset");

				String assetType = getAttributeValue("asset_type",
						collection.getMetadataEntries().getSelfMetadataEntries(), "Asset");

				String isReferenceDataset = getAttributeValue("is_reference_dataset",
						collection.getMetadataEntries().getSelfMetadataEntries(), "Asset");

				String isModelDeployed = getAttributeValue("is_model_deployed",
						collection.getMetadataEntries().getSelfMetadataEntries(), "Asset");

				String resultFileName = getAttributeValue("outcome_file_name",
						collection.getMetadataEntries().getSelfMetadataEntries(), "Asset");

				String applicableModelName = getAttributeValue("applicable_model_paths",
						collection.getMetadataEntries().getSelfMetadataEntries(), "Asset");

				// display all self metadata with is_visible not false in Look up table
				List<KeyValueBean> selfMetadata = getUserMetadata(
						collection.getMetadataEntries().getSelfMetadataEntries(), "Asset", systemAttrs, Boolean.TRUE);

				if (StringUtils.isNotEmpty(returnToSearch)) {
					model.addAttribute("returnToSearch", true);
				}

				model.addAttribute("assetType", assetType);
				model.addAttribute("dme_Data_Id", dme_Data_Id);
				model.addAttribute("asset_Identifier", asset_Identifier);
				model.addAttribute("accessGrp", CollectionUtils.isNotEmpty(accessGrpList) ? String.join(",", accessGrpList) : "public");
				model.addAttribute("assetName", assetName);
				model.addAttribute("assetMetadata", selfMetadata);
				model.addAttribute("studyName", studyName);
				model.addAttribute("progName", progName);
				model.addAttribute("isReferenceDataset", isReferenceDataset);
				model.addAttribute("resultFileName", resultFileName);
				model.addAttribute("applicableModelName", applicableModelName);

				model.addAttribute("assetPath", collection.getCollection().getCollectionName());
				model.addAttribute("assetPermission", assetPermission);
				model.addAttribute("assetLink", webServerName + "/assetDetails?dme_data_id=" + dme_Data_Id);

				ModelInfo modelInfo = modelInfoService.getModelInfo(collection.getCollection().getCollectionName());
				if (Boolean.TRUE.equals(getIsUploader())
						&& ((isModelDeployed != null && isModelDeployed.equalsIgnoreCase("Yes"))
								|| "Yes".equalsIgnoreCase(isReferenceDataset))) {

					// show generate predictions button
					model.addAttribute("showGeneratePredictions",
							isModelDeployed != null && isModelDeployed.equalsIgnoreCase("Yes") ? true : false);
					model.addAttribute("isExternalDataSetSupported",
							modelInfo != null ? modelInfo.getIsExternalDatasetSupported() : false);

					// check if there are any predictions avaiable for logged on user

					List<String> grpsList = LambdaUtils.map(loggedOnUserPermissions, KeyValueBean::getKey);
					List<PredictionAccess> predictionResults = predictionAccessService
							.getAllPredictionsForUserByAssetPath(user, grpsList,
									collection.getCollection().getCollectionName());

					if (CollectionUtils.isNotEmpty(predictionResults)) {

						model.addAttribute("showGeneratePredTab", true);

					}

				}

			} else {
				if (StringUtils.isEmpty(user)) {
					return "redirect:/loginTab";
				} else {
					throw new DoeWebException("Not Authorized", HttpServletResponse.SC_UNAUTHORIZED);
				}
			}
		} catch (Exception e) {
			throw new DoeWebException(e.getMessage());
		}
		return "assetDetails";

	}

	public String updatePredictionAccess(String path, String collectionId, String[] selectedPredictionAccess,
			String isPublic) throws DoeWebException {

		log.info("get prediction access list for path: " + path + " for user: " + getLoggedOnUserInfo());

		try {

			List<String> newPredictionAccessList = selectedPredictionAccess == null ? new ArrayList<String>()
					: Arrays.asList(selectedPredictionAccess);

			List<String> existingPredictionAccessList = predictionAccessService
					.getAllPredictionAccessGroupsByCollectionPath(path);

			// get the added and deleted groups
			List<String> deletedgroups = existingPredictionAccessList.stream()
					.filter(e -> !newPredictionAccessList.contains(e)).filter(value -> value != null)
					.collect(Collectors.toList());

			List<String> addedGroups = newPredictionAccessList.stream()
					.filter(e -> !existingPredictionAccessList.contains(e)).collect(Collectors.toList());

			predictionAccessService.updatePredictionAccess(path, Integer.valueOf(collectionId), addedGroups,
					deletedgroups, StringUtils.isNotEmpty(isPublic) ? Boolean.valueOf(isPublic) : null);

			return "SUCCESS";
		} catch (Exception e) {
			throw new DoeWebException("Failed to save prediction access " + e.getMessage());
		}
	}

	@PostMapping(value = "/predictionAccessGroups")
	@ResponseBody
	public String savePredictionAccessGroups(HttpSession session, @RequestHeader HttpHeaders headers,
			@RequestParam(value = "predCollId") String predCollId,
			@RequestParam(value = "predCollPath") String predCollPath,
			@RequestParam(value = "selectedGrps[]", required = false) String[] selectedGrps,
			@RequestParam(value = "isPublic") String isPublic) throws DoeWebException {
		log.info("save prediction access groups");
		return updatePredictionAccess(predCollPath, predCollId, selectedGrps, isPublic);
	}

}
