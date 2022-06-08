package gov.nih.nci.doe.web.service;

import java.util.List;

import gov.nih.nci.doe.web.domain.PredictionAccess;

public interface PredictionAccessService {

	public void savePredictionAccess(String user, String progList, Integer collectionId, String collectionPath);

	public void updatePredictionAccess(String collectionPath, Integer collectionId, List<String> addedGroups,
			List<String> deletedgroups, Boolean isPublic);

	public List<String> getAllPredictionAccessGroupsByCollectionPath(String collectionPath);

	public List<PredictionAccess> getPredictionAccessByCollectionPath(String collectionPath);

	public List<PredictionAccess> getAllPredictionsForUserByAssetPath(String userId, List<String> grpsList,
			String assetPath);

	public void deleteAllPermissionsByCollectionPath(String user, String collectionPath);
}