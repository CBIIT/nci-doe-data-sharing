package gov.nih.nci.doe.web.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import gov.nih.nci.doe.web.domain.DoeUsers;
import gov.nih.nci.doe.web.domain.Group;
import gov.nih.nci.doe.web.domain.PredictionAccess;
import gov.nih.nci.doe.web.repository.DoeUserRepository;
import gov.nih.nci.doe.web.repository.GroupRepository;
import gov.nih.nci.doe.web.repository.PredictionAccessRepository;
import gov.nih.nci.doe.web.service.PredictionAccessService;
import gov.nih.nci.doe.web.util.LambdaUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component
@Transactional
public class PredictionAccessServiceImpl implements PredictionAccessService {

	private static final Logger log = LoggerFactory.getLogger(PredictionAccessServiceImpl.class);

	@Autowired
	private DoeUserRepository doeUserRepository;

	@Autowired
	private GroupRepository groupRepository;

	@Autowired
	private PredictionAccessRepository predictionAccessRepository;

	@Override
	public void savePredictionAccess(String user, String progList, Integer collectionId, String collectionPath) {
		log.info("save prediction access list for user " + user + " with prog list " + progList + " and collection id"
				+ collectionId);

		// create a new record for the owner
		PredictionAccess predAccess = predictionAccessRepository
				.getPredictionAccessForOwnerByCollectionId(collectionId);

		if (predAccess == null) {
			DoeUsers d = doeUserRepository.getUserInfo(user);
			PredictionAccess access = new PredictionAccess();
			access.setCollectionId(collectionId);
			access.setCreatedDate(new Date());
			access.setUser(d);
			access.setCollectionPath(collectionPath);
			access.setIsPublic(Boolean.FALSE);
			predictionAccessRepository.saveAndFlush(access);
		}
		// create rows for each group
		if (!StringUtils.isEmpty(progList)) {
			List<String> groupNameList = Arrays.asList(progList.split(","));

			Iterator<String> proggrpIterator = groupNameList.iterator();
			while (proggrpIterator.hasNext()) {
				String grpName = proggrpIterator.next();
				Group g = groupRepository.getGroup(grpName);
				PredictionAccess access = new PredictionAccess();
				access.setCollectionId(collectionId);
				access.setCreatedDate(new Date());
				access.setCollectionPath(collectionPath);
				access.setGroup(g);
				predictionAccessRepository.saveAndFlush(access);
			}
		}

	}

	@Override
	public void updatePredictionAccess(String collectionPath, Integer collectionId, List<String> addedGroups,
			List<String> deletedgroups, Boolean isPublic) {
		log.info("updating prediction access groups for collection path: " + collectionPath);

		if (isPublic != null) {
			PredictionAccess predAccess = predictionAccessRepository
					.getPredictionAccessForOwnerByCollectionId(collectionId);
			predAccess.setIsPublic(isPublic);
			predictionAccessRepository.saveAndFlush(predAccess);
		}
		if (CollectionUtils.isNotEmpty(deletedgroups)) {
			// delete the existing access groups
			Iterator<String> grpDeleteIterator = deletedgroups.iterator();
			while (grpDeleteIterator.hasNext()) {
				String grpName = grpDeleteIterator.next();
				PredictionAccess g = predictionAccessRepository
						.getPredictionAccessByGroupAndCollectionPath(collectionPath, grpName);
				if (g != null) {
					predictionAccessRepository.delete(g);
				}

			}
		}
		if (CollectionUtils.isNotEmpty(addedGroups)) {
			// create new records for new access groups
			Iterator<String> grpIterator = addedGroups.iterator();
			while (grpIterator.hasNext()) {
				String grpName = grpIterator.next();
				Group g = groupRepository.getGroup(grpName);
				if (g != null) {
					PredictionAccess groupAccessGroups = new PredictionAccess();
					groupAccessGroups.setCollectionId(collectionId);
					groupAccessGroups.setCreatedDate(new Date());
					groupAccessGroups.setCollectionPath(collectionPath);
					groupAccessGroups.setGroup(g);
					predictionAccessRepository.saveAndFlush(groupAccessGroups);
				}
			}
		}

	}

	@Override
	public List<String> getAllPredictionAccessGroupsByCollectionPath(String collectionPath) {
		log.info("get all prediction access groups by collection path " + collectionPath);
		return predictionAccessRepository.getAllPredictionAccessGroupsByCollectionPath(collectionPath);
	}

	@Override
	public List<PredictionAccess> getPredictionAccessByCollectionPath(String collectionPath) {
		log.info("get all prediction access by collection path " + collectionPath);
		return predictionAccessRepository.getPredictionAccessByCollectionPath(collectionPath);
	}

	@Override
	public List<PredictionAccess> getAllPredictionsForUserByAssetPath(String userId, List<String> grpsList,
			String assetPath) {
		log.info("verify if user has access to any predictions for :  " + userId);

		List<PredictionAccess> predictionResults = new ArrayList<PredictionAccess>();

		List<PredictionAccess> predictionResultsForAsset = new ArrayList<PredictionAccess>();
		// get all owner predictions
		List<PredictionAccess> userList = predictionAccessRepository.checkIsPredictionsByUserId(userId);

		List<Integer> collectionIds = LambdaUtils.map(userList, PredictionAccess::getCollectionId);
		List<PredictionAccess> groupsList;
		// get all group access predictions which are not under logged on user list
		if (CollectionUtils.isNotEmpty(collectionIds)) {
			groupsList = predictionAccessRepository.checkIsPredictionsByGroupsAndCollections(grpsList, collectionIds);
		} else {
			groupsList = predictionAccessRepository.checkIsPredictionsByGroups(grpsList);
		}

		// get all public predictions
		List<PredictionAccess> publicAccessList = predictionAccessRepository.getAllPublicPredictionsForUser(userId);

		predictionResults.addAll(publicAccessList);
		predictionResults.addAll(groupsList);
		predictionResults.addAll(userList);

		predictionResults.stream().forEach((e -> {

			String predictionsPath = e.getCollectionPath();

			String path = predictionsPath.substring(0, predictionsPath.lastIndexOf('/'));
			if (path.equalsIgnoreCase(assetPath)) {
				predictionResultsForAsset.add(e);
			}

		}));

		// filter predictions for the given asset Path

		return predictionResultsForAsset;

	}

	@Override
	public void deleteAllPermissionsByCollectionPath(String user, String collectionPath) {
		log.info("get all predictions by collection path " + collectionPath);
		List<PredictionAccess> permissionsList = predictionAccessRepository
				.getPredictionAccessByCollectionPath(collectionPath);
		Iterator<PredictionAccess> permissionListIterator = permissionsList.iterator();
		while (permissionListIterator.hasNext()) {
			predictionAccessRepository.delete(permissionListIterator.next());
		}

	}

}
