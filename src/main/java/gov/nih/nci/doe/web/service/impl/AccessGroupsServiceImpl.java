package gov.nih.nci.doe.web.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import gov.nih.nci.doe.web.domain.AccessGroups;
import gov.nih.nci.doe.web.domain.Group;
import gov.nih.nci.doe.web.repository.AccessGroupsRepository;
import gov.nih.nci.doe.web.repository.GroupRepository;
import gov.nih.nci.doe.web.service.AccessGroupsService;

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
public class AccessGroupsServiceImpl implements AccessGroupsService {

	private static final Logger log = LoggerFactory.getLogger(AccessGroupsServiceImpl.class);

	@Autowired
	AccessGroupsRepository accessGroupsRepository;

	@Autowired
	private GroupRepository groupRepository;

	@Override
	public void saveAccessGroups(Integer collectionId, String collectionPath, String accessGroups, String userName) {
		log.info("Save access groups for " + userName + " for collection id: " + collectionId + " with access groups "
				+ accessGroups);

		// create a record for each access group
		if (!StringUtils.isEmpty(accessGroups)) {
			List<String> groupNameList = Arrays.asList(accessGroups.split(","));

			Iterator<String> grpIterator = groupNameList.iterator();
			while (grpIterator.hasNext()) {
				String grpName = grpIterator.next();
				Group g = groupRepository.getGroup(grpName);
				if (g != null) {
					AccessGroups groupAccessGroups = new AccessGroups();
					groupAccessGroups.setCollectionId(collectionId);
					groupAccessGroups.setCreatedDate(new Date());
					groupAccessGroups.setCollectionPath(collectionPath);
					groupAccessGroups.setGroup(g);
					accessGroupsRepository.saveAndFlush(groupAccessGroups);
				}
			}
		}
	}

	@Override
	public List<String> getGroupsByCollectionPath(String collectionPath) {
		log.info("get groups for collection path :" + collectionPath);
		return accessGroupsRepository.getGroupsByCollectionPath(collectionPath);
	}

	@Override
	public void updateAccessGroups(String collectionPath, Integer collectionId, List<String> addedGroups,
			List<String> deletedgroups) {
		log.info("updating access groups for collection path: " + collectionPath);

		if (CollectionUtils.isNotEmpty(deletedgroups)) {
			// delete the existing access groups
			Iterator<String> grpDeleteIterator = deletedgroups.iterator();
			while (grpDeleteIterator.hasNext()) {
				String grpName = grpDeleteIterator.next();
				AccessGroups g = accessGroupsRepository.getAccessGroupsByGroupAndCollectionPath(collectionPath,
						grpName);
				if (g != null) {
					accessGroupsRepository.delete(g);
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
					AccessGroups groupAccessGroups = new AccessGroups();
					groupAccessGroups.setCollectionId(collectionId);
					groupAccessGroups.setCreatedDate(new Date());
					groupAccessGroups.setCollectionPath(collectionPath);
					groupAccessGroups.setGroup(g);
					accessGroupsRepository.saveAndFlush(groupAccessGroups);
				}
			}
		}

	}
}
