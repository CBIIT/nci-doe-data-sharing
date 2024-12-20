package gov.nih.nci.doe.web.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import gov.nih.nci.doe.web.domain.DoeUsers;
import gov.nih.nci.doe.web.domain.Group;
import gov.nih.nci.doe.web.model.CollectionPermissions;
import gov.nih.nci.doe.web.domain.MetaDataPermissions;
import gov.nih.nci.doe.web.repository.DoeUserRepository;
import gov.nih.nci.doe.web.repository.GroupRepository;
import gov.nih.nci.doe.web.repository.MetaDataPermissionsRepository;
import gov.nih.nci.doe.web.service.MetaDataPermissionsService;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component
@Transactional
public class MetaDataPermissionsServiceImpl implements MetaDataPermissionsService {

	private static final Logger log = LoggerFactory.getLogger(MetaDataPermissionsServiceImpl.class);

	@Autowired
	private MetaDataPermissionsRepository metaDataPermissionsRepository;

	@Autowired
	private DoeUserRepository doeUserRepository;

	@Autowired
	private GroupRepository groupRepository;

	@Override
	public void savePermissionsList(String user, String progList, Integer collectionId, String collectionPath) {
		log.info("save permission list for user " + user + " with prog list " + progList + " and collection id"
				+ collectionId);

		// create a new record for the owner
		MetaDataPermissions ownerPermission = metaDataPermissionsRepository
				.getMetaDataPermissionsOwnerByCollectionPath(collectionPath);

		if (ownerPermission == null) {
			DoeUsers d = doeUserRepository.getUserInfo(user);
			MetaDataPermissions permissions = new MetaDataPermissions();
			permissions.setCollectionId(collectionId);
			permissions.setCreatedDate(new Date());
			permissions.setUser(d);
			permissions.setCollectionPath(collectionPath);
			metaDataPermissionsRepository.saveAndFlush(permissions);
		}
		// create rows for each group
		if (!StringUtils.isEmpty(progList)) {
			List<String> groupNameList = Arrays.asList(progList.split(","));

			Iterator<String> proggrpIterator = groupNameList.iterator();
			while (proggrpIterator.hasNext()) {
				String grpName = proggrpIterator.next();
				Group g = groupRepository.getGroup(grpName);
				MetaDataPermissions perm = new MetaDataPermissions();
				perm.setCollectionId(collectionId);
				perm.setCreatedDate(new Date());
				perm.setCollectionPath(collectionPath);
				perm.setGroup(g);
				metaDataPermissionsRepository.saveAndFlush(perm);
			}
		}

	}

	@Override
	public List<MetaDataPermissions> getAllMetaDataPermissionsByCollectionId(Integer collectionId) {
		log.info("get all permissions by collection Id " + collectionId);
		return metaDataPermissionsRepository.getAllMetaDataPermissionsByCollectionId(collectionId);
	}

	@Override
	public List<MetaDataPermissions> getAllMetaDataPermissionsByCollectionPath(String collectionPath) {
		log.info("get all permissions by collection path " + collectionPath);
		return metaDataPermissionsRepository.getAllMetaDataPermissionsByCollectionPath(collectionPath);
	}

	@Override
	public List<MetaDataPermissions> getAllGroupMetaDataPermissionsByCollectionId(Integer collectionId) {
		log.info("get all permissions by collection Id " + collectionId);
		return metaDataPermissionsRepository.getAllGroupMetaDataPermissionsByCollectionId(collectionId);
	}

	@Override
	public void deletePermissionsList(String user, List<String> deletedList, Integer collectionId) {
		log.info("deleting permissions" + deletedList + " for user: " + user + "with collectionId: " + collectionId);
		Iterator<String> permissionListIterator = deletedList.iterator();
		while (permissionListIterator.hasNext()) {
			String permission = permissionListIterator.next();
			if (permission != null) {
				MetaDataPermissions p = metaDataPermissionsRepository
						.findPermissionByGroupNameAndCollectionId(permission, collectionId);
				metaDataPermissionsRepository.delete(p);
			}
		}

	}

	@Override
	public MetaDataPermissions getMetaDataPermissionsOwnerByCollectionId(Integer collectionId) {
		log.info("get owner metadata permissions for collectionId: " + collectionId);
		return metaDataPermissionsRepository.getMetaDataPermissionsOwnerByCollectionId(collectionId);
	}

	@Override
	public void deletePermissionByCollectionPath(String collectionPath) {
		log.info("get owner metadata permissions for collectionPath: " + collectionPath);
		MetaDataPermissions perm = metaDataPermissionsRepository
				.getMetaDataPermissionsOwnerByCollectionPath(collectionPath);
		metaDataPermissionsRepository.delete(perm);
	}

	@Override
	public void deleteAllPermissionsByCollectionId(String user, Integer collectionId) {
		log.info("get all permissions by collection Id " + collectionId);
		List<MetaDataPermissions> permissionsList = metaDataPermissionsRepository
				.getAllMetaDataPermissionsByCollectionId(collectionId);
		Iterator<MetaDataPermissions> permissionListIterator = permissionsList.iterator();
		while (permissionListIterator.hasNext()) {
			metaDataPermissionsRepository.delete(permissionListIterator.next());
		}
	}

	@Override
	public HashMap<Integer, CollectionPermissions> getAllMetadataPermissionsForLoggedOnUser(String emailAddress,
			List<String> groupList) {
		log.info("get all permissions for logged on user with user name " + emailAddress);
		List<MetaDataPermissions> list = metaDataPermissionsRepository
				.getAllMetadataPermissionsForLoggedOnUser(emailAddress, groupList);
		HashMap<Integer, CollectionPermissions> permissionSet = new HashMap<Integer, CollectionPermissions>();
		list.stream().forEach((g -> {

			Integer collectionId = g.getCollectionId();

			CollectionPermissions permissions;
			if (permissionSet.containsKey(collectionId)) {
				permissions = permissionSet.get(collectionId);
			} else {
				permissions = new CollectionPermissions();
			}
			if (g.getUser() != null) {
				permissions.setOwner(g.getUser().getEmailAddrr());
			} else if (g.getGroup() != null) {
				List<String> grpList;
				if (CollectionUtils.isEmpty(permissions.getGrpList())) {
					grpList = new ArrayList<String>();
				} else {
					grpList = permissions.getGrpList();
				}
				grpList.add(g.getGroup().getGroupName());
				permissions.setGrpList(grpList);
			}
			permissionSet.put(collectionId, permissions);

		}));

		return permissionSet;
	}

	@Override
	public MetaDataPermissions getMetaDataPermissionsOwnerByCollectionPath(String collectionPath) {
		log.info("get owner metadata permissions for collectionPath: " + collectionPath);
		MetaDataPermissions perm = metaDataPermissionsRepository
				.getMetaDataPermissionsOwnerByCollectionPath(collectionPath);
		return perm;
	}
}
