package gov.nih.nci.doe.web.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import gov.nih.nci.doe.web.domain.MetaDataPermissions;
import gov.nih.nci.doe.web.repository.MetaDataPermissionsRepository;
import gov.nih.nci.doe.web.service.MetaDataPermissionsService;

import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component
public class MetaDataPermissionsServiceImpl implements MetaDataPermissionsService {

	private static final Logger log = LoggerFactory.getLogger(MetaDataPermissionsServiceImpl.class);

	@Autowired
	private MetaDataPermissionsRepository metaDataPermissionsRepository;
	
	@Override
	 public List<MetaDataPermissions> getAllMetaDataPermissionsByUserId(String userId) {
		
		return null;
		
	}

	@Override
	public void savePermissionsList(String user, String progList, Integer collectionId) {
		log.info("save permission list for user " + user  + " with prog list " + progList + 
				" and collection id" + collectionId);
		
		MetaDataPermissions permissions = new MetaDataPermissions();
		permissions.setCollectionId(collectionId);
		permissions.setCreatedDate(new Date());
		permissions.setIsGroup(false);
		permissions.setIsOwner(true);
		permissions.setUserGroupId(user);
		metaDataPermissionsRepository.saveAndFlush(permissions);
		
		//create for groups
		if(!StringUtils.isEmpty(progList)) {
		 List<String> groupNameList = Arrays.asList(progList.split(","));
		 
		   Iterator proggrpIterator = groupNameList.iterator();
	        while (proggrpIterator.hasNext())  {
	        	String grpName = proggrpIterator.next().toString();
	    		MetaDataPermissions perm = new MetaDataPermissions();
	    		perm.setCollectionId(collectionId);
	    		perm.setCreatedDate(new Date());
	    		perm.setIsGroup(true);
	    		perm.setIsOwner(false);
	    		perm.setUserGroupId(grpName);
	    		metaDataPermissionsRepository.saveAndFlush(perm);
	        }
		}
		
		
	}

	@Override
	public List<MetaDataPermissions> getAllMetaDataPermissionsByCollectionId(Integer collectionId) {
		log.info("get all permissions by collection Id " + collectionId);
		return metaDataPermissionsRepository.getAllMetaDataPermissionsByCollectionId(collectionId);
	}

}
