package gov.nih.nci.doe.web.service;

import java.util.HashMap;
import java.util.List;

import gov.nih.nci.doe.web.domain.MetaDataPermissions;
import gov.nih.nci.doe.web.model.CollectionPermissions;

public interface MetaDataPermissionsService {

	public void savePermissionsList(String user, String progList, Integer collectionId, String collectionPath);

	public void deletePermissionsList(String user, List<String> deletedList, Integer collectionId);

	public List<MetaDataPermissions> getAllMetaDataPermissionsByCollectionId(Integer collectionId);

	public List<MetaDataPermissions> getAllMetaDataPermissionsByCollectionPath(String collectionPath);

	public List<MetaDataPermissions> getAllGroupMetaDataPermissionsByCollectionId(Integer collectionId);

	public HashMap<Integer, CollectionPermissions> getAllMetadataPermissionsForLoggedOnUser(String user,
			List<String> groupList);

	public MetaDataPermissions getMetaDataPermissionsOwnerByCollectionId(Integer collectionId);

	public void deleteAllPermissionsByCollectionId(String emailAddress, Integer collectionId);

}