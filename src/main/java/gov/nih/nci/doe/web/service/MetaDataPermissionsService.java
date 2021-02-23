package gov.nih.nci.doe.web.service;

import java.util.List;

import gov.nih.nci.doe.web.domain.MetaDataPermissions;

public interface MetaDataPermissionsService {

	public void savePermissionsList(String user, String progList, Integer collectionId, String collectionPath);

	public void deletePermissionsList(String user, List<String> deletedList, Integer collectionId);

	public List<MetaDataPermissions> getAllMetaDataPermissionsByCollectionId(Integer collectionId);

	public List<MetaDataPermissions> getAllGroupMetaDataPermissionsByCollectionId(Integer collectionId);

	public MetaDataPermissions getMetaDataPermissionsOwnerByCollectionId(Integer collectionId);

}