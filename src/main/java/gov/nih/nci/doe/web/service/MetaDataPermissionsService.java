package gov.nih.nci.doe.web.service;

import java.util.List;

import gov.nih.nci.doe.web.domain.MetaDataPermissions;


public interface MetaDataPermissionsService {

    
    public List<MetaDataPermissions> getAllMetaDataPermissionsByUserId(String userId);
    
    public void savePermissionsList(String user,String progList,Integer collectionId);
    
    public List<MetaDataPermissions> getAllMetaDataPermissionsByCollectionId(Integer collectionId);

}