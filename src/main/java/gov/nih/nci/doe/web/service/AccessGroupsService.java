package gov.nih.nci.doe.web.service;

import java.util.List;

public interface AccessGroupsService {

	public void saveAccessGroups(Integer collectionId, String collectionPath, String accessGroups, String userName);
	
	public List<String> getGroupsByCollectionPath(String collectionPath);

	public void updateAccessGroups(String collectionPath,String accessGroups, String userName);
	
}