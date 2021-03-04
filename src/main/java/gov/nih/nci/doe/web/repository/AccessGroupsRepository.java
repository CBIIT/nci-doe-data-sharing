package gov.nih.nci.doe.web.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import gov.nih.nci.doe.web.domain.AccessGroups;

public interface AccessGroupsRepository extends JpaRepository<AccessGroups, String> {

	@Query("select a.group.groupName from AccessGroups a where a.collectionPath =?1 and a.group IS NOT NULL")
	List<String> getGroupsByCollectionPath(String collectionPath);

	@Query("select a from AccessGroups a where a.collectionPath =?1 and a.group.groupName =?2 and a.group IS NOT NULL")
	AccessGroups getAccessGroupsByGroupAndCollectionPath(String collectionPath, String grpName);

	@Query("select DISTINCT(a.collectionId) from AccessGroups a where a.collectionPath =?1")
	Integer getCollectionIdFromPath(String collectionPath);
}
