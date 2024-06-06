package gov.nih.nci.doe.web.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import gov.nih.nci.doe.web.domain.MetaDataPermissions;

public interface MetaDataPermissionsRepository extends JpaRepository<MetaDataPermissions, String> {

	@Query("select a from MetaDataPermissions a where a.collectionId =?1")
	List<MetaDataPermissions> getAllMetaDataPermissionsByCollectionId(Integer collectionId);

	@Query("select a from MetaDataPermissions a where a.collectionPath =?1")
	List<MetaDataPermissions> getAllMetaDataPermissionsByCollectionPath(String collectionPath);

	@Query("select a from MetaDataPermissions a where a.collectionId =?1 and a.group IS NOT NULL")
	List<MetaDataPermissions> getAllGroupMetaDataPermissionsByCollectionId(Integer collectionId);

	@Query("select a from MetaDataPermissions a where a.collectionId =?1 and a.user IS NOT NULL")
	MetaDataPermissions getMetaDataPermissionsOwnerByCollectionId(Integer collectionId);

	@Query("select a from MetaDataPermissions a where a.group IS NOT NULL and a.group.groupName =?1 and a.collectionId =?2")
	MetaDataPermissions findPermissionByGroupNameAndCollectionId(String grpName, Integer collectionId);

	@Query("select a from MetaDataPermissions a left join DoeUsers u on a.user.id = u.id left join Group g on a.group.id = g.id where u.emailAddrr = :emailAddress OR g.groupName IN (:grpList)")
	List<MetaDataPermissions> getAllMetadataPermissionsForLoggedOnUser(@Param("emailAddress") String emailAddress,
			@Param("grpList") List<String> grpList);

	@Query("select a from MetaDataPermissions a where a.collectionId IS NULL")
	List<MetaDataPermissions> getAllCollectionsWithEmptyCollectionId();

	@Query("select a from MetaDataPermissions a where a.collectionPath =?1 and a.user IS NOT NULL")
	MetaDataPermissions getMetaDataPermissionsOwnerByCollectionPath(String collectionPath);
}
