package gov.nih.nci.doe.web.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import gov.nih.nci.doe.web.domain.PredictionAccess;

public interface PredictionAccessRepository extends JpaRepository<PredictionAccess, String> {

	@Query("select a from PredictionAccess a where a.collectionId =?1 and a.user IS NOT NULL")
	PredictionAccess getPredictionAccessForOwnerByCollectionId(Integer collectionId);

	@Query("select a from PredictionAccess a where a.collectionPath =?1 and a.group.groupName =?2 and a.group IS NOT NULL")
	PredictionAccess getPredictionAccessByGroupAndCollectionPath(String collectionPath, String grpName);

	@Query("select a.group.groupName from PredictionAccess a where a.collectionPath =?1")
	List<String> getAllPredictionAccessGroupsByCollectionPath(String collectionPath);

	@Query("select a from PredictionAccess a where a.collectionPath =?1")
	List<PredictionAccess> getPredictionAccessByCollectionPath(String collectionPath);

	@Query("select p from PredictionAccess p where p.user.emailAddrr =?1")
	List<PredictionAccess> checkIsPredictionsByUserId(String userId);

	@Query("select p from PredictionAccess p where p.group.groupName in ?1")
	List<PredictionAccess> checkIsPredictionsByGroups(List<String> grpsList);

	@Query("select p from PredictionAccess p where p.isPublic = 'Y' and p.user.emailAddrr !=?1")
	List<PredictionAccess> getAllPublicPredictionsForUser(String userId);

}
