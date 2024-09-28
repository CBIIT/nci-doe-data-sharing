package gov.nih.nci.doe.web.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import gov.nih.nci.doe.web.domain.LookUp;

public interface LookUpRepository extends JpaRepository<LookUp, String> {

	@Query("select a from LookUp a where a.displayName =?1")
	public LookUp getLookUpByDisplayName(String displayName);

	@Query("select a from LookUp a where a.searchCriteriaDisplay ='Y' order by a.displayOrder asc")
	public List<LookUp> findAllBySearchCriteriaDisplay();

	@Query("select a from LookUp a")
	public List<LookUp> getAllValues();

}
