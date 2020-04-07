package gov.nih.nci.doe.web.repository;


import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;


import gov.nih.nci.doe.web.domain.Consortium;

public interface ConsortiumRepository extends JpaRepository<Consortium, String> {

	 @Query("select a.groupName from Consortium a where a.userId =?1")
	public String getConsortiumGroupByUserId(String userId);
}
