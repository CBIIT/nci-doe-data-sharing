package gov.nih.nci.doe.web.repository;


import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import gov.nih.nci.doe.web.domain.Group;

public interface GroupRepository extends JpaRepository<Group, String> {

	@Query("select a from Group a where a.groupName=?1")
	Group getGroup(String groupName);
	
	
}
