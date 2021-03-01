package gov.nih.nci.doe.web.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import gov.nih.nci.doe.web.domain.UserGroupMapping;

public interface UserGroupRespository extends JpaRepository<UserGroupMapping, String> {
	
	@Query("select gp.groupName from Group gp where gp.id in (select ug.group.id from UserGroupMapping ug where ug.user.id = ?1)")
	public List<String> getProgramNames(Integer userID);

}
