package gov.nih.nci.doe.web.repository;


import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import gov.nih.nci.doe.web.domain.TaskManager;

public interface TaskManagerRepository extends JpaRepository<TaskManager, String> {

	
	public List<TaskManager> findAllByUserId(String userId);
	
	@Query("select a from TaskManager a where a.userId =?1 and a.taskName =?2")
	public List<TaskManager> getTaskDetails(String userId, String name);
}
