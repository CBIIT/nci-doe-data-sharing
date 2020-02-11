package gov.nih.nci.doe.web.repository;


import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;


import gov.nih.nci.doe.web.domain.TaskManager;

public interface TaskManagerRepository extends JpaRepository<TaskManager, String> {

	
	public List<TaskManager> findAllByUserId(String userId);
}
