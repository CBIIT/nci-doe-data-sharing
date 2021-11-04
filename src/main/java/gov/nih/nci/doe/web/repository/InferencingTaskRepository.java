package gov.nih.nci.doe.web.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import gov.nih.nci.doe.web.domain.InferencingTask;

public interface InferencingTaskRepository extends JpaRepository<InferencingTask, String> {

	@Query("select a from InferencingTask a where a.taskId =?1")
	InferencingTask getInferenceTask(String taskId);
	
	@Query("select a from InferencingTask a where a.status =?1 and a.dmeTaskId IS NOT NULL")
	List<InferencingTask> getAllNotStartedTasks(String status);
	
	@Query("select a from InferencingTask a where a.userId =?1")
	List<InferencingTask> getAllTaskByUserId(String userId);
}
