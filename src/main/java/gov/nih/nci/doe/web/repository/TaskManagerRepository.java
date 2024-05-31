package gov.nih.nci.doe.web.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import gov.nih.nci.doe.web.domain.TaskManager;

public interface TaskManagerRepository extends JpaRepository<TaskManager, String> {

	public List<TaskManager> findAllByUserId(String userId);

	@Query("select a from TaskManager a")
	public List<TaskManager> getAllTasks();

	@Query("select a from TaskManager a where a.userId =?1 and a.taskName =?2 and a.taskDate <= sysdate and a.taskDate >= (sysdate-1)")
	public List<TaskManager> getTaskDetails(String userId, String name);

	@Query("select a from TaskManager a WHERE a.path LIKE :path and a.taskType ='Upload' ORDER BY a.taskDate DESC")
	public List<TaskManager> findByPath(@Param("path") String path);

	@Query("select a from TaskManager a where a.taskId =?1 ORDER BY a.taskDate DESC")
	public List<TaskManager> getAllTasksById(String taskId);

	@Query("select a from TaskManager a where (a.status IS NULL or a.status = 'In progress') OR (a.status in ('Completed', 'Failed') and a.isNotified IS NULL)")
	public List<TaskManager> getAllInProgressTasks();

}
