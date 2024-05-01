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

	@Query(value = "SELECT * FROM NCI_DOE_DB.TASK_MANAGER_T a WHERE a.PATH LIKE :path ORDER BY a.TASK_DATE DESC FETCH FIRST 1 ROW ONLY", nativeQuery = true)
	public TaskManager findByPath(@Param("path") String path);
}
