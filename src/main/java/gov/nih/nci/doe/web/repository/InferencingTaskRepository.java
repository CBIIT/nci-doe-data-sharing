package gov.nih.nci.doe.web.repository;

import java.util.Date;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import gov.nih.nci.doe.web.domain.InferencingTask;

public interface InferencingTaskRepository extends JpaRepository<InferencingTask, String> {

	@Query("select a from InferencingTask a where a.taskId =?1")
	InferencingTask getInferenceTask(String taskId);

	@Query("select a from InferencingTask a where a.status =?1")
	List<InferencingTask> getAllNotStartedTasks(String status);

	@Query("select a from InferencingTask a where a.status =?1 and a.dmeTaskId IS NULL")
	List<InferencingTask> getAllInProgressTasks(String status);

	@Query("select a from InferencingTask a where a.userId =?1")
	List<InferencingTask> getAllTaskByUserId(String userId);

	@Query("select a from InferencingTask a where a.userId =?1 and a.resultPath = ?2")
	InferencingTask getInferenceByUserIdAndPredName(String userId, String predictionsPath);

	@Query("select a from InferencingTask a where a.userId =?1 and a.assetPath = ?2 and a.testDataSetPath = ?3 and a.status IS NOT NULL and a.status !='FAILED'")
	InferencingTask getInferenceByUserIdAndInputName(String userId, String assetPath, String inputPath);

	@Query("select a from InferencingTask a where a.userId =?1 and a.assetPath = ?2 and a.actualResultsFileName = ?3 and a.status IS NOT NULL and a.status !='FAILED'")
	InferencingTask getInferenceByUserIdAndOutcomeName(String userId, String assetPath, String outcomeName);

	@Query("select a from InferencingTask a where a.userId =?1 and a.assetPath = ?2 and a.status IS NOT NULL and a.status !='FAILED'")
	List<InferencingTask> getInferenceByUserIdAndModelPath(String userId, String assetPath);

	@Query("select a from InferencingTask a where a.startDate >= sysdate() - 1 and a.status IS NOT NULL and a.status not in ('INPROGRESS', 'NOTSTARTED')")
	List<InferencingTask> getAllCompletedAndFailedTasks();

	@Query("select a from InferencingTask a where ((a.status = 'INPROGRESS' and a.dmeTaskId IS NOT NULL) OR (a.status in ('COMPLETED', 'FAILED') and a.isNotified IS NULL))")
	List<InferencingTask> getTasksForSendingNotification(Date notificationDate);
}
