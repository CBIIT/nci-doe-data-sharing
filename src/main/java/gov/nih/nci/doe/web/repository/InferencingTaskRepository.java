package gov.nih.nci.doe.web.repository;

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

	@Query("select a from InferencingTask a where a.userId =?1 and a.testDataSetPath = ?2")
	InferencingTask getInferenceByUserIdAndInputName(String userId, String inputPath);

	@Query("select a from InferencingTask a where a.userId =?1 and a.actualResultsFileName = ?2")
	InferencingTask getInferenceByUserIdAndOutcomeName(String userId, String outcomeName);
	
	@Query("select a from InferencingTask a where a.userId =?1 and a.assetPath = ?2 and a.status IS NOT NULL and a.status !='FAILED'")
	List<InferencingTask> getInferenceByUserIdAndModelPath(String userId, String assetPath);
}
