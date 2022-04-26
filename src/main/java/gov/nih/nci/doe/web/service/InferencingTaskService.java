package gov.nih.nci.doe.web.service;

import java.util.List;

import gov.nih.nci.doe.web.domain.InferencingTask;
import gov.nih.nci.doe.web.model.InferencingTaskModel;

public interface InferencingTaskService {

	public void saveInferenceTask(InferencingTaskModel inference);

	public void updateInferenceTask(String taskId, String dmeTaskId);

	public List<InferencingTask> getAllTaskByUserId(String userId);

	public void save(InferencingTask t);

	public InferencingTask getInferenceByUserIdAndPredName(String userId, String resultPath);

	public InferencingTask getInferenceByUserIdAndOutcomeName(String userId, String outcomeName);

	public InferencingTask getInferenceByUserIdAndInputName(String userId, String inputFileName);
	
	public Boolean checkifFileExistsForUser(String user, String modelPath, String inputFileName);
}
