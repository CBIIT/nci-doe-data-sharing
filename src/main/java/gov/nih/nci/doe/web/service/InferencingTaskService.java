package gov.nih.nci.doe.web.service;

import java.util.List;

import gov.nih.nci.doe.web.domain.InferencingTask;

public interface InferencingTaskService {

	public void saveInferenceTask(String userId, String taskId, String modelPath, String resultPath,
			String testModelPath, String modelh5Path);

	public void updateInferenceTask(String taskId, String dmeTaskId);
	
	public List<InferencingTask> getAllTaskByUserId(String userId);
	public void save(InferencingTask t);
}