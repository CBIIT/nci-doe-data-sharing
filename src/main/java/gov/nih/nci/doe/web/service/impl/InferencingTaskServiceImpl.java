package gov.nih.nci.doe.web.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import gov.nih.nci.doe.web.domain.InferencingTask;
import gov.nih.nci.doe.web.repository.InferencingTaskRepository;
import gov.nih.nci.doe.web.service.InferencingTaskService;

@Component
@Transactional
public class InferencingTaskServiceImpl implements InferencingTaskService {

	private static final Logger log = LoggerFactory.getLogger(InferencingTaskServiceImpl.class);

	@Autowired
	InferencingTaskRepository inferencingTaskRepository;

	@Override
	public void saveInferenceTask(String userId, String taskId, String modelPath, String resultPath,
			String testModelPath, String modelh5Path) {

		log.info("save inference task");
		InferencingTask t = new InferencingTask();
		if (StringUtils.isNotEmpty(modelPath)) {
			String modelIdentifier = modelPath.substring(modelPath.lastIndexOf('/') + 1,modelPath.length());
			t.setModelIdentifier(modelIdentifier);
		}

		t.setUserId(userId);
		t.setTaskId(taskId);
		t.setAssetPath(modelPath);
		t.setResultPath(resultPath);
		t.setTestDataSetPath(testModelPath);
		t.setStartDate(new Date());
		t.setModelh5Path(modelh5Path);
		t.setStatus("NOTSTARTED");
		inferencingTaskRepository.saveAndFlush(t);

	}

	@Override
	public void updateInferenceTask(String taskId, String dmeTaskId) {
		log.info("update inference task for task Id: " + taskId + "and dme task id: " + dmeTaskId);
		InferencingTask t = inferencingTaskRepository.getInferenceTask(taskId);
		t.setDmeTaskId(dmeTaskId);
		inferencingTaskRepository.saveAndFlush(t);
	}

	@Override
	public List<InferencingTask> getAllTaskByUserId(String userId) {
		log.info("get all Tasks for user: " + userId);
		List<InferencingTask> results = new ArrayList<InferencingTask>();
		results = inferencingTaskRepository.getAllTaskByUserId(userId);
		return results;
	}

	@Override
	public void save(InferencingTask t) {
		log.info("save task " + t.getTaskId());
		inferencingTaskRepository.saveAndFlush(t);

	}

}
