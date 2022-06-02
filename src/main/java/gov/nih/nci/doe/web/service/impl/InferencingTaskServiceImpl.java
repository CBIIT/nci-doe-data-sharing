package gov.nih.nci.doe.web.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import gov.nih.nci.doe.web.domain.InferencingTask;
import gov.nih.nci.doe.web.model.InferencingTaskModel;
import gov.nih.nci.doe.web.repository.InferencingTaskRepository;
import gov.nih.nci.doe.web.service.InferencingTaskService;

@Component
@Transactional
public class InferencingTaskServiceImpl implements InferencingTaskService {

	private static final Logger log = LoggerFactory.getLogger(InferencingTaskServiceImpl.class);

	@Autowired
	InferencingTaskRepository inferencingTaskRepository;

	@Override
	public void saveInferenceTask(InferencingTaskModel inference) {

		log.info("save inference task");
		InferencingTask t = new InferencingTask();
		String assetPath = inference.getAssetPath();
		if (StringUtils.isNotEmpty(assetPath)) {
			String assetIdentifier = assetPath.substring(assetPath.lastIndexOf('/') + 1, assetPath.length());
			t.setModelIdentifier(assetIdentifier);
		}

		t.setUserId(inference.getUserId());
		t.setTaskId(inference.getTaskId());
		t.setAssetPath(assetPath);
		t.setResultPath(inference.getResultPath());
		t.setTestDataSetPath(inference.getTestInputPath());
		t.setStartDate(new Date());
		t.setModelh5Path(inference.getModelPath());
		t.setUploadFrom(inference.getUploadFrom() != null ? inference.getUploadFrom() : "gdcData");
		t.setActualResultsFileName(inference.getOutcomeFileName());
		t.setStatus("NOTSTARTED");
		t.setIsReferenceAsset(inference.getIsReferenceAsset());
		t.setOutcomeFilePath(inference.getOutcomeFilePath());
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

	@Override
	public InferencingTask getInferenceByUserIdAndPredName(String userId, String resultPath) {
		log.info("get inference for user: " + userId + " and pred path: " + resultPath);
		return inferencingTaskRepository.getInferenceByUserIdAndPredName(userId, resultPath);

	}

	@Override
	public InferencingTask getInferenceByUserIdAndOutcomeName(String userId, String assetPath, String outcomeName) {
		log.info("get inference for user: " + userId + " and outcomeName: " + outcomeName);
		return inferencingTaskRepository.getInferenceByUserIdAndOutcomeName(userId, assetPath, outcomeName);
	}

	@Override
	public InferencingTask getInferenceByUserIdAndInputName(String userId, String assetPath, String inputFileName) {
		log.info("get inference for user: " + userId + " and input path: " + inputFileName);
		return inferencingTaskRepository.getInferenceByUserIdAndInputName(userId, assetPath, inputFileName);
	}

	@Override
	public Boolean checkifFileExistsForUser(String user, String modelPath, String inputFileName) {
		log.info("get inference for user : " + user + " and model path: " + modelPath + " and input file: "
				+ inputFileName);
		List<InferencingTask> inferenceList = inferencingTaskRepository.getInferenceByUserIdAndModelPath(user,
				modelPath);
		Boolean isFileExists = false;
		if (CollectionUtils.isNotEmpty(inferenceList)) {

			isFileExists = inferenceList.stream().anyMatch(g -> g.getTestDataSetPath()
					.substring(g.getTestDataSetPath().lastIndexOf('/') + 1).equals(inputFileName));

		}

		return isFileExists;
	}

	@Override
	public InferencingTask getInferenceByTaskId(String taskId) {
		log.info("get inference by task Id: " + taskId);
		return inferencingTaskRepository.getInferenceTask(taskId);
	}

	@Override
	public void deleteInferenceByTaskId(String taskId) {
		log.info("delete inference by task Id: " + taskId);
		InferencingTask task = inferencingTaskRepository.getInferenceTask(taskId);
		inferencingTaskRepository.delete(task);
	}

}
