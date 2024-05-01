package gov.nih.nci.doe.web.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import gov.nih.nci.doe.web.domain.TaskManager;
import gov.nih.nci.doe.web.repository.TaskManagerRepository;
import gov.nih.nci.doe.web.service.TaskManagerService;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component
public class TaskManagerServiceImpl implements TaskManagerService {

	private static final Logger log = LoggerFactory.getLogger(TaskManagerServiceImpl.class);

	@Autowired
	private TaskManagerRepository taskManagerRepository;

	@Override
	@Transactional(readOnly = false)
	public void saveTransfer(String taskId, String transferType, String type, String name, String userId) {
		log.info("saving task for user " + userId + " with Task ID: " + taskId + " and transfer type: " + transferType);

		TaskManager task = new TaskManager();
		task.setTaskDate(new Date());
		task.setTaskId(taskId);
		task.setTaskName(name);
		task.setUserId(userId);
		task.setType(type);
		task.setTaskType(transferType);
		taskManagerRepository.saveAndFlush(task);
	}

	@Override
	public List<TaskManager> getAllByUserId(String userId) {
		log.info("get all Tasks for user :" + userId);
		List<TaskManager> results = new ArrayList<TaskManager>();
		results = taskManagerRepository.findAllByUserId(userId);
		return results;
	}

	public List<TaskManager> getTaskDetails(String userId, String name) {
		log.info("get all Tasks for user and task name :" + userId + " " + name);
		return taskManagerRepository.getTaskDetails(userId, name);
	}

	@Override
	public List<TaskManager> getAlltasks() {
		log.info("get all Tasks");
		return taskManagerRepository.getAllTasks();
	}
}
