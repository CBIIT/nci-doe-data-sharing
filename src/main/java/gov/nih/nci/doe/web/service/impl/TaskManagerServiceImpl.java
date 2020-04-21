package gov.nih.nci.doe.web.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;


import gov.nih.nci.doe.web.domain.TaskManager;
import gov.nih.nci.doe.web.repository.TaskManagerRepository;
import gov.nih.nci.doe.web.service.TaskManagerService;

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
	public void saveTransfer(String taskId, String transferType, String name, String userId) {
		log.info("saving task for user " + userId + " with task id: " + taskId + " and transfer type: " +  transferType);
		
		 TaskManager task = new TaskManager();
		 task.setTaskDate(new Date());
		 task.setTaskId(taskId);
		 task.setTaskName(name);
		 task.setUserId(userId);
		 task.setTaskType(transferType);
		 taskManagerRepository.saveAndFlush(task);
	}

	@Override
	public List<TaskManager> getAllByUserId(String userId) {
		log.info("get all Tasks for user :" + userId);
		return taskManagerRepository.findAllByUserId(userId);
	}
	  

}
