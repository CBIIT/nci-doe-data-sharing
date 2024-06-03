package gov.nih.nci.doe.web.model;

import java.util.Date;

public class PredictionTaskNotification {

	private String userId;
	private String status;
	private String resultPath;
	private String inputDataset;
	private Date completedDate;
	private String failureMsg;
	private String taskId;
	private String displayStatus;

	public String getDisplayStatus() {
		return displayStatus;
	}

	public void setDisplayStatus(String displayStatus) {
		this.displayStatus = displayStatus;
	}

	public String getTaskId() {
		return taskId;
	}

	public void setTaskId(String taskId) {
		this.taskId = taskId;
	}

	public String getUserId() {
		return userId;
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public String getResultPath() {
		return resultPath;
	}

	public void setResultPath(String resultPath) {
		this.resultPath = resultPath;
	}

	public String getInputDataset() {
		return inputDataset;
	}

	public void setInputDataset(String inputDataset) {
		this.inputDataset = inputDataset;
	}

	public Date getCompletedDate() {
		return completedDate;
	}

	public void setCompletedDate(Date completedDate) {
		this.completedDate = completedDate;
	}

	public String getFailureMsg() {
		return failureMsg;
	}

	public void setFailureMsg(String failureMsg) {
		this.failureMsg = failureMsg;
	}

}
