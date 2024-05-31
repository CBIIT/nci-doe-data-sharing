package gov.nih.nci.doe.web.model;

import java.util.Date;

public class UploadTaskNotification {

	private String userId;
	private String taskId;
	private String status;
	private String registrationItems;
	private String errorMsg;
	private Date completedDate;

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public Date getCompletedDate() {
		return completedDate;
	}

	public void setCompletedDate(Date completedDate) {
		this.completedDate = completedDate;
	}

	public String getErrorMsg() {
		return errorMsg;
	}

	public void setErrorMsg(String errorMsg) {
		this.errorMsg = errorMsg;
	}

	public String getUserId() {
		return userId;
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	public String getTaskId() {
		return taskId;
	}

	public void setTaskId(String taskId) {
		this.taskId = taskId;
	}

	public String getRegistrationItems() {
		return registrationItems;
	}

	public void setRegistrationItems(String registrationItems) {
		this.registrationItems = registrationItems;
	}

}
