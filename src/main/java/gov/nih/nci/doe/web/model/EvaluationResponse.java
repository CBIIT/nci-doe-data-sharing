package gov.nih.nci.doe.web.model;

import java.util.List;

public class EvaluationResponse {

	public String message;
	public List<String> taskId;

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public List<String> getTaskId() {
		return taskId;
	}

	public void setTaskId(List<String> taskId) {
		this.taskId = taskId;
	}

}
