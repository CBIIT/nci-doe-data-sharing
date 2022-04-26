package gov.nih.nci.doe.web.model;

import java.util.Date;

public class MoDaCPredictionsResults {
	private String inputDatasetPath;
	private String predictionsPath;
	private String inputDatasetName;
	private String predictionsName;

	private String outcomeFileName;
	private String outcomeFilePath;

	private String taskId;
	private Date taskCompletedDate;

	public String getInputDatasetPath() {
		return inputDatasetPath;
	}

	public void setInputDatasetPath(String inputDatasetPath) {
		this.inputDatasetPath = inputDatasetPath;
	}

	public String getPredictionsPath() {
		return predictionsPath;
	}

	public void setPredictionsPath(String predictionsPath) {
		this.predictionsPath = predictionsPath;
	}

	public String getInputDatasetName() {
		return inputDatasetName;
	}

	public void setInputDatasetName(String inputDatasetName) {
		this.inputDatasetName = inputDatasetName;
	}

	public String getPredictionsName() {
		return predictionsName;
	}

	public void setPredictionsName(String predictionsName) {
		this.predictionsName = predictionsName;
	}

	public String getOutcomeFileName() {
		return outcomeFileName;
	}

	public void setOutcomeFileName(String outcomeFileName) {
		this.outcomeFileName = outcomeFileName;
	}

	public String getOutcomeFilePath() {
		return outcomeFilePath;
	}

	public void setOutcomeFilePath(String outcomeFilePath) {
		this.outcomeFilePath = outcomeFilePath;
	}

	public String getTaskId() {
		return taskId;
	}

	public void setTaskId(String taskId) {
		this.taskId = taskId;
	}

	public Date getTaskCompletedDate() {
		return taskCompletedDate;
	}

	public void setTaskCompletedDate(Date taskCompletedDate) {
		this.taskCompletedDate = taskCompletedDate;
	}

}
