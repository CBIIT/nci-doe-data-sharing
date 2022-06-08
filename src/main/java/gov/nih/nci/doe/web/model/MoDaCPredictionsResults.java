package gov.nih.nci.doe.web.model;

import java.util.Date;

public class MoDaCPredictionsResults {
	private String inputDatasetPath;
	private String inputDatasetName;
	private String predictionsPath;
	private String predictionsName;
	private String outcomeFileName;
	private String outcomeFilePath;
	private String taskId;
	private Date taskCompletedDate;
	private Boolean isOwner;
	private String fullName;
	private String predictionFolderPath;
	private Integer predCollId;
	private String predAccessGrps;
	private Boolean isPublic;
	private Boolean isReferenceDataset;

	public String getPredAccessGrps() {
		return predAccessGrps;
	}

	public void setPredAccessGrps(String predAccessGrps) {
		this.predAccessGrps = predAccessGrps;
	}

	public String getPredictionFolderPath() {
		return predictionFolderPath;
	}

	public void setPredictionFolderPath(String predictionFolderPath) {
		this.predictionFolderPath = predictionFolderPath;
	}

	public Integer getPredCollId() {
		return predCollId;
	}

	public void setPredCollId(Integer predCollId) {
		this.predCollId = predCollId;
	}

	public Boolean getIsOwner() {
		return isOwner;
	}

	public void setIsOwner(Boolean isOwner) {
		this.isOwner = isOwner;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

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

	public Boolean getIsPublic() {
		return isPublic;
	}

	public void setIsPublic(Boolean isPublic) {
		this.isPublic = isPublic;
	}

	public Boolean getIsReferenceDataset() {
		return isReferenceDataset;
	}

	public void setIsReferenceDataset(Boolean isReferenceDataset) {
		this.isReferenceDataset = isReferenceDataset;
	}

}
