package gov.nih.nci.doe.web.model;

public class InferencingTaskModel {

	private String taskId;
	private String userId;
	private String resultPath;
	private String assetPath;
	private String testInputPath;
	private String modelPath;
	private String uploadFrom;
	private String outcomeFileName;
	private String applicableModelNamesList;
	private String outcomeFilePath;
	private Boolean isReferenceAsset;
	private String referenceDatasetList;

	public String getReferenceDatasetList() {
		return referenceDatasetList;
	}

	public void setReferenceDatasetList(String referenceDatasetList) {
		this.referenceDatasetList = referenceDatasetList;
	}

	public String getApplicableModelNamesList() {
		return applicableModelNamesList;
	}

	public void setApplicableModelNamesList(String applicableModelNamesList) {
		this.applicableModelNamesList = applicableModelNamesList;
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

	public String getResultPath() {
		return resultPath;
	}

	public void setResultPath(String resultPath) {
		this.resultPath = resultPath;
	}

	public String getAssetPath() {
		return assetPath;
	}

	public void setAssetPath(String assetPath) {
		this.assetPath = assetPath;
	}

	public String getTestInputPath() {
		return testInputPath;
	}

	public void setTestInputPath(String testInputPath) {
		this.testInputPath = testInputPath;
	}

	public String getModelPath() {
		return modelPath;
	}

	public void setModelPath(String modelPath) {
		this.modelPath = modelPath;
	}

	public String getUploadFrom() {
		return uploadFrom;
	}

	public void setUploadFrom(String uploadFrom) {
		this.uploadFrom = uploadFrom;
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

	public Boolean getIsReferenceAsset() {
		return isReferenceAsset;
	}

	public void setIsReferenceAsset(Boolean isReferenceAsset) {
		this.isReferenceAsset = isReferenceAsset;
	}

}
