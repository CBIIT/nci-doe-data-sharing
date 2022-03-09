package gov.nih.nci.doe.web.model;

public class InferencingTaskModel {

	private String taskId;
	private String userId;
	private String resultPath;
	private String assetPath;
	private String testInputPath;
	private String modelPath;
	private String uploadFrom;
	private String outputResultName;
	private String applicableModelNames;

	public String getApplicableModelNames() {
		return applicableModelNames;
	}

	public void setApplicableModelNames(String applicableModelNames) {
		this.applicableModelNames = applicableModelNames;
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

	public String getOutputResultName() {
		return outputResultName;
	}

	public void setOutputResultName(String outputResultName) {
		this.outputResultName = outputResultName;
	}

}
