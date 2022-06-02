package gov.nih.nci.doe.web.model;

public class DeletePredictionsModel {

	public String deletepaths;
	public String taskId;
	public String predCollectionPath;

	public String getDeletepaths() {
		return deletepaths;
	}

	public void setDeletepaths(String deletepaths) {
		this.deletepaths = deletepaths;
	}

	public String getTaskId() {
		return taskId;
	}

	public void setTaskId(String taskId) {
		this.taskId = taskId;
	}

	public String getPredCollectionPath() {
		return predCollectionPath;
	}

	public void setPredCollectionPath(String predCollectionPath) {
		this.predCollectionPath = predCollectionPath;
	}

}
