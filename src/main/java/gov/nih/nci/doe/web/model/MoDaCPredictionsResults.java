package gov.nih.nci.doe.web.model;

public class MoDaCPredictionsResults {
	private String inputDatasetPath;
	private String predictionsPath;
	private String inputDatasetName;
	private String predictionsName;
	private Integer predCollectionId;
	private Integer inputDatasetCollectionId;

	public Integer getPredCollectionId() {
		return predCollectionId;
	}

	public void setPredCollectionId(Integer predCollectionId) {
		this.predCollectionId = predCollectionId;
	}

	public Integer getInputDatasetCollectionId() {
		return inputDatasetCollectionId;
	}

	public void setInputDatasetCollectionId(Integer inputDatasetCollectionId) {
		this.inputDatasetCollectionId = inputDatasetCollectionId;
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

}
