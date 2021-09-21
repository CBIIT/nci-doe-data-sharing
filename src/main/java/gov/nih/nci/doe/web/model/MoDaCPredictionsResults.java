package gov.nih.nci.doe.web.model;

import java.util.List;

public class MoDaCPredictionsResults {
	private String inputDatasetPath;
	private String predictionsPath;
	private String inputDatasetName;
	private String predictionsName;
	private String modelAnalysisPredName;
	private String modelAnalysisInputDatasetName;
	private List<KeyValueBean> inputDatasetSelfMetadata;
	private List<KeyValueBean> predictionsSelfMetadata;
	private List<KeyValueBean> inputDatasetSystemMetadata;
	private List<KeyValueBean> predictionsSystemMetadata;

	
	
	public String getModelAnalysisPredName() {
		return modelAnalysisPredName;
	}

	public void setModelAnalysisPredName(String modelAnalysisPredName) {
		this.modelAnalysisPredName = modelAnalysisPredName;
	}

	public String getModelAnalysisInputDatasetName() {
		return modelAnalysisInputDatasetName;
	}

	public void setModelAnalysisInputDatasetName(String modelAnalysisInputDatasetName) {
		this.modelAnalysisInputDatasetName = modelAnalysisInputDatasetName;
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

	public List<KeyValueBean> getInputDatasetSelfMetadata() {
		return inputDatasetSelfMetadata;
	}

	public void setInputDatasetSelfMetadata(List<KeyValueBean> inputDatasetSelfMetadata) {
		this.inputDatasetSelfMetadata = inputDatasetSelfMetadata;
	}

	public List<KeyValueBean> getPredictionsSelfMetadata() {
		return predictionsSelfMetadata;
	}

	public void setPredictionsSelfMetadata(List<KeyValueBean> predictionsSelfMetadata) {
		this.predictionsSelfMetadata = predictionsSelfMetadata;
	}

	public List<KeyValueBean> getInputDatasetSystemMetadata() {
		return inputDatasetSystemMetadata;
	}

	public void setInputDatasetSystemMetadata(List<KeyValueBean> inputDatasetSystemMetadata) {
		this.inputDatasetSystemMetadata = inputDatasetSystemMetadata;
	}

	public List<KeyValueBean> getPredictionsSystemMetadata() {
		return predictionsSystemMetadata;
	}

	public void setPredictionsSystemMetadata(List<KeyValueBean> predictionsSystemMetadata) {
		this.predictionsSystemMetadata = predictionsSystemMetadata;
	}

}
