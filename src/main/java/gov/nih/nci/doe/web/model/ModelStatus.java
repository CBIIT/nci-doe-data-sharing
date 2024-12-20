package gov.nih.nci.doe.web.model;

public class ModelStatus {
	private String status;
	private String startDate;
	private String completedDate;
	private String resultPath;
	private String failureMsg;
	private String inputFile;
	private String outcomeFile;

	public String getOutcomeFile() {
		return outcomeFile;
	}

	public void setOutcomeFile(String outcomeFile) {
		this.outcomeFile = outcomeFile;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public String getStartDate() {
		return startDate;
	}

	public void setStartDate(String startDate) {
		this.startDate = startDate;
	}

	public String getCompletedDate() {
		return completedDate;
	}

	public void setCompletedDate(String completedDate) {
		this.completedDate = completedDate;
	}

	public String getResultPath() {
		return resultPath;
	}

	public void setResultPath(String resultPath) {
		this.resultPath = resultPath;
	}

	public String getFailureMsg() {
		return failureMsg;
	}

	public void setFailureMsg(String failureMsg) {
		this.failureMsg = failureMsg;
	}

	public String getInputFile() {
		return inputFile;
	}

	public void setInputFile(String inputFile) {
		this.inputFile = inputFile;
	}

}
