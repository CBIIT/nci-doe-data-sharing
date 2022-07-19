package gov.nih.nci.doe.web.domain;

import java.util.Date;

public class ModelStatus {
	private String status;
	private Date startDate;
	private Date completedDate;
	private String resultPath;
	private String failureMsg;
	private String inputDataSetPath;

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public Date getStartDate() {
		return startDate;
	}

	public void setStartDate(Date startDate) {
		this.startDate = startDate;
	}

	public Date getCompletedDate() {
		return completedDate;
	}

	public void setCompletedDate(Date completedDate) {
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

	public String getInputDataSetPath() {
		return inputDataSetPath;
	}

	public void setInputDataSetPath(String inputDataSetPath) {
		this.inputDataSetPath = inputDataSetPath;
	}

}
