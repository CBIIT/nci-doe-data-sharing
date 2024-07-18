package gov.nih.nci.doe.web.model;

import java.util.List;

public class UploadCollectionModel {

	private String programPath;
	private String studyPath;
	private String dataSetPath;
	private List<KeyValueBean> dataObjectsList;
	private String action;
	private String uploadType;
	private String uploadPath;

	public String getUploadPath() {
		return uploadPath;
	}

	public void setUploadPath(String uploadPath) {
		this.uploadPath = uploadPath;
	}

	public String getUploadType() {
		return uploadType;
	}

	public void setUploadType(String uploadType) {
		this.uploadType = uploadType;
	}

	public String getAction() {
		return action;
	}

	public void setAction(String action) {
		this.action = action;
	}

	public String getProgramPath() {
		return programPath;
	}

	public void setProgramPath(String programPath) {
		this.programPath = programPath;
	}

	public String getStudyPath() {
		return studyPath;
	}

	public void setStudyPath(String studyPath) {
		this.studyPath = studyPath;
	}

	public String getDataSetPath() {
		return dataSetPath;
	}

	public void setDataSetPath(String dataSetPath) {
		this.dataSetPath = dataSetPath;
	}

	public List<KeyValueBean> getDataObjectsList() {
		return dataObjectsList;
	}

	public void setDataObjectsList(List<KeyValueBean> dataObjectsList) {
		this.dataObjectsList = dataObjectsList;
	}

}
