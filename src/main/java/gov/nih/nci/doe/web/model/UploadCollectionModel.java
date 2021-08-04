package gov.nih.nci.doe.web.model;

import java.util.List;

public class UploadCollectionModel {

	private String institutionPath;
	private String studyPath;
	private String dataSetPath;
	private List<KeyValueBean> dataObjectsList;
	private String action;
	private String uploadType;

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

	public String getInstitutionPath() {
		return institutionPath;
	}

	public void setInstitutionPath(String institutionPath) {
		this.institutionPath = institutionPath;
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
