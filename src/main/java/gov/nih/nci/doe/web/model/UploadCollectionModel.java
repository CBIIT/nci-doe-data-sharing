package gov.nih.nci.doe.web.model;

import java.util.List;

public class UploadCollectionModel {

	private String institutionPath;
	private String studyPath;
	private String dataSetPath;
	private List<KeyValueBean> dataObjectsList;
	
	
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
