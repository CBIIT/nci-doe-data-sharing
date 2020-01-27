package gov.nih.nci.doe.web.model;

import java.util.List;

public class DoeSearchResult {

    private String dataSetName;
    private String dataSetDescription;
    private Integer numOfDataSets;
    private String dataSetPath;
    private String studyName;
    private String instituteName;
    private List<KeyValueBean> selfMetadata;
    private List<KeyValueBean> studyUserMetadata;
    private List<KeyValueBean> instituteUserMetadata;
    
    
    
	public String getDataSetName() {
		return dataSetName;
	}
	public void setDataSetName(String dataSetName) {
		this.dataSetName = dataSetName;
	}
	public String getDataSetDescription() {
		return dataSetDescription;
	}
	public void setDataSetDescription(String dataSetDescription) {
		this.dataSetDescription = dataSetDescription;
	}
	public Integer getNumOfDataSets() {
		return numOfDataSets;
	}
	public void setNumOfDataSets(Integer numOfDataSets) {
		this.numOfDataSets = numOfDataSets;
	}
	public String getStudyName() {
		return studyName;
	}
	public void setStudyName(String studyName) {
		this.studyName = studyName;
	}
	
	public String getInstituteName() {
		return instituteName;
	}
	public void setInstituteName(String instituteName) {
		this.instituteName = instituteName;
	}
	public List<KeyValueBean> getSelfMetadata() {
		return selfMetadata;
	}
	public void setSelfMetadata(List<KeyValueBean> selfMetadata) {
		this.selfMetadata = selfMetadata;
	}
	public List<KeyValueBean> getStudyUserMetadata() {
		return studyUserMetadata;
	}
	public void setStudyUserMetadata(List<KeyValueBean> studyUserMetadata) {
		this.studyUserMetadata = studyUserMetadata;
	}
	public List<KeyValueBean> getInstituteUserMetadata() {
		return instituteUserMetadata;
	}
	public void setInstituteUserMetadata(List<KeyValueBean> instituteUserMetadata) {
		this.instituteUserMetadata = instituteUserMetadata;
	}
	public String getDataSetPath() {
		return dataSetPath;
	}
	public void setDataSetPath(String dataSetPath) {
		this.dataSetPath = dataSetPath;
	}
	
    
    
    
}
