package gov.nih.nci.doe.web.model;

public class PermissionsModel {

	private String dataLevelAccessGroups;
	private String studyLevelAccessGroups;
	private String programLevelAccessGroups;
	private String dataCollectionId;
	private String studyCollectionId;
	private String progCollectionId;
	private String selectedAccessGroups;
	private String selectedCollection;
	private String selectedEntry;
	private String path;
	private String parentAccessGroups;
	private Integer selectedCollectionId;

	public Integer getSelectedCollectionId() {
		return selectedCollectionId;
	}

	public void setSelectedCollectionId(Integer selectedCollectionId) {
		this.selectedCollectionId = selectedCollectionId;
	}

	public String getParentAccessGroups() {
		return parentAccessGroups;
	}

	public void setParentAccessGroups(String parentAccessGroups) {
		this.parentAccessGroups = parentAccessGroups;
	}

	public String getSelectedEntry() {
		return selectedEntry;
	}

	public void setSelectedEntry(String selectedEntry) {
		this.selectedEntry = selectedEntry;
	}

	public String getSelectedCollection() {
		return selectedCollection;
	}

	public void setSelectedCollection(String selectedCollection) {
		this.selectedCollection = selectedCollection;
	}

	public String getDataLevelAccessGroups() {
		return dataLevelAccessGroups;
	}

	public void setDataLevelAccessGroups(String dataLevelAccessGroups) {
		this.dataLevelAccessGroups = dataLevelAccessGroups;
	}

	public String getStudyLevelAccessGroups() {
		return studyLevelAccessGroups;
	}

	public void setStudyLevelAccessGroups(String studyLevelAccessGroups) {
		this.studyLevelAccessGroups = studyLevelAccessGroups;
	}

	public String getProgramLevelAccessGroups() {
		return programLevelAccessGroups;
	}

	public void setProgramLevelAccessGroups(String programLevelAccessGroups) {
		this.programLevelAccessGroups = programLevelAccessGroups;
	}

	public String getDataCollectionId() {
		return dataCollectionId;
	}

	public void setDataCollectionId(String dataCollectionId) {
		this.dataCollectionId = dataCollectionId;
	}

	public String getStudyCollectionId() {
		return studyCollectionId;
	}

	public void setStudyCollectionId(String studyCollectionId) {
		this.studyCollectionId = studyCollectionId;
	}

	public String getProgCollectionId() {
		return progCollectionId;
	}

	public void setProgCollectionId(String progCollectionId) {
		this.progCollectionId = progCollectionId;
	}

	public String getSelectedAccessGroups() {
		return selectedAccessGroups;
	}

	public void setSelectedAccessGroups(String selectedAccessGroups) {
		this.selectedAccessGroups = selectedAccessGroups;
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

}
