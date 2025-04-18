package gov.nih.nci.doe.web.model;

public class DoeSearchResult {

	private String dataSetName;
	private String dataSetDescription;
	private Integer numOfDataSets;
	private String dataSetPath;
	private String studyName;
	private String programName;
	private String studyPath;
	private String programPath;
	private String dataSetPermissionRole;
	private String studyPermissionRole;
	private String programPermissionRole;
	private Integer dataSetCollectionId;
	private Integer studyCollectionId;
	private Integer programCollectionId;
	private String dataSetdmeDataId;
	private String assetType;
	private String dmeDataId;
	private Boolean isBulkAsset;
	private String isReferenceDataset;
	private String displayAssetSize;
	private Long collectionSize;

	public String getDisplayAssetSize() {
		return displayAssetSize;
	}

	public void setDisplayAssetSize(String displayAssetSize) {
		this.displayAssetSize = displayAssetSize;
	}

	public Long getCollectionSize() {
		return collectionSize;
	}

	public void setCollectionSize(Long collectionSize) {
		this.collectionSize = collectionSize;
	}

	public String getIsReferenceDataset() {
		return isReferenceDataset;
	}

	public void setIsReferenceDataset(String isReferenceDataset) {
		this.isReferenceDataset = isReferenceDataset;
	}

	public Boolean getIsBulkAsset() {
		return isBulkAsset;
	}

	public void setIsBulkAsset(Boolean isBulkAsset) {
		this.isBulkAsset = isBulkAsset;
	}

	public String getDmeDataId() {
		return dmeDataId;
	}

	public void setDmeDataId(String dmeDataId) {
		this.dmeDataId = dmeDataId;
	}

	public String getAssetType() {
		return assetType;
	}

	public void setAssetType(String assetType) {
		this.assetType = assetType;
	}

	public String getDataSetdmeDataId() {
		return dataSetdmeDataId;
	}

	public void setDataSetdmeDataId(String dataSetdmeDataId) {
		this.dataSetdmeDataId = dataSetdmeDataId;
	}

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

	public String getProgramName() {
		return programName;
	}

	public void setProgramName(String programName) {
		this.programName = programName;
	}

	public String getDataSetPath() {
		return dataSetPath;
	}

	public void setDataSetPath(String dataSetPath) {
		this.dataSetPath = dataSetPath;
	}

	public String getStudyPath() {
		return studyPath;
	}

	public void setStudyPath(String studyPath) {
		this.studyPath = studyPath;
	}

	public String getProgramPath() {
		return programPath;
	}

	public void setProgramPath(String programPath) {
		this.programPath = programPath;
	}

	public String getDataSetPermissionRole() {
		return dataSetPermissionRole;
	}

	public void setDataSetPermissionRole(String dataSetPermissionRole) {
		this.dataSetPermissionRole = dataSetPermissionRole;
	}

	public String getStudyPermissionRole() {
		return studyPermissionRole;
	}

	public void setStudyPermissionRole(String studyPermissionRole) {
		this.studyPermissionRole = studyPermissionRole;
	}

	public String getProgramPermissionRole() {
		return programPermissionRole;
	}

	public void setProgramPermissionRole(String programPermissionRole) {
		this.programPermissionRole = programPermissionRole;
	}

	public Integer getDataSetCollectionId() {
		return dataSetCollectionId;
	}

	public void setDataSetCollectionId(Integer dataSetCollectionId) {
		this.dataSetCollectionId = dataSetCollectionId;
	}

	public Integer getStudyCollectionId() {
		return studyCollectionId;
	}

	public void setStudyCollectionId(Integer studyCollectionId) {
		this.studyCollectionId = studyCollectionId;
	}

	public Integer getProgramCollectionId() {
		return programCollectionId;
	}

	public void setProgramCollectionId(Integer programCollectionId) {
		this.programCollectionId = programCollectionId;
	}

}