package gov.nih.nci.doe.web.model;

import java.util.List;

public class SearchList {

	private String attributeName;
	private List<String> attrValues;
	private Integer datasetCount;
	private Integer modelCount;
	private Integer referenceDatasetCount;
	private Integer nonReferenceDatasetCount;
	private Integer modelDeployedCount;
	private Integer modelNotDeployedCount;

	public Integer getDatasetCount() {
		return datasetCount;
	}

	public void setDatasetCount(Integer datasetCount) {
		this.datasetCount = datasetCount;
	}

	public Integer getModelCount() {
		return modelCount;
	}

	public void setModelCount(Integer modelCount) {
		this.modelCount = modelCount;
	}

	public String getAttributeName() {
		return attributeName;
	}

	public void setAttributeName(String attributeName) {
		this.attributeName = attributeName;
	}

	public List<String> getAttrValues() {
		return attrValues;
	}

	public void setAttrValues(List<String> attrValues) {
		this.attrValues = attrValues;
	}

	public Integer getReferenceDatasetCount() {
		return referenceDatasetCount;
	}

	public void setReferenceDatasetCount(Integer referenceDatasetCount) {
		this.referenceDatasetCount = referenceDatasetCount;
	}

	public Integer getNonReferenceDatasetCount() {
		return nonReferenceDatasetCount;
	}

	public void setNonReferenceDatasetCount(Integer nonReferenceDatasetCount) {
		this.nonReferenceDatasetCount = nonReferenceDatasetCount;
	}

	public Integer getModelDeployedCount() {
		return modelDeployedCount;
	}

	public void setModelDeployedCount(Integer modelDeployedCount) {
		this.modelDeployedCount = modelDeployedCount;
	}

	public Integer getModelNotDeployedCount() {
		return modelNotDeployedCount;
	}

	public void setModelNotDeployedCount(Integer modelNotDeployedCount) {
		this.modelNotDeployedCount = modelNotDeployedCount;
	}

}
