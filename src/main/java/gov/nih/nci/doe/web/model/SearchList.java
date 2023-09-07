package gov.nih.nci.doe.web.model;

import java.util.List;

public class SearchList {

	private String attributeName;

	private Integer attrCount;

	private List<String> attrValues;

	private Integer datasetCount;

	private Integer modelCount;

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

	public Integer getAttrCount() {
		return attrCount;
	}

	public void setAttrCount(Integer attrCount) {
		this.attrCount = attrCount;
	}

}
