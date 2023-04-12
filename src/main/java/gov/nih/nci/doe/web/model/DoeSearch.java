package gov.nih.nci.doe.web.model;

public class DoeSearch {

	private String[] rowId;
	private String[] level;
	private String[] operator;
	private String[] attrName;
	private String[] attrValue;
	private String attrValuesString;
	private boolean detailed;
	private int pageNumber = 1;
	private boolean[] isExcludeParentMetadata;
	private boolean[] iskeyWordSearch;
	private String searchName;
	private String isShowMyCollection;

	public String getIsShowMyCollection() {
		return isShowMyCollection;
	}

	public void setIsShowMyCollection(String isShowMyCollection) {
		this.isShowMyCollection = isShowMyCollection;
	}

	public String getAttrValuesString() {
		return attrValuesString;
	}

	public void setAttrValuesString(String attrValuesString) {
		this.attrValuesString = attrValuesString;
	}

	public String getSearchName() {
		return searchName;
	}

	public void setSearchName(String searchName) {
		this.searchName = searchName;
	}

	public boolean[] getIskeyWordSearch() {
		return iskeyWordSearch;
	}

	public void setIskeyWordSearch(boolean[] iskeyWordSearch) {
		this.iskeyWordSearch = iskeyWordSearch;
	}

	public String[] getRowId() {
		return rowId;
	}

	public void setRowId(String[] rowId) {
		this.rowId = rowId;
	}

	public String[] getAttrName() {
		return attrName;
	}

	public void setAttrName(String[] attrName) {
		this.attrName = attrName;
	}

	public String[] getAttrValue() {
		if (attrValue != null) {
			return attrValue;
		}

		// Spring boot split by comma is causing an issue when the attribute value
		// contains commas, so using a different escape pattern to split the string
		// array of attribute values.
		return attrValuesString.split("@@");

	}

	public void setAttrValue(String[] attrValue) {
		this.attrValue = attrValue;
	}

	public void setLevel(String[] level) {
		this.level = level;
	}

	public String[] getLevel() {
		return level;
	}

	public boolean isDetailed() {
		return detailed;
	}

	public void setDetailed(boolean detailed) {
		this.detailed = detailed;
	}

	public int getPageNumber() {
		return pageNumber;
	}

	public void setPageNumber(int pageNumber) {
		this.pageNumber = pageNumber;
	}

	public boolean[] getIsExcludeParentMetadata() {
		return isExcludeParentMetadata;
	}

	public void setIsExcludeParentMetadata(boolean[] isExcludeParentMetadata) {
		this.isExcludeParentMetadata = isExcludeParentMetadata;
	}

	public String[] getOperator() {
		return operator;
	}

	public void setOperator(String[] operator) {
		this.operator = operator;
	}

}
