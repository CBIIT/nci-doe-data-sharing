package gov.nih.nci.doe.web.model;

public class DoeSearch {

	private String[] rowId;
	private String[] level;
	private String[] operator;
	private String[] attrName;
	private String[] attrValue;
	private boolean detailed;
	private String searchType;
	private int pageNumber=1;
	private int pageSize=100;
	private boolean[] isExcludeParentMetadata;
	private boolean[] iskeyWordSearch;
	private boolean[] isAdvancedSearch;
	
	
	
	

	public boolean[] getIskeyWordSearch() {
		return iskeyWordSearch;
	}

	public void setIskeyWordSearch(boolean[] iskeyWordSearch) {
		this.iskeyWordSearch = iskeyWordSearch;
	}

	public boolean[] getIsAdvancedSearch() {
		return isAdvancedSearch;
	}

	public void setIsAdvancedSearch(boolean[] isAdvancedSearch) {
		this.isAdvancedSearch = isAdvancedSearch;
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
		return attrValue;
	}

	public void setAttrValue(String[] attrValue) {
		this.attrValue = attrValue;
	}

	public String getSearchType() {
		return searchType;
	}

	public void setSearchType(String searchType) {
		this.searchType = searchType;
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

    public int getPageSize() {
      return pageSize;
    }

    public void setPageSize(int pageSize) {
      this.pageSize = pageSize;
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
