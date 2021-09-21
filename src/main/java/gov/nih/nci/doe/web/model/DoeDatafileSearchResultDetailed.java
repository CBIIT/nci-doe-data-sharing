package gov.nih.nci.doe.web.model;

import java.util.List;

public class DoeDatafileSearchResultDetailed {
	private String path;
	private String name;
	private String fileSize;
	private List<KeyValueBean> selfMetadata;
	private List<KeyValueBean> systemMetadata;
	private Boolean isFolder;
	private List<DoeDatafileSearchResultDetailed> filesList;
	private Integer collectionId;

	public Integer getCollectionId() {
		return collectionId;
	}

	public void setCollectionId(Integer collectionId) {
		this.collectionId = collectionId;
	}

	public List<DoeDatafileSearchResultDetailed> getFilesList() {
		return filesList;
	}

	public void setFilesList(List<DoeDatafileSearchResultDetailed> filesList) {
		this.filesList = filesList;
	}

	public Boolean getIsFolder() {
		return isFolder;
	}

	public void setIsFolder(Boolean isFolder) {
		this.isFolder = isFolder;
	}

	public String getFileSize() {
		return fileSize;
	}

	public void setFileSize(String fileSize) {
		this.fileSize = fileSize;
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public List<KeyValueBean> getSelfMetadata() {
		return selfMetadata;
	}

	public void setSelfMetadata(List<KeyValueBean> selfMetadata) {
		this.selfMetadata = selfMetadata;
	}

	public List<KeyValueBean> getSystemMetadata() {
		return systemMetadata;
	}

	public void setSystemMetadata(List<KeyValueBean> systemMetadata) {
		this.systemMetadata = systemMetadata;
	}

}
