package gov.nih.nci.doe.web.model;

import java.util.List;

import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntries;

public class HpcDatafileSearchResultDetailed {
	private String path;
	private String uuid;
	private String registeredBy;
	private String collectionType;
	private String createdOn;
	private String checksum;
	private String download;
	private String permission;
	private String name;
	private String fileSize;
	private Integer fileSizeActual;
	private List<KeyValueBean> selfMetadata;
	private List<KeyValueBean> systemMetadata;
	HpcMetadataEntries metadataEntries;
	
	 
	public Integer getFileSizeActual() {
		return fileSizeActual;
	}

	public void setFileSizeActual(Integer fileSizeActual) {
		this.fileSizeActual = fileSizeActual;
	}

	public String getFileSize() {
		return fileSize;
	}

	public void setFileSize(String fileSize) {
		this.fileSize = fileSize;
	}

	public String getPermission() {
		return permission;
	}

	public void setPermission(String permission) {
		this.permission = permission;
	}

	public String getDownload() {
		return download;
	}

	public void setDownload(String download) {
		this.download = download;
	}

	public String getChecksum() {
		return checksum;
	}

	public void setChecksum(String checksum) {
		this.checksum = checksum;
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public String getUuid() {
		return uuid;
	}

	public void setUuid(String uuid) {
		this.uuid = uuid;
	}

	public String getRegisteredBy() {
		return registeredBy;
	}

	public void setRegisteredBy(String registeredBy) {
		this.registeredBy = registeredBy;
	}

	public String getCollectionType() {
		return collectionType;
	}

	public void setCollectionType(String collectionType) {
		this.collectionType = collectionType;
	}

	public String getCreatedOn() {
		return createdOn;
	}

	public void setCreatedOn(String createdOn) {
		this.createdOn = createdOn;
	}

	public HpcMetadataEntries getMetadataEntries() {
		return metadataEntries;
	}

	public void setMetadataEntries(HpcMetadataEntries metadataEntries) {
		this.metadataEntries = metadataEntries;
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

