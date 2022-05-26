package gov.nih.nci.doe.web.model;

public class EditCollectionsModel {

	private Integer collectionId;
	private String collectionPath;
	private String collectionType;
	private String permissionRole;
	private String collectionName;

	public Integer getCollectionId() {
		return collectionId;
	}

	public void setCollectionId(Integer collectionId) {
		this.collectionId = collectionId;
	}

	public String getCollectionPath() {
		return collectionPath;
	}

	public void setCollectionPath(String collectionPath) {
		this.collectionPath = collectionPath;
	}

	public String getCollectionType() {
		return collectionType;
	}

	public void setCollectionType(String collectionType) {
		this.collectionType = collectionType;
	}

	public String getPermissionRole() {
		return permissionRole;
	}

	public void setPermissionRole(String permissionRole) {
		this.permissionRole = permissionRole;
	}

	public String getCollectionName() {
		return collectionName;
	}

	public void setCollectionName(String collectionName) {
		this.collectionName = collectionName;
	}

}
