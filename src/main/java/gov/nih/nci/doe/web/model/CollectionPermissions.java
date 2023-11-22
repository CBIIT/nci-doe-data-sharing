package gov.nih.nci.doe.web.model;

import java.util.List;

public class CollectionPermissions {

	public String owner;

	public List<String> grpList;

	public String getOwner() {
		return owner;
	}

	public void setOwner(String owner) {
		this.owner = owner;
	}

	public List<String> getGrpList() {
		return grpList;
	}

	public void setGrpList(List<String> grpList) {
		this.grpList = grpList;
	}

}
