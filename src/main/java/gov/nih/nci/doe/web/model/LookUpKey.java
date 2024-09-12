package gov.nih.nci.doe.web.model;

import java.util.Objects;

public class LookUpKey {

	private String levelName;
	private String attrName;

	public String getLevelName() {
		return levelName;
	}

	public void setLevelName(String levelName) {
		this.levelName = levelName;
	}

	public String getAttrName() {
		return attrName;
	}

	public void setAttrName(String attrName) {
		this.attrName = attrName;
	}

	public LookUpKey(String levelName, String attrName) {
		this.levelName = levelName;
		this.attrName = attrName;
	}

	@Override
	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (o == null || getClass() != o.getClass())
			return false;
		LookUpKey that = (LookUpKey) o;
		return levelName.equals(that.levelName) && attrName.equals(that.attrName);
	}

	@Override
	public int hashCode() {
		return Objects.hash(levelName, attrName);
	}

}
