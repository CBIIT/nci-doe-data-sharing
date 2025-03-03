package gov.nih.nci.doe.web.model;

import java.util.List;

public class DoeMetadataAttrEntry {
	private String attrName;
	private String attrValue;
	private String attrUnit;
	private boolean systemAttr;
	private List<KeyValueBean> validValues = null;
	private String defaultValue;
	private String description;
	private Boolean mandatory;
	private String displayName;
	private Boolean isEditable;
	private Integer displayOrder;
	private Boolean controllerAttribute;
	private Boolean isVisible;
	private Boolean isVisibleOnUplaodPage;
	private Boolean isVisibleForReviewCommiteeMember;
	private String controllerAttrName;
	private String controllerAttrValue;

	public String getControllerAttrName() {
		return controllerAttrName;
	}

	public void setControllerAttrName(String controllerAttrName) {
		this.controllerAttrName = controllerAttrName;
	}

	public String getControllerAttrValue() {
		return controllerAttrValue;
	}

	public void setControllerAttrValue(String controllerAttrValue) {
		this.controllerAttrValue = controllerAttrValue;
	}

	public Boolean getIsVisibleForReviewCommiteeMember() {
		return isVisibleForReviewCommiteeMember;
	}

	public void setIsVisibleForReviewCommiteeMember(Boolean isVisibleForReviewCommiteeMember) {
		this.isVisibleForReviewCommiteeMember = isVisibleForReviewCommiteeMember;
	}

	public Boolean getIsVisibleOnUplaodPage() {
		return isVisibleOnUplaodPage;
	}

	public void setIsVisibleOnUplaodPage(Boolean isVisibleOnUplaodPage) {
		this.isVisibleOnUplaodPage = isVisibleOnUplaodPage;
	}

	public Integer getDisplayOrder() {
		return displayOrder;
	}

	public void setDisplayOrder(Integer displayOrder) {
		this.displayOrder = displayOrder;
	}

	public Boolean getIsEditable() {
		return isEditable;
	}

	public void setIsEditable(Boolean isEditable) {
		this.isEditable = isEditable;
	}

	public String getDisplayName() {
		return displayName;
	}

	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	public Boolean getMandatory() {
		return mandatory;
	}

	public void setMandatory(Boolean mandatory) {
		this.mandatory = mandatory;
	}

	public List<KeyValueBean> getValidValues() {
		return validValues;
	}

	public void setValidValues(List<KeyValueBean> validValues) {
		this.validValues = validValues;
	}

	public String getDefaultValue() {
		return defaultValue;
	}

	public void setDefaultValue(String defaultValue) {
		this.defaultValue = defaultValue;
	}

	public String getAttrName() {
		return attrName;
	}

	public void setAttrName(String attrName) {
		this.attrName = attrName;
	}

	public String getAttrValue() {
		return attrValue;
	}

	public void setAttrValue(String attrValue) {
		this.attrValue = attrValue;
	}

	public String getAttrUnit() {
		return attrUnit;
	}

	public void setAttrUnit(String attrUnit) {
		this.attrUnit = attrUnit;
	}

	public boolean isSystemAttr() {
		return systemAttr;
	}

	public void setSystemAttr(boolean systemAttr) {
		this.systemAttr = systemAttr;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Boolean getControllerAttribute() {
		return controllerAttribute;
	}

	public void setControllerAttribute(Boolean controllerAttribute) {
		this.controllerAttribute = controllerAttribute;
	}

	public Boolean getIsVisible() {
		return isVisible;
	}

	public void setIsVisible(Boolean isVisible) {
		this.isVisible = isVisible;
	}

}
