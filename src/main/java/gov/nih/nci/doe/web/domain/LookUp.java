package gov.nih.nci.doe.web.domain;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Basic;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.hibernate.annotations.Type;

@Entity
@Table(name = "lookup_t")
public class LookUp implements Serializable {

	private static final long serialVersionUID = 3092597482427044459L;

	private String id;
	private String levelName;
	private String attrName;
	private String displayName;
	private String createdBy;
	private Date createdDate;
	private Integer displayOrder;
	private String searchCriteriaDisplay;
	private String searchResultsDisplay;
	private Boolean isEditable;

	public LookUp() {

	}

	public LookUp(String id, String levelName, String attrName, String displayName) {
		this.id = id;
		this.levelName = levelName;
		this.attrName = attrName;
		this.displayName = displayName;
	}

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", unique = true, nullable = false)
	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	@Column(name = "level_name", nullable = false, length = 255)
	public String getLevelName() {
		return levelName;
	}

	public void setLevelName(String levelName) {
		this.levelName = levelName;
	}

	@Column(name = "attr_name", nullable = false, length = 255)
	public String getAttrName() {
		return attrName;
	}

	public void setAttrName(String attrName) {
		this.attrName = attrName;
	}

	@Column(name = "display_name", nullable = false, length = 500)
	public String getDisplayName() {
		return displayName;
	}

	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	@Column(name = "created_by", nullable = false, length = 50)
	public String getCreatedBy() {
		return createdBy;
	}

	public void setCreatedBy(String createdBy) {
		this.createdBy = createdBy;
	}

	@Temporal(TemporalType.TIMESTAMP)
	@Column(name = "created_date", nullable = false, length = 29)
	public Date getCreatedDate() {
		return createdDate;
	}

	public void setCreatedDate(Date createdDate) {
		this.createdDate = createdDate;
	}

	@Column(name = "display_order", nullable = true, length = 5)
	public Integer getDisplayOrder() {
		return displayOrder;
	}

	public void setDisplayOrder(Integer displayOrder) {
		this.displayOrder = displayOrder;
	}

	@Column(name = "search_criteria_display", nullable = true, length = 1)
	public String getSearchCriteriaDisplay() {
		return searchCriteriaDisplay;
	}

	public void setSearchCriteriaDisplay(String searchCriteriaDisplay) {
		this.searchCriteriaDisplay = searchCriteriaDisplay;
	}

	@Basic
	@Column(name = "IS_EDITABLE", nullable = true, length = 1)
	@Type(type = "yes_no")
	public Boolean getIsEditable() {
		return isEditable;
	}

	public void setIsEditable(Boolean isEditable) {
		this.isEditable = isEditable;
	}

	@Column(name = "search_results_display", nullable = true, length = 1)
	public String getSearchResultsDisplay() {
		return searchResultsDisplay;
	}

	public void setSearchResultsDisplay(String searchResultsDisplay) {
		this.searchResultsDisplay = searchResultsDisplay;
	}

}
