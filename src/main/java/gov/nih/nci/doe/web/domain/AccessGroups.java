package gov.nih.nci.doe.web.domain;

import java.util.Date;

import javax.persistence.*;

@Entity
@Table(name = "COLLECTION_ACCESS_GROUP_T")
public class AccessGroups {

	private Integer id;
	private Integer collectionId;
	private String collectionPath;
	private Group group;
	private Date createdDate;
	private Date lastChangedDate;

	public boolean equals(Object object) {
		if (this == object) {
			return true;
		}
		if (object == null || getClass() != object.getClass()) {
			return false;
		}
		if (!super.equals(object)) {
			return false;
		}

		AccessGroups that = (AccessGroups) object;

		if (collectionId != null ? !collectionId.equals(that.collectionId) : that.collectionId != null) {
			return false;
		}
		if (group != null ? !group.equals(that.group) : that.group != null) {
			return false;
		}

		if (collectionPath != null ? !collectionPath.equals(that.collectionPath) : that.collectionPath != null) {
			return false;
		}

		return createdDate != null ? createdDate.equals(that.createdDate) : that.createdDate == null;

	}

	@Id
	@Column(name = "ID", nullable = false, precision = 0)
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "ACCESS_GROUPS_SEQ")
	@SequenceGenerator(name = "ACCESS_GROUPS_SEQ", sequenceName = "ACCESS_GROUPS_SEQ", allocationSize = 1)
	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
	}

	@Column(name = "COLLECTION_ID", precision = 4, scale = 0)
	public Integer getCollectionId() {
		return collectionId;
	}

	public void setCollectionId(Integer collectionId) {
		this.collectionId = collectionId;
	}

	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "GROUP_ID", referencedColumnName = "ID", nullable = false)
	public Group getGroup() {
		return group;
	}

	public void setGroup(Group group) {
		this.group = group;
	}

	@Temporal(TemporalType.TIMESTAMP)
	@Column(name = "CREATED_DATE", length = 29)
	public Date getCreatedDate() {
		return createdDate;
	}

	public void setCreatedDate(Date createdDate) {
		this.createdDate = createdDate;
	}

	@Basic
	@Column(name = "COLLECTION_PATH")
	public String getCollectionPath() {
		return collectionPath;
	}

	public void setCollectionPath(String collectionPath) {
		this.collectionPath = collectionPath;
	}

	@Temporal(TemporalType.TIMESTAMP)
	@Column(name = "LAST_CHANGED_DATE", length = 29)
	public Date getLastChangedDate() {
		return lastChangedDate;
	}

	public void setLastChangedDate(Date lastChangedDate) {
		this.lastChangedDate = lastChangedDate;
	}

	@Override
	public String toString() {
		return "MetaDataPermissions{" + ", collectionId='" + collectionId + '\'' + ", lastChangedDate='"
				+ lastChangedDate + '\'' + ",createdDate=" + createdDate + ", collectionPath=" + collectionPath
				+ ", group=" + group + '}';
	}
}
