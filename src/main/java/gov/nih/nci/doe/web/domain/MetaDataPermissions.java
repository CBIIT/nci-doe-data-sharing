package gov.nih.nci.doe.web.domain;

import java.util.Date;

import javax.persistence.*;

@Entity
@Table(name = "COLLECTION_UPDATE_PERMISSIONS_T")
public class MetaDataPermissions {

	private Integer id;
	private Integer collectionId;
	private String collectionPath;
	private Group group;
	private DoeUsers user;
	private Date createdDate;

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

		MetaDataPermissions that = (MetaDataPermissions) object;

		if (collectionId != null ? !collectionId.equals(that.collectionId) : that.collectionId != null) {
			return false;
		}
		if (group != null ? !group.equals(that.group) : that.group != null) {
			return false;
		}
		if (user != null ? !user.equals(that.user) : that.user != null) {
			return false;
		}
		if (collectionPath != null ? !collectionPath.equals(that.collectionPath) : that.collectionPath != null) {
			return false;
		}

		return createdDate != null ? createdDate.equals(that.createdDate) : that.createdDate == null;

	}

	@Id
	@Column(name = "ID", nullable = false, precision = 0)
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "METADATA_PERMISSIONS_SEQ")
	@SequenceGenerator(name = "METADATA_PERMISSIONS_SEQ", sequenceName = "METADATA_PERMISSIONS_SEQ", allocationSize = 1)
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

	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "USER_ID", referencedColumnName = "ID", nullable = false)
	public DoeUsers getUser() {
		return user;
	}

	public void setUser(DoeUsers user) {
		this.user = user;
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

	@Override
	public String toString() {
		return "MetaDataPermissions{" + ", collectionId='" + collectionId + '\'' + ", user='" + user + '\''
				+ ",createdDate=" + createdDate + ", collectionPath=" + collectionPath + ", group=" + group
				+ '}';
	}
}
