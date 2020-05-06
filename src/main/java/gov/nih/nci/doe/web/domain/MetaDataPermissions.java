package gov.nih.nci.doe.web.domain;

import java.util.Date;

import javax.persistence.*;

import org.hibernate.annotations.Type;






@Entity
@Table(name = "METADATA_PERMISSIONS_T",schema = "NCI_DOE_DB")
public class MetaDataPermissions {

	private Integer id;
	private Integer collectionId;
	private String userGroupId;
	private Boolean isGroup;
	private Boolean isOwner;
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
        if (userGroupId != null ? !userGroupId.equals(that.userGroupId) : that.userGroupId != null) {
            return false;
        }
        if (isGroup != null ? !isGroup.equals(that.isGroup) : that.isGroup != null) {
            return false;
        }
        if (isOwner != null ? !isOwner.equals(that.isOwner) : that.isOwner != null) {
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

	@Basic
    @Column(name = "USER_GROUP_ID")
	public String getUserGroupId() {
		return userGroupId;
	}


	public void setUserGroupId(String userGroupId) {
		this.userGroupId = userGroupId;
	}

	@Basic
	@Column(name = "IS_GROUP", nullable = true, length = 1)
	@Type(type = "yes_no")
	public Boolean getIsGroup() {
		return isGroup;
	}


	public void setIsGroup(Boolean isGroup) {
		this.isGroup = isGroup;
	}

	@Basic
	@Column(name = "IS_OWNER", nullable = true, length = 1)
	@Type(type = "yes_no")
	public Boolean getIsOwner() {
		return isOwner;
	}


	public void setIsOwner(Boolean isOwner) {
		this.isOwner = isOwner;
	}

	@Temporal(TemporalType.TIMESTAMP)
	@Column(name = "CREATED_DATE", length = 29)
	public Date getCreatedDate() {
		return createdDate;
	}


	public void setCreatedDate(Date createdDate) {
		this.createdDate = createdDate;
	}


	@Override
    public String toString() {
        return "MetaDataPermissions{" +
            ", collectionId='" + collectionId + '\'' +
            ", userGroupId='" + userGroupId + '\'' +
            ", isOwner=" + isOwner +
             ", createdDate=" + createdDate +
              ", isGroup=" + isGroup +
            '}';
    }
}
