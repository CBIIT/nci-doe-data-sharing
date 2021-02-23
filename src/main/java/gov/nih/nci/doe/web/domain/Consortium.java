package gov.nih.nci.doe.web.domain;

import java.util.Date;
import javax.persistence.*;

@Entity
@Table(name = "CONSORTIUM_T")
public class Consortium {

	private Integer id;
	private String groupName;
	private String userId;
	private String createdBy;
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

		Consortium that = (Consortium) object;

		if (id != null ? !id.equals(that.id) : that.id != null) {
			return false;
		}
		if (groupName != null ? !groupName.equals(that.groupName) : that.groupName != null) {
			return false;
		}

		if (userId != null ? !userId.equals(that.userId) : that.userId != null) {
			return false;
		}

		if (createdDate != null ? !createdDate.equals(that.createdDate) : that.createdDate != null) {
			return false;
		}

		return createdBy != null ? createdBy.equals(that.createdBy) : that.createdBy == null;

	}

	@Id
	@Column(name = "ID", nullable = false, precision = 0)
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "CONSORITUM_SEQ")
	@SequenceGenerator(name = "CONSORITUM_SEQ", sequenceName = "CONSORITUM_SEQ", allocationSize = 1)
	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
	}

	@Basic
	@Column(name = "USER_ID")
	public String getUserId() {
		return userId;
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	@Basic
	@Column(name = "GROUP_NAME")
	public String getGroupName() {
		return groupName;
	}

	public void setGroupName(String groupName) {
		this.groupName = groupName;
	}

	@Basic
	@Column(name = "CREATED_BY")
	public String getCreatedBy() {
		return createdBy;
	}

	public void setCreatedBy(String createdBy) {
		this.createdBy = createdBy;
	}

	@Basic
	@Column(name = "CREATED_DATE")
	public Date getCreatedDate() {
		return createdDate;
	}

	public void setCreatedDate(Date createdDate) {
		this.createdDate = createdDate;
	}

	@Override
	public String toString() {
		return "Consortium{" + ", id='" + id + '\'' + ", createdDate=" + createdDate + ", createdBy=" + createdBy
				+ ", groupName=" + groupName + ", userId=" + userId + '}';
	}
}
