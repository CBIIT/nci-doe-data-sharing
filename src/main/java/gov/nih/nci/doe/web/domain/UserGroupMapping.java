package gov.nih.nci.doe.web.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name = "USER_GROUP_T")
public class UserGroupMapping {

	private Integer id;
	private DoeUsers user;
	private Group group;

	public UserGroupMapping() {

	}

	public UserGroupMapping(DoeUsers user, Group group) {
		this.user = user;
		this.group = group;

	}

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

		UserGroupMapping that = (UserGroupMapping) object;

		if (user != null ? !user.equals(that.user) : that.user != null) {
			return false;
		}

		return group != null ? group.equals(that.group) : that.group == null;

	}

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", unique = true, nullable = false)
	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
	}

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "USER_ID", referencedColumnName = "ID", nullable = false)
	public DoeUsers getUser() {
		return user;
	}

	public void setUser(DoeUsers user) {
		this.user = user;
	}

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "GROUP_ID", referencedColumnName = "ID", nullable = false)
	public Group getGroup() {
		return group;
	}

	public void setGroup(Group group) {
		this.group = group;
	}
}
