package gov.nih.nci.doe.web.domain;

import java.io.Serializable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "GROUP_T")
public class Group implements Serializable {

	private static final long serialVersionUID = 1L;
	private Integer id;
	private String groupName;

	public Group() {

	}

	public Group(Integer id, String groupName) {
		this.id = id;
		this.groupName = groupName;

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

		Group that = (Group) object;

		if (id != null ? !id.equals(that.id) : that.id != null) {
			return false;
		}

		return groupName != null ? groupName.equals(that.groupName) : that.groupName == null;

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

	@Column(name = "GROUP_NAME", nullable = false, length = 255)
	public String getGroupName() {
		return groupName;
	}

	public void setGroupName(String groupName) {
		this.groupName = groupName;
	}

}
