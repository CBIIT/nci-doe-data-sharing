package gov.nih.nci.doe.web.domain;

import java.util.Date;

import javax.persistence.*;

import org.hibernate.annotations.Type;

@Entity
@Table(name = "TASK_MANAGER_T")
public class TaskManager {

	private Integer id;
	private String taskId;
	private String taskName;
	private String taskType;
	private String userId;
	private Date taskDate;
	private String type;
	private String path;
	private String status;
	private Boolean isNotified;

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

		TaskManager that = (TaskManager) object;

		if (taskId != null ? !taskId.equals(that.taskId) : that.taskId != null) {
			return false;
		}
		if (taskName != null ? !taskName.equals(that.taskName) : that.taskName != null) {
			return false;
		}

		if (taskType != null ? !taskType.equals(that.taskType) : that.taskType != null) {
			return false;
		}
		if (type != null ? !type.equals(that.type) : that.type != null) {
			return false;
		}

		if (path != null ? !path.equals(that.path) : that.path != null) {
			return false;
		}
		if (status != null ? !status.equals(that.status) : that.status != null) {
			return false;
		}
		if (isNotified != null ? !isNotified.equals(that.isNotified) : that.isNotified != null) {
			return false;
		}
		return taskDate != null ? taskDate.equals(that.taskDate) : that.taskDate == null;

	}

	@Id
	@Column(name = "ID", nullable = false, precision = 0)
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "TASK_MANAGER_SEQ")
	@SequenceGenerator(name = "TASK_MANAGER_SEQ", sequenceName = "TASK_MANAGER_SEQ", allocationSize = 1)
	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
	}

	@Basic
	@Column(name = "TASK_ID")
	public String getTaskId() {
		return taskId;
	}

	public void setTaskId(String taskId) {
		this.taskId = taskId;
	}

	@Basic
	@Column(name = "TASK_NAME")
	public String getTaskName() {
		return taskName;
	}

	public void setTaskName(String taskName) {
		this.taskName = taskName;
	}

	@Basic
	@Column(name = "TASK_TYPE")
	public String getTaskType() {
		return taskType;
	}

	public void setTaskType(String taskType) {
		this.taskType = taskType;
	}

	@Basic
	@Column(name = "TASK_DATE")
	public Date getTaskDate() {
		return taskDate;
	}

	public void setTaskDate(Date taskDate) {
		this.taskDate = taskDate;
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
	@Column(name = "TYPE")
	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	@Basic
	@Column(name = "PATH")
	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	@Basic
	@Column(name = "STATUS")
	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	@Basic
	@Column(name = "IS_NOTIFIED", nullable = true, length = 1)
	@Type(type = "yes_no")
	public Boolean getIsNotified() {
		return isNotified;
	}

	public void setIsNotified(Boolean isNotified) {
		this.isNotified = isNotified;
	}

	@Override
	public String toString() {
		return "TaskManager{" + ", taskName='" + taskName + '\'' + ", taskType=" + taskType + ", taskDate=" + taskDate
				+ ", taskId=" + taskId + ", userId=" + userId + ", status=" + status + ", path=" + path
				+ " ,isNotified=" + isNotified + "type=" + type + '}';
	}
}
