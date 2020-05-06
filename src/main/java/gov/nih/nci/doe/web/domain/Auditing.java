package gov.nih.nci.doe.web.domain;

import java.util.Date;
import javax.persistence.*;


@Entity
@Table(name = "AUDITING_T",schema = "NCI_DOE_DB")
public class Auditing {

	private Integer id;
	private String name;
	private String path;
	private String operation;
	private String status;
	private String errorMsg;
	private String taskId;
	private String transferType;
	private Date startTime;
	private Date completionTime;

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

        Auditing that = (Auditing) object;

        if (id != null ? !id.equals(that.id) : that.id != null) {
            return false;
        }
        if (name != null ? !name.equals(that.name) : that.name != null) {
            return false;
        }
        
        if (path != null ? !path.equals(that.path) : that.path != null) {
            return false;
        }
        
        if (operation != null ? !operation.equals(that.operation) : that.operation != null) {
            return false;
        }

        if (errorMsg != null ? !errorMsg.equals(that.errorMsg) : that.errorMsg != null) {
            return false;
        }
        
        if (taskId != null ? !taskId.equals(that.taskId) : that.taskId != null) {
            return false;
        }
        
        if (transferType != null ? !transferType.equals(that.transferType) : that.transferType != null) {
            return false;
        }
        if (startTime != null ? !startTime.equals(that.startTime) : that.startTime != null) {
            return false;
        }
        
        if (completionTime != null ? !completionTime.equals(that.completionTime) : that.completionTime != null) {
            return false;
        }
        return status != null ? status.equals(that.status) : that.status == null;

    }
    
    
    @Id
    @Column(name = "ID", nullable = false, precision = 0)
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "AUDITING_SEQ")
    @SequenceGenerator(name = "AUDITING_SEQ", sequenceName = "AUDITING_SEQ", allocationSize = 1)
	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
	}
	
	
	@Basic
    @Column(name = "NAME")
	public String getName() {
		return name;
	}


	public void setName(String name) {
		this.name = name;
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
    @Column(name = "OPERATION")
	public String getOperation() {
		return operation;
	}


	public void setOperation(String operation) {
		this.operation = operation;
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
    @Column(name = "ERROR_MSG")
	public String getErrorMsg() {
		return errorMsg;
	}


	public void setErrorMsg(String errorMsg) {
		this.errorMsg = errorMsg;
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
    @Column(name = "TRANSFER_TYPE")
	public String getTransferType() {
		return transferType;
	}


	public void setTransferType(String transferType) {
		this.transferType = transferType;
	}

	@Temporal(TemporalType.TIMESTAMP)
	@Column(name = "START_TIME", length = 29)
	public Date getStartTime() {
		return startTime;
	}


	public void setStartTime(Date startTime) {
		this.startTime = startTime;
	}

	@Temporal(TemporalType.TIMESTAMP)
	@Column(name = "COMPLETION_TIME", length = 29)
	public Date getCompletionTime() {
		return completionTime;
	}


	public void setCompletionTime(Date completionTime) {
		this.completionTime = completionTime;
	}


	@Override
    public String toString() {
        return "Consortium{" +
            ", id='" + id + '\'' +
             ", name=" + name +
              ", path=" + path +
                ", operation=" + operation +
                  ", status=" + status +
                    ", errorMsg=" + errorMsg +
             ", taskId=" + taskId +
              ", transferType=" + transferType +
               ", startTime=" + startTime +
               ", completionTime=" + completionTime +
            '}';
    }
}
