package gov.nih.nci.doe.web.domain;

import java.sql.Clob;
import java.util.Date;
import javax.persistence.*;

@Entity
@Table(name = "AUDIT_TRANSFER_METADATA_T")
public class AuditMetadataTransfer {

	private Integer id;
	private Date startTime;
	private Date completedTime;
	private String status;
	private String errorMsg;
	private String process;
	private String fileName;
	private Clob metadataFile;

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

		AuditMetadataTransfer that = (AuditMetadataTransfer) object;

		if (id != null ? !id.equals(that.id) : that.id != null) {
			return false;
		}

		if (errorMsg != null ? !errorMsg.equals(that.errorMsg) : that.errorMsg != null) {
			return false;
		}
		if (process != null ? !process.equals(that.process) : that.process != null) {
			return false;
		}
		if (fileName != null ? !fileName.equals(that.fileName) : that.fileName != null) {
			return false;
		}
		if (startTime != null ? !startTime.equals(that.startTime) : that.startTime != null) {
			return false;
		}

		if (completedTime != null ? !completedTime.equals(that.completedTime) : that.completedTime != null) {
			return false;
		}

		return status != null ? status.equals(that.status) : that.status == null;

	}

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID", updatable = false, nullable = false)
	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
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

	@Temporal(TemporalType.TIMESTAMP)
	@Column(name = "START_TIME", length = 29)
	public Date getStartTime() {
		return startTime;
	}

	public void setStartTime(Date startTime) {
		this.startTime = startTime;
	}

	@Temporal(TemporalType.TIMESTAMP)
	@Column(name = "COMPLETED_TIME", length = 29)
	public Date getCompletedTime() {
		return completedTime;
	}

	public void setCompletedTime(Date completedTime) {
		this.completedTime = completedTime;
	}

	@Basic
	@Column(name = "PROCESS")
	public String getProcess() {
		return process;
	}

	public void setProcess(String process) {
		this.process = process;
	}

	@Basic
	@Column(name = "FILE_NAME")
	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}
	
	@Lob
	@Basic
	@Column(name = "METADATA_FILE")
	public Clob getMetadataFile() {
		return metadataFile;
	}

	public void setMetadataFile(Clob metadataFile) {
		this.metadataFile = metadataFile;
	}

	@Override
	public String toString() {
		return "AuditMetadataTransfer{" + ", id='" + id + '\'' + ", process=" + process + ", status=" + status
				+ ", errorMsg=" + errorMsg + ", fileName=\" + fileName + \", startTime=" + startTime
				+ ", startTime=\" + startTime + \", completedTime=" + completedTime + '}';
	}

}
