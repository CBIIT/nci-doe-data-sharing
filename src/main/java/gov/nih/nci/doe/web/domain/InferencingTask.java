package gov.nih.nci.doe.web.domain;

import java.util.Date;

import javax.persistence.*;

@Entity
@Table(name = "INFERENCING_TASK_T")
public class InferencingTask {

	private Integer id;
	private String taskId;
	private String status;
	private String modelIdentifier;
	private String userId;
	private Date startDate;
	private Date completedDate;
	private String resultPath;
	private String dmeTaskId;
	private String modelh5Path;
	private String testDataSetPath;
	private String assetPath;
	private String batchId;
	private String errorMessage;
	private String uploadFrom;
	private String actualResultsFileName;

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

		InferencingTask that = (InferencingTask) object;

		if (taskId != null ? !taskId.equals(that.taskId) : that.taskId != null) {
			return false;
		}
		if (status != null ? !status.equals(that.status) : that.status != null) {
			return false;
		}

		if (modelIdentifier != null ? !modelIdentifier.equals(that.modelIdentifier) : that.modelIdentifier != null) {
			return false;
		}
		if (userId != null ? !userId.equals(that.userId) : that.userId != null) {
			return false;
		}

		if (startDate != null ? !startDate.equals(that.startDate) : that.startDate != null) {
			return false;
		}

		if (completedDate != null ? !completedDate.equals(that.completedDate) : that.completedDate != null) {
			return false;
		}
		return resultPath != null ? resultPath.equals(that.resultPath) : that.resultPath == null;

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
	@Column(name = "TASK_ID")
	public String getTaskId() {
		return taskId;
	}

	public void setTaskId(String taskId) {
		this.taskId = taskId;
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
	@Column(name = "MODEL_IDENTIFIER")
	public String getModelIdentifier() {
		return modelIdentifier;
	}

	public void setModelIdentifier(String modelIdentifier) {
		this.modelIdentifier = modelIdentifier;
	}

	@Basic
	@Column(name = "TEST_DATASET_PATH")
	public String getTestDataSetPath() {
		return testDataSetPath;
	}

	public void setTestDataSetPath(String testDataSetPath) {
		this.testDataSetPath = testDataSetPath;
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
	@Column(name = "MODEL_H5_PATH")
	public String getModelh5Path() {
		return modelh5Path;
	}

	public void setModelh5Path(String modelh5Path) {
		this.modelh5Path = modelh5Path;
	}

	@Basic
	@Column(name = "UPLOAD_FROM")
	public String getUploadFrom() {
		return uploadFrom;
	}

	public void setUploadFrom(String uploadFrom) {
		this.uploadFrom = uploadFrom;
	}

	@Basic
	@Column(name = "DME_TASK_ID")
	public String getDmeTaskId() {
		return dmeTaskId;
	}

	public void setDmeTaskId(String dmeTaskId) {
		this.dmeTaskId = dmeTaskId;
	}

	@Temporal(TemporalType.TIMESTAMP)
	@Column(name = "START_DATE", length = 29)
	public Date getStartDate() {
		return startDate;
	}

	public void setStartDate(Date startDate) {
		this.startDate = startDate;
	}

	@Basic
	@Column(name = "ACTUAL_RESULTS_FILE_NAME")
	public String getActualResultsFileName() {
		return actualResultsFileName;
	}

	public void setActualResultsFileName(String actualResultsFileName) {
		this.actualResultsFileName = actualResultsFileName;
	}

	@Temporal(TemporalType.TIMESTAMP)
	@Column(name = "COMPLETED_DATE", length = 29)
	public Date getCompletedDate() {
		return completedDate;
	}

	public void setCompletedDate(Date completedDate) {
		this.completedDate = completedDate;
	}

	@Basic
	@Column(name = "RESULT_PATH")
	public String getResultPath() {
		return resultPath;
	}

	public void setResultPath(String resultPath) {
		this.resultPath = resultPath;
	}

	@Basic
	@Column(name = "ASSET_PATH")
	public String getAssetPath() {
		return assetPath;
	}

	public void setAssetPath(String assetPath) {
		this.assetPath = assetPath;
	}

	@Basic
	@Column(name = "BATCH_ID")
	public String getBatchId() {
		return batchId;
	}

	public void setBatchId(String batchId) {
		this.batchId = batchId;
	}

	@Basic
	@Column(name = "ERROR_MESSAGE")
	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	@Override
	public String toString() {
		return "InferencingTask{" + ", taskId='" + taskId + '\'' + ", status=" + status + ", modelIdentifier="
				+ modelIdentifier + ", startDate=" + startDate + ", completedDate=" + completedDate + ", resultPath="
				+ resultPath + '}';
	}
}
