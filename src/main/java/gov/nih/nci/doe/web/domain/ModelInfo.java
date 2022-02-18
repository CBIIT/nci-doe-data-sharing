package gov.nih.nci.doe.web.domain;

import java.io.Serializable;

import javax.persistence.Basic;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import org.hibernate.annotations.Type;

@Entity
@Table(name = "MODEL_INFO_T")
public class ModelInfo implements Serializable {

	private static final long serialVersionUID = 1L;
	private Integer id;
	private String modelPath;
	private Boolean isReferencedDataset;

	public ModelInfo() {

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

		ModelInfo that = (ModelInfo) object;

		if (id != null ? !id.equals(that.id) : that.id != null) {
			return false;
		}

		if (isReferencedDataset != null ? !isReferencedDataset.equals(that.isReferencedDataset)
				: that.isReferencedDataset != null) {
			return false;
		}

		return modelPath != null ? modelPath.equals(that.modelPath) : that.modelPath == null;

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
	@Column(name = "MODEL_PATH")
	public String getModelPath() {
		return modelPath;
	}

	public void setModelPath(String modelPath) {
		this.modelPath = modelPath;
	}

	@Basic
	@Column(name = "IS_REFERENCED_DATASET", nullable = true, length = 1)
	@Type(type = "yes_no")
	public Boolean getIsReferencedDataset() {
		return isReferencedDataset;
	}

	public void setIsReferencedDataset(Boolean isReferencedDataset) {
		this.isReferencedDataset = isReferencedDataset;
	}

}
