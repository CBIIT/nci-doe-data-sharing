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
	private String assetPath;
	private Boolean isExternalDatasetSupported;

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

		if (isExternalDatasetSupported != null ? !isExternalDatasetSupported.equals(that.isExternalDatasetSupported)
				: that.isExternalDatasetSupported != null) {
			return false;
		}

		return assetPath != null ? assetPath.equals(that.assetPath) : that.assetPath == null;

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
	@Column(name = "ASSET_PATH")
	public String getAssetPath() {
		return assetPath;
	}

	public void setAssetPath(String assetPath) {
		this.assetPath = assetPath;
	}

	@Basic
	@Column(name = "IS_EXTERNAL_DATASET_SUPPORTED", nullable = true, length = 1)
	@Type(type = "yes_no")
	public Boolean getIsExternalDatasetSupported() {
		return isExternalDatasetSupported;
	}

	public void setIsExternalDatasetSupported(Boolean isExternalDatasetSupported) {
		this.isExternalDatasetSupported = isExternalDatasetSupported;
	}

}
