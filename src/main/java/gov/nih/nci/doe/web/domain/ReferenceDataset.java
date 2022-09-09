package gov.nih.nci.doe.web.domain;

import java.util.List;

import gov.nih.nci.doe.web.model.Path;

public class ReferenceDataset {

	private List<Path> modelPaths;
	private List<Path> referenceDatasetPaths;

	public List<Path> getModelPaths() {
		return modelPaths;
	}

	public void setModelPaths(List<Path> modelPaths) {
		this.modelPaths = modelPaths;
	}

	public List<Path> getReferenceDatasetPaths() {
		return referenceDatasetPaths;
	}

	public void setReferenceDatasetPaths(List<Path> referenceDatasetPaths) {
		this.referenceDatasetPaths = referenceDatasetPaths;
	}

}
