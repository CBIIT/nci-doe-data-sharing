package gov.nih.nci.doe.web.model;

import java.util.List;

import gov.nih.nci.doe.web.model.Path;

public class ReferenceDataset {

	private List<Path> referenceDatasetPaths;

	public List<Path> getReferenceDatasetPaths() {
		return referenceDatasetPaths;
	}

	public void setReferenceDatasetPaths(List<Path> referenceDatasetPaths) {
		this.referenceDatasetPaths = referenceDatasetPaths;
	}

}