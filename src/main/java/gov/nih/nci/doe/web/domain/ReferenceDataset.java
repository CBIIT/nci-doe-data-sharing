package gov.nih.nci.doe.web.domain;

import java.util.List;

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

	public class Path {
		private String path;

		public String getPath() {
			return path;
		}

		public void setPath(String path) {
			this.path = path;
		}

	}

}
