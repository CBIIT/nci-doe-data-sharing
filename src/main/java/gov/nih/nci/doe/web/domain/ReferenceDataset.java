package gov.nih.nci.doe.web.domain;

import java.util.List;

public class ReferenceDataset {

	private List<Path> assetPath;

	private List<Path> referenceDatasetPath;

	public List<Path> getAssetPath() {
		return assetPath;
	}

	public void setAssetPath(List<Path> assetPath) {
		this.assetPath = assetPath;
	}

	public List<Path> getReferenceDatasetPath() {
		return referenceDatasetPath;
	}

	public void setReferenceDatasetPath(List<Path> referenceDatasetPath) {
		this.referenceDatasetPath = referenceDatasetPath;
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
