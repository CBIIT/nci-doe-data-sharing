package gov.nih.nci.doe.web.model;

public class DoeDownloadDatafile {

	private String endPointName;
	private String endPointLocation;
	private String destinationPath;
	private String downloadFileName;
	private String bucketName;
	private String s3Path;
	private String accessKey;
	private String secretKey;
	private String region;
	private String searchType;
	private String downloadType;
	private String selectedPaths;
	private String drivePath;
	private String googleCloudPath;
	private String googleCloudBucketName;
	private String downloadToDestination;

	public String getDownloadToDestination() {
		return downloadToDestination;
	}

	public void setDownloadToDestination(String downloadToDestination) {
		this.downloadToDestination = downloadToDestination;
	}

	public String getGoogleCloudPath() {
		return googleCloudPath;
	}

	public void setGoogleCloudPath(String googleCloudPath) {
		this.googleCloudPath = googleCloudPath;
	}

	public String getGoogleCloudBucketName() {
		return googleCloudBucketName;
	}

	public void setGoogleCloudBucketName(String googleCloudBucketName) {
		this.googleCloudBucketName = googleCloudBucketName;
	}

	public String getDrivePath() {
		return drivePath;
	}

	public void setDrivePath(String drivePath) {
		this.drivePath = drivePath;
	}

	public String getSelectedPaths() {
		return selectedPaths;
	}

	public void setSelectedPaths(String selectedPaths) {
		this.selectedPaths = selectedPaths;
	}

	public String getSearchType() {
		return searchType;
	}

	public void setSearchType(String searchType) {
		this.searchType = searchType;
	}

	public String getDownloadFileName() {
		return downloadFileName;
	}

	public void setDownloadFileName(String downloadFileName) {
		this.downloadFileName = downloadFileName;
	}

	public String getEndPointName() {
		return endPointName;
	}

	public void setEndPointName(String endPointName) {
		this.endPointName = endPointName;
	}

	public String getEndPointLocation() {
		return endPointLocation;
	}

	public void setEndPointLocation(String endPointLocation) {
		this.endPointLocation = endPointLocation;
	}

	public String getDestinationPath() {
		return destinationPath;
	}

	public void setDestinationPath(String destinationPath) {
		this.destinationPath = destinationPath;
	}

	public String getDownloadType() {
		return downloadType;
	}

	public void setDownloadType(String downloadType) {
		this.downloadType = downloadType;
	}

	public String getBucketName() {
		return bucketName;
	}

	public void setBucketName(String bucketName) {
		this.bucketName = bucketName;
	}

	public String getS3Path() {
		return s3Path;
	}

	public void setS3Path(String s3Path) {
		this.s3Path = s3Path;
	}

	public String getAccessKey() {
		return accessKey;
	}

	public void setAccessKey(String accessKey) {
		this.accessKey = accessKey;
	}

	public String getSecretKey() {
		return secretKey;
	}

	public void setSecretKey(String secretKey) {
		this.secretKey = secretKey;
	}

	public String getRegion() {
		return region;
	}

	public void setRegion(String region) {
		this.region = region;
	}
}
