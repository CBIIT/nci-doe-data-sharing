package gov.nih.nci.doe.web.scheduler;

import java.io.File;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.stream.Collectors;

import javax.ws.rs.core.Response;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.cxf.jaxrs.client.WebClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.util.UriComponentsBuilder;

import com.amazonaws.AmazonServiceException;
import com.amazonaws.SdkClientException;
import com.amazonaws.auth.AWSStaticCredentialsProvider;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3ClientBuilder;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.MappingJsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.controller.AbstractDoeController;
import gov.nih.nci.doe.web.domain.Auditing;
import gov.nih.nci.doe.web.domain.InferencingTask;
import gov.nih.nci.doe.web.model.DoeSearch;
import gov.nih.nci.doe.web.repository.AuditingRepository;
import gov.nih.nci.doe.web.repository.InferencingTaskRepository;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.domain.datatransfer.HpcFileLocation;
import gov.nih.nci.hpc.domain.datatransfer.HpcUploadSource;
import gov.nih.nci.hpc.domain.datatransfer.HpcUserDownloadRequest;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionRegistrationDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDownloadSummaryDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationTaskDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcRegistrationSummaryDTO;
import gov.nih.nci.hpc.dto.datasearch.HpcCompoundMetadataQueryDTO;

public class ManageTasksScheduler extends AbstractDoeController {

	@Value("${gov.nih.nci.hpc.server.download}")
	private String queryServiceURL;

	@Value("${gov.nih.nci.hpc.server.user.authenticate}")
	private String authenticateURL;

	@Value("${doe.writeaccount.password}")
	private String writeAccessUserPassword;

	@Value("${doe.writeaccount.username}")
	private String writeAccessUserName;

	@Value("${modac.flask.webservice}")
	private String modacFlaskServer;

	@Value("${modac.transfer.s3.accesskey}")
	private String accessKey;

	@Value("${modac.transfer.s3.secretkey}")
	private String secretKey;

	@Value("${modac.transfer.s3.bucketName}")
	private String bucketName;

	@Autowired
	InferencingTaskRepository inferencingTaskRepository;

	@Autowired
	AuditingRepository auditingRepository;

	@Value("${gov.nih.nci.hpc.server.v2.bulkregistration}")
	private String registrationServiceV2URL;

	public void init() {
		log.info("manage scheduler called");
	}

	SimpleDateFormat format = new SimpleDateFormat("MM_dd_yyyy");

	@Scheduled(cron = "${doe.scheduler.cron.auditing}")
	public void updateAuditingService() throws DoeWebException {

		log.info("auditing service scheduler");
		String authToken = DoeClientUtil.getAuthenticationToken(writeAccessUserName, writeAccessUserPassword,
				authenticateURL);

		String serviceURL = queryServiceURL + "?page=" + 1 + "&totalCount=true";
		HpcDownloadSummaryDTO downloads = DoeClientUtil.getDownloadSummary(authToken, serviceURL);

		final MultiValueMap<String, String> paramsMap = new LinkedMultiValueMap<>();
		paramsMap.set("totalCount", Boolean.TRUE.toString());
		HpcRegistrationSummaryDTO registrations = DoeClientUtil.getRegistrationSummary(authToken,
				registrationServiceV2URL, paramsMap);

		List<HpcUserDownloadRequest> downloadResults = new ArrayList<HpcUserDownloadRequest>();
		if (downloads != null) {
			downloadResults.addAll(downloads.getActiveTasks());
			downloadResults.addAll(downloads.getCompletedTasks());
		}

		List<HpcBulkDataObjectRegistrationTaskDTO> uploadResults = new ArrayList<HpcBulkDataObjectRegistrationTaskDTO>();
		if (registrations != null) {
			uploadResults.addAll(registrations.getActiveTasks());
			uploadResults.addAll(registrations.getCompletedTasks());
		}

		List<Auditing> auditingTaskIds = auditingService.getAllTaskIds();

		if (CollectionUtils.isNotEmpty(auditingTaskIds)) {
			for (Auditing audit : auditingTaskIds) {
				if (audit.getOperation().equalsIgnoreCase("Upload")) {
					HpcBulkDataObjectRegistrationTaskDTO upload = uploadResults.stream()
							.filter(x -> audit.getTaskId().equals(x.getTaskId())).findAny().orElse(null);

					if (upload != null) {
						audit.setCompletionTime(
								(upload != null && upload.getCompleted() != null) ? upload.getCompleted().getTime()
										: null);
						if (upload.getResult() == null) {
							audit.setStatus("In progress");
						} else if (Boolean.TRUE.equals(upload.getResult())) {
							audit.setStatus("Completed");
						} else if (Boolean.FALSE.equals(upload.getResult())) {
							audit.setStatus("Failed");
							List<String> message = new ArrayList<String>();
							upload.getFailedItems().stream().forEach(x -> message.add(x.getMessage()));
							audit.setErrorMsg(message.get(0));
						}
						auditingRepository.saveAndFlush(audit);
					}
				} else if (audit.getOperation().equalsIgnoreCase("Download")) {
					HpcUserDownloadRequest download = downloadResults.stream()
							.filter(x -> audit.getTaskId().equals(x.getTaskId())).findAny().orElse(null);
					if (download != null) {
						audit.setCompletionTime((download != null && download.getCompleted() != null)
								? download.getCompleted().getTime()
								: null);
						if (download != null && download.getResult() != null
								&& download.getResult().value().equals("FAILED")) {
							List<String> message = new ArrayList<String>();
							download.getItems().stream().forEach(x -> message.add(x.getMessage()));
							audit.setStatus("Failed");
							audit.setErrorMsg(message.get(0));

						} else if (download.getResult() != null && download.getResult().value().equals("COMPLETED")) {
							audit.setStatus("Completed");
						} else {
							audit.setStatus("In Progress");
						}
						auditingRepository.saveAndFlush(audit);
					}
				}

			}
		}
	}

	@Scheduled(cron = "${doe.scheduler.cron.auditing}")
	public void deleteInferncingFilesFromMount() throws DoeWebException {

		log.info("delete any intermediate files from mount when generating predictions");

		// retrieve all completed and failed inferencing tasks for the past 24 hours
		List<InferencingTask> getAllCompletedAndFailedTasks = inferencingTaskRepository.getAllCompletedAndFailedTasks();

		for (InferencingTask t : getAllCompletedAndFailedTasks) {

			log.info("verify the inferencing files for : " + t.getTestDataSetPath());
			try {
				String dataFilePath = t.getTestDataSetPath();
				String resultPath = t.getResultPath();
				String outcomeFilePath = t.getActualResultsFileName();

				String dataFileName = dataFilePath != null
						? dataFilePath.substring(dataFilePath.lastIndexOf('/') + 1, dataFilePath.length())
						: null;
				String predFileName = resultPath != null
						? resultPath.substring(resultPath.lastIndexOf('/') + 1, resultPath.length())
						: null;

				String outcomeFileName = outcomeFilePath != null
						? outcomeFilePath.substring(outcomeFilePath.lastIndexOf('/') + 1, outcomeFilePath.length())
						: null;

				File predFile = new File(uploadPath + predFileName);
				File errorFile = new File(uploadPath + predFileName + "_error.txt");
				File outcomeFile = new File(uploadPath + outcomeFileName);
				File inputFile = new File(uploadPath + dataFileName);

				// verify and delete predictions file
				if (predFile.exists()) {
					if (predFile.delete()) {
						log.info("Pred file deleted successfully: " + predFile);
					} else {
						log.info("Failed to delete the pred file: " + predFile);
					}
				} else {
					log.info("Pred file does not exist: " + predFile);
				}

				// verify and delete error file
				if (errorFile.exists()) {
					if (errorFile.delete()) {
						log.info("error file deleted successfully: " + errorFile);
					} else {
						log.info("Failed to delete the error file: " + errorFile);
					}
				} else {
					log.info("error file does not exist: " + errorFile);
				}

				// verify and delete outcome file
				if (outcomeFile.exists()) {
					if (outcomeFile.delete()) {
						log.info("outcome file deleted successfully: " + outcomeFile);
					} else {
						log.info("Failed to delete the outcome file: " + outcomeFile);
					}
				} else {
					log.info("outcome file does not exist: " + outcomeFile);
				}

				// verify and delete input file
				if (inputFile.exists()) {
					if (inputFile.delete()) {
						log.info("inputFile file deleted successfully: " + inputFile);
					} else {
						log.info("Failed to delete the input file: " + inputFile);
					}
				} else {
					log.info("input file does not exist: " + inputFile);
				}

			} catch (Exception e) {
				log.error("Exception in deleting inferencing files from mount: " + e);
				throw new DoeWebException(e.getMessage());
			}
		}

	}

	@Scheduled(cron = "${doe.scheduler.cron.infer}")
	public void performInferencing() throws DoeWebException, MalformedURLException {

		// get all not Started tasks to call the flask web service to perform
		// inferencing
		log.info("generate prediction scheduler");
		List<InferencingTask> getAllNotStartedTasks = inferencingTaskRepository.getAllNotStartedTasks("NOTSTARTED");

		String authToken = DoeClientUtil.getAuthenticationToken(writeAccessUserName, writeAccessUserPassword,
				authenticateURL);
		for (InferencingTask t : getAllNotStartedTasks) {
			log.info("call flask API for File Path: " + t.getTestDataSetPath());
			try {
				String dataFilePath = t.getTestDataSetPath();
				String modelh5Path = t.getModelh5Path();
				String resultPath = t.getResultPath();

				String dataFileName = dataFilePath != null
						? dataFilePath.substring(dataFilePath.lastIndexOf('/') + 1, dataFilePath.length())
						: null;
				String modelName = modelh5Path != null
						? modelh5Path.substring(modelh5Path.lastIndexOf('/') + 1, modelh5Path.length())
						: null;
				String resultFileName = resultPath != null
						? resultPath.substring(resultPath.lastIndexOf('/') + 1, resultPath.length())
						: null;
				if (StringUtils.isNotEmpty(dataFileName) && StringUtils.isNotEmpty(modelName)
						&& StringUtils.isNotEmpty(resultFileName) && t.getBatchId() == null) {
					// call flask API for each not started task
					log.info("call flask web service for " + dataFileName);
					String url = modacFlaskServer + "modac-routing";
					UriComponentsBuilder ucBuilder = UriComponentsBuilder.fromHttpUrl(url);

					ucBuilder.queryParam("dataFileName", dataFileName);
					ucBuilder.queryParam("modelName", modelName);
					ucBuilder.queryParam("resultFileName", resultFileName);
					ucBuilder.queryParam("uploadFrom", t.getUploadFrom());
					ucBuilder.queryParam("outputResultsName",
							t.getActualResultsFileName() != null ? t.getActualResultsFileName() : "None");

					final String requestURL = ucBuilder.build().encode().toUri().toURL().toExternalForm();

					WebClient client1 = DoeClientUtil.getWebClient(requestURL);
					Response restResponse = client1.invoke("GET", null);

					try {
						log.info("response from flask web service" + restResponse);
						if (restResponse.getStatus() == 200 || restResponse.getStatus() == 201) {
							MappingJsonFactory factory = new MappingJsonFactory();
							JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
							String jobId = parser.readValueAs(String.class);
							if (StringUtils.isNotEmpty(jobId)) {
								t.setBatchId(jobId);
								t.setStatus("INPROGRESS");
								inferencingTaskRepository.saveAndFlush(t);
							}

						} else {
							String errorMessage = DoeClientUtil.getErrorMessage(restResponse);
							throw new DoeWebException(errorMessage, restResponse.getStatus());
						}
					} catch (Exception e) {
						throw new DoeWebException(e.getMessage());
					}
				}
			} catch (Exception e) {
				log.error(e.getMessage());
			}

		}

		// verify all the in-progress tasks with dmeTaskId not null to check if the
		// y_prediction file is
		// available to download.

		List<InferencingTask> getAllInProgressTasks = inferencingTaskRepository.getAllInProgressTasks("INPROGRESS");

		for (InferencingTask t : getAllInProgressTasks) {

			log.info("verifying inprogress task for" + t.getTestDataSetPath());
			// get the result path and verify if the y_pred file is available in cloudian
			String resultPath = t.getResultPath();
			String predFileName = resultPath.substring(resultPath.lastIndexOf('/') + 1, resultPath.length());
			String testDataSetFileName = t.getTestDataSetPath().substring(t.getTestDataSetPath().lastIndexOf('/') + 1,
					t.getTestDataSetPath().length());

			String outcomeFileName = t.getActualResultsFileName() != null ? t.getActualResultsFileName().substring(
					t.getActualResultsFileName().lastIndexOf('/') + 1, t.getActualResultsFileName().length()) : null;

			try {
				log.info("verify if inferencing file is available on mount " + predFileName);

				boolean check = new File(uploadPath + predFileName).exists();
				boolean isErrorFile = new File(uploadPath + predFileName + "_error.txt").exists();

				// if predictions file is available, create a collection folder under Asset and
				// upload
				// prediction under this folder
				if (Boolean.TRUE.equals(check)) {
					log.info("pred file available on mount: " + predFileName);
					String parentPath = t.getAssetPath();
					String folderPath = parentPath + "/Prediction_" + t.getTaskId();

					Boolean isFolderPathExists = false;

					try {
						HpcCollectionListDTO collection = DoeClientUtil.getCollection(authToken, serviceURL, folderPath,
								false, true);
						if (collection != null && collection.getCollections() != null
								&& CollectionUtils.isNotEmpty(collection.getCollections())) {
							log.info("Collection already exists: " + folderPath);
							isFolderPathExists = true;
						}
					} catch (DoeWebException e) {
						log.debug("Error in get collection of type folder" + e.getMessage());
					}

					if (Boolean.FALSE.equals(isFolderPathExists)) {

						log.info("Collection does not exist. creating collection of type Folder" + folderPath);
						HpcCollectionRegistrationDTO dto = new HpcCollectionRegistrationDTO();
						List<HpcMetadataEntry> metadataEntries = new ArrayList<>();

						HpcMetadataEntry entry = new HpcMetadataEntry();
						entry.setValue("Prediction");
						entry.setAttribute("collection_type");
						metadataEntries.add(entry);

						// add folder identifier metadata
						HpcMetadataEntry folderIdentifierMetadata = new HpcMetadataEntry();
						folderIdentifierMetadata.setValue("Prediction_" + t.getTaskId());
						folderIdentifierMetadata.setAttribute("prediction_identifier");
						metadataEntries.add(folderIdentifierMetadata);

						dto.getMetadataEntries().addAll(metadataEntries);
						Integer responseStatus = DoeClientUtil.updateCollection(authToken, serviceURL, dto, folderPath);
						if (responseStatus == 200 || responseStatus == 201) {
							log.info("collection created: " + "Prediction_" + t.getTaskId());
							// save folder permissions to MoDaC
							HpcCollectionListDTO collections = DoeClientUtil.getCollection(authToken, serviceURL,
									folderPath, false);

							if (collections != null && collections.getCollections() != null
									&& !CollectionUtils.isEmpty(collections.getCollections())) {

								HpcCollectionDTO collection = collections.getCollections().get(0);

								// save collection prediction permissions in MoDaC DB

								predictionAccessService.savePredictionAccess(t.getUserId(), null,
										collection.getCollection().getCollectionId(), folderPath);
							}
						}
					}

					if (t.getIsReferenceAsset() == null || Boolean.FALSE.equals(t.getIsReferenceAsset())) {

						// upload input dataset file
						log.info("upload test dataset to cloudian: " + testDataSetFileName);
						HpcBulkDataObjectRegistrationRequestDTO registrationDTO = constructV2BulkRequest(t.getUserId(),
								folderPath + "/" + testDataSetFileName, testDataSetFileName);

						// call the FileUpload API to upload the test dataset file to cloudian
						HpcBulkDataObjectRegistrationResponseDTO responseDTO = DoeClientUtil
								.registerBulkDatafiles(authToken, registrationServiceV2URL, registrationDTO);
						if (responseDTO != null) {
							log.info("dme task id for uplaoding test data set" + responseDTO.getTaskId());
							t.setTestDataSetPath(folderPath + "/" + testDataSetFileName);
						}

						// upload outcome file if exists to cloudian
						if (StringUtils.isNotEmpty(outcomeFileName)) {
							HpcBulkDataObjectRegistrationRequestDTO outcomeDTO = constructV2BulkRequest(t.getUserId(),
									folderPath + "/" + outcomeFileName, outcomeFileName);

							// call the FileUpload API to upload the test dataset file to cloudian
							HpcBulkDataObjectRegistrationResponseDTO outcomeResponseDto = DoeClientUtil
									.registerBulkDatafiles(authToken, registrationServiceV2URL, outcomeDTO);
							if (outcomeResponseDto != null) {
								log.info("dme task id for uplaoding test data set" + outcomeResponseDto.getTaskId());
								t.setOutcomeFilePath(folderPath + "/" + outcomeFileName);
							}
						}
					}

					// upload predictions file to cloudian

					HpcBulkDataObjectRegistrationRequestDTO registrationDTO1 = constructV2BulkRequest(t.getUserId(),
							folderPath + "/" + predFileName, predFileName);

					// call the FileUpload API to upload the pred file to cloudian
					HpcBulkDataObjectRegistrationResponseDTO responseDTO1 = DoeClientUtil
							.registerBulkDatafiles(authToken, registrationServiceV2URL, registrationDTO1);
					if (responseDTO1 != null) {
						String dmeTaskId = responseDTO1.getTaskId();
						t.setResultPath(folderPath + "/" + predFileName);
						t.setDmeTaskId(dmeTaskId);
						inferencingTaskRepository.saveAndFlush(t);
					}

				} else if (Boolean.TRUE.equals(isErrorFile)) {
					log.info("error file available on mount: " + predFileName + "_error.txt");
					t.setStatus("FAILED");
					// read contents of error file
					String content = Files.lines(Paths.get(uploadPath + predFileName + "_error.txt"))
							.collect(Collectors.joining(System.lineSeparator()));

					t.setErrorMessage(content);
					inferencingTaskRepository.saveAndFlush(t);

				}

			} catch (Exception e) {
				log.error("Exception in verifying predictions file and uploading: " + e);
				throw new DoeWebException(e.getMessage());
			}

		}
	}

	private gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO constructV2BulkRequest(
			String userId, String path, String fileName) {

		log.info("construct dto for fileName : " + fileName + " and path: " + path);
		gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO dto = new gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO();
		gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO file = new gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO();
		HpcUploadSource fileSystemUploadSource = new HpcUploadSource();
		HpcFileLocation location = new HpcFileLocation();
		location.setFileContainerId(containerId);
		location.setFileId(uploadPath + fileName);
		fileSystemUploadSource.setSourceLocation(location);
		file.setFileSystemUploadSource(fileSystemUploadSource);
		file.setPath(path);

		dto.getDataObjectRegistrationItems().add(file);
		return dto;
	}

	@Scheduled(cron = "${modac.scheduler.cron.transfer.metadata}")
	private void transferTaskToRC() {

		log.info("scheduler to get asset metadata and transfer to an S3 bucket");

		try {
			DoeSearch search = new DoeSearch();

			String[] attrNames = { "collection_type", "metadata_updated" };
			final Calendar cal = Calendar.getInstance();
			cal.add(Calendar.DATE, -30);
			String[] attrValues = { "Asset", format.format(cal.getTime()) };
			search.setAttrName(attrNames);
			search.setAttrValue(attrValues);

			String[] levelValues = { "Asset", "Asset" };
			boolean[] isExcludeParentMetadata = { true, true };
			String[] rowIds = { "1", "2" };
			String[] operators = { "EQUAL", "TIMESTAMP_GREATER_THAN" };
			boolean[] iskeyWordSearch = { false, false };

			search.setLevel(levelValues);
			search.setIsExcludeParentMetadata(isExcludeParentMetadata);
			search.setRowId(rowIds);
			search.setOperator(operators);
			search.setIskeyWordSearch(iskeyWordSearch);

			HpcCompoundMetadataQueryDTO compoundQuery = constructCriteria(search, "Asset");
			compoundQuery.setDetailedResponse(true);
			log.info("search compund query" + compoundQuery);

			UriComponentsBuilder ucBuilder = UriComponentsBuilder.fromHttpUrl(compoundCollectionSearchServiceURL);

			if (ucBuilder != null) {
				final String requestURL = ucBuilder.build().encode().toUri().toURL().toExternalForm();

				WebClient client = DoeClientUtil.getWebClient(requestURL);
				String authToken = DoeClientUtil.getAuthenticationToken(writeAccessUserName, writeAccessUserPassword,
						authenticateURL);
				client.header("Authorization", "Bearer " + authToken);
				Response restResponse = client.invoke("POST", compoundQuery);

				if (restResponse.getStatus() == 200) {

					MappingJsonFactory factory = new MappingJsonFactory();
					JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());
					ObjectMapper mapper = new ObjectMapper();
					mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
					mapper.setSerializationInclusion(JsonInclude.Include.NON_EMPTY);
					String json = mapper.writeValueAsString(parser.readValueAs(HpcCollectionListDTO.class));
					log.info("json for modac transfer: " + json);
					// transfer to modac transfer bucket
					uploadMetadataToS3(json);

				} else if (restResponse.getStatus() == 204) {
					// no metadata retrieved
					log.info("No Metadata updates retrieved for the previous day: " + cal.getTime());
				}
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);

		}
	}

	public void uploadMetadataToS3(String responseJson) throws JsonProcessingException {

		log.info("upload metadata to S3");
		final Calendar cal = Calendar.getInstance();
		String key = "Metadata_" + format.format(cal.getTime()) + ".json";

		try {

			BasicAWSCredentials awsCreds = new BasicAWSCredentials(accessKey, secretKey);
			AmazonS3 s3Client = AmazonS3ClientBuilder.standard()
					.withCredentials(new AWSStaticCredentialsProvider(awsCreds)).withRegion("us-east-1").build();

			s3Client.putObject(bucketName, key, responseJson);

		} catch (AmazonServiceException e) {
			// The call was transmitted successfully, but Amazon S3 couldn't process
			// it, so it returned an error response.
			log.error(e.getMessage(), e);
		} catch (SdkClientException e) {
			// Amazon S3 couldn't be contacted for a response, or the client
			// couldn't parse the response from Amazon S3.
			log.error(e.getMessage(), e);
		}

	}
}
