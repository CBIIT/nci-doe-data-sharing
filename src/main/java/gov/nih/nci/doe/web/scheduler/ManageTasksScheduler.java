package gov.nih.nci.doe.web.scheduler;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.controller.AbstractDoeController;
import gov.nih.nci.doe.web.domain.Auditing;
import gov.nih.nci.doe.web.repository.AuditingRepository;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.domain.datatransfer.HpcUserDownloadRequest;
import gov.nih.nci.hpc.dto.datamanagement.HpcDownloadSummaryDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationTaskDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcRegistrationSummaryDTO;


@Component
public class ManageTasksScheduler extends AbstractDoeController {
	
	@Value("${gov.nih.nci.hpc.server.download}")
	private String queryServiceURL;
	
	@Value("${gov.nih.nci.hpc.server.user.authenticate}")
	private String authenticateURL;	
	
	@Value("${doe.writeaccount.password}")
	private String writeAccessUserPassword;

	@Value("${doe.writeaccount.username}")
	private String writeAccessUserName;
	
	@Value("${gov.nih.nci.hpc.server.bulkregistration}")
	private String registrationServiceUrl;
	
	@Autowired
	AuditingRepository auditingRepository;
	
	  @Scheduled(cron = "${doe.scheduler.cron.auditing}")
	  public void updateAuditingService() throws DoeWebException{
		  
		  String authToken = DoeClientUtil.getAuthenticationToken(writeAccessUserName, writeAccessUserPassword,authenticateURL);
		  
		  String serviceURL = queryServiceURL + "?page=" + 1 + "&totalCount=true";
			HpcDownloadSummaryDTO downloads = DoeClientUtil.getDownloadSummary(authToken, serviceURL, sslCertPath,
					sslCertPassword);
			
			final MultiValueMap<String,String> paramsMap = new LinkedMultiValueMap<>();
		      paramsMap.set("totalCount", Boolean.TRUE.toString());
			HpcRegistrationSummaryDTO registrations = DoeClientUtil.getRegistrationSummary(authToken, registrationServiceUrl, paramsMap,
		        sslCertPath, sslCertPassword);
			
			List<HpcUserDownloadRequest> downloadResults = new ArrayList<HpcUserDownloadRequest>();
			if(downloads != null) {
				downloadResults.addAll(downloads.getActiveTasks());
    			downloadResults.addAll(downloads.getCompletedTasks());  
			}
			
			List<HpcBulkDataObjectRegistrationTaskDTO> uploadResults = new ArrayList<HpcBulkDataObjectRegistrationTaskDTO>();
			if(registrations != null) {
				uploadResults.addAll(registrations.getActiveTasks());
				uploadResults.addAll(registrations.getCompletedTasks());  
			}
			  	
			
			List<Auditing> auditingTaskIds = auditingService.getAllTaskIds();
			
			if(CollectionUtils.isNotEmpty(auditingTaskIds)) {
				for(Auditing audit : auditingTaskIds) {
					if(audit.getOperation().equalsIgnoreCase("Upload")) {
						HpcBulkDataObjectRegistrationTaskDTO upload = uploadResults.stream().filter(
								x -> audit.getTaskId().equals(x.getTaskId())).findAny().orElse(null);
					    
						if(upload != null) {
						audit.setCompletionTime((upload != null && upload.getCompleted() != null) ? upload.getCompleted().getTime(): null);
					    if(upload.getResult() == null) {
					    	audit.setStatus("In progress");
    					} else if(Boolean.TRUE.equals(upload.getResult())) {
    						audit.setStatus("Completed");
    					} else if(Boolean.FALSE.equals(upload.getResult())) {
    						audit.setStatus("Failed");
    						List<String> message = new ArrayList<String>();
    						upload.getFailedItems().stream().forEach(x -> message.add(x.getMessage()));
    						audit.setErrorMsg(message.get(0));
    					}
					    auditingRepository.save(audit);
						}
					} else if(audit.getOperation().equalsIgnoreCase("Download")) { 
						HpcUserDownloadRequest download = downloadResults.stream().filter(
								x -> audit.getTaskId().equals(x.getTaskId())).findAny().orElse(null);
						if(download != null) {
						audit.setCompletionTime((download != null && download.getCompleted() != null)? download.getCompleted().getTime(): null);
					       if(download != null && download.getResult() != null && download.getResult().value().equals("FAILED")) {
	    						List<String> message = new ArrayList<String>();
	    						download.getItems().stream().forEach(x -> message.add(x.getMessage()));    						
	    						audit.setStatus("Failed");
	    						audit.setErrorMsg(message.get(0));
	    										
	    					} else if(download.getResult() != null && download.getResult().value().equals("COMPLETED")) {
	    						audit.setStatus("Completed");
	    					} else {
	    						audit.setStatus("In Progress");
	    					}
					       auditingRepository.save(audit);
						}
					}
					   
				}
			}
	  }

}
