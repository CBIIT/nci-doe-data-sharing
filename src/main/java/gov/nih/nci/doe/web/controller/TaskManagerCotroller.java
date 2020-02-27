package gov.nih.nci.doe.web.controller;

import javax.servlet.http.HttpSession;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import gov.nih.nci.doe.web.domain.TaskManager;
import gov.nih.nci.doe.web.model.TaskManagerDto;
import gov.nih.nci.doe.web.service.TaskManagerService;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.doe.web.util.LambdaUtils;
import gov.nih.nci.hpc.domain.datatransfer.HpcUserDownloadRequest;
import gov.nih.nci.hpc.dto.datamanagement.HpcBulkDataObjectRegistrationTaskDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDownloadSummaryDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcRegistrationSummaryDTO;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;


/**
 *
 * DOE register Controller
 *
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/tasks")
public class TaskManagerCotroller extends AbstractDoeController {

    
	@Autowired
	TaskManagerService taskManagerService;
	
	@Value("${gov.nih.nci.hpc.server.download}")
	private String queryServiceURL;
	
	@Value("${gov.nih.nci.hpc.server.bulkregistration}")
	private String registrationServiceUrl;

	SimpleDateFormat format = new SimpleDateFormat("MM/dd/yyyy");
	

	@RequestMapping(method = RequestMethod.GET)
	public ResponseEntity<?> register(HttpSession session,@RequestHeader HttpHeaders headers, 
			HttpServletRequest request, @RequestParam(value = "userId") String userId) throws Exception {
		
		  log.info("get all tasks by user Id");
		  String authToken = (String) session.getAttribute("hpcUserToken");
		  try {
            	List<TaskManager> results = new ArrayList<TaskManager>();
            	results = taskManagerService.getAllByUserId(userId);            	
            	List<String> taskIds = LambdaUtils.map(results,TaskManager::getTaskId);
            	
                
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
    			  			
    			
    			 List<HpcUserDownloadRequest> finalTaskIds = LambdaUtils.filter(downloadResults, (HpcUserDownloadRequest n) ->
    			 taskIds.contains(n.getTaskId()));
    			
    			 List<TaskManagerDto> taskResults = new ArrayList<TaskManagerDto>();
                 
    			
    		    for (HpcUserDownloadRequest download : finalTaskIds) {
    					TaskManagerDto task = new TaskManagerDto();
    					TaskManager t = results.stream().filter(x -> download.getTaskId().equals(x.getTaskId())).findAny().orElse(null);
    					
    	    			task.setTaskId(download.getTaskId());
    					task.setTaskDate(t.getTaskDate()!= null ? format.format(t.getTaskDate()) : "");
    					task.setTaskName(t.getTaskName());
    					task.setUserId(t.getUserId());
    					task.setTaskType(t.getTaskType());
    					
    					/*if(download.getResult()) {
    						task.setTransferStatus("Completed");
    					} else if(!download.getResult()){
    						task.setTransferStatus("Failed");
    					} else {
    						task.setTransferStatus("In Progress");
    					}*/
    					
    					taskResults.add(task);
    			}
    		    
    		    
    		    //same for uploads
    		    
    			 List<HpcBulkDataObjectRegistrationTaskDTO> finalTaskUploadIds = LambdaUtils.filter(uploadResults, (HpcBulkDataObjectRegistrationTaskDTO n) ->
    			 taskIds.contains(n.getTaskId()));
    			
    			 List<TaskManagerDto> uploadTaskResults = new ArrayList<TaskManagerDto>();
                 
    			
    		    for (HpcBulkDataObjectRegistrationTaskDTO download : finalTaskUploadIds) {
    					TaskManagerDto task = new TaskManagerDto();
    					TaskManager t = results.stream().filter(x -> download.getTaskId().equals(x.getTaskId())).findAny().orElse(null);
    					
    	    			task.setTaskId(download.getTaskId());
    					task.setTaskDate(t.getTaskDate()!= null ? format.format(t.getTaskDate()) : "");
    					task.setTaskName(t.getTaskName());
    					task.setUserId(t.getUserId());
    					task.setTaskType(t.getTaskType());
    					if(download.getResult() == null) {
    						task.setTransferStatus("In Progress");
    					} else if(download.getResult()) {
    						task.setTransferStatus("Completed");
    					} else if(!download.getResult()) {
    						List<String> message = new ArrayList<String>();
    						download.getFailedItems().stream().forEach(x -> message.add(x.getMessage()));
    						
    						task.setTransferStatus("Failed (" + String.join(",", message) + ")");
    					} 
    					uploadTaskResults.add(task);
    			}
    		    
    		    taskResults.addAll(uploadTaskResults);
    			
    			return new ResponseEntity<>(taskResults, headers, HttpStatus.OK);
    			
             } catch (Exception e) {
                e.printStackTrace();
           }
		
         return new ResponseEntity<>(null, headers, HttpStatus.SERVICE_UNAVAILABLE);
		
	}
	



}
