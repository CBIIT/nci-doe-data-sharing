package gov.nih.nci.doe.web.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.fasterxml.jackson.annotation.JsonView;
import gov.nih.nci.doe.web.model.AjaxResponseBody;
import gov.nih.nci.doe.web.model.Views;
import gov.nih.nci.doe.web.service.TaskManagerService;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.domain.datatransfer.HpcCollectionDownloadTaskItem;
import gov.nih.nci.hpc.domain.datatransfer.HpcDownloadTaskType;
import gov.nih.nci.hpc.domain.datatransfer.HpcFileLocation;
import gov.nih.nci.hpc.domain.datatransfer.HpcGlobusDownloadDestination;
import gov.nih.nci.hpc.dto.datamanagement.HpcBulkDataObjectDownloadResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDownloadStatusDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectDownloadStatusDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectDownloadRequestDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcDownloadRequestDTO;
@Controller
@EnableAutoConfiguration
@RequestMapping("/downloadtask")
public class DoeRetryDownloadTaskController extends AbstractDoeController {

	
	@Value("${gov.nih.nci.hpc.server.bulkregistration}")
	private String registrationServiceURL;
	  @Value("${gov.nih.nci.hpc.server.download}")
	  private String dataObjectsDownloadServiceURL;

	  @Value("${gov.nih.nci.hpc.server.collection.download}")
	  private String collectionDownloadServiceURL;

	  @Value("${gov.nih.nci.hpc.server.dataObject.download}")
	  private String dataObjectDownloadServiceURL;

	  @Value("${gov.nih.nci.hpc.server.v2.dataObject}")
	  private String dataObjectServiceURL;

	  @Value("${gov.nih.nci.hpc.server.download}")
	  private String downloadServiceURL;
	  
	  @Value("${gov.nih.nci.hpc.server.v2.download}")
	  private String downloadServiceURL2;
	  
		@Autowired
		TaskManagerService taskManagerService;
	
	 @JsonView(Views.Public.class)
	 @PostMapping
	 @ResponseBody
	 public String retryDownload(@RequestParam String taskName,
	      @RequestParam String taskId, @RequestParam String taskType,
	      HttpSession session, HttpServletRequest request) {
	    AjaxResponseBody result = new AjaxResponseBody();
	    try {
	    	String authToken = null;
	    	String loggedOnUser = getLoggedOnUserInfo();
			if(loggedOnUser != null && !StringUtils.isEmpty(loggedOnUser) && !StringUtils.isBlank(loggedOnUser)) {
				 authToken = (String) session.getAttribute("writeAccessUserToken");
			} else {
				 authToken = (String) session.getAttribute("hpcUserToken");
			}
			if (authToken == null) {
				result.setMessage("Invalid user session, expired. Login again.");
				return result.getMessage();
			}
	      HpcBulkDataObjectDownloadRequestDTO dto = new HpcBulkDataObjectDownloadRequestDTO();
	      if (taskType.equalsIgnoreCase(HpcDownloadTaskType.COLLECTION.name())
	          || taskType.equalsIgnoreCase(HpcDownloadTaskType.DATA_OBJECT_LIST.name())
	          || taskType.equalsIgnoreCase(HpcDownloadTaskType.COLLECTION_LIST.name())) {
	        
	        String queryServiceURL = null;
	        if (taskType.equalsIgnoreCase(HpcDownloadTaskType.COLLECTION.name()))
	            queryServiceURL = collectionDownloadServiceURL + "?taskId=" + taskId;
	        else
	            queryServiceURL = dataObjectsDownloadServiceURL + "/" + taskId;
	            
	        HpcCollectionDownloadStatusDTO downloadTask = DoeClientUtil
	            .getDataObjectsDownloadTask(authToken, queryServiceURL, sslCertPath, sslCertPassword);
	        if (downloadTask.getFailedItems() != null && !downloadTask.getFailedItems().isEmpty()) {
	          for (HpcCollectionDownloadTaskItem item : downloadTask.getFailedItems())
	            dto.getDataObjectPaths().add(item.getPath());
	          HpcGlobusDownloadDestination globusDownloadDestination = new HpcGlobusDownloadDestination();
	          HpcFileLocation location = downloadTask.getDestinationLocation();
			  globusDownloadDestination.setDestinationLocation(location);
			  globusDownloadDestination.setDestinationOverwrite(true);
	          dto.setGlobusDownloadDestination(globusDownloadDestination);
	        }
	        if (downloadTask.getCanceledItems() != null && !downloadTask.getCanceledItems().isEmpty()) {
	          for (HpcCollectionDownloadTaskItem item : downloadTask.getCanceledItems())
	            dto.getDataObjectPaths().add(item.getPath());
	          HpcGlobusDownloadDestination globusDownloadDestination = new HpcGlobusDownloadDestination();
	          HpcFileLocation location = downloadTask.getDestinationLocation();
	          globusDownloadDestination.setDestinationLocation(location);
	          globusDownloadDestination.setDestinationOverwrite(true);
	          dto.setGlobusDownloadDestination(globusDownloadDestination);
	        }
	        try {
	          HpcBulkDataObjectDownloadResponseDTO downloadDTO =  DoeClientUtil.downloadFiles(authToken,
	            		  downloadServiceURL2, dto, sslCertPath, sslCertPassword);
	          if (downloadDTO != null) {
	        	  if(loggedOnUser != null) {
	                  taskManagerService.saveTransfer(downloadDTO.getTaskId(),"Download",taskType,taskName,getLoggedOnUserInfo());  
	              }
	            result.setMessage("Download request successful. Task Id: " + downloadDTO.getTaskId());	            
	          }

	        } catch (Exception e) {
	        	log.error("Download request is not successful: " + e);
	          result.setMessage("Download request is not successful: " + e.getMessage());
	        }
	        return result.getMessage();

	      } else if (taskType.equalsIgnoreCase(HpcDownloadTaskType.DATA_OBJECT.name())) {
	        String queryServiceURL = dataObjectDownloadServiceURL + "?taskId=" + taskId;
	        HpcDataObjectDownloadStatusDTO downloadTask = DoeClientUtil
	            .getDataObjectDownloadTask(authToken, queryServiceURL, sslCertPath, sslCertPassword);
	        String serviceURL = dataObjectServiceURL + downloadTask.getPath() + "/download";
	        if (downloadTask.getResult() != null && downloadTask.getResult().value().equals("COMPLETED")) {
	          HpcDownloadRequestDTO downloadDTO = new HpcDownloadRequestDTO();
	          HpcGlobusDownloadDestination destination = new HpcGlobusDownloadDestination();
			  HpcFileLocation location = downloadTask.getDestinationLocation();
			  destination.setDestinationLocation(location);
	          downloadDTO.setGlobusDownloadDestination(destination);
	          AjaxResponseBody responseBody = DoeClientUtil.downloadDataFile(authToken, serviceURL,
	              downloadDTO, HpcDownloadTaskType.DATA_OBJECT.name(), sslCertPath, sslCertPassword);
	          if(loggedOnUser != null) {
                  taskManagerService.saveTransfer(responseBody.getMessage(),"Download",taskType,taskName,getLoggedOnUserInfo());  
              }
	          return responseBody.getMessage();
	        }

	      }
	    }  catch (Exception e) {
	    	log.error("Download request is not successful: " + e);
	      result.setMessage("Download request is not successful: " + e.getMessage());
	      return result.getMessage();
	    }
	    return null;
	  }
}
