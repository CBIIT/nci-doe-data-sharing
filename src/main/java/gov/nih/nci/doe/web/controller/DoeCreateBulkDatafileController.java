package gov.nih.nci.doe.web.controller;

import java.util.Date;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.AuditingModel;
import gov.nih.nci.doe.web.model.DoeDatafileModel;
import gov.nih.nci.doe.web.service.TaskManagerService;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementModelDTO;
/**
 * <p>
 * Add data file controller.
 * </p>
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/addbulk")
public class DoeCreateBulkDatafileController extends DoeCreateCollectionDataFileController {
	
	@Value("${gov.nih.nci.hpc.server.dataObject}")
	private String serviceURL;
	@Value("${gov.nih.nci.hpc.server.collection}")
	private String collectionServiceURL;
	@Value("${gov.nih.nci.hpc.server.model}")
	private String hpcModelURL;
	@Value("${gov.nih.nci.hpc.server.v2.bulkregistration}")
	private String bulkRegistrationURL;
	@Value("${gov.nih.nci.hpc.web.server}")
	private String webServerName;	
	@Value("${doe.basePath}")
	private String basePath;
	
	@Autowired
	TaskManagerService taskManagerService;
		
	
	@GetMapping
	public String home(Model model, HttpSession session, HttpServletRequest request) {

		
		if(request.getParameterNames().hasMoreElements()) {
		  setInputParameters(request, session,model);
		} else {
			clearSessionAttrs(session);
		}
		return "upload";
	}
	

	/**
	 * Post operation to update metadata
	 * 
	 * @param hpcDatafile
	 * @param model
	 * @param bindingResult
	 * @param session
	 * @param request
	 * @param response
	 * @param redirectAttributes
	 * @return
	 */
	@SuppressWarnings("unchecked")
	@PostMapping
	@ResponseBody
	public String createDatafile(@Valid DoeDatafileModel doeDataFileModel, HttpSession session, HttpServletRequest request, HttpServletResponse response) {
		
		String authToken = (String) session.getAttribute("writeAccessUserToken");
		String user = getLoggedOnUserInfo();
			String dataFilePath = request.getParameter("bulkDatafilePath");
			if(dataFilePath != null) {
			doeDataFileModel.setPath(dataFilePath.trim());
			}
			if (doeDataFileModel.getPath() == null || doeDataFileModel.getPath().trim().length() == 0)
				return "Invalid Data file path";
			
			doeDataFileModel.setPath(doeDataFileModel.getPath().trim());
			HpcBulkDataObjectRegistrationRequestDTO registrationDTO = constructV2BulkRequest(request, session,
					doeDataFileModel.getPath().trim());
			String bulkType = request.getParameter("uploadType");
			
			if( CollectionUtils.isEmpty(registrationDTO.getDataObjectRegistrationItems()) &&
					CollectionUtils.isEmpty(registrationDTO.getDirectoryScanRegistrationItems()))
				throw new DoeWebException("No input file(s) / folder(s) are selected");
			Set<String> basePaths = (Set<String>) session.getAttribute("basePaths");
			
			if (basePaths == null || basePaths.isEmpty()) {
				HpcDataManagementModelDTO modelDTO = (HpcDataManagementModelDTO) session.getAttribute("userDOCModel");
				if (modelDTO == null) {
					modelDTO = DoeClientUtil.getDOCModel(authToken, hpcModelURL, sslCertPath, sslCertPassword);
					session.setAttribute("userDOCModel", modelDTO);
				}
				String userId = (String) session.getAttribute("hpcUserId");
				DoeClientUtil.populateBasePaths(session, modelDTO, authToken, userId, serviceURL, sslCertPath,
						sslCertPassword);
				basePaths = (Set<String>) session.getAttribute("basePaths");
			}
			
			HpcBulkDataObjectRegistrationResponseDTO responseDTO = DoeClientUtil.registerBulkDatafiles(authToken,
					bulkRegistrationURL, registrationDTO, sslCertPath, sslCertPassword);
			
			if (responseDTO != null) {				
				   clearSessionAttrs(session);
				    String taskId = responseDTO.getTaskId();
				    String name = doeDataFileModel.getPath().substring(doeDataFileModel.getPath().lastIndexOf('/') + 1);
				    taskManagerService.saveTransfer(taskId,"Upload",null,name,user);
				    
				    //store the auditing info
	                  AuditingModel audit = new AuditingModel();
	                  audit.setName(user);
	                  audit.setOperation("Upload");
	                  audit.setStartTime(new Date());
	                  audit.setTransferType(bulkType);
	                  audit.setPath(doeDataFileModel.getPath());
	                  audit.setTaskId(taskId);
	                  auditingService.saveAuditInfo(audit);
	                  
					return "Bulk Data file registration request is submitted! Task Id: " +taskId;			
		
				
			}

		return null;
	}

}
