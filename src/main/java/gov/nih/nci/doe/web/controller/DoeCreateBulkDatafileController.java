package gov.nih.nci.doe.web.controller;

import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.DoeDatafileModel;
import gov.nih.nci.doe.web.service.TaskManagerService;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataManagementModelDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO;
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
	@Value("${gov.nih.nci.hpc.server.bulkregistration}")
	private String bulkRegistrationURL;
	@Value("${gov.nih.nci.hpc.web.server}")
	private String webServerName;	
	@Value("${doe.basePath}")
	private String basePath;
	
	@Autowired
	TaskManagerService taskManagerService;
		
	
	@RequestMapping(method = RequestMethod.GET)
	public String home(Model model, HttpSession session, HttpServletRequest request) {

		setInputParameters(request, session,model);
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
	@RequestMapping(method = RequestMethod.POST)
	@ResponseBody
	public String createDatafile(@Valid DoeDatafileModel doeDataFileModel, HttpSession session, HttpServletRequest request, HttpServletResponse response) {
		
		String authToken = (String) session.getAttribute("hpcUserToken");
		String checksum = request.getParameter("checksum");
		if (checksum == null || checksum.isEmpty())
			checksum = (String) request.getAttribute("checksum");


			String dataFilePath = request.getParameter("bulkDatafilePath");
			if(dataFilePath != null) {
			doeDataFileModel.setPath(dataFilePath.trim());
			}
			

		try {
			if (doeDataFileModel.getPath() == null || doeDataFileModel.getPath().trim().length() == 0)
				return "Invalid Data file path";
			
			doeDataFileModel.setPath(doeDataFileModel.getPath().trim());
			HpcBulkDataObjectRegistrationRequestDTO registrationDTO = constructV2BulkRequest(request, session,
					doeDataFileModel.getPath().trim());
			
			if(registrationDTO.getDataObjectRegistrationItems().size() == 0 && registrationDTO.getDirectoryScanRegistrationItems().size() == 0)
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
				StringBuffer info = new StringBuffer();
				   for (HpcDataObjectRegistrationItemDTO responseItem : responseDTO.getDataObjectRegistrationItems()) {
					info.append(responseItem.getPath()).append("<br/>");
				   }
				    String taskId = responseDTO.getTaskId();
				    taskManagerService.saveTransfer(taskId,"Upload",getLoggedOnUserInfo());
					return "Bulk Data file registration request is submitted! Task Id: " +taskId;			
		
				
			}
		} catch (Exception e) {
		   log.error("failed to create data file: " + e.getMessage(), e);
		   String msg = e.getMessage().replace("\n", "<br/>");
		   return "Failed to create data file:" + msg;
     
		} 

		return null;
	}

}
