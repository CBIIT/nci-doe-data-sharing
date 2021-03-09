package gov.nih.nci.doe.web.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

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

import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationRequestDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcBulkDataObjectRegistrationStatusDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationItemDTO;

@Controller
@EnableAutoConfiguration
@RequestMapping("/uploadtask")
public class DoeRetryUploadtaskController extends AbstractDoeController {

	@Value("${gov.nih.nci.hpc.server.v2.bulkregistration}")
	private String registrationServiceURL;

	@Autowired
	TaskManagerService taskManagerService;

	/**
	 * POST action to retry failed upload files.
	 * 
	 * @param uploadFile
	 * @param model
	 * @param bindingResult
	 * @param session
	 * @param request
	 * @param response
	 * @return
	 */
	@JsonView(Views.Public.class)
	@PostMapping
	@ResponseBody
	public String retryUpload(@RequestParam(value = "taskId") String taskId,
			@RequestParam(value = "taskName") String taskName, HttpSession session, HttpServletRequest request) {

		AjaxResponseBody result = new AjaxResponseBody();
		try {
			String authToken = (String) session.getAttribute("writeAccessUserToken");

			HpcBulkDataObjectRegistrationStatusDTO uploadTask = DoeClientUtil.getDataObjectRegistrationTask(authToken,
					this.registrationServiceURL, taskId, this.sslCertPath, this.sslCertPassword);

			HpcBulkDataObjectRegistrationRequestDTO registrationDTO = constructBulkRequest(request, session,
					uploadTask);

			HpcBulkDataObjectRegistrationResponseDTO responseDTO = DoeClientUtil.registerBulkDatafiles(authToken,
					registrationServiceURL, registrationDTO, sslCertPath, sslCertPassword);
			if (responseDTO != null) {
				StringBuffer info = new StringBuffer();
				for (HpcDataObjectRegistrationItemDTO responseItem : responseDTO.getDataObjectRegistrationItems()) {
					info.append(responseItem.getPath()).append("<br/>");
				}

				taskManagerService.saveTransfer(responseDTO.getTaskId(), "Upload", null, taskName,
						getLoggedOnUserInfo());
				return "Your bulk data file registration request has the following task ID: <a href='uploadtask?type=&taskId="
						+ responseDTO.getTaskId() + "'>" + responseDTO.getTaskId() + "</a>";
			}

		} catch (Exception e) {
			log.error("Upload request is not successful: " + e.getMessage(), e);
			result.setMessage("Upload request is not successful: " + e.getMessage());
			return result.getMessage();
		}
		return null;
	}

	protected HpcBulkDataObjectRegistrationRequestDTO constructBulkRequest(HttpServletRequest request,
			HttpSession session, HpcBulkDataObjectRegistrationStatusDTO uploadTask) {
		HpcBulkDataObjectRegistrationRequestDTO dto = new HpcBulkDataObjectRegistrationRequestDTO();
		dto.getDataObjectRegistrationItems().addAll(uploadTask.getTask().getFailedItemsRequest());
		return dto;
	}
}
