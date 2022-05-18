package gov.nih.nci.doe.web.controller;

import java.util.Date;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;
import javax.ws.rs.core.Response;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.multipart.MultipartFile;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.AuditingModel;
import gov.nih.nci.doe.web.model.DoeDatafileModel;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectRegistrationRequestDTO;

/**
 *
 * Add data file controller.
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/addDatafile")
public class DoeCreateDatafileController extends DoeCreateCollectionDataFileController {

	@Value("${gov.nih.nci.hpc.server.collection}")
	private String collectionServiceURL;

	/**
	 * Post operation to update metadata
	 * 
	 * @param doeDataFile
	 * @param session
	 * @param request
	 * @param response
	 * @return
	 */
	@PostMapping
	@ResponseBody
	public String createDatafile(@Valid DoeDatafileModel doeDataFileModel,
			@RequestParam("doeDataFile") MultipartFile doeDataFile, @RequestParam("dataFilePath") String dataFilePath,
			HttpSession session, HttpServletRequest request, HttpServletResponse response) {

		log.info("Upload Single file from local system");
		String authToken = (String) session.getAttribute("writeAccessUserToken");
		String user = getLoggedOnUserInfo();
		if (StringUtils.isEmpty(user)) {
			return "Not Authorized";
		}
		String checksum = request.getParameter("checksum");
		if (checksum == null || checksum.isEmpty())
			checksum = (String) request.getAttribute("checksum");

		String path = dataFilePath;
		if (path != null) {
			doeDataFileModel.setPath(path.trim());
		}
		try {
			if (doeDataFileModel.getPath() == null || doeDataFileModel.getPath().trim().length() == 0)
				return "Invald Data file path";
			// Validate parent path
			String parentPath = null;
			doeDataFileModel.setPath(doeDataFileModel.getPath().trim());

			parentPath = doeDataFileModel.getPath().substring(0, doeDataFileModel.getPath().lastIndexOf('/'));
			if (!parentPath.isEmpty()) {
				HpcCollectionListDTO parentCollectionDto = DoeClientUtil.getCollection(authToken, collectionServiceURL,
						parentPath, true);
				Boolean isValidPermissions = verifyCollectionPermissions(parentPath, parentCollectionDto);
				if (Boolean.FALSE.equals(isValidPermissions)) {
					return "Insufficient privileges to add data files.";
				}
			}

			HpcDataObjectRegistrationRequestDTO registrationDTO = constructSyncRequest(request, session, path);

			registrationDTO.setChecksum(checksum);
			Response restResponse = DoeClientUtil.registerDatafile(authToken, doeDataFile, dataObjectAsyncServiceURL,
					registrationDTO, path);
			if (restResponse.getStatus() == 200 || restResponse.getStatus() == 201) {

				// store the auditing info
				AuditingModel audit = new AuditingModel();
				audit.setName(user);
				audit.setOperation("Upload Single File");
				audit.setStartTime(new Date());
				audit.setPath(doeDataFileModel.getPath());
				auditingService.saveAuditInfo(audit);

				return "The system has registered your file.";
			}

			clearSessionAttrs(session);
			return "Error in registration.";
		} catch (Exception e) {
			log.error("failed to register data file" + e);
			return e.getMessage();
		}

	}

	private HpcDataObjectRegistrationRequestDTO constructSyncRequest(HttpServletRequest request, HttpSession session,
			String path) throws DoeWebException {

		HpcDataObjectRegistrationRequestDTO dto = new HpcDataObjectRegistrationRequestDTO();
		dto.getMetadataEntries().addAll(getMetadataEntries(request, session, path));
		return dto;
	}

}
