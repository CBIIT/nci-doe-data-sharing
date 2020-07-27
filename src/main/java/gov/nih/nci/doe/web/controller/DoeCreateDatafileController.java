/**
 * HpcCollectionController.java
 *
 * Copyright SVG, Inc.
 * Copyright Leidos Biomedical Research, Inc
 * 
 * Distributed under the OSI-approved BSD 3-Clause License.
 * See https://ncisvn.nci.nih.gov/svn/HPC_Data_Management/branches/hpc-prototype-dev/LICENSE.txt for details.
 */
package gov.nih.nci.doe.web.controller;


import java.util.Date;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.multipart.MultipartFile;

import gov.nih.nci.doe.web.model.AuditingModel;
import gov.nih.nci.doe.web.model.DoeDatafileModel;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectRegistrationRequestDTO;

/**
 * <p>
 * Add data file controller.
 * </p>
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/addDatafile")
public class DoeCreateDatafileController extends DoeCreateCollectionDataFileController {
	
	@Value("${gov.nih.nci.hpc.server.dataObject}")
	private String serviceURL;
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
	public String createDatafile(@Valid DoeDatafileModel doeDataFileModel,@RequestParam("doeDataFile") MultipartFile doeDataFile,
			@RequestParam("dataFilePath") String  dataFilePath,HttpSession session, HttpServletRequest request, HttpServletResponse response) {
		
		String authToken = (String) session.getAttribute("writeAccessUserToken");
		String user = getLoggedOnUserInfo();

		String checksum = request.getParameter("checksum");
		if (checksum == null || checksum.isEmpty())
			checksum = (String) request.getAttribute("checksum");

		String path = dataFilePath;
		if(path != null) {
			doeDataFileModel.setPath(path.trim());
		}

			if (doeDataFileModel.getPath() == null || doeDataFileModel.getPath().trim().length() == 0)
				return  "Invald Data file path";
			// Validate parent path
			String parentPath = null;
			doeDataFileModel.setPath(doeDataFileModel.getPath().trim());

				parentPath = doeDataFileModel.getPath().substring(0, doeDataFileModel.getPath().lastIndexOf('/'));
				if (!parentPath.isEmpty()) {
					HpcCollectionListDTO parentCollectionDto = DoeClientUtil.getCollection(authToken, collectionServiceURL, parentPath, true, sslCertPath,
							sslCertPassword);
					Boolean isValidPermissions = verifyCollectionPermissions(parentPath,parentCollectionDto);
					if (Boolean.FALSE.equals(isValidPermissions)) {
							return "Insufficient privileges to add data files.";
						}
				   }
				

			HpcDataObjectRegistrationRequestDTO registrationDTO = constructSyncRequest(request, session, path);

			registrationDTO.setChecksum(checksum);
			boolean created =  DoeClientUtil.registerDatafile(authToken, doeDataFile, serviceURL, registrationDTO,
					path, sslCertPath, sslCertPassword);
			if(created) {
				
				//store the auditing info
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
		
	}


	private HpcDataObjectRegistrationRequestDTO constructSyncRequest(HttpServletRequest request, HttpSession session,
			String path) {

		HpcDataObjectRegistrationRequestDTO dto = new HpcDataObjectRegistrationRequestDTO();
		dto.getMetadataEntries().addAll(getMetadataEntries(request, session, path));
		dto.setSource(null);
		return dto;
	}


}
