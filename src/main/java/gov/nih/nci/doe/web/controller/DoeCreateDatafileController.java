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


import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.multipart.MultipartFile;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.DoeDatafileModel;
import gov.nih.nci.doe.web.util.DoeClientUtil;
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
	@Value("${doe.basePath}")
	private String basePath;



	/**
	 * Post operation to update metadata
	 * 
	 * @param doeDataFile
	 * @param session
	 * @param request
	 * @param response
	 * @return
	 */
	@RequestMapping(method = RequestMethod.POST)
	@ResponseBody
	public String createDatafile(@Valid DoeDatafileModel doeDataFileModel,@RequestParam("doeDataFile") MultipartFile doeDataFile,
			@RequestParam("dataFilePath") String  dataFilePath,HttpSession session, HttpServletRequest request, HttpServletResponse response) {
		
		String authToken = (String) session.getAttribute("hpcUserToken");

		String checksum = request.getParameter("checksum");
		if (checksum == null || checksum.isEmpty())
			checksum = (String) request.getAttribute("checksum");

		String path = dataFilePath;
		if(path != null) {
			doeDataFileModel.setPath(path.trim());
		}


		try {
			if (doeDataFileModel.getPath() == null || doeDataFileModel.getPath().trim().length() == 0)
				return  "Invald Data file path";
			// Validate parent path
			String parentPath = null;
			doeDataFileModel.setPath(doeDataFileModel.getPath().trim());
			try {
				
				parentPath = doeDataFileModel.getPath().substring(0, doeDataFileModel.getPath().lastIndexOf("/"));
				if (!parentPath.isEmpty())
					DoeClientUtil.getCollection(authToken, collectionServiceURL, parentPath, true, sslCertPath,
							sslCertPassword);
			} catch (DoeWebException e) {
				return "Invalid parent collection: " + e.getMessage();
			}

			HpcDataObjectRegistrationRequestDTO registrationDTO = constructSyncRequest(request, session, path);

			registrationDTO.setChecksum(checksum);
			boolean created =  DoeClientUtil.registerDatafile(authToken, doeDataFile, serviceURL, registrationDTO,
					path, sslCertPath, sslCertPassword);
			if(created) {
				return "Data File registered";
			}

			clearSessionAttrs(session);
			
		} catch (Exception e) {
			log.debug(e.getMessage());
			return "Failure to register Data File";
		} 
		
		
        return null;
		
	}


	private HpcDataObjectRegistrationRequestDTO constructSyncRequest(HttpServletRequest request, HttpSession session,
			String path) {

		HpcDataObjectRegistrationRequestDTO dto = new HpcDataObjectRegistrationRequestDTO();
		dto.getMetadataEntries().addAll(getMetadataEntries(request, session, path));
		dto.setSource(null);
		return dto;
	}


}
