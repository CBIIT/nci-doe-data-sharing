
package gov.nih.nci.doe.web.controller;


import java.util.*;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.AuditingModel;
import gov.nih.nci.doe.web.model.DoeCollectionModel;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionRegistrationDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectRegistrationRequestDTO;


/**
 * <p>
 * Collection controller. Gets selected collection details. Updates collection
 * metadata.
 * </p>
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/collection")
public class DoeCollectionController extends AbstractDoeController {
   
	@Value("${gov.nih.nci.hpc.server.collection}")
	private String serviceURL;
	
	@Value("${gov.nih.nci.hpc.server.model}")
	private String hpcModelURL;

	@Value("${gov.nih.nci.hpc.server.dataObject}")
	private String serviceDataURL;
	/**
	 * Update collection
	 *
	 * @param hpcCollection
	 * @param model
	 * @param bindingResult
	 * @param session
	 * @param request
	 * @param response
	 * @param redirectAttributes
	 * @return
	 */
	@PostMapping
	public @ResponseBody String updateCollection(@Valid DoeCollectionModel doeCollection, 
			HttpSession session, HttpServletRequest request, HttpServletResponse response) throws DoeWebException{

		String authToken = (String) session.getAttribute("writeAccessUserToken");
			String loggedOnUser = getLoggedOnUserInfo();
			String[] path = request.getParameterValues("path");
			String isDataObject = request.getParameter("isDataObject");
			
			
			if(path[0] != null) {
				doeCollection.setPath(path[0].trim());
			}
			
			if (doeCollection.getPath() == null || doeCollection.getPath().trim().length() == 0) {
			  return "Invalid collection path";
			}

			if(isDataObject != null && isDataObject.equalsIgnoreCase("true")) {
				HpcDataObjectRegistrationRequestDTO registrationDTO = constructDataRequest(request);
				boolean updated = DoeClientUtil.updateDatafile(authToken, serviceDataURL, registrationDTO,
						doeCollection.getPath(), sslCertPath, sslCertPassword);
				if (updated) {
					session.removeAttribute("selectedUsers");
					return "The metadata was successfully updated.";
					
				}
				
			} else {
				HpcCollectionRegistrationDTO registrationDTO = constructRequest(request);
				Integer restResponse = DoeClientUtil.updateCollection(authToken, serviceURL, registrationDTO,
						doeCollection.getPath(), sslCertPath, sslCertPassword);
				if (restResponse == 200 || restResponse == 201) {
					session.removeAttribute("selectedUsers");
					 //store the auditing info
	                  AuditingModel audit = new AuditingModel();
	                  audit.setName(loggedOnUser);
	                  audit.setOperation("Edit Meta Data");
	                  audit.setStartTime(new Date());
	                  audit.setPath(doeCollection.getPath());
	                  auditingService.saveAuditInfo(audit);
					return "The metadata was successfully updated.";
				}
			}
			
		
		final Map<String, String> paramsMap = new HashMap<>();
		paramsMap.put("path", doeCollection.getPath());		
		return "ERROR";
		
	}


	private HpcCollectionRegistrationDTO constructRequest(HttpServletRequest request) throws DoeWebException{
		Enumeration<String> params = request.getParameterNames();
		HpcCollectionRegistrationDTO dto = new HpcCollectionRegistrationDTO();
		List<HpcMetadataEntry> metadataEntries = new ArrayList<>();

		while (params.hasMoreElements()) {
			String paramName = params.nextElement();
			if (paramName.startsWith("zAttrStr_")) {
				HpcMetadataEntry entry = new HpcMetadataEntry();
				String attrName = paramName.substring("zAttrStr_".length());
				String[] attrValue = request.getParameterValues(paramName);
				entry.setAttribute(attrName);
				entry.setValue(attrValue[0]);
				//if(StringUtils.isNotEmpty(entry.getValue())) {
				metadataEntries.add(entry);
				//}
			} else if (paramName.startsWith("_addAttrName")) {
				HpcMetadataEntry entry = new HpcMetadataEntry();
				String attrId = paramName.substring("_addAttrName".length());
				String[] attrName = request.getParameterValues(paramName);
				String[] attrValue = request.getParameterValues("_addAttrValue" + attrId);
				if (attrName.length > 0 && !attrName[0].isEmpty())
					entry.setAttribute(attrName[0]);
				else
					throw new DoeWebException("Invalid metadata attribute name. Empty value is not valid!");
				if (attrValue.length > 0 && !attrValue[0].isEmpty())
					entry.setValue(attrValue[0]);
				else
					throw new DoeWebException("Invalid metadata attribute value. Empty value is not valid!");
				metadataEntries.add(entry);
			}
		}
		dto.getMetadataEntries().addAll(metadataEntries);
		return dto;
	}

	
    private HpcDataObjectRegistrationRequestDTO constructDataRequest(HttpServletRequest request) throws DoeWebException{
		Enumeration<String> params = request.getParameterNames();
		HpcDataObjectRegistrationRequestDTO dto = new HpcDataObjectRegistrationRequestDTO();
		List<HpcMetadataEntry> metadataEntries = new ArrayList<>();

		while (params.hasMoreElements()) {
			String paramName = params.nextElement();
			if (paramName.startsWith("zAttrStr_")) {
				HpcMetadataEntry entry = new HpcMetadataEntry();
				String attrName = paramName.substring("zAttrStr_".length());
				String[] attrValue = request.getParameterValues(paramName);
				entry.setAttribute(attrName);
				entry.setValue(attrValue[0]);
				//if(StringUtils.isNotEmpty(entry.getValue())) {
					metadataEntries.add(entry);
				//}
			} else if (paramName.startsWith("_addAttrName")) {
				HpcMetadataEntry entry = new HpcMetadataEntry();
				String attrId = paramName.substring("_addAttrName".length());
				String[] attrName = request.getParameterValues(paramName);
				String[] attrValue = request.getParameterValues("_addAttrValue" + attrId);
				if (attrName.length > 0 && !attrName[0].isEmpty())
					entry.setAttribute(attrName[0]);
				else
					throw new DoeWebException("Invalid metadata attribute name. Empty value is not valid!");
				if (attrValue.length > 0 && !attrValue[0].isEmpty())
					entry.setValue(attrValue[0]);
				else
					throw new DoeWebException("Invalid metadata attribute value. Empty value is not valid!");
				metadataEntries.add(entry);
			}
		}
		dto.getMetadataEntries().addAll(metadataEntries);
		return dto;
	}
}
