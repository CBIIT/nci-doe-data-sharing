package gov.nih.nci.doe.web.controller;


import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.doe.web.util.ExcelExportProc;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectListDTO;

@CrossOrigin
@Controller
@EnableAutoConfiguration
@RequestMapping("/export")
public class ExportController extends AbstractDoeController{

	@Value("${gov.nih.nci.hpc.server.dataObject}")
	private String serviceURL;
	
	@Value("${gov.nih.nci.hpc.server.collection}")
	private String collectionURL;
	

	@GetMapping
    public String exportMetadata(HttpSession session, HttpServletResponse
            response, HttpServletRequest request,@RequestParam(value = "selectedPaths") String selectedPaths,
            @RequestParam(value="isParent") String isParent)
    		throws Exception {
		
		String authToken = (String) session.getAttribute("writeAccessUserToken");
		List<String> headers = new ArrayList<String>();
		
		if(StringUtils.isNotEmpty(selectedPaths)) {
			 String[] paths = selectedPaths.split(",");
			 if(paths != null && paths.length >= 1) {
				 List<List<String>> rows = new ArrayList<>();
				 
				 for(String path :paths) {
						List<String> result = new ArrayList<String>();
					  HpcDataObjectListDTO datafiles = DoeClientUtil.getDatafiles(authToken, serviceURL, 
							  path, true, true,sslCertPath, sslCertPassword);

						if (datafiles != null && datafiles.getDataObjects() != null &&
								!datafiles.getDataObjects().isEmpty()) {
							HpcDataObjectDTO dataFile = datafiles.getDataObjects().get(0);
							for (HpcMetadataEntry entry : dataFile.getMetadataEntries().getSelfMetadataEntries()) {
								if(headers.contains("Dataobject" + "_" + entry.getAttribute())) {
									result.add(entry.getValue());
								} else {
									headers.add("Dataobject" + "_" + entry.getAttribute());
									result.add(entry.getValue());
								}
								
								
						}
							if (!StringUtils.isEmpty(isParent) && isParent.equalsIgnoreCase("true")) {
								for (HpcMetadataEntry entry : dataFile.getMetadataEntries().getParentMetadataEntries()) {
									if(headers.contains(entry.getLevelLabel() + "_" + entry.getAttribute())) {
										result.add(entry.getValue());
									} else {
										headers.add(entry.getLevelLabel() + "_" + entry.getAttribute());
										result.add(entry.getValue());
									}
							    }
							}
						
				      }
						rows.add(result);
						
				 }
				
					
					ExcelExportProc proc = new ExcelExportProc();
			        proc.setReportName("Download Metadata Results");
			        proc.setHeaders(headers);
			        proc.setData(rows);
			        proc.setFileName("download_metadata_");
			        proc.setExtension(".xls");
			        proc.setMimeType("application/vnd.ms-excel");
			        proc.setFieldSeparator("\t");

			        proc.doExport(request, response); 
			 }
		}
        return null;		
	}
}
