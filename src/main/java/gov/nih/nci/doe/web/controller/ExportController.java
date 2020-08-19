package gov.nih.nci.doe.web.controller;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import gov.nih.nci.doe.web.util.ExcelExportProc;

@CrossOrigin
@Controller
@EnableAutoConfiguration
@RequestMapping("/export")
public class ExportController extends AbstractDoeController{

	private static final String[] headers = new String[]{
	        "Test",
	        "Test"
	    };
    
	@GetMapping
    public String exportCancerActivities(HttpServletResponse
            response, HttpServletRequest request,@RequestParam(value = "selectedPaths") String selectedPaths)
    		throws Exception {
		
		List<List<String>> rows = new ArrayList<>();
		List<String> result = new ArrayList<String>();
		result.add("test");
		result.add("test");
		rows.add(result);
		ExcelExportProc proc = new ExcelExportProc();
        proc.setReportName("Cancer Activity Results");
        proc.setHeaders(Arrays.asList(headers));
        proc.setData(rows);
        proc.setFileName("cancer_activities_");
        proc.setExtension(".xls");
        proc.setMimeType("application/vnd.ms-excel");
        proc.setFieldSeparator("\t");

        proc.doExport(request, response);

        return null;
		
	}
}
