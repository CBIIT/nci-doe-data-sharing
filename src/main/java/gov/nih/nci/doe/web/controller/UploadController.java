package gov.nih.nci.doe.web.controller;

import javax.servlet.http.HttpSession;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import gov.nih.nci.doe.web.model.UploadCollectionModel;
import gov.nih.nci.doe.web.util.MiscUtil;



@CrossOrigin
@Controller
@EnableAutoConfiguration
@RequestMapping("/upload")
public class UploadController extends AbstractDoeController {

	@Value("${gov.nih.nci.hpc.web.server}")
	private String webServerName;
	
	@Value("${doe.basePath}")
	private String basePath;
	

      @RequestMapping(method = RequestMethod.GET)
	  public ResponseEntity<?> home(HttpSession session,@RequestHeader HttpHeaders headers, UploadCollectionModel uploadCollectionModel) throws Exception {
		  
		session.setAttribute("basePathSelected", basePath);
		session.removeAttribute("GlobusEndpoint");
		session.removeAttribute("GlobusEndpointPath");
		session.removeAttribute("GlobusEndpointFiles");
		session.removeAttribute("GlobusEndpointFolders");
		session.setAttribute("datafilePath",uploadCollectionModel.getDataSetPath());
		session.setAttribute("institutePath",uploadCollectionModel.getInstitutionPath());
		session.setAttribute("studyPath",uploadCollectionModel.getStudyPath());
		
		final String percentEncodedReturnURL = MiscUtil.performUrlEncoding(
				this.webServerName + "/addbulk");
		
		
		return new ResponseEntity<>("https://app.globus.org/file-manager?method=GET&action=" + percentEncodedReturnURL, HttpStatus.OK);

	  }
}
