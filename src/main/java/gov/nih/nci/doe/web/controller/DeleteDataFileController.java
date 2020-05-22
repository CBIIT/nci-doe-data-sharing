package gov.nih.nci.doe.web.controller;

import javax.servlet.http.HttpSession;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import gov.nih.nci.doe.web.util.DoeClientUtil;


@Controller
@EnableAutoConfiguration
@RequestMapping("/delete")
public class DeleteDataFileController extends AbstractDoeController{
	
	@Value("${gov.nih.nci.hpc.server.dataObject}")
	private String serviceURL;
	

	@PostMapping(value = "/datafile")
	@ResponseBody
	public String deleteObject(@RequestParam(value = "deletepath") String deletepath,
			HttpSession session,@RequestHeader HttpHeaders headers) {
		
		   String authToken = (String) session.getAttribute("writeAccessUserToken");
          			if (deletepath == null) {
				return "Invaliad Data object path!";
			}
			boolean deleted = DoeClientUtil.deleteDatafile(authToken, serviceURL, deletepath, sslCertPath, sslCertPassword);
			if (deleted) {
				return "Data object deleted!";
			}
		return null;
	}

}
