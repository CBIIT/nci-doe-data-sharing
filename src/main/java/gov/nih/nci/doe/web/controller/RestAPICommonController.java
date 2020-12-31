package gov.nih.nci.doe.web.controller;

import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.util.UriComponentsBuilder;
import org.springframework.http.MediaType;


import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectDownloadResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDownloadRequestDTO;


import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.ws.rs.core.Response;

import org.apache.cxf.jaxrs.client.WebClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;

@CrossOrigin
@RestController
@RequestMapping(value = "/v2", produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
public class RestAPICommonController extends AbstractDoeController{
	
	 @Value("${gov.nih.nci.hpc.server.dataObject}")
	 private String dataObjectServiceURL;
	 
	 @PostMapping(value="/dataObject/download")
	 public ResponseEntity<?> synchronousDownload(@RequestHeader HttpHeaders headers, HttpSession session,
			 HttpServletResponse response,
	 @RequestParam(value ="path", required=false) String path) {

		 log.info("download sync:");
	     log.info("Headers: {}", headers);
	     log.info("pathName: " +path);

	      try {
	          String authToken = (String) session.getAttribute("writeAccessUserToken");
	          log.info("authToken: " + authToken);
	          if (authToken == null) {
	            return null;
	          }
	          String doeLogin = (String) session.getAttribute("doeLogin");
	          log.info("doeLogin: " + doeLogin);
	          if (doeLogin == null) {
	            return null;
	          }
     			      	
	          final String serviceURL = UriComponentsBuilder.fromHttpUrl(
	            this.dataObjectServiceURL).path("/{dme-archive-path}/download")
	            .buildAndExpand(path).encode().toUri()
	            .toURL().toExternalForm();

	          final HpcDownloadRequestDTO dto = new HpcDownloadRequestDTO();
	          dto.setGenerateDownloadRequestURL(true);

	          WebClient client = DoeClientUtil.getWebClient(serviceURL, sslCertPath, sslCertPassword);
	          client.header("Authorization", "Bearer " + authToken);

	          Response restResponse = client.invoke("POST", dto);
	          log.info("rest response:" + restResponse.getStatus());
	          if (restResponse.getStatus() == 200) {
	                HpcDataObjectDownloadResponseDTO downloadDTO =
	                  (HpcDataObjectDownloadResponseDTO) DoeClientUtil.getObject(
	                  restResponse, HpcDataObjectDownloadResponseDTO.class);
	                downloadToUrl(downloadDTO.getDownloadRequestURL(), 1000000,"test", response);	          
	          }   
	      } catch (Exception e) {
	          log.error("error in download" + e);
	      }
	        return null;
	 }
}
