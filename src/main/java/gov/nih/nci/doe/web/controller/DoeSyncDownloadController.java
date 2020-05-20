/**
 * DoeSyncDownloadController.java
 *
 * Copyright SVG, Inc. Copyright Leidos Biomedical Research, Inc
 * 
 * Distributed under the OSI-approved BSD 3-Clause License. See
 * https://ncisvn.nci.nih.gov/svn/HPC_Data_Management/branches/hpc-prototype-dev/LICENSE.txt for
 * details.
 */
package gov.nih.nci.doe.web.controller;


import java.io.IOException;
import java.io.InputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;
import javax.ws.rs.core.Response;
import org.apache.commons.io.IOUtils;
import org.apache.cxf.jaxrs.client.WebClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.core.io.Resource;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.MappingJsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.introspect.AnnotationIntrospectorPair;
import com.fasterxml.jackson.databind.introspect.JacksonAnnotationIntrospector;
import com.fasterxml.jackson.databind.type.TypeFactory;
import com.fasterxml.jackson.module.jaxb.JaxbAnnotationIntrospector;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.DoeDownloadDatafile;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectDownloadResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDownloadRequestDTO;
import gov.nih.nci.hpc.dto.error.HpcExceptionDTO;
import org.springframework.web.util.UriComponentsBuilder;

/**
 * <p>
 * Controller to manage synchronous download of a data file
 * </p>
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/downloadsync")
public class DoeSyncDownloadController extends AbstractDoeController {
  @Value("${gov.nih.nci.hpc.server.dataObject}")
  private String dataObjectServiceURL;

  /**
   * POST action for sync download
   * 
   * @param downloadFile
   * @param model
   * @param bindingResult
   * @param session
   * @param request
   * @param response
   * @return
   */
  @PostMapping(produces = MediaType.APPLICATION_OCTET_STREAM_VALUE) 
  public @ResponseBody Resource download(
		   @Valid DoeDownloadDatafile downloadFile, HttpSession session, HttpServletRequest request,
      HttpServletResponse response) {
    try {
      String authToken = (String) session.getAttribute("hpcUserToken");
      if (authToken == null) {
        return null;
      }
      final String serviceURL = UriComponentsBuilder.fromHttpUrl(
        this.dataObjectServiceURL).path("/{dme-archive-path}/download")
        .buildAndExpand(downloadFile.getDestinationPath()).encode().toUri()
        .toURL().toExternalForm();

      final HpcDownloadRequestDTO dto = new HpcDownloadRequestDTO();
      dto.setGenerateDownloadRequestURL(true);

      WebClient client = DoeClientUtil.getWebClient(serviceURL, sslCertPath, sslCertPassword);
      client.header("Authorization", "Bearer " + authToken);

      Response restResponse = client.invoke("POST", dto);
            
      if (restResponse.getStatus() == 200) {
            HpcDataObjectDownloadResponseDTO downloadDTO =
              (HpcDataObjectDownloadResponseDTO) DoeClientUtil.getObject(
              restResponse, HpcDataObjectDownloadResponseDTO.class);
            downloadToUrl(downloadDTO.getDownloadRequestURL(), 1000000,downloadFile.getDownloadFileName(), response);
      } else if (restResponse.getStatus() == 400) {
        dto.setGenerateDownloadRequestURL(false);
        restResponse = client.invoke("POST", dto);
        if (restResponse.getStatus() == 200) {
          handleStreamingDownloadData(downloadFile, response, restResponse);
        } else {
          return handleDownloadProblem(restResponse);
        }
      } else {
        return handleDownloadProblem(restResponse);
      }
    } catch (Exception e) {
      return new ByteArrayResource(("Failed to download: " +
        e.getMessage()).getBytes());
    }
    return null;
  }


  public void downloadToUrl(String urlStr, int bufferSize, String fileName,
      HttpServletResponse response) {
    try {
      WebClient client = DoeClientUtil.getWebClient(urlStr, null, null);
      Response restResponse = client.invoke("GET", null);
      response.setContentType("application/octet-stream");
      response.setHeader("Content-Disposition", "attachment; filename=" + fileName);
      IOUtils.copy((InputStream) restResponse.getEntity(), response.getOutputStream());
    } catch (IOException e) {
      throw new DoeWebException(e);
    }
  }


  private Resource handleDownloadProblem(Response restResponse)
    throws IOException {
    ObjectMapper mapper = new ObjectMapper();
    AnnotationIntrospectorPair intr = new AnnotationIntrospectorPair(
        new JaxbAnnotationIntrospector(TypeFactory.defaultInstance()),
        new JacksonAnnotationIntrospector());
    mapper.setAnnotationIntrospector(intr);
    mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

    MappingJsonFactory factory = new MappingJsonFactory(mapper);
    JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());

    try {
      HpcExceptionDTO exception = parser.readValueAs(HpcExceptionDTO.class);
      return new ByteArrayResource(("Failed to download: " +
        exception.getMessage()).getBytes());
    } catch (Exception e) {
   
      return new ByteArrayResource(("Failed to download: " +
        e.getMessage()).getBytes());
    }
  }


  private @ResponseBody void handleStreamingDownloadData(
		   @Valid DoeDownloadDatafile
      downloadFile,
    HttpServletResponse response,
    Response restResponse) throws IOException
  {
    response.setContentType("application/octet-stream");
    response.setHeader("Content-Disposition", "attachment; filename=" +
      downloadFile.getDownloadFileName());
    IOUtils.copy((InputStream) restResponse.getEntity(),
      response.getOutputStream());
  }

}
