package gov.nih.nci.doe.web.controller;

import java.io.IOException;
import java.io.InputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;
import javax.ws.rs.core.Response;

import org.apache.commons.io.IOUtils;

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
import gov.nih.nci.doe.web.model.DoeDownloadDatafile;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcDownloadRequestDTO;
import gov.nih.nci.hpc.dto.error.HpcExceptionDTO;

/**
 *
 * Controller to manage synchronous download of a data file
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/downloadsync")
public class DoeSyncDownloadController extends AbstractDoeController {

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
	public @ResponseBody Resource download(@Valid DoeDownloadDatafile downloadFile, HttpSession session,
			HttpServletRequest request, HttpServletResponse response) {
		try {
			String authToken = (String) session.getAttribute("hpcUserToken");
			if (authToken == null) {
				return null;
			}

			HpcDownloadRequestDTO downloadRequest = new HpcDownloadRequestDTO();
			Response restResponse = DoeClientUtil.syncAndasynchronousDownload(authToken, dataObjectAsyncServiceURL,
					downloadFile.getDestinationPath(), downloadRequest);
			log.info("rest response:" + restResponse.getStatus());

			if (restResponse.getStatus() == 200) {

				response.setContentType("application/octet-stream");
				response.setHeader("Content-Disposition", "attachment; filename=" + downloadFile.getDownloadFileName());
				IOUtils.copy((InputStream) restResponse.getEntity(), response.getOutputStream(), bufferSize);

			} else {
				return handleDownloadProblem(restResponse);
			}
		} catch (Exception e) {
			return new ByteArrayResource(("Failed to download: " + e.getMessage()).getBytes());
		}
		return null;
	}

	private Resource handleDownloadProblem(Response restResponse) throws IOException {
		ObjectMapper mapper = new ObjectMapper();
		AnnotationIntrospectorPair intr = new AnnotationIntrospectorPair(
				new JaxbAnnotationIntrospector(TypeFactory.defaultInstance()), new JacksonAnnotationIntrospector());
		mapper.setAnnotationIntrospector(intr);
		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

		MappingJsonFactory factory = new MappingJsonFactory(mapper);
		JsonParser parser = factory.createParser((InputStream) restResponse.getEntity());

		try {
			HpcExceptionDTO exception = parser.readValueAs(HpcExceptionDTO.class);
			return new ByteArrayResource(("Failed to download: " + exception.getMessage()).getBytes());
		} catch (Exception e) {

			return new ByteArrayResource(("Failed to download: " + e.getMessage()).getBytes());
		}
	}

}
