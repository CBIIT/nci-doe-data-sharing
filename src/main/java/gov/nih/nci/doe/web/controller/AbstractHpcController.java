package gov.nih.nci.doe.web.controller;

import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.context.request.WebRequest;

import gov.nih.nci.doe.web.model.DoeResponse;

public abstract class AbstractHpcController {
	@Value("${gov.nih.nci.hpc.ssl.cert}")
	protected String sslCertPath;
	@Value("${gov.nih.nci.hpc.ssl.cert.password}")
	protected String sslCertPassword;
	@Value("${hpc.SYSTEM_ADMIN}")
	protected String SYSTEM_ADMIN;
	@Value("${hpc.GROUP_ADMIN}")
	protected String GROUP_ADMIN;
	@Value("${hpc.USER}")
	protected String USER;

	protected Logger log = LoggerFactory.getLogger(this.getClass());

	@ExceptionHandler({ Exception.class, java.net.ConnectException.class })
	public @ResponseBody DoeResponse handleUncaughtException(Exception ex, WebRequest request,
			HttpServletResponse response) {
		log.info("Converting Uncaught exception to RestResponse : " + ex.getMessage());

		response.setHeader("Content-Type", "application/json");
		response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		return new DoeResponse("Error occurred", ex.toString());
	}

	@ExceptionHandler(IllegalArgumentException.class)
	public @ResponseBody DoeResponse handleIllegalArgumentException(IllegalArgumentException ex, WebRequest request,
			HttpServletResponse response) {
		log.info("Converting IllegalArgumentException to RestResponse : " + ex.getMessage());

		response.setHeader("Content-Type", "application/json");
		response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
		return new DoeResponse("Error occurred", ex.toString());
	}
}