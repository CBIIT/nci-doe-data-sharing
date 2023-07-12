package gov.nih.nci.doe.web.controller;

import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.SiteFeedback;
import gov.nih.nci.doe.web.util.DoeClientUtil;

/**
 *
 * MoDaC Site Feedback Controller
 *
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/siteFeedback")
public class SiteFeedbackController extends AbstractDoeController {

	@PostMapping
	public ResponseEntity<?> siteFeedback(@RequestHeader HttpHeaders headers, @RequestBody SiteFeedback siteFeedback)
			throws Exception {
		log.info("site feedback");
		try {
			if (StringUtils.isEmpty(siteFeedback.getResponse())) {
				return new ResponseEntity<>("Captcha is Empty", HttpStatus.OK);
			} else {
				Boolean success = DoeClientUtil.getResponseFromGoogleCaptcha(secretKey, siteFeedback.getResponse());
				if (Boolean.TRUE.equals(success)) {
					mailService.sendSiteFeedbackEmail(siteFeedback);

					log.info("successfully reset the password...");
					return new ResponseEntity<>("SUCCESS", HttpStatus.OK);
				} else {
					return new ResponseEntity<>("Failed to validate captcha.", HttpStatus.OK);
				}
			}

		} catch (Exception e) {
			throw new DoeWebException("Failed to send email to modac support " + e.getMessage());
		}
	}
}
