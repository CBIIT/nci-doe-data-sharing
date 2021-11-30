package gov.nih.nci.doe.web.controller;

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
import gov.nih.nci.doe.web.model.ContactUs;

/**
 *
 * MoDaC Contact Us Controller
 *
 *
 */

@Controller
@EnableAutoConfiguration
@RequestMapping("/contactUs")
public class ContactUsController extends AbstractDoeController {

	@PostMapping
	public ResponseEntity<?> contactUs(@RequestHeader HttpHeaders headers, @RequestBody ContactUs contactUs)
			throws Exception {
		log.info("contact us");
		try {

			mailService.sendContactUsEmail(contactUs.getName(), contactUs.getEmailAddress(), contactUs.getMessage());

			log.info("successfully reset the password...");
			return new ResponseEntity<>("SUCCESS", HttpStatus.OK);

		} catch (Exception e) {
			throw new DoeWebException("Failed to send email to modac support " + e.getMessage());
		}
	}
}
