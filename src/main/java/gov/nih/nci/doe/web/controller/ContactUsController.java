package gov.nih.nci.doe.web.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.ContactUs;
import gov.nih.nci.doe.web.util.DoeClientUtil;

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

	@GetMapping
	public String getContactUs(@RequestParam(value = "typeOfInquiry", required = false) String typeOfInquiry,
			Model model, HttpSession session, HttpServletRequest request) {

		if (StringUtils.isNotEmpty(typeOfInquiry)) {
			model.addAttribute("typeOfInquiry", typeOfInquiry);
		}
		model.addAttribute("siteKey", siteKey);
		return "contactus/contactUsTab";
	}

	@PostMapping
	public ResponseEntity<?> contactUs(@RequestHeader HttpHeaders headers, @RequestBody ContactUs contactUs)
			throws Exception {
		log.info("contact us");
		try {
			if (StringUtils.isEmpty(contactUs.getResponse())) {
				return new ResponseEntity<>("Captcha is Empty", HttpStatus.OK);
			} else {
				Boolean success = DoeClientUtil.getResponseFromGoogleCaptcha(secretKey, contactUs.getResponse());
				if (Boolean.TRUE.equals(success)) {
					if (contactUs.getInquiry() != null && contactUs.getInquiry().contains("unsubscribe from MoDaC")) {
						String emailAddress = contactUs.getEmailAddress();
						if (StringUtils.isNotEmpty(emailAddress)
								&& Boolean.FALSE.equals(authService.doesUsernameExist(emailAddress))) {
							return new ResponseEntity<>(
									"User should be registered to unsubscribe from MoDaC notifications.",
									HttpStatus.OK);
						}

					}
					mailService.sendContactUsEmail(contactUs);

					log.info("successfully sending contact us email...");
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
