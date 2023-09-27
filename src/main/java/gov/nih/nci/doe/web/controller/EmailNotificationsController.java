package gov.nih.nci.doe.web.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.service.EmailNotificationsService;

@Controller
@EnableAutoConfiguration
@RequestMapping("/emailNotifications")
public class EmailNotificationsController extends AbstractDoeController {

	@Autowired
	EmailNotificationsService emailUpdatesService;

	@GetMapping
	public String openOutlook(Model model, HttpSession session, HttpServletRequest request) throws DoeWebException {

		log.info("open notification email to all the users subscribed");
		try {

			// This API can only be used by admins
			if (Boolean.TRUE.equals(getIsAdmin())) {

				String mailUrl = mailService.sendNotificationEmail(webServerName, getLoggedOnUserInfo());

				model.addAttribute("mailUrl", mailUrl);

			}

			return "home";

		} catch (Exception e) {
			throw new DoeWebException("Failed to send notification email to users: " + e.getMessage());

		}

	}

	@PostMapping(value = "/subscribe")
	@ResponseBody
	public String sendEmailUpdates(@RequestParam(value = "emailAddress") String emailAddress, HttpSession session,
			@RequestHeader HttpHeaders headers) throws Exception {

		log.info("send email updates");

		try {
			String message = null;
			// verify if the email address is a registered user to MoDaC
			if (StringUtils.isNotEmpty(emailAddress) && authService.doesUsernameExist(emailAddress)) {
				message = emailUpdatesService.saveEmailUpdateInfo(emailAddress);
			} else {
				message = "Register to MoDaC";
			}
			return message;

		} catch (Exception e) {
			throw new DoeWebException("Failed to send updates to the user: " + emailAddress + " " + e.getMessage());
		}
	}
}
