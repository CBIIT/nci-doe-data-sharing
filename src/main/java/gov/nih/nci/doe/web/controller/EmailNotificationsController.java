package gov.nih.nci.doe.web.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.multipart.MultipartFile;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.service.EmailNotificationsService;

@Controller
@EnableAutoConfiguration
@RequestMapping("/emailNotifications")
public class EmailNotificationsController extends AbstractDoeController {

	@Autowired
	EmailNotificationsService emailUpdatesService;

	@GetMapping
	public String getNotificationPage(HttpSession session, HttpServletRequest request) {

		log.info("notification page");
		return "notification";

	}

	@PostMapping
	@ResponseBody
	public String sendNotificationUpdateToUsers(HttpSession session, @RequestHeader HttpHeaders headers,
			@RequestParam(value = "releaseDoc", required = false) MultipartFile releaseDoc,
			@RequestParam(value = "message") String message, HttpServletRequest request) throws Exception {

		log.info("send notification email to all the users subscribed");

		try {
			if (Boolean.TRUE.equals(getIsAdmin())) {
				mailService.sendNotificationEmail(releaseDoc != null ? releaseDoc : null, message, webServerName,
						getLoggedOnUserInfo());
				return "Notification sent to all the users";
			}
			return "Invalid Permissions";

		} catch (Exception e) {
			throw new DoeWebException("Failed to send notification email to users: " + e.getMessage());
		}
	}

	@PostMapping(value = "/subscribe")
	public ResponseEntity<?> sendEmailUpdates(HttpSession session, @RequestHeader HttpHeaders headers,
			HttpServletRequest request, @RequestParam(value = "emailAddress") String emailAddress) throws Exception {

		log.info("send email updates");

		try {
			String message = null;
			// verify if the email address is a registered user to MoDaC
			if (StringUtils.isNotEmpty(emailAddress) && authService.doesUsernameExist(emailAddress)) {
				message = emailUpdatesService.saveEmailUpdateInfo(emailAddress);
			} else {
				message = "Please register to MoDaC to receive notifications.";
			}
			return new ResponseEntity<>(message, HttpStatus.OK);

		} catch (Exception e) {
			throw new DoeWebException("Failed to send updates to the user: " + emailAddress + " " + e.getMessage());
		}
	}

	@PostMapping(value = "/unsubscribe")
	public ResponseEntity<?> unsubscribeFromEmailUpdates(HttpSession session, @RequestHeader HttpHeaders headers,
			HttpServletRequest request, @RequestParam(value = "emailAddress") String emailAddress) throws Exception {

		log.info("unsubscribe from email updates");

		try {
			String message = null;
			// verify if the email address is a registered user to MoDaC
			if (StringUtils.isNotEmpty(emailAddress) && authService.doesUsernameExist(emailAddress)) {
				message = emailUpdatesService.unsubscribeEmailUpdates(emailAddress);
			} else {
				message = "User not found in MoDaC.";
			}
			return new ResponseEntity<>(message, HttpStatus.OK);

		} catch (Exception e) {
			throw new DoeWebException(
					"Failed to unsubscribe from updates to the user: " + emailAddress + " " + e.getMessage());
		}
	}

}
