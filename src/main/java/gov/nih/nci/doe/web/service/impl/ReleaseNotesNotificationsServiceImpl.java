package gov.nih.nci.doe.web.service.impl;

import java.util.Date;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import gov.nih.nci.doe.web.domain.EmailUpdates;
import gov.nih.nci.doe.web.repository.ReleaseNotesNotificationRepository;
import gov.nih.nci.doe.web.service.ReleaseNotesNotificationsService;

@Component
public class ReleaseNotesNotificationsServiceImpl implements ReleaseNotesNotificationsService {

	private static final Logger log = LoggerFactory.getLogger(ReleaseNotesNotificationsServiceImpl.class);

	@Autowired
	ReleaseNotesNotificationRepository emailNotificationRepository;

	@Override
	public String saveEmailUpdateInfo(String emailAddress) {

		log.info("save email updates for user: " + emailAddress);
		EmailUpdates emailInfo = emailNotificationRepository.getEmailNotificationRecord(emailAddress);
		if (emailInfo != null) {
			// user is already subscribed to MoDaC notifications
			log.info(emailAddress + " is already subscribed to notifications.");
			return "You are already subscribed to MoDaC notifications";
		} else {
			log.info("new user " + emailAddress + " is subscribing to notifications");
			EmailUpdates newRecord = new EmailUpdates();
			newRecord.setEmailAddress(emailAddress);
			newRecord.setCreatedDate(new Date());
			emailNotificationRepository.saveAndFlush(newRecord);
		}

		return "You are successfully subscribed to MoDaC notifications";
	}

}
