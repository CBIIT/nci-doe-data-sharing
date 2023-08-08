package gov.nih.nci.doe.web.service;

public interface EmailNotificationsService {

	public String saveEmailUpdateInfo(String emailAddress);

	public String unsubscribeEmailUpdates(String emailAddress);
}
