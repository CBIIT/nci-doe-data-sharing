package gov.nih.nci.doe.web.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import gov.nih.nci.doe.web.domain.EmailUpdates;

public interface ReleaseNotesNotificationRepository extends JpaRepository<EmailUpdates, String> {

	@Query("select a from EmailUpdates a where a.emailAddress =?1")
	EmailUpdates getEmailNotificationRecord(String emailAddress);

	@Query("select a.emailAddress from EmailUpdates a")
	List<String> getAllEmailAddress();

}
