package gov.nih.nci.doe.web.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import gov.nih.nci.doe.web.domain.Auditing;

public interface AuditingRepository extends JpaRepository<Auditing, String> {

	@Query("select a from Auditing a where a.taskId IS NOT NULL AND a.completionTime IS NULL")
	List<Auditing> getAllTaskIdsInprogress();

}
