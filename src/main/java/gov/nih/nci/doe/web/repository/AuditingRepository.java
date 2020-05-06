package gov.nih.nci.doe.web.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import gov.nih.nci.doe.web.domain.Auditing;

public interface AuditingRepository extends JpaRepository<Auditing, String> {


}
