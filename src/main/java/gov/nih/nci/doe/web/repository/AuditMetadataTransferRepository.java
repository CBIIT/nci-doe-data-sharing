package gov.nih.nci.doe.web.repository;

import java.util.Date;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import gov.nih.nci.doe.web.domain.AuditMetadataTransfer;

public interface AuditMetadataTransferRepository extends JpaRepository<AuditMetadataTransfer, String> {

	@Query("select a from AuditMetadataTransfer a where TRUNC(a.startTime) = TRUNC(sysdate())")
	AuditMetadataTransfer getAuditMetadaTransferForCurrentDay();

	@Query("select a from AuditMetadataTransfer a where TRUNC(a.startTime) = TRUNC(sysdate() -1)")
	AuditMetadataTransfer getAuditMetadaTransferForPreviousDay();

	@Query("select a from AuditMetadataTransfer a where a.fileName = ?1 and TRUNC(a.startTime) = TRUNC(?2)")
	AuditMetadataTransfer getAuditMetadaTransferForFileName(String fileName, Date startDate);

}
