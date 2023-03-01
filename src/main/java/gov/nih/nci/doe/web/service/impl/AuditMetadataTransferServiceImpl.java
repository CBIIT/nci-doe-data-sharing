package gov.nih.nci.doe.web.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import gov.nih.nci.doe.web.domain.AuditMetadataTransfer;
import gov.nih.nci.doe.web.repository.AuditMetadataTransferRepository;
import gov.nih.nci.doe.web.service.AuditMetadataTransferService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component
public class AuditMetadataTransferServiceImpl implements AuditMetadataTransferService {

	private static final Logger log = LoggerFactory.getLogger(AuditMetadataTransferServiceImpl.class);

	@Autowired
	AuditMetadataTransferRepository auditMetadataTransferRepository;

	@Override
	public AuditMetadataTransfer getAuditMetadaTransferForCurrentDay() {
		log.info("get audit metadata for today");
		AuditMetadataTransfer auditMetadataCurr = auditMetadataTransferRepository.getAuditMetadaTransferForCurrentDay();
		if (auditMetadataCurr != null) {
			return auditMetadataCurr;
		}
		return null;
	}

	@Override
	public void saveAuditForMetadataTransfer(AuditMetadataTransfer audit) {
		log.info("Save audit metadata transfer information for: " + audit.getStartTime());

		auditMetadataTransferRepository.saveAndFlush(audit);
	}

}
