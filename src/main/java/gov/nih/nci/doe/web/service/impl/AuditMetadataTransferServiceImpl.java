package gov.nih.nci.doe.web.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import gov.nih.nci.doe.web.domain.AuditMetadataTransfer;
import gov.nih.nci.doe.web.repository.AuditMetadataTransferRepository;
import gov.nih.nci.doe.web.service.AuditMetadataTransferService;

import java.util.Date;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component
public class AuditMetadataTransferServiceImpl implements AuditMetadataTransferService {

	private static final Logger log = LoggerFactory.getLogger(AuditMetadataTransferServiceImpl.class);

	@Autowired
	AuditMetadataTransferRepository auditMetadataTransferRepository;

	@Override
	public void saveAuditForMetadataTransfer(AuditMetadataTransfer audit) {
		log.info("Save audit metadata transfer information for: " + audit.getStartTime());

		auditMetadataTransferRepository.saveAndFlush(audit);
	}

	@Override
	public AuditMetadataTransfer getAuditMetadaTransferForFileName(String fileName, Date startDate) {
		log.info("get audit metadata for the filename: " + fileName);
		AuditMetadataTransfer auditMetadata = auditMetadataTransferRepository
				.getAuditMetadaTransferForFileName(fileName, startDate);
		if (auditMetadata != null) {
			return auditMetadata;
		}
		return null;
	}

}
