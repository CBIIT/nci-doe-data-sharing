package gov.nih.nci.doe.web.service;

import java.util.Date;

import gov.nih.nci.doe.web.domain.AuditMetadataTransfer;

public interface AuditMetadataTransferService {

	public void saveAuditForMetadataTransfer(AuditMetadataTransfer audit);

	public AuditMetadataTransfer getAuditMetadaTransferForFileName(String fileName, Date startDate);
}