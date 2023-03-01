package gov.nih.nci.doe.web.service;

import gov.nih.nci.doe.web.domain.AuditMetadataTransfer;

public interface AuditMetadataTransferService {

	public AuditMetadataTransfer getAuditMetadaTransferForCurrentDay();

	public void saveAuditForMetadataTransfer(AuditMetadataTransfer audit);
}