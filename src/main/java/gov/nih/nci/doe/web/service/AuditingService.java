package gov.nih.nci.doe.web.service;

import java.util.List;


import gov.nih.nci.doe.web.domain.Auditing;
import gov.nih.nci.doe.web.model.AuditingModel;

public interface AuditingService {

    
    public void saveAuditInfo(AuditingModel audit);
    
    List<Auditing> getAllTaskIds();
   
}