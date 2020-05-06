package gov.nih.nci.doe.web.service.impl;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import gov.nih.nci.doe.web.domain.Auditing;
import gov.nih.nci.doe.web.model.AuditingModel;
import gov.nih.nci.doe.web.repository.AuditingRepository;
import gov.nih.nci.doe.web.service.AuditingService;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component
public class AuditingServiceImpl implements AuditingService {

	private static final Logger log = LoggerFactory.getLogger(AuditingServiceImpl.class);
	
	@Autowired
	AuditingRepository auditingRepository;

	@Override
	public void saveAuditInfo(AuditingModel model) {
		log.info("save audit info " + model.getName());		
		Auditing audit = new Auditing();
		BeanUtils.copyProperties(model, audit);
		auditingRepository.saveAndFlush(audit);		
	}


}
