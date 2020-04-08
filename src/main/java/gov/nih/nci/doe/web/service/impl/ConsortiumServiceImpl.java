package gov.nih.nci.doe.web.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import gov.nih.nci.doe.web.repository.ConsortiumRepository;
import gov.nih.nci.doe.web.service.ConsortiumService;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component
public class ConsortiumServiceImpl implements ConsortiumService {

	private static final Logger log = LoggerFactory.getLogger(ConsortiumServiceImpl.class);

	@Autowired
	private ConsortiumRepository consortiumRepository;
	
	@Override
	 public String getConsortitumGroupByUserId(String userId) {
		log.info("get consortium group by user is :" + userId);
		return consortiumRepository.getConsortiumGroupByUserId(userId);
	}


}
