package gov.nih.nci.doe.web.service.impl;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import gov.nih.nci.doe.web.domain.LookUp;
import gov.nih.nci.doe.web.repository.LookUpRepository;
import gov.nih.nci.doe.web.service.LookUpService;

@Component
public class LookUpServiceImpl implements LookUpService {

	private static final Logger log = LoggerFactory.getLogger(LookUpServiceImpl.class);

	@Autowired
	private LookUpRepository lookUpRepository;

	@Override
	public String getDisplayName(String levelName, String attrName) {
		log.info("get display name for " + levelName + " ," + attrName);
		return lookUpRepository.getDisplayName(levelName, attrName);
	}

	@Override
	public List<LookUp> getAllDisplayNames() {
		return lookUpRepository.findAllBySearchCriteriaDisplay();
	}

	@Override
	public LookUp getLookUpByDisplayName(String displayName) {
		return lookUpRepository.getLookUpByDisplayName(displayName);
	}

	@Override
	public LookUp getLookUpByAttrName(String attrName) {
		return lookUpRepository.getLookUpByAttrName(attrName);
	}

	@Override
	public LookUp getLookUpByLevelAndName(String levelName, String attrName) {
		return lookUpRepository.getLookUpByLevelAndName(levelName, attrName);
	}
}
