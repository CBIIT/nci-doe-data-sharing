package gov.nih.nci.doe.web.service.impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import gov.nih.nci.doe.web.domain.ModelInfo;
import gov.nih.nci.doe.web.repository.ModelInfoRpository;
import gov.nih.nci.doe.web.service.ModelInfoService;

@Component
public class ModelInfoServiceImpl implements ModelInfoService {

	private static final Logger log = LoggerFactory.getLogger(AuditingServiceImpl.class);

	@Autowired
	ModelInfoRpository modelInfoRepository;

	@Override
	public Boolean isModelExistsForInferencing(String modelPath) {
		log.info("verify if model is avaiable for inferencing: " + modelPath);
		ModelInfo info = modelInfoRepository.getModelInfoByModelPath(modelPath);

		if (info != null) {
			return true;
		}

		return false;
	}
}
