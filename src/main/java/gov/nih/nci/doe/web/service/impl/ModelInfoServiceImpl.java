package gov.nih.nci.doe.web.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import gov.nih.nci.doe.web.domain.ModelInfo;
import gov.nih.nci.doe.web.model.KeyValueBean;
import gov.nih.nci.doe.web.repository.ModelInfoRpository;
import gov.nih.nci.doe.web.service.ModelInfoService;

@Component
public class ModelInfoServiceImpl implements ModelInfoService {

	private static final Logger log = LoggerFactory.getLogger(AuditingServiceImpl.class);

	@Autowired
	ModelInfoRpository modelInfoRepository;

	@Override
	public ModelInfo getModelInfo(String assetPath) {
		log.info("get model Info for : " + assetPath);
		ModelInfo info = modelInfoRepository.getModelInfoByModelPath(assetPath);

		return info;
	}

	@Override
	public List<KeyValueBean> getAllModelInfoPaths() {
		log.info("get all model paths");
		List<ModelInfo> list = modelInfoRepository.getAllModelInfoPaths();
		List<KeyValueBean> returnList = new ArrayList<KeyValueBean>();
		if (CollectionUtils.isNotEmpty(list)) {
			list.stream().forEach(e -> returnList.add(new KeyValueBean(e.getAssetPath(), e.getAssetIdentifier())));
		}
		return returnList;
	}

}
