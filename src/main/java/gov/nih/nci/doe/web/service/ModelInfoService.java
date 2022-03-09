package gov.nih.nci.doe.web.service;

import java.util.List;

import gov.nih.nci.doe.web.domain.ModelInfo;
import gov.nih.nci.doe.web.model.KeyValueBean;

public interface ModelInfoService {

	public ModelInfo getModelInfo(String assetPath);

	public List<KeyValueBean> getAllModelInfoPaths();

}
