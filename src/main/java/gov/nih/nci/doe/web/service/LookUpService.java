package gov.nih.nci.doe.web.service;

import java.util.List;

import gov.nih.nci.doe.web.domain.LookUp;

public interface LookUpService {

	
	public String getDisplayName(String levelName, String attrName);
	
	List<LookUp> getAllDisplayNames();
	
	LookUp getLookUpByDisplayName(String displayName);
	
	LookUp getLookUpByAttrName(String attrName);
}
