package gov.nih.nci.doe.web.service;

import java.util.List;

import javax.servlet.http.HttpSession;

import gov.nih.nci.doe.web.domain.LookUp;

public interface LookUpService {

	List<LookUp> getAllDisplayNames();

	LookUp getLookUpByDisplayName(HttpSession session, String displayName);

	LookUp getLookUpByLevelAndName(HttpSession session, String levelName, String attrName);
}
