package gov.nih.nci.doe.web.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpSession;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import gov.nih.nci.doe.web.domain.LookUp;
import gov.nih.nci.doe.web.model.LookUpKey;
import gov.nih.nci.doe.web.repository.LookUpRepository;
import gov.nih.nci.doe.web.service.LookUpService;

@Component
public class LookUpServiceImpl implements LookUpService {

	@Autowired
	private LookUpRepository lookUpRepository;

	@Override
	public List<LookUp> getAllDisplayNames() {
		return lookUpRepository.findAllBySearchCriteriaDisplay();
	}

	public List<LookUp> getAllLookUpValues() {
		List<LookUp> lookUpValues = lookUpRepository.getAllValues();
		return lookUpValues;
	}

	@SuppressWarnings("unchecked")
	public List<LookUp> getLookUpNames(HttpSession session) {
		List<LookUp> lookUpValues = new ArrayList<>();
		lookUpValues = (List<LookUp>) session.getAttribute("lookUpValues");
		if (lookUpValues == null) {

			lookUpValues = getAllLookUpValues();

			session.setAttribute("lookUpValues", lookUpValues);
		}

		return lookUpValues;
	}

	@SuppressWarnings("unchecked")
	public Map<String, LookUp> getValuesByDisplayName(HttpSession session) {

		Map<String, LookUp> lookUpMap = (Map<String, LookUp>) session.getAttribute("lookUpMapByDisplayNames");
		if (lookUpMap == null) {
			lookUpMap = new HashMap<String, LookUp>();
			List<LookUp> lookUpList = getLookUpNames(session);

			for (LookUp lookUp : lookUpList) {

				lookUpMap.put(lookUp.getDisplayName(), lookUp);
			}

			session.setAttribute("lookUpMapByDisplayNames", lookUpMap);
		}
		return lookUpMap;

	}

	@Override
	public LookUp getLookUpByDisplayName(HttpSession session, String displayName) {
		Map<String, LookUp> lookUpMap = getValuesByDisplayName(session);

		LookUp lookUp = lookUpMap.get(displayName);
		return lookUp;
	}

	@SuppressWarnings("unchecked")
	public Map<LookUpKey, LookUp> getValuesByAttrNameAndVal(HttpSession session) {

		Map<LookUpKey, LookUp> lookUpMap = (Map<LookUpKey, LookUp>) session.getAttribute("lookUpMap");
		if (lookUpMap == null) {
			lookUpMap = new HashMap<LookUpKey, LookUp>();
			List<LookUp> lookUpList = getLookUpNames(session);

			for (LookUp lookUp : lookUpList) {
				LookUpKey key = new LookUpKey(lookUp.getLevelName(), lookUp.getAttrName());
				lookUpMap.put(key, lookUp);
			}

			session.setAttribute("lookUpMap", lookUpMap);
		}
		return lookUpMap;

	}

	@Override
	public LookUp getLookUpByLevelAndName(HttpSession session, String levelName, String attrName) {
		Map<LookUpKey, LookUp> lookUpMap = getValuesByAttrNameAndVal(session);
		LookUpKey key = new LookUpKey(levelName, attrName);
		LookUp lookUp = lookUpMap.get(key);
		return lookUp;

	}
}
