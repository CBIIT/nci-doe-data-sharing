package gov.nih.nci.doe.web.constants;

import java.util.Arrays;
import java.util.List;

public class BasicAuthRequestUrlList {

	private BasicAuthRequestUrlList() {

	}

	public static final List<String> requestUrlList = 
			Arrays.asList("/api/dataObject/query", "/api/collection/query",
			"/api/v2/dataObject/","/api/collection/","/api/authenticate");
}
