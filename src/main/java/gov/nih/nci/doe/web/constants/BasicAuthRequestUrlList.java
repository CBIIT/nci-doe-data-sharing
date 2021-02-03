package gov.nih.nci.doe.web.constants;

import java.util.Arrays;
import java.util.List;

public class BasicAuthRequestUrlList {

	private BasicAuthRequestUrlList() {

	}

	public static final List<String> requestUrlList = 
			Arrays.asList("/dataObject/query", "/collection/query",
			"/v2/dataObject/","/collection/","/authenticate");
}
