package gov.nih.nci.doe.web.util;

import gov.nih.nci.hpc.domain.datamanagement.HpcPermission;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcUserPermissionDTO;

import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.util.Arrays;
import java.util.Properties;


/**
 * Utility class for methods supporting working with Collection items.
 *
 * @author liuwy
 */
public class DoeCollectionUtil {

//    @Value("${gov.nih.nci.hpc.server.collection}")
//    @Value("${gov.nih.nci.hpc.ssl.cert}")
//    @Value("${gov.nih.nci.hpc.ssl.cert.password}")
    private static final String
        APP_PROPERTIES_FILE = "application.properties",
        CLASSPATH_LOC_4_APP_PROPS_FILE = "/".concat(APP_PROPERTIES_FILE),
        PROP_KEY__$REST_API_ROOT_URL = "gov.nih.nci.hpc.server",
        PROP_KEY__$URI_COLLECTION = "gov.nih.nci.hpc.server.collection",
        PROP_KEY__$SSL_CERT = "gov.nih.nci.hpc.ssl.cert",
        PROP_KEY__$SSL_CERT_PASSWORD = "gov.nih.nci.hpc.ssl.cert.password";

 


    /**
     * Check if given Collection is empty.  Empty means Collection has 0
     * Sub-Collections and 0 Data Objects.
     *
     * @param someCollection  The Collection
     * @return  true if Collection is empty, false otherwise
     */
    public static boolean isCollectionEmpty(HpcCollectionDTO someCollection)
    {
        boolean retVal = false;
        if (null != someCollection &&
            null != someCollection.getCollection() &&
            null != someCollection.getCollection().getSubCollections() &&
            null != someCollection.getCollection().getDataObjects())
        {
            retVal =
              someCollection.getCollection().getSubCollections().isEmpty() &&
              someCollection.getCollection().getDataObjects().isEmpty();
        }
        return retVal;
    }


    /**
     * Retrieve from application properties the 3 property values needed to
     * support checking whether current (authenticated) User has Ownership
     * on a Collection.
     *
     * Provides the necessary property values as String array of length 3
     * where:
     * - at index 0, element is URL for page displaying detailed info about
     *    a Collection
     * - at index 1, element is location of keystore file
     * - at index 2, element is password for keystore file
     *
     * For each of the 3 necessary properties, if it could not be found, then
     * the returned String for its value is empty String.
     *
     * @return String array as described above
     */
    private static final
      String[] fetchAppPropertiesForCollectionOwnershipCheck()
    {
        String[] propVals = new String[3];
        Arrays.fill(propVals, "");
        try {
            final Properties appProperties = new Properties();
            appProperties.load(
              DoeCollectionUtil.class.getResourceAsStream(CLASSPATH_LOC_4_APP_PROPS_FILE)
            );
            final String serviceURL = fetchCollectionServiceURL(appProperties);
            final String sslCertPath =
              appProperties.getProperty(PROP_KEY__$SSL_CERT, "");
            final String sslCertPassword =
              appProperties.getProperty(PROP_KEY__$SSL_CERT_PASSWORD, "");
            propVals[0] = serviceURL;
            propVals[1] = sslCertPath;
            propVals[2] = sslCertPassword;
        } catch (IOException ioe) {
            throw new RuntimeException(
                    String.format("%s file was not found on classpath!",
                            APP_PROPERTIES_FILE)
            );
        }
        return propVals;
    }

    private static String fetchCollectionServiceURL(Properties appProps) {
        String retServiceURL = "";
        final String rawServiceURL =
          appProps.getProperty(PROP_KEY__$URI_COLLECTION);
        if (null != rawServiceURL) {
            final String prefix2Replace =
              "${".concat(PROP_KEY__$REST_API_ROOT_URL).concat("}");
            final String rootURL =
              appProps.getProperty(PROP_KEY__$REST_API_ROOT_URL);
            retServiceURL = rawServiceURL.replace(prefix2Replace, rootURL);
        }
        return retServiceURL;
    }

}
