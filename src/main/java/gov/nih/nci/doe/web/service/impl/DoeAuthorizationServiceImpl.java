/**
 * DoeAuthorizationServiceImpl.java
 *
 * Copyright SVG, Inc.
 * Copyright Leidos Biomedical Research, Inc
 * 
 * Distributed under the OSI-approved BSD 3-Clause License.
 * See https://github.com/CBIIT/HPC_DME_APIs/blob/master/LICENSE.txt for details.
 */
package gov.nih.nci.doe.web.service.impl;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import com.google.api.client.googleapis.auth.oauth2.GoogleAuthorizationCodeFlow;
import com.google.api.client.googleapis.auth.oauth2.GoogleAuthorizationCodeRequestUrl;
import com.google.api.client.googleapis.auth.oauth2.GoogleTokenResponse;
import com.google.api.client.http.HttpTransport;
import com.google.api.client.http.javanet.NetHttpTransport;
import com.google.api.client.json.GenericJson;
import com.google.api.client.json.JsonFactory;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.google.api.services.drive.DriveScopes;
import com.google.api.services.storage.StorageScopes;
import com.google.gson.Gson;

import gov.nih.nci.doe.web.service.DoeAuthorizationService;

@Service
public class DoeAuthorizationServiceImpl implements DoeAuthorizationService {

	private static final JsonFactory JSON_FACTORY = JacksonFactory.getDefaultInstance();
	private static final List<String> SCOPES = Collections.singletonList(DriveScopes.DRIVE);
	private static final List<String> SCOPES_CLOUD = Collections.singletonList(StorageScopes.DEVSTORAGE_READ_WRITE);
	private static HttpTransport HTTP_TRANSPORT = new NetHttpTransport();

	@Value("${gov.nih.nci.hpc.drive.clientid}")
	private String clientId;
	@Value("${gov.nih.nci.hpc.drive.clientsecret}")
	private String clientSecret;

	private Logger logger = LoggerFactory.getLogger(DoeAuthorizationServiceImpl.class);

	private GoogleAuthorizationCodeFlow flowGoogleDrive;
	private GoogleAuthorizationCodeFlow flowGoogleCloudWithForcedGoogleLogin;
	private GoogleAuthorizationCodeFlow flow;
	private String refreshToken;
	private Gson gson = new Gson();

	@PostConstruct
	public void init() throws Exception {

		// Build flow and trigger user authorization request for Google Drive
		flowGoogleDrive = new GoogleAuthorizationCodeFlow.Builder(HTTP_TRANSPORT, JSON_FACTORY, clientId, clientSecret,
				SCOPES).setAccessType("offline").build();

		// Build flow and trigger user authorization request for Google Cloud with a
		// forced login
		flowGoogleCloudWithForcedGoogleLogin = new GoogleAuthorizationCodeFlow.Builder(HTTP_TRANSPORT, JSON_FACTORY,
				clientId, clientSecret, SCOPES_CLOUD).setAccessType("offline").setApprovalPrompt("force").build();
	}

	@Override
	public String authorize(String redirectUri, ResourceType resourceType) throws Exception {
		logger.info("authorize redirectUri: " + redirectUri);
		flow = getFlow(resourceType);
		GoogleAuthorizationCodeRequestUrl url = flow.newAuthorizationUrl();
		String redirectUrl = url.setRedirectUri(redirectUri).setAccessType("offline").build();
		logger.info("redirectUrl: , " + redirectUrl);
		return redirectUrl;
	}

	@Override
	public String getToken(String code, String redirectUri, ResourceType resourceType) throws Exception {
		GoogleTokenResponse tokenResponse = new GoogleTokenResponse();
		// exchange the code against the access token and refresh token
		flow = getFlow(resourceType);
		tokenResponse = flow.newTokenRequest(code).setRedirectUri(redirectUri).execute();
		return tokenResponse.getAccessToken();
	}

	@Override
	public String getRefreshToken(String code, String redirectUri, ResourceType resourceType) throws Exception {
		GoogleTokenResponse tokenResponse = new GoogleTokenResponse();
		logger.info("refreshToken before executing newTokenRequest: " + refreshToken);
		flow = getFlow(resourceType);
		// exchange the code against the access token and refresh token
		tokenResponse = flow.newTokenRequest(code).setRedirectUri(redirectUri).execute();

		String newRefreshToken = tokenResponse.getRefreshToken();

		if (newRefreshToken != null) {
			refreshToken = newRefreshToken;
		}

		// Refresh token should be populated at this point, if not return null.
		if ((refreshToken == null || refreshToken.isEmpty())
				&& (newRefreshToken == null || newRefreshToken.isEmpty())) {
			logger.info("tokenResponse with forced login to Google: " + gson.toJson(tokenResponse));
			return null;
		}

		GenericJson json = new GenericJson();
		if (clientId != null) {
			json.put("client_id", clientId);
		}
		if (clientSecret != null) {
			json.put("client_secret", clientSecret);
		}
		if (refreshToken != null) {
			json.put("refresh_token", refreshToken);
		}
		json.put("type", "authorized_user");
		// Logging the JSON associated with the refresh token
		String generatedJsonForGoogleToken = gson.toJson(json);
		logger.info("Final JSON with refreshToken: " + generatedJsonForGoogleToken);

		return generatedJsonForGoogleToken;
	}

	private GoogleAuthorizationCodeFlow getFlow(ResourceType resourceType) throws Exception {
		if (resourceType == ResourceType.GOOGLEDRIVE) {
			return flowGoogleDrive;
		} else if (resourceType == ResourceType.GOOGLECLOUD) {
			return flowGoogleCloudWithForcedGoogleLogin;
		} else {
			logger.error("getRefreshToken: resourceType cannot be null");
			return null;
		}
	}

}
