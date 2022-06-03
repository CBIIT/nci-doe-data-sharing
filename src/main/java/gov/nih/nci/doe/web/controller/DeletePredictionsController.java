package gov.nih.nci.doe.web.controller;

import java.util.Arrays;
import java.util.Date;
import java.util.List;

import javax.servlet.http.HttpSession;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.AuditingModel;
import gov.nih.nci.doe.web.model.DeletePredictionsModel;
import gov.nih.nci.doe.web.util.DoeClientUtil;

@Controller
@EnableAutoConfiguration
@RequestMapping("/deletePredictions")
public class DeletePredictionsController extends AbstractDoeController {

	@Value("${gov.nih.nci.hpc.server.dataObject}")
	private String dataObjectUrl;

	@PostMapping
	@ResponseBody
	public String deletePredictions(@RequestBody DeletePredictionsModel deletePredModel, HttpSession session,
			@RequestHeader HttpHeaders headers) throws DoeWebException {

		String authToken = (String) session.getAttribute("writeAccessUserToken");
		String userInfo = getLoggedOnUserInfo();

		if (authToken == null || StringUtils.isEmpty(userInfo)) {
			return "Not Authorized";
		}

		if (StringUtils.isNotEmpty(deletePredModel.getDeletepaths())) {
			List<String> pathsList = Arrays.asList(deletePredModel.getDeletepaths().split(","));
			for (String path : pathsList) {
				String deleted = DoeClientUtil.deleteDatafile(authToken, dataObjectUrl, path);
				if (StringUtils.isNotEmpty(deleted) && deleted.equalsIgnoreCase("true")) {

					// store the auditing info
					AuditingModel audit = new AuditingModel();
					audit.setName(userInfo);
					audit.setOperation("delete data file");
					audit.setStartTime(new Date());
					audit.setPath(path);
					auditingService.saveAuditInfo(audit);

				} else {
					return "Failed to delete predictions " + deleted;
				}
			}

			String deleted = DoeClientUtil.deleteCollection(authToken, serviceURL,
					deletePredModel.getPredCollectionPath());
			if (StringUtils.isNotEmpty(deleted) && deleted.equalsIgnoreCase("SUCCESS")) {

				// delete from collection permission table
				predictionAccessService.deleteAllPermissionsByCollectionPath(getLoggedOnUserInfo(),
						deletePredModel.getPredCollectionPath());

				// store the auditing info
				AuditingModel audit = new AuditingModel();
				audit.setName(getLoggedOnUserInfo());
				audit.setOperation("delete collection");
				audit.setStartTime(new Date());
				audit.setPath(deletePredModel.getPredCollectionPath());
				auditingService.saveAuditInfo(audit);

				// delete from inference table

				if (StringUtils.isNoneEmpty(deletePredModel.getTaskId())) {
					inferencingTaskService.deleteInferenceByTaskId(deletePredModel.getTaskId());
				}

			} else {
				return "Failed to delete prediction " + deleted;
			}
		}

		return "SUCCESS";
	}

}
