package gov.nih.nci.doe.web.controller;

import java.util.Date;
import java.util.List;

import javax.servlet.http.HttpSession;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.domain.MetaDataPermissions;
import gov.nih.nci.doe.web.model.AuditingModel;
import gov.nih.nci.doe.web.util.DoeClientUtil;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;

@Controller
@EnableAutoConfiguration
@RequestMapping("/deleteCollection")
public class DeleteCollectionController extends AbstractDoeController {

	@Value("${gov.nih.nci.hpc.server.collection}")
	private String serviceURL;

	@PostMapping
	@ResponseBody
	public String deletCollection(@RequestParam(value = "collPath") String collPath, HttpSession session,
			@RequestHeader HttpHeaders headers) throws DoeWebException {

		String authToken = (String) session.getAttribute("writeAccessUserToken");
		String userInfo = getLoggedOnUserInfo();

		if (authToken == null || StringUtils.isEmpty(userInfo)) {
			return "Invalid token. Login again.";
		}

		if (collPath == null) {
			return "Invalid Data object path!";
		}

		HpcCollectionListDTO collections = DoeClientUtil.getCollection(authToken, serviceURL, collPath, false);

		if (collections != null && collections.getCollections() != null
				&& !CollectionUtils.isEmpty(collections.getCollections())) {

			HpcCollectionDTO collection = collections.getCollections().get(0);
			MetaDataPermissions perm = metaDataPermissionService
					.getMetaDataPermissionsOwnerByCollectionId(collection.getCollection().getCollectionId());

			if (perm != null && perm.getUser().getEmailAddrr().equalsIgnoreCase(userInfo)) {

				String deleted = DoeClientUtil.deleteCollection(authToken, serviceURL, collPath);
				if (StringUtils.isNotEmpty(deleted) && deleted.equalsIgnoreCase("SUCCESS")) {

					// delete from collection permission table
					metaDataPermissionService.deleteAllPermissionsByCollectionId(getLoggedOnUserInfo(),
							collection.getCollection().getCollectionId());

					// delete from access group table
					List<String> existingGroups = accessGroupsService.getGroupsByCollectionPath(collPath);
					if (CollectionUtils.isNotEmpty(existingGroups)) {
						accessGroupsService.updateAccessGroups(collPath, collection.getCollection().getCollectionId(),
								null, existingGroups);
					}

					// store the auditing info
					AuditingModel audit = new AuditingModel();
					audit.setName(getLoggedOnUserInfo());
					audit.setOperation("delete collection");
					audit.setStartTime(new Date());
					audit.setPath(collPath);
					auditingService.saveAuditInfo(audit);

					return "SUCCESS";
				} else {
					return "Failed to delete collection." + deleted;
				}
			} else {
				return "Only the collection owner can delete this.";
			}
		}

		return "Cannot find collection";

	}

}
