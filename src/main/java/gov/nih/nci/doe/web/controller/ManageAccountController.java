package gov.nih.nci.doe.web.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;

import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.doe.web.model.DoeUsersModel;

@Controller
@EnableAutoConfiguration
@RequestMapping("")
public class ManageAccountController extends AbstractDoeController {

	@GetMapping(value = "/manageAccount")
	public String getMyAccount(Model model, HttpSession session, HttpServletRequest request) {
		log.info("get account");
		String user = getLoggedOnUserInfo();
		if (StringUtils.isEmpty(user)) {
			return "redirect:/loginTab";
		}
		try {
			log.info("get user details for : " + user);
			DoeUsersModel userInfo = authService.getUserInfo(user);
			model.addAttribute("userInfo", userInfo);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return "manageAccount/manageAccountTab";
	}
	
	

	@PostMapping(value = "user-info")
	public String updateUserInfo(@Valid DoeUsersModel doeModel, HttpSession session, HttpServletRequest request,
			HttpServletResponse response) throws DoeWebException {

		log.info("update user info for user " + doeModel.getFirstName());
		try {
			String user = getLoggedOnUserInfo();
			if (StringUtils.isEmpty(user)) {
				return "redirect:/loginTab";
			}
			doeModel.setEmailAddrr(user);
			authService.saveUserInfo(doeModel);

			return "redirect:/";
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			throw new DoeWebException("Failed to update user info: " + e.getMessage());
		}

	}
}
