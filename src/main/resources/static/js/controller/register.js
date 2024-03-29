function checkEmail() {
	var valid = false;
	var filter = new RegExp('^[_A-Za-z0-9-!#$%&\'*+/=?^_`{|}~]+(\\.[_A-Za-z0-9-!#$%&\'*+/=?^_`{|}~]+)*@[A-Za-z0-9-]+(\\.[A-Za-z0-9-]+)*(\\.[A-Za-z]{2,})$');
    if($("#id_user_email").val() != null && $("#id_user_email").val() != "") {
    	var email = $("#id_user_email").val();
    	if (filter.test(email)) {
	        valid = true;
	    }
    }
    return valid;
}


function checkPasswordAndConfirmPasswordEquality() {
	var isEqual = true;
	if(($("#id_password").val() != null && $("#id_password").val() != "") &&
			($("#id_password_confirm").val() != null && $("#id_password_confirm").val() != "")) {
		return $("#id_password").val() == $("#id_password_confirm").val();
	}
	return isEqual;
}

function checkPasswordEquality() {
	var isEqual = true;
	if(($("#forgot_password").val() != null && $("#forgot_password").val() != "") &&
			($("#forgot_password_confirm").val() != null && $("#forgot_password_confirm").val() != "")) {
		return $("#forgot_password").val() == $("#forgot_password_confirm").val();
	}
	return isEqual;
}


function callRegisterFormValidation() {
	jQuery.validator.addMethod("validEmail", checkEmail);
	jQuery.validator.addMethod("isPasswordAndConfirmPasswordEqual", checkPasswordAndConfirmPasswordEquality);
	$("#register-form").validate({
		  rules: {
			  id_first_name: {
			      required: true
			  },
			  id_last_name: {
				  required: true
			  },
			  id_user_email: {				  
				  required: true,
				  validEmail: true
			  },
			  id_password: {
				  required: true
			  },
			  id_password_confirm: {
				  required: true,
				  isPasswordAndConfirmPasswordEqual: true
			  },
			  id_institution: {
				  required: true
			  },
		  },
		  messages: { 
			  id_user_email: {
				  validEmail: "Enter a valid email address."
			  },
			  id_password_confirm: {
				  isPasswordAndConfirmPasswordEqual: "Enter matching values for Password and Confirm Password."
			  },
		  },
		  submitHandler: function(form) {
			var rcres = grecaptcha.getResponse(1);
	        if(rcres.length){
	        	$('#btnRegister').prop('disabled',true);
				$("#spinner").show();
	            $("#dimmer").show();
				var registerForm= {};
				registerForm.firstName = $("#id_first_name").val();
				registerForm.lastName = $("#id_last_name").val();
				registerForm.emailAddress = $("#id_user_email").val();
				registerForm.password = $("#id_password").val();
				registerForm.confirmPassword = $("#id_password_confirm").val();
				registerForm.institution = $("#id_institution").val();
				registerForm.response = rcres;
				invokeAjax('/register','POST',JSON.stringify(registerForm),postRegisterFunction,postRegistrationFailure,null,'text');	
	        } else {
	        	//alert("Please verify reCAPTCHA");
	        	$(".errorBlockRegister").show();
	    		$(".errorMsgRegister").html("Please verify reCAPTCHA");
	        	$('#btnRegister').prop('disabled',false);
            	return false; 
	        }
			
		  },
	});
}

function postRegisterFunction(data, status) {
	if(data == 'SUCCESS') {
		$('#btnRegister').prop('disabled',false);
		$(".errorBlockRegister").hide();
		$(".successBlockRegister").show();
		$(".successMsgRegister").html("Check your email inbox for an activation link.");
	} else  {
		$('#btnRegister').prop('disabled',false);
		$(".successBlockRegister").hide();
		$(".errorBlockRegister").show();
		$(".errorMsgRegister").html(data);
	}
}

function postRegistrationFailure(url, params, status, error, data) {
	alert(error);
}

function validateUpdateProfile(){
	$("#update-profile-form").validate({
		  rules: {			  
			  firstName: {
				  required: true
			  },
			  lastName: {
				  required: true,
			  },
			  institution: {
				  required: true,
			  },
		  },
		  submitHandler: function(form) {
			$('#btnUpdateProfile').prop('disabled',true);
			$("#spinner").show();
            $("#dimmer").show();
            
            $('#update-profile-form').attr('action', '/user-info');
			$("#update-profile-form").submit();
	     },
	});
}

function postUpdateUserFunction(data,status) {
	location.replace("/");
}

function validateForgotPassword () {
	jQuery.validator.addMethod("isPswdEqual", checkPasswordEquality);
	$("#forgot-password-form").validate({
		  rules: {			  
			  forgot_password: {
				  required: true
			  },
			  forgot_password_confirm: {
				  required: true,
				  isPswdEqual: true
			  },
		  },
		  messages: { 
			  forgot_password_confirm: {
				  isPswdEqual: "Enter matching values for Password and Confirm Password."
			  },
		  },
		  submitHandler: function(form) {
			$('#btnforgotPassword').prop('disabled',true);
			$("#spinner").show();
            $("#dimmer").show();
        	var forgot_password = {};
        	forgot_password.password = $("#forgot_password").val();
        	forgot_password.confirmPassword = $("#forgot_password_confirm").val();
        	
        	invokeAjax('/changePassword','POST',JSON.stringify(forgot_password),postForgotPassword,postForgotPasswordFailure,null,'text');
		  },
	});
}

function postForgotPasswordFailure(url, params, status, error, data) {
	bootbox.alert("Unknown error. Contact <a class='modacSupportLink' href='/contactUs'>MoDaC Support</a>.");
}

function postForgotPassword(data,status) {
	$('#btnforgotPassword').prop('disabled',false);
	if(data == 'SUCCESS') {
		$(".successMsg").html("Password change successful.");
		$(".successBlock").show();
		$(".errorBlock").hide();
	} else if(data == 'loginTab') {
		location.replace("/loginTab");
	}
	else {
		$('#btnforgotPassword').prop('disabled',false);
		$(".errorMsg").html(data);
		$(".errorBlock").show();
		$(".successBlock").hide();
	}
	
}


function postResetLinkFunction(data,status) {
		
	if(data == 'SUCCESS') {
		$("#forgotPasswordLightbox").find(".forgotPswdErrorMsg").html("");
		$("#forgotPasswordLightbox").find(".forgorPswdErrorBlock").hide();
	    $("#forgotPasswordLightbox").find(".forgotPswdSuccessMsg").html("Check your email inbox for a new password.");
		$("#forgotPasswordLightbox").find(".forgorPswdSuccessBlock").show();
	} else {
		$("#forgotPasswordLightbox").find(".forgotPswdSuccessMsg").html("");
		$("#forgotPasswordLightbox").find(".forgorPswdSuccessBlock").hide();
	    $("#forgotPasswordLightbox").find(".forgotPswdErrorMsg").html(data);
	    $("#forgotPasswordLightbox").find(".forgorPswdErrorBlock").show();
		 
	}
}