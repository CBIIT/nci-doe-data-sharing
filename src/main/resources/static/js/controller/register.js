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
				  validEmail: "Invalid email address."
			  },
			  id_password_confirm: {
				  isPasswordAndConfirmPasswordEqual: "Password and Confirm Password values do not match."
			  },
		  },
		  submitHandler: function(form) {
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
			invokeAjax('/register','GET',registerForm,postRegisterFunction,postRegistrationFailure,null,'text');
		  },
	});
}

function postRegisterFunction(data, status) {
	if(data == 'SUCCESS') {
		location.reload();
	} else  {
		$('#btnRegister').prop('disabled',false);
		$(".errorBlockRegister").show();
		$(".errorMsgRegister").html(data);
	}
}

function postRegistrationFailure(url, params, status, error, data) {
	alert(error);
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
				  isPswdEqual: "Password and Confirm Password values do not match."
			  },
		  },
		  submitHandler: function(form) {
			$('#btnforgotPassword').prop('disabled',true);
			$("#spinner").show();
            $("#dimmer").show();
        	var forgot_password = {};
        	forgot_password.emailAddrr = $("#txtforgotPasswordEmail").text();
        	forgot_password.password = $("#forgot_password").val();
        	forgot_password.confirmPassword = $("#forgot_password_confirm").val();
        	
        	invokeAjax('/forgotPassword','GET',forgot_password,postForgotPassword,null,null,'text');
		  },
	});
}



function postForgotPassword(data,status) {
	$('#btnforgotPassword').prop('disabled',false);
	if(data == 'SUCCESS') {
		$(".successMsg").html("Password reset successful.");
		$(".successBlock").show();
		$(".errorBlock").hide();
	} else {
		$('#btnforgotPassword').prop('disabled',false);
		$(".errorMsg").html(data);
		$(".errorBlock").show();
		$(".successBlock").hide();
	}
	
}


function postResetLinkFunction(data,status) {
	$("#forgotPasswordLightbox").modal('hide');	
	if(data == 'SUCCESS') {		
	bootbox.alert("A Password reset Link has been sent successfully!");
	} else {
		bootbox.alert("Unknown Error. Contact Technical Support!");
	}
}
