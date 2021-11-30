var onloadCallback = function() {
	grecaptcha.render('google_recaptcha', {
		'sitekey' : '6LcdTWwdAAAAAF7D-Gm3B_zSBdALSOtEnXBIqJEq'
	});
};

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



function callContactUsFormValidation() {
	jQuery.validator.addMethod("validEmail", checkEmail);
	$("#contact-us-form").validate({
		  rules: {
			  contact_us_txtarea: {
				  required: true,
				  minlength:1
			  },
			  contact_us_name : {
				  required: true
			  },
			  id_user_email: {				  
				  required: true,
				  validEmail: true
			  },
		  },
		  messages: { 
			  id_user_email: {
				  validEmail: "Enter a valid email address."
			  },
		  },
		  submitHandler: function(form) {			
            var rcres = grecaptcha.getResponse();
            if(rcres.length){
            	$('#btnSubmitEmail').prop('disabled',true);
    			$("#spinner").show();
                $("#dimmer").show();
            	grecaptcha.reset();
            	var contactusForm= {};
    			contactusForm.name = $("#contact_us_name").val();
    			contactusForm.emailAddress = $("#id_user_email").val();
    			contactusForm.message = $("#contact_us_txtarea").val();
    			invokeAjax('/contactUs','POST',JSON.stringify(contactusForm),postRegisterFunction,postRegistrationFailure,null,'text');
            } else {
            	alert("Please verify reCAPTCHA");
            	return false; 
            }
			
		  },
	});
}

function postRegisterFunction(data, status) {
	if(data == 'SUCCESS') {
		$('#btnSubmitEmail').prop('disabled',false);
		$(".errorBlockRegister").hide();
		$(".successBlockRegister").show();
		$(".successMsgRegister").html("Check your email inbox for an activation link.");
	} else  {
		$('#btnSubmitEmail').prop('disabled',false);
		$(".successBlockRegister").hide();
		$(".errorBlockRegister").show();
		$(".errorMsgRegister").html(data);
	}
}

function postRegistrationFailure(url, params, status, error, data) {
	alert(error);
}