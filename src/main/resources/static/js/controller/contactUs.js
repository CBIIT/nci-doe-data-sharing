$(".landing-tab").removeClass('active');
$("#contact-tab").addClass('active');
$(".aboutTabNav").addClass('active');


$(".contactUsTextbox").keyup(function() {
	if ($(this).val()) {
		$(this).parent().find('.TextField-floatingLabel-qefpP').show();
	} else {
		$(this).parent().find('.TextField-floatingLabel-qefpP').hide();
	}
});

var onloadCallback = function() {
	var siteKey = $("#siteKey").val();
	grecaptcha.render('google_recaptcha', {
		'sitekey' : siteKey
	});
};

function checkEmail() {
	var valid = false;
	var filter = new RegExp(
			'^[_A-Za-z0-9-!#$%&\'*+/=?^_`{|}~]+(\\.[_A-Za-z0-9-!#$%&\'*+/=?^_`{|}~]+)*@[A-Za-z0-9-]+(\\.[A-Za-z0-9-]+)*(\\.[A-Za-z]{2,})$');
	if ($("#id_user_email").val() != null && $("#id_user_email").val() != "") {
		var email = $("#id_user_email").val();
		if (filter.test(email)) {
			valid = true;
		}
	}
	return valid;
}


function callContactUsFormValidation() {
	jQuery.validator.addMethod("validEmail", checkEmail);
	$("#contact-us-form").validate(
			{
				rules : {
					contact_us_first_name : {
						required : true
					},
					contact_us_lastname : {
						required : true
					},
					contact_us_org : {
						required : true
					},
					id_user_email : {
						required : true,
						validEmail : true
					},
				},
				messages : {
					id_user_email : {
						validEmail : "Enter a valid email address."
					},
				},
				submitHandler : function(form) {
					var rcres = grecaptcha.getResponse();
					var valid = true;
					if ($("#inquiryList").val() && $("#inquiryList").val() == "Select") {
						valid = false;
						$(".errorBlock").show();
						$(".errorMsg").html("Please select a type of inquiry.");
						$('#btnSubmitEmail').prop('disabled', false);
						return false;
					}
					if(!rcres.length) {
						valid = false;
						$(".errorBlock").show();
						$(".errorMsg").html("Please verify reCAPTCHA");
						$('#btnSubmitEmail').prop('disabled', false);
						return false;
					}
					
					if (valid) {
						$('#btnSubmitEmail').prop('disabled', true);
						$("#spinner").show();
						$("#dimmer").show();
						grecaptcha.reset();
						var contactusForm = {};
						contactusForm.firstName = $("#contact_us_first_name").val();
						contactusForm.emailAddress = $("#id_user_email").val();
						contactusForm.lastName = $("#contact_us_lastname").val();
						contactusForm.org = $("#contact_us_org").val();
						contactusForm.message = $("#contact_us_txtarea").val();
						contactusForm.inquiry = $('#inquiryList option:selected').text();
						contactusForm.response = rcres;
						invokeAjax('/contactUs', 'POST', JSON.stringify(contactusForm), postContactUsFunction,
								postContactUsFailure, null, 'text');
					}

				},
			});
}

function postContactUsFunction(data, status) {
	if (data == 'SUCCESS') {
		$('#btnSubmitEmail').hide();
		$(".errorBlock").hide();
		$(".successBlock").show();
		$("#contactusMsg").fadeIn(1000).delay(1000).fadeOut(1000, function() {
        	location.replace('/');
        });
	} else {
		$('#btnSubmitEmail').show();
		$('#btnSubmitEmail').prop('disabled', false);
		$(".successBlock").hide();
		$(".errorBlock").show();
		$(".errorMsg").html(data);
	}
}

function postContactUsFailure(url, params, status, error, data) {
	alert(error);
}