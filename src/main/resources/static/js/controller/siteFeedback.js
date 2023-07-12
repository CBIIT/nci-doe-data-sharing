$(".landing-tab").removeClass('active');
$("#contact-tab").addClass('active');
$(".aboutTabNav").addClass('active');



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
		var email = $("#site_feedback_email").val();
		if (filter.test(email)) {
			valid = true;
		}
	}
	return valid;
}

function callSiteFeedbackFormValidation() {
	jQuery.validator.addMethod("validEmail", checkEmail);
	$("#site-feedback-form").validate(
			{
				rules : {
					site_feedback_name : {
						required : true,
						minlength : 1
					},
					site_feedback_name : {
						required : true
					},
					site_feedback_lastname : {
						required : true
					},
					site_feedback_org : {
						required : true
					},
					site_feedback_email : {
						required : true,
						validEmail : true
					},
				},
				messages : {
					site_feedback_email : {
						validEmail : "Enter a valid email address."
					},
				},
				submitHandler : function(form) {
					var rcres = grecaptcha.getResponse();
					if (rcres.length) {
						$('#submitFeedback').prop('disabled', true);
						$("#spinner").show();
						$("#dimmer").show();
						grecaptcha.reset();
						var siteFeedbackForm = {};
						siteFeedbackForm.firstName = $("#site_feedback_name").val();
						siteFeedbackForm.lastName = $("#site_feedback_lastname").val();
						siteFeedbackForm.emailAddress = $("#site_feedback_email").val();
						siteFeedbackForm.message = $("#site_feedback_txtarea").val();
						siteFeedbackForm.response = rcres;
						invokeAjax('/siteFeedback', 'POST', JSON.stringify(siteFeedbackForm), postSiteFeedbackFunction,
								postSiteFeedbackFailure, null, 'text');
					} else {
						$(".errorBlock").show();
						$(".errorMsg").html("Please verify reCAPTCHA");
						$('#btnSubmitEmail').prop('disabled', false);
						return false;
					}

				},
			});
}

function postSiteFeedbackFunction(data, status) {
	if (data == 'SUCCESS') {
		$('#submitFeedback').prop('disabled', false);
		$(".errorBlock").hide();
		$(".successBlock").show();
		$("#siteFeedbackMsg").fadeIn(1000).delay(5000).fadeOut(1000, function() {
        location.replace('/');
      });
	} else {
		$('#submitFeedback').prop('disabled', false);
		$(".successBlock").hide();
		$(".errorBlock").show();
		$(".errorMsg").html(data);
	}
}

function postSiteFeedbackFailure(url, params, status, error, data) {
	alert(error);
}