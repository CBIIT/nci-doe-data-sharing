var onloadCallback = function() {
	var siteKey = $("#siteKey").val();
	grecaptcha.render('logincaptchadiv', {'sitekey' : siteKey});
    grecaptcha.render('signupcaptchadiv', {'sitekey' : siteKey});
};
$(document).ready(function () {
	
  $(document).keypress(function(event){	
	var keycode = (event.keyCode ? event.keyCode : event.which);
	  if(keycode == '13'){
		 event.preventDefault();
		 if($("#btnforgotPassword").is(":visible")) {
			 $("#btnforgotPassword").trigger("click");
		 } else if($("#loginButton").is(":visible")) {
			 $("#loginButton").trigger("click");
		 } else if($("#btnUpdateProfile").is(":visible")) {
			 $("#btnUpdateProfile").trigger("click"); 
		 } else if($("#btnRegister").is(":visible")) {
			 $("#btnRegister").trigger("click");
		 }
		 
	  }

	});
});

$('#loginTab').on('hide.bs.tab hidden.bs.tab', function(e) {
        clearMessages();
    });

 function clearMessages() {
//clear all messages
 }
 
function validateUserLogin() {
	
	$("#userlogin").validate({
		rules: {
			username: {
				required: true
			},
			password: {
		        required: true,
		    }
		},
		messages: {
			 username: {
			    	required: "Enter an email address."
			    },
			password: {
		        required: "Enter a password.",
		    },
		   
		},
		submitHandler: function(form) {
			$('#loginButton').prop('disabled',true);
			var rcres = grecaptcha.getResponse(0);
			if(rcres.length){
				 var data = $('#userlogin').serialize();
				   $.ajax({
				      data: data,
				      type: 'POST',
				      url: '/login',
				      success: function (data, status) {
				    	  $('#loginButton').prop('disabled',false);
				    	  postLoginFunction(data,status);
				      },
				      error: function (data, status, error) {
				    	  $('#loginButton').prop('disabled',false);
				          handleAjaxError('/login', null, status, error, data);
				          loginFailureFunction(data,status);
				      }
				    });
			} else {
				alert("Please verify reCAPTCHA");
				$('#loginButton').prop('disabled',false);
            	return false; 
			}
			 
		}
	});		
}

function postLoginFunction(data,status) {
	$(".successBlockLogin").hide();
	if("loginFailure" == data) {
		
		$(".errorBlockLogin").show();
		$(".errorMsgLogin").html("Enter valid credentials.");
		
	} else if("inValidEmail" == data) {
		
		$(".errorBlockLogin").show();
		$(".errorMsgLogin").html("Enter an email address.");
		
	} else if("inValidPassword" == data) {
		
		$(".errorBlockLogin").show();
		$(".errorMsgLogin").html("Enter a password.");
		
	} else if("loginlocked" == data) {
		
		$(".errorBlockLogin").show();
		$(".errorMsgLogin").html("Maximum login attempts exceeded. Request a password via " +
				"<a style='text-decoration: underline;color: #fff;'href='javascript:void(0);' " +
				"data-toggle='modal' data-target='#forgotPasswordLightbox'>forgot password</a>.");
		
	} else if("loginInactivated" == data) {
		$(".errorBlockLogin").show();
		$(".errorMsgLogin").html("Check your email inbox for an activation link.");
		
	} else if("InvalidCaptcha" == data) {
		$(".errorBlockLogin").show();
		$(".errorMsgLogin").html("Invalid Captcha.");
		
	} else {
		location.replace("/");
	}
	
}

function loginFailureFunction(data, status) {
	$(".successBlockLogin").hide();
	$(".errorBlockLogin").show();
	if (data && data.responseText) {
		var errorJson = JSON.parse(data.responseText);

		$(".errorMsgLogin").html(errorJson.message);

	} else {
		$(".errorMsgLogin").html("Unknown Error. Contact Technical Support!");
	}

}

function postUpdateUserFunction(data,status) {
	location.replace("/");
}
