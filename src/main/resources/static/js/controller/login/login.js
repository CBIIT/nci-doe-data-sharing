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
	
	var redirectMsg = $("#redirectMsg").val();
	if(redirectMsg) {
	  $("#register-tab").click();
	}
});
 
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
				 form.submit();
			} else {
				$(".errorBlockLogin").show();
				$(".errorMsgLogin").html("Please verify reCAPTCHA");
				$('#loginButton').prop('disabled',false);
            	return false; 
			}
			 
		}
	});		
}