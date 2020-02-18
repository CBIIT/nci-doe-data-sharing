
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
		        minlength: 8,
		        maxlength: 14
		    }
		},
		messages: {
			 username: {
			    	required: "Email Address is required."
			    },
			password: {
		        required: "Password is required.",
		        minlength: "Password must be at least eight (8) characters in length and no more than 14 characters.",
		        maxlength: "Password must be at least eight (8) characters in length and no more than 14 characters."
		    },
		   
		},
		submitHandler: function(form) {
			$('#loginButton').prop('disabled',true);
			  var data = 'username=' + $('#username').val() + '&password=' + $('#password').val();				
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
			          handleAjaxError(url, params, status, error, data);
			          loginFailureFunction(data,status);
			      }
			    });
		}
	});		
}

function postLoginFunction(data,status) {
	if("loginFailure" == data) {
		
		$(".errorBlockLogin").show();
		$(".errorMsgLogin").html("Invalid Credentials");
		
	} else if("inValidEmail" == data) {
		
		$(".errorBlockLogin").show();
		$(".errorMsgLogin").html("Provide an email address.");
		
	} else if("inValidPassword" == data) {
		
		$(".errorBlockLogin").show();
		$(".errorMsgLogin").html("Provide password.");
		
	} else if("loginlocked" == data) {
		
		$(".errorBlockLogin").show();
		$(".errorMsgLogin").html("Maximum attempts of login Exceeded. Please request a password resend link via forgot password.");
		
	} else {
		location.replace("/");
	}
	
}

function loginFailureFunction(data,status) {
	$(".errorBlockLogin").show();
	$(".errorMsgLogin").html(data);
}

function postLogOutFunction(data, status) {
	//$('.nav-tabs a[href="#' + landingTab + '"]').tab('show');
	location.replace("/");
}

function postGetUserInfoFunction (data,status) {
	$("#firstNameTxt").val(data.firstName);
	$("#lastNameTxt").val(data.lastName);
	$("#institutionTxt").val(data.institution);
}

function postUpdateUserFunction(data,status) {
	location.reload();
}
