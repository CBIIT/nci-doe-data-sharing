
$(document).ready(function () {
	
  $(document).keypress(function(event){	
	var keycode = (event.keyCode ? event.keyCode : event.which);
	  if(keycode == '13'){
		 event.preventDefault();
		 $("#loginButton").trigger("click");
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
			  //var data = 'username=' + $('#username').val() + '&password=' + $('#password').val();	
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
			          handleAjaxError(url, params, status, error, data);
			          loginFailureFunction(data,status);
			      }
			    });
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
		
	} else {
		location.replace("/");
	}
	
}

function loginFailureFunction(data,status) {
	$(".successBlockLogin").hide();
	$(".errorBlockLogin").show();
	$(".errorMsgLogin").html(data);
}

function postLogOutFunction(data, status) {
	location.replace("/");
}

function postGetUserInfoFunction (data,status) {
	var userData = JSON.parse(data);
	$("#firstNameTxt").val(userData.firstName);
	$("#lastNameTxt").val(userData.lastName);
	$("#institutionTxt").val(userData.institution);
	$("#groupNames").text(userData.programName);
}

function postUpdateUserFunction(data,status) {
	location.replace("/");
}
