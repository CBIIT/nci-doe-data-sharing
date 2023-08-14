$(document).ready(function () {

 //onclick on sign up for email updates sign up button

 	
 	$('#signup').submit(function(event) {
	    
	   event.preventDefault(); // Prevent the default form submission
	   var emailAddress = $("#signUpEmailAddress").val();

	   var params = {emailAddress : emailAddress};
	       	
       invokeAjax('/emailNotifications/subscribe', 'POST', params, postSuccessEmailUpdatesFunction,
						postFailureEmailUpdatesFunction,
						'application/x-www-form-urlencoded; charset=UTF-8', 'text');
		
	
 	});
});

function postSuccessEmailUpdatesFunction(data, status) {
	$("#signUpEmailAddress").val("");
	return bootbox.alert(data);
}

function postFailureEmailUpdatesFunction(url, params, status, error, data) {
	return bootbox.alert(error);
}