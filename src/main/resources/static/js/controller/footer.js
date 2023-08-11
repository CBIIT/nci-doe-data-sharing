$(document).ready(function () {

 //onclick on sign up for email updates sign up button

 	
 	$('#myForm').submit(function(event) {
	    
	   event.preventDefault(); // Prevent the default form submission
	   var emailAddress = $("#signUpEmailAddress").val();
		
	   var valid = false;
	   var filter = new RegExp('^[_A-Za-z0-9-!#$%&\'*+/=?^_`{|}~]+(\\.[_A-Za-z0-9-!#$%&\'*+/=?^_`{|}~]+)*@[A-Za-z0-9-]+(\\.[A-Za-z0-9-]+)*(\\.[A-Za-z]{2,})$');
       if(emailAddress != null && emailAddress != "") {
	    	if (filter.test(emailAddress)) {
		        valid = true;
		    }
   		}
   		
	    if (emailAddress === '') {
	      $('.errorBanner').text('Please fill out this field');
	    } else if(valid == false) {
	      $('.errorBanner').text('Please enter a valid email address');
	    } else {
	      $('.errorBanner').text('');
	       	var params = {emailAddress : emailAddress};
	       	
	       	invokeAjax('/emailNotifications/subscribe', 'POST', params, postSuccessEmailUpdatesFunction,
								postFailureEmailUpdatesFunction,
								'application/x-www-form-urlencoded; charset=UTF-8', 'text');
		}
	
 	});
});

function postSuccessEmailUpdatesFunction(data, status) {
	$("#signUpEmailAddress").val("");
	return bootbox.alert(data);
}

function postFailureEmailUpdatesFunction(url, params, status, error, data) {
	return bootbox.alert(error);
}