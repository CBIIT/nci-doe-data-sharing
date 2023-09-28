$(document).ready(function () {

 var signUpEmailInput = $("input[name=signUpEmailAddress]");

       signUpEmailInput.on("input", function () {
       
	       var email= $("#signUpEmailAddress").val();
	       var filter = new RegExp('^[_A-Za-z0-9-!#$%&\'*+/=?^_`{|}~]+(\\.[_A-Za-z0-9-!#$%&\'*+/=?^_`{|}~]+)*@[A-Za-z0-9-]+(\\.[A-Za-z0-9-]+)*(\\.[A-Za-z]{2,})$');
	    		
		  	if (!filter.test(email) || email == null || email == "") {
		  		this.setCustomValidity("Enter a valid email.");		        
		    }  else {
	            this.setCustomValidity("");
	        }
        });
 
  //onclick on sign up for email updates sign up button
 	
 	$('#signup').submit(function(event) {
	    
	   event.preventDefault(); // Prevent the default form submission
	   var emailAddress = $("#signUpEmailAddress").val();

	   var params = {emailAddress : emailAddress};
	   
	   var filter = new RegExp('^[_A-Za-z0-9-!#$%&\'*+/=?^_`{|}~]+(\\.[_A-Za-z0-9-!#$%&\'*+/=?^_`{|}~]+)*@[A-Za-z0-9-]+(\\.[A-Za-z0-9-]+)*(\\.[A-Za-z]{2,})$');
	   var signUpEmailInput = $("input[name=signUpEmailAddress]");	
	   	
		  	if (!filter.test(emailAddress) || emailAddress == null || emailAddress == "") {
		  		signUpEmailInput[0].setCustomValidity("Enter a valid email.");		        
		    }  else {
		    	signUpEmailInput[0].setCustomValidity("");
		    	
    	       invokeAjax('/emailNotifications/subscribe', 'POST', params, postSuccessEmailUpdatesFunction,
				postFailureEmailUpdatesFunction,
				'application/x-www-form-urlencoded; charset=UTF-8', 'text');
	        }
	       	

		
	
 	});
 	
 	// open default mail when calling emailNotifications API
 	var mailUrl = $("#mailUrl").val();
 	if(mailUrl) {
        // Open the user's default email client
        window.location.href = mailUrl;
 	}
 	
});

function postSuccessEmailUpdatesFunction(data, status) {
	$("#signUpEmailAddress").val("");
	return bootbox.alert(data);
}

function postFailureEmailUpdatesFunction(url, params, status, error, data) {
	return bootbox.alert(error);
}