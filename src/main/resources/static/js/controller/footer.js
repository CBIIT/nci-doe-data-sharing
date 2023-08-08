$(document).ready(function () {

 //onclick on sign up for email updates button
 
 	$("#signUpBtn").click(function(){
 			
 			var emailAddr = $("#signUpEmailAddress").val();
 		var params = {emailAddr : emailAddr};
			$
			.ajax({
				type : "POST",
				url : "/emailNotifications/subscribe",
				contentType : 'application/json',
				data : params,
				beforeSend : function() {
					$("#spinner").show();
					$("#dimmer").show();
				},
				success : function(msg) {
					$("#spinner").hide();
					$("#dimmer").hide();
					console.log('SUCCESS: ', msg);

					alert(msg);

				},
				error : function(e) {
					console.log('ERROR: ', e);
					$("#spinner").hide();
					$("#dimmer").hide();
					alert(e.message);
				}
			});
 		
 	});
});