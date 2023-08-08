$(document)
		.ready(
				function() {
				$("#sendNotificationBtn").click(function(e) {
						callNotificationFormValidation();

					});
			});

function callNotificationFormValidation() {
	$("#release-notification-form").validate({
		  submitHandler: function(form) {
	        	$('#sendNotificationBtn').prop('disabled',true);
				$("#spinner").show();
	            $("#dimmer").show();
	            var message = $("#notification_txtarea").val();
	            var form = $('#release-notification-form')[0];
				var data = new FormData(form);
	
				$.ajax({
					type : "POST",
					enctype : "multipart/form-data",
					url : "/emailNotifications?message=" + message,
					data : data,
					processData : false,
					contentType : false,
					beforeSend : function() {
						$("#spinner").show();
						$("#dimmer").show();
					},
					success : function(msg) {
						$("#spinner").hide();
						$("#dimmer").hide();
						console.log('SUCCESS: ', msg);
		
						if (!msg) {
							msg = "Error in sending notification";
						}
						bootbox.dialog({
							message : msg,
							onEscape : function() {
								$('#sendNotificationBtn').prop('disabled',false);
							}
						});
					},
					error : function(e) {
						$("#spinner").hide();
						$("#dimmer").hide();
						console.log('ERROR: ', e);
						bootbox.dialog({
							message : e,
							onEscape : function() {
								$('#sendNotificationBtn').prop('disabled',false);
							}
						});
		
					}
				});		
		  },
	});
}
