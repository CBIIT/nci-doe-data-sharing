$(document)
		.ready(
				function() {
				console.log("collection and files download");
				
									
					var selectedPathsString = $("#selectedPathsString").val();
					var downloadType = $("#downloadType").val();
					var downloadFileName = $("#downloadFileName").val();
					var asyncSearchType = $("#asyncSearchType").val();	

					if (selectedPathsString && downloadType
							&& (downloadType == 'collection' || downloadType == 'collectionfiles')) {
						$("#syncRadioSet").hide();
						$("#SyncDiv").hide();
						if (asyncSearchType) {
							$("input[name=searchType][value=" + asyncSearchType + "]").click();
						}

						var selectedPaths = selectedPathsString.split(',');
						$("#selectedFilesList").val(selectedPaths);

						$.each(selectedPaths, function(index, value) {
							$(".selectedFilesListDisplay").append("<p>" + value + "</p>");
						});
						$(".selectedFilesDiv").show();

						if (selectedPaths.length == 1) {
							$("#destinationPathId").val(selectedPaths);
						}

						$("#informationalText")
								.html(
										"This page allows you to download the "
												+ "selected data files "
												+ "asynchronously to a Globus endpoint location, an S3 bucket, or Google Drive.");
					} else if (selectedPathsString && downloadType
							&& (downloadType == 'data_object' || downloadType == 'datafiles')) {

						if (downloadFileName && downloadFileName != "null" && downloadType == 'data_object') {
						
						    var fileSize = $("#fileSize").val();
						    
						    /* show sync download if file size is less than 2GB */
						    
						    if((fileSize && fileSize < 2147483648) || !fileSize) {
						    	$("#syncRadioSet").show();
						    } else  {
						     	$("#syncRadioSet").hide();
						    }
							
							$(".selectedFilesListDisplay").append("<p>" + selectedPathsString + "</p>");
							$(".selectedFilesDiv").show();
							$("#destinationPathId").val(selectedPathsString);
							$("#drivePath").val(downloadFileName);
							$("#cloudPath").val(downloadFileName);
							$("#informationalText")
									.html(
											"This page allows you to download the "
													+ "selected data file either synchronously to your computer or asynchronously "
													+ "to Globus endpoint location, an S3 bucket, or Google Drive.");
							if (asyncSearchType) {
								$("input[name=searchType][value=" + asyncSearchType + "]").click();
							} else {
								$("#searchTypeSync").click();
							}

						} else {
							$("#syncRadioSet").hide();
							$("#SyncDiv").hide();
							if (asyncSearchType) {
								$("input[name=searchType][value=" + asyncSearchType + "]").click();
							}
							$("#informationalText")
									.html(
											"This page allows you to download the "
													+ "selected data files "
													+ "asynchronously to a Globus endpoint location, an S3 bucket, or Google Drive.");

							var selectedPaths = selectedPathsString.split(',');
							$("#selectedFilesList").val(selectedPaths);

							$.each(selectedPaths, function(index, value) {
								$(".selectedFilesListDisplay").append("<p><input type='checkbox' class='checkboxMultipleDwnlodPaths' checked='true' value="+value+"> &nbsp;&nbsp;" + value + "</p>");
							});
							$(".selectedFilesDiv").show();

							if (selectedPaths.length == 1) {
								$("#destinationPathId").val(selectedPaths);
							}
						}

					}
					
				

					$("#download-btn")
							.click(
									function(e) {
										e.preventDefault();
										var selectedFiles = $("#selectedFilesList").val();
										var searchType = $('input[name=searchType]:checked:visible').val();
										var d = {};
										d.searchType = searchType;
										d.destinationPath = $("#destinationPathId").val();
										d.downloadType = $("#downloadType").val();
										d.downloadFileName = $("#downloadFileNameVal").val();
										$("#message").hide();
										var validate = true;
										if (!searchType) {
											$('.downloadErrorMsg').html("Select a download destination.");
											$("#message").show();
										} else if($('input:checkbox.checkboxMultipleDwnlodPaths').is(':visible') && 
												$('input:checkbox.checkboxMultipleDwnlodPaths:checked').length == 0){
											/*
											 * multiple checkbox download and
											 * validate if either one is checked
											 */
											$('.downloadErrorMsg').html("Select atleast one path.");
											$("#message").show();
										} else if (searchType == 's3' || searchType == 'async' || searchType == 'drive' || searchType == 'cloud'
												|| selectedFiles) {
											d.bucketName = $("#downloadBucketName").val();
											d.s3Path = $("#downloadS3Path").val();
											d.accessKey = $("#downloadAccessKey").val();
											d.secretKey = $("#downloadSecretKey").val();
											d.region = $("#downloadRegion").val();
											d.endPointName = $("#endPointName").val();
											d.endPointLocation = $("#endPointLocation").val();
											d.drivePath = $("#drivePath").val();
											d.googleCloudPath = $("#cloudPath").val();
											d.googleCloudBucketName = $("#cloudBucketName").val();

											var url;
											if (selectedFiles) {
												url = "/downloadfiles/download";
												d.selectedPaths = selectedFiles;
											} else {
												url = "/download";

											}
											if (searchType == 'async') {
												$('div#AsyncDiv input[type="text"]').each(function() {
													if (!$(this).val()) {
														validate = false;
													}
												});
											} else if (searchType == 's3') {
												$('div#s3Div input[type="text"]').each(function() {
													if (!$(this).val()) {
														validate = false;
													}
												});
											} else if (searchType == 'drive') {
												$('div#driveDiv input[type="text"]').each(function() {
													if (!$(this).val()) {
														validate = false;
													}
												});
											} else if (searchType == 'cloud') {
												$('div#googleCloudDiv input[type="text"]').each(function() {
													if (!$(this).val()) {
														validate = false;
													}
												});
											}
											

											if (!validate) {
												$('.downloadErrorMsg').html("Enter the destination information.");
												$("#message").show();
											} else {
												$
														.ajax({
															type : "POST",
															url : url,
															contentType : 'application/json',
															data : JSON.stringify(d),
															beforeSend : function() {
																$("#spinner").show();
																$("#dimmer").show();
															},
															success : function(msg) {
																$("#spinner").hide();
																$("#dimmer").hide();
																console.log('SUCCESS: ', msg);

																if (msg
																		&& msg.message
																		&& msg.message
																				.indexOf("Download request is not successful:") != -1) {
																	$('.downloadErrorMsg').html(msg.message);
																	$("#message").show();
																	$('.downloadSuccessMsg').html("");
																	$("#successBlockDownload").hide();
																} else {
																	$('.downloadSuccessMsg').html(msg.message);
																	$("#successBlockDownload").show();
																	$('.downloadErrorMsg').html("");
																	$("#message").hide();
																}

															},
															error : function(e) {
																console.log('ERROR: ', e);
																$("#spinner").hide();
																$("#dimmer").hide();
																$('#downloadErrorMsg').html(e.message);
																$("#message").show();
															}
														});
											}
										} else {
											$('div#SyncDiv input[type="text"]').each(function() {
												if (!$(this).val()) {
													validate = false;
												}
											});
											if (!validate) {
												$('.downloadErrorMsg').html("Enter the destination information.");
												$("#message").show();
											} else {
												$('#downloadSyncForm').attr('action', '/downloadsync');
												$("#downloadSyncForm").submit();
											}
										}

									});
									
		$("#driveAuthlink").click(
				function(e) {
					var params = {
						type : $("#downloadType").val(),
						downloadFilePath : $("#selectedFilesList").val(),
						action : "drive",
						assetIdentifier : $("#assetIdentifier").val(),
						returnToStatus : $("#returnToStatus").val(),
						returnToSearch : $("#returnToSearch").val()
					}
	
					invokeAjax('/download', 'GET', params, postDownloadFunction, postFailureFunction,
							null, 'text');
				});
					
		$("#googleCloudAuthlink").click(
				function(e) {
					var params = {
						type : $("#downloadType").val(),
						downloadFilePath : $("#selectedFilesList").val(),
						action : "cloud",
						assetIdentifier : $("#assetIdentifier").val(),
						returnToStatus : $("#returnToStatus").val(),
						returnToSearch : $("#returnToSearch").val()
					}

					invokeAjax('/download', 'GET', params, postDownloadFunction, postFailureFunction,
							null, 'text');
				});

		$("#downloadGlobuslink").click(
				function(e) {
					var params = {
						type : $("#downloadType").val(),
						downloadFilePath : $("#selectedFilesList").val(),
						action : "Globus",
						assetIdentifier : $("#assetIdentifier").val(),
						returnToStatus : $("#returnToStatus").val(),
						returnToSearch : $("#returnToSearch").val()
					}

					invokeAjax('/download', 'GET', params, postDownloadFunction, postFailureFunction,
							null, 'text');
				});
				
		function postDownloadFunction(data, status) {
		    location.replace(data);
	    }
});


	