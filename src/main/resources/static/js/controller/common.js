$(document)
		.ready(
				function() {
					console.log("initialize dirty checking");
					
					//for mobile only
						$(".landing-tab").addClass('active');
					//
					
					$('form.dirty-check').areYouSure();

					 $(document).keypress(function(event){	
							var keycode = (event.keyCode ? event.keyCode : event.which);
							if(keycode == '13'){
								if($("#landingSearch").is(':visible')) {					
									 $("#landingSearch").click();
								} else if($("#landingSearchMobileButton").is(':visible')) {					
									 $("#landingSearchMobileButton").click();
								}			 
							}
						});

                    // Start To activate css for active navigation link
					var current = location.pathname;
					$('li button a').each(function(){
						var $this = $(this);
						// if the current path is like this link, make it active
						if($this.attr('href') == current){
							$this.parent().addClass('active-nav');
						}
					})
					$('li a').each(function(){
						var $this = $(this);
						// if the current path is like this link, make it active
						if($this.attr('href') == current){
							$this.children(":first").addClass('active-nav');
						}
					})
					// End To activate css for active navigation link

					// Auto-lowercase Email Username (Usernames are always
					// stored in lowercase)
					$('#username').blur(function() {
						$(this).val($(this).val().trim().toLowerCase());
					});

					$("#id_user_email").blur(function() {
						$(this).val($(this).val().trim().toLowerCase());
					});

					$("#txtResetPswdLink").blur(function() {
						$(this).val($(this).val().trim().toLowerCase());
					});

					$(".loginFieldsTextBox").keyup(function() {
						if ($(this).val()) {
							$(this).parent().find('.TextField-floatingLabel-qefpP').show();
						} else {
							$(this).parent().find('.TextField-floatingLabel-qefpP').hide();
						}
					});
					$(".keywordSearch").keyup(function(){
						if ($(this).val()) {
							$("#resetBtn").show();
							$("#resetBtnMobile").show();
							$("#resetBtnLanding").show();
							$("#resetBtnLandingForMobile").show();
						} else {
							$("#resetBtn").hide();
							$("#resetBtnMobile").hide();
							$("#resetBtnLanding").hide();
							$("#resetBtnLandingForMobile").hide();
						}
					});

					$(window).scroll(function() {
						if ($(this).scrollTop() >= 50) { // If page is
							// scrolled more
							// than 50px
							$('#return-to-top').show(200); // Fade in the arrow
						} else {
							$('#return-to-top').hide(200); // Else fade out the
							// arrow
						}
					});

					$('#return-to-top').click(function() { // When arrow is
						// clicked
						$('body,html').animate({
							scrollTop : 0
						// Scroll to top of body
						}, 500);
					});

					$('.simple-select2').select2({
						theme : 'bootstrap4',
						allowClear : false
					});

					$('.simple-select2-sm').select2({
						theme : 'bootstrap4',
						containerCssClass : ':all:',
						placeholder : "",
						allowClear : false,
					});

					$('[data-toggle="tooltip"]').tooltip();


					$("#searchBtn").click(function(e) {
						e.preventDefault();
						populateSearchCriteria();
					});
					$("#searchMobileBtn").click(function(e) {
						e.preventDefault();
						$("#mobileKeyworkSearchDiv").hide();
						$("#filterSectionDiv").hide();
						populateSearchCriteria();
					});
					
					$("#searchFiltersMobilebtn").click(function(e) {
						e.preventDefault();
						$("#mobileKeyworkSearchDiv").hide();
						$("#filterSectionDiv").hide();
						populateSearchCriteria();
					});
					

					$("#backtoFiltersMobilebtn").click(function(e){
						$("#searchResultsDiv").hide();
						$("#mobileKeyworkSearchDiv").show();
						$("#filterSectionDiv").show();
						$("#searchResultsMobileDiv").hide();
						showFirstFewFields();
					});
					
					$("#landingSearch").click(function(e) {
						location.replace('/searchTab?keyWord='+$("#attributeVallanding").val());
					});
					
					$("#landingSearchMobileButton").click(function(e) {
						// var txt = $(".mobileContainer").find("#attributeVallanding").val();
						location.replace('/searchTab?keyWord='+$("#attributeVallanding").val());
					});
					
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

					$("#btnRegister").click(function(e) {
						callRegisterFormValidation();

					});
					
					$("#btnSubmitEmail").click(function(e) {
						callContactUsFormValidation();

					});
					

					$("#loginButton").click(function(e) {
						validateUserLogin();

					});

					$("#btnforgotPassword").click(function(e) {
						validateForgotPassword();
					});
					
					$("#navigateToaboutTab").click(function(e){
						location.replace('/aboutTab');
					});

					$("#forgotPswdLink").click(function(e) {
						$("#forgotPasswordLightbox").find(".forgotPswdErrorMsg").html("");
						$("#forgotPasswordLightbox").find(".forgorPswdErrorBlock").hide();
						$("#forgotPasswordLightbox").find(".forgotPswdSuccessMsg").html("");
						$("#forgotPasswordLightbox").find(".forgorPswdSuccessBlock").hide();
						$("#forgotPasswordLightbox").find("#txtResetPswdLink").val("");
						$("#forgotPasswordLightbox").modal('show');

					});

					$("#btnSubmitPswdLink").click(function(e) {

						var params = {
							emailAddr : $('#forgotPasswordLightbox').find("#txtResetPswdLink").val()
						};
						invokeAjax('/resetPasswordLink', 'GET', params, postResetLinkFunction, null, null, 'text');
					});

					$("#logout").click(function(e) {
						invokeAjax('/logOut', 'POST', null, postLogOutFunction, null, null, 'text');
					});

					$("#register-tab").click(function(e) {
						$("#registrationTab").show();
						$("#loginSubTab").hide();
					});

					$("#returnToLoginForm").click(function(e) {
						$("#registrationTab").hide();
						$("#loginSubTab").show();
					});

					$("#btnUpdateProfile").click(function(e) {
						validateUpdateProfile();
					});

					$("#resetBtn").click(function(e) {
						$("#attributeVal").val("");
						$("#resetBtn").hide();
						populateSearchCriteria();
					});
					
					$("#resetBtnMobile").click(function(e) {
						$(this).parent().find("#attributeVal").val("");
						$("#resetBtnMobile").hide();
					});
					
					$("#resetBtnLanding").click(function(e) {
						$("#attributeVallanding").val("");
						$("#resetBtnLanding").hide();
					});
					
					$("#resetBtnLandingForMobile").click(function(e) {
						$(this).parent().find("#attributeVallanding").val("");
						$("#resetBtnLandingForMobile").hide();
					});

					
					$(".backToSearchBtn").click(function(e) {
						$("#searchFragmentDiv").show();
						$("#dataSetFragment").hide();
						$("#editCollectionFragment").hide();
					});

					$(".backToAssetDetailsBtn").click(function(e) {
						$("#assetDetailsFragment").show();
						$("#editCollectionFragment").hide();
					});

					$(".backtoAssetFromDwnldBtn").click(function(e) {
						var assetIdentifier = $("#assetIdentifier").val();
						location.replace('/assetDetails?assetIdentifier=' + assetIdentifier + '&&returnToSearch=true');
					});

					$("#backtoSearch").click(function(e) {
						location.replace("/searchTab?returnToSearch=true");
					});

					$(".backToStatusTabLink").click(function(e) {
						location.replace("/tasksTab");
					});

					$("#addAsets").click(function(e) {
						resetAssetsSelection();
					});

					$('body').on('click', 'a.button.closeBtn', function() {
						$(this).closest('div.popover').popover('hide');
					});

					$('body').on('click',function(e) {
						
						 $('[data-toggle=popover]').each(function() {
							 if (!$(this).is(e.target) && $(this).has(e.target).length === 0
								   && $('.popover').has(e.target).length === 0) {
								 (($(this).popover('hide').data('bs.popover') || {}).inState || {}).click = false;
							 }
						 });
						 
						 
					});

					$("#addMetaData").click(function(e) {
						addCollectionMetaDataRows();
					});

					$("#updateMetaData").click(function(e) {
						updateMetaDataCollection();
					});

					$("#registerCollectionBtn").click(function(e) {
						registerCollection();
					});

					$("#registerDataFileBtn").click(function(e) {
						registerDataFile();
					});

					$("#registerBulkAssets").click(function(e) {
						registerBulkAssets();
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

								invokeAjax('/download', 'GET', params, postGoogleDriveFunction, postFailureFunction,
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

								invokeAjax('/download', 'GET', params, postGoogleDriveFunction, postFailureFunction,
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

								invokeAjax('/download', 'GET', params, postGoogleDriveFunction, postFailureFunction,
										null, 'text');
							});

					function postGoogleDriveFunction(data, status) {
						location.replace(data);
					}
					
					function postLogOutFunction(data, status) {
						location.replace("/");
					}
					
					$(".addNewMetaDataForDataFiles").click(function(e) {
						addNewMetaDataRowsForDataFile($(this));
					});

					$("#updatePermissions").click(function(e) {
						editPermissionsOpenModal();
					});

					$("#btnUpdatePermissions").click(function(e) {
						updatePermissionsFunction();
					});
					$("#btnUpdateAccessGroup").click(function(e) {
						updateAccessGroupsFunction();
					});
					
					//prepopulate the type of inquiry dropdown when user is trying to unsubscribe
					var typeOfInquiry= $("#typeOfInquiry").val();
					if(typeOfInquiry) {
						
							var $select = $("#inquiryList");
							$select.select2().val(typeOfInquiry).trigger('change');
					}
				

					$('.asset-attributes').click(function() {
						if ($(this).hasClass('collapsed')) {
							$(this).find('img').attr('src', '/images/arrow.collapse-open.svg')
						} else {
							$(this).find('img').attr('src', '/images/arrow.collapse.svg')
						}
					});

					$(".select2-selection").removeAttr("role");
					$(".select2-search__field").removeAttr("role");
					$(".select2-search__field").attr("aria-label", "textbox");
					$(".select2-search__field").attr("type", "textbox");

					$(document).ajaxStop(function() {
						console.log("Last ajax call completed");
						$(".select2-selection").removeAttr("role");
						$(".select2-search__field").removeAttr("role");
						$(".select2-search__field").attr("aria-label", "textbox");
						$(".select2-search__field").attr("type", "textbox");
					});

				});


/* function on change of metadata with select dropdown values */
function onChangeForMetadata(form, isUploadPage, isValid, table, selectId) {
	if(isValid == true) {
	    var assetType = $("#assetType").val();
	    
	    if(!assetType) {
	     assetType = $("#assetTypeSelect").val();
	    }
		var value = selectId.value;
		var metadataAttr = selectId.id;
		var tableId  = table.id;

		if(value != 'Select') {
			var params= {collectionType:'Asset',controllerAttribute:metadataAttr};
			
			$.ajax({
				"url": "/addCollection/getControlAttributesList",
				"type": "GET",
				data: params,
				beforeSend: function() {
					$("#spinner").show();
					$("#dimmer").show();
				},
				success: function(msg) {
				   $("#spinner").hide();
				   $("#dimmer").hide();
				   postSuccessOnChangeIsReferenceDataset(form, msg, tableId, isUploadPage, value);
				},
				error: function(e) {
					console.log('ERROR: ', e);
					$("#spinner").hide();
					$("#dimmer").hide();								
				}
			})
		}
	}
	
}
function postSuccessOnChangeIsReferenceDataset(form, data, tableId, isUploadPage, controlAttributeValue) {

	var found = false;
    if(data.length != 0) {
    
            var controlAttributeNames = []; 	
		 		
		 	$("#"+tableId+ " tbody tr").each(function() {
				var displayName = $(this).find('td').eq(0).text().trim();
				var val = data.filter(function(x){ return x.displayName == displayName });
				if(val.length != 0) {
				 controlAttributeNames.push(val[0]);
				}
			});
			
		  $.each(data, function(key, value) {
		  
				
		    if(value.controllerAttrValue == controlAttributeValue && controlAttributeNames.length == 0) {
		    
		       var infoHtml = "";
			   if(value.description) {
					
				  infoHtml = '<i class="fas fa-question-circle" data-toggle="tooltip"'
													+ 'data-placement="right" title="'
													+ value.description
													+ '"></i>';
			   }  
					
			// check if metadata is visible only for review commitee member
			var isShow = false;
			
			if((value.isVisibleForReviewCommiteeMember == true && isReviewCommiteeMember) || (value.isVisibleForReviewCommiteeMember != true)) {
			  isShow = true;
			}
			
			if(isShow == true && value.isVisible != true && value.validValues != null) {
				   
				if(tableId == 'assetBulkMetadataTable') {
					var width = 'width:99%;';
				} else {
					var width = 'width:70%;';
				}
				
			if ((isUploadPage && value.isVisibleOnUplaodPage != false) || !isUploadPage ) {
			
				if(value.attrName  == 'applicable_model_paths') {
					   
				$("#"+tableId+" tbody").append('<tr><td>' +  value.displayName + '&nbsp;&nbsp;' + infoHtml + '</td><td>'+
			       '<select class="simple-select2" multiple="multiple" placeholder="Required" is_mandatory="'+value.mandatory+'" id="'+value.attrName+'" name="zAttrStr_'+value.attrName+'" ' +
			       'style="' + width +'""></select></td></tr>');
				   
				  var $select = $("#"+value.attrName);
				   
				} else {
					$("#"+tableId+" tbody").append('<tr><td>' +  value.displayName + '&nbsp;&nbsp;' + infoHtml + '</td><td>'+
				    '<select class="simple-select2" is_mandatory="'+value.mandatory+'" style="' + width +'" id="'+value.attrName+'" name="zAttrStr_'+value.attrName+'" value="'+value.attrValue+'"></select></td></tr>');
					
				  var $select = $("#"+value.attrName);
		    	  if(value.attrValue && value.attrValue != 'None'){
		    	 	$select.append($('<option></option>').attr('value', value.attrValue).text(value.attrValue));
		    	  } else {
		    	    $select.append($('<option></option>').attr('value', 'Select').text('Select'));
		    	  }
				} 
	    	  
		    	  for (var i = 0; i < value.validValues.length; i++) {
		    		   $select.append($('<option></option>').attr('value', value.validValues[i].key).text(value.validValues[i].value));
	              }
	               
		    	  if(value.attrValue != null) {
			            var attrValModifiedList = value.attrValue.split(',');
			    		$select.select2().val(attrValModifiedList);
			    	} else {
			    		$select.select2();
			    	}
			  }
  	
		   } else if(isShow == true && value.isVisible != true) {
					   			   
			    var placeholder = value.mandatory == true ? 'Required' : "";
			    var attrVal = value.attrValue != null ? value.attrValue : "";
			    var inputType = "";
			    if(value.attrName == 'curation_date') {
			    	inputType = "date";
			    } else {
			    	inputType = "text";
			    }
			    
				   if(tableId == 'assetBulkMetadataTable') {
					   $("#"+tableId+" tbody").append('<tr><td>' +  value.displayName + '&nbsp;&nbsp;' + infoHtml + '</td><td>'+
					        	'<input type = "' + inputType + '" is_mandatory = "'+ value.mandatory+'"  value = "' + attrVal + '" class="bulkAssetTextbox" placeholder="'+placeholder+'" aria-label="value of meta data" name="zAttrStr_'+value.attrName+'"' +
					        	'></td></tr>');
					} else {
						$("#"+tableId+" tbody").append('<tr><td>' +  value.displayName + '&nbsp;&nbsp;' + infoHtml + '</td><td>'+
					        	'<input type = "' + inputType + '" is_mandatory = "' + value.mandatory+'" value = "' + attrVal + '" placeholder="'+placeholder+'" aria-label="value of meta data" name="zAttrStr_'+value.attrName+'"' +
					        	'style="width:70%;"></td></tr>');
					}
			}	    
			
		    } else if(value.controllerAttrValue != controlAttributeValue && controlAttributeNames.length != 0) {
					/* the removed attributes value should be set to empty */
					$("#"+tableId+ " tbody tr").each(function() {
						var metadataName = $(this).find('td').eq(0).text().trim();
						var controlAttributeDisplayNames = Object.values(controlAttributeNames).map(item => item.displayName);
						var removedMetadataName = $(this).find('td').eq(1).children().attr('name');
						
						if ($.inArray(metadataName, controlAttributeDisplayNames) !== -1) {
							$('<input>').attr({type: 'hidden',name: removedMetadataName,value:""}).appendTo(form);
							$(this).remove();
						}
					});
				
		    }
		  
       });
    }
	
}

$(document).on('change','.checkboxMultipleDwnlodPaths', function() {
	var selectedPaths = [];
	$(".checkboxMultipleDwnlodPaths").each(function(e){
		if($(this).is(':checked')) {
			selectedPaths.push($(this).val());
		}
	});
	$("#selectedFilesList").val(selectedPaths.join(","));
});

$(document).on('change', '#editPublicAccess', function() {
	if ($(this).is(":checked")) {
		$("#updateAccessPermissionsModal").find("#updateAccessGroupsList").next(".select2-container").hide();
		$("#updateAccessPermissionsModal").find("#updateAccessGroupsList").val("").trigger('change');

	} else {
		$("#updateAccessPermissionsModal").find("#updateAccessGroupsList").next(".select2-container").show();
	}

});

$(document).on('click', '#pickerUploadLink', function() {
	loadUploadPicker();
});
$(document).on('click', '#pickerLink', function() {
	loadDownloadPicker();
});

$(document).on('click', '.clearMetadata', function() {
	$(this).parent().find("input[type='text']").val("");
});


function deleteFolderCollection(name, path, postSuccessDeleteFunction) {

  				bootbox.confirm({
				message : "Are you sure you want to delete " + name + "?",
				buttons : {
					confirm : {
						label : 'Yes',
						className : 'btn-success'
					},
					cancel : {
						label : 'No',
						className : 'btn-danger'
					}
				},
				callback : function(result) {
					if (result == true) {
						var params = {
							collPath : path,
							collectionType :'Folder'
						};

						invokeAjax('/deleteCollection', 'POST', params, postSuccessDeleteFunction,
								postFailureDeleteCollectionFunction,
								'application/x-www-form-urlencoded; charset=UTF-8', 'text');

					}
				}
			});
}

// Funtion when clicking on checkboxes for search sidebar filters

$(document).on('change', '.filteritem', function () {

	  // Get the parent div of the clicked checkbox
	  var parentDiv = $(this).closest(".filterGroupDiv");
	
	  if ($(this).is(':checked')) {
	
	     // Move the parent div to the top of the list
	     $(this).closest('.dataDivCollapse').prepend(parentDiv);
	     $(this).closest('label.showMorefields').addClass('filter_checked');
	
	  } else {
	     // place the unchecked filter item to its original position
	     resetFilterItemToOriginalPosition($(this), parentDiv);
	  }

  	var attrName = $(this).parent().parent().attr('id');

  	populateSearchCriteria();

	  // based on child selection, search at parent level and check the
	  // parent checkbox

	 $('.filterComponentDiv').find('.attributeLabel').each(function (e) {
	  filterPrevAndNextElemets($(this), attrName);
	 });

   });