$(document)
		.ready(
				function() {
					console.log("initialize dirty checking");
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
							$(".resetBtnSpan").show();
							$("#resetBtnMobile").show();
						} else {
							$(".resetBtnSpan").hide();
							$("#resetBtnMobile").hide();
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
						populateSearchCriteria('simpleSearch');
					});
					$("#searchMobileBtn").click(function(e) {
						e.preventDefault();
						$("#mobileKeyworkSearchDiv").hide();
						$("#filterSectionDiv").hide();
						populateSearchCriteria('simpleSearch');
					});
					

					$("#backtoFiltersMobilebtn").click(function(e){
						$("#searchResultsDiv").hide();
						$("#mobileKeyworkSearchDiv").show();
						$("#filterSectionDiv").show();
						$("#searchResultsMobileDiv").hide();
					});
					
					$("#landingSearch").click(function(e) {
						location.replace('/searchTab?keyWord='+$("#attributeVallanding").val());
					});
					
					$("#landingSearchMobileButton").click(function(e) {
						var txt = $(".mobileContainer").find("#attributeVallanding").val();
						location.replace('/searchTab?keyWord='+txt);
					});
					
					$("#downloadSelected").click(
							function(e) {
								var selectedPaths = [];
								$("#searchResultTable tbody input[type=checkbox]:checked").each(function() {
									selectedPaths.push($(this).attr('id'));
								});
								if (selectedPaths.length == 0) {
									$("#searchResultTable tbody input[type=radio]:checked").each(function() {
										selectedPaths.push($(this).attr('id'));
									});
								}

								if (selectedPaths.length == 1) {
									location.replace('/downloadTab?selectedPaths=' + selectedPaths
											+ '&&downloadAsyncType=collection&&returnToSearch=true');
								} else {
									$("#downloadType").val("collectionfiles");
									location.replace('/downloadTab?selectedPaths=' + selectedPaths
											+ '&&downloadAsyncType=collectionfiles&&returnToSearch=true');
								}

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
							$("#syncRadioSet").show();
							$(".selectedFilesListDisplay").append("<p>" + selectedPathsString + "</p>");
							$(".selectedFilesDiv").show();
							$("#destinationPathId").val(selectedPathsString);
							$("#drivePath").val(downloadFileName);
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
										} else if (searchType == 's3' || searchType == 'async' || searchType == 'drive'
												|| selectedFiles) {
											d.bucketName = $("#downloadBucketName").val();
											d.s3Path = $("#downloadS3Path").val();
											d.accessKey = $("#downloadAccessKey").val();
											d.secretKey = $("#downloadSecretKey").val();
											d.region = $("#downloadRegion").val();
											d.endPointName = $("#endPointName").val();
											d.endPointLocation = $("#endPointLocation").val();
											d.drivePath = $("#drivePath").val();

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
						populateSearchCriteria('simpleSearch');
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
									action : "Drive",
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


/* function on change of reference dataset value */
function onChangeForMetadata(form,isValid, table,selectId) {
	if(isValid == true) {
		var value = selectId.value;
		var metadataAttr = selectId.id;
		var tableId  = table.id;
		var controllerAttributeList = [];
		var controllerValueList = [];
		var path = $("#path").val();
		
		controllerAttributeList.push(metadataAttr);
		controllerAttributeList.push("asset_type");
		controllerValueList.push(value);
		controllerValueList.push("Dataset");
		if(value != 'Select') {
			var params= {selectedPath:path , collectionType:'Asset',controllerValue:controllerValueList.join(),refresh:false,controllerAttribute:controllerAttributeList.join()};
			
			$.ajax({
				"url": "/addCollection",
				"type": "GET",
				data: params,
				beforeSend: function() {
					$("#spinner").show();
					$("#dimmer").show();
				},
				success: function(msg) {
				   $("#spinner").hide();
				   $("#dimmer").hide();
				   postSuccessOnChangeIsReferenceDataset(form,msg,tableId);
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
function postSuccessOnChangeIsReferenceDataset(form,data, tableId) {

	var displayNames = [];
	var found = false;
	
		$("#"+tableId+ " tbody tr").each(function() {
			var displayName = $(this).find('td').eq(0).text().trim();
			displayNames.push(displayName);
			var x = data.filter(function(x){ return x.displayName == displayName });
			
			/*
			 * for asset bulk creation, do not remove the metadata Asset group
			 * Identifier
			 */
			
			if(x.length == 0 && displayName !='Asset Group Identifier') {
				/* the removed attributes value should be set to empty */
				var removedMetadataName = $(this).find('td').eq(1).children().attr('name');
				$('<input>').attr({type: 'hidden',name: removedMetadataName,value:""}).appendTo(form);
				$(this).remove();
			}
		});
		
	  var newElements = data.filter(x => !displayNames.includes(x.displayName));
	
	  /* new elements added from new conditional attribute */
	  if(newElements.length != 0) {
		  $.each(newElements, function(key, value) {
			  
			if(value.isVisible != true && value.validValues != null && value.attrName !='asset_type') {
				   
				if(tableId == 'assetBulkMetadataTable') {
					var width = 'width:99%;';
				} else {
					var width = 'width:70%;';
				}
				
				if(value.attrName  == 'applicable_model_paths') {
					   
				$("#"+tableId+" tbody").append('<tr><td>' +  value.displayName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
			       'data-placement="right" title="'+value.description+'"></i></td><td>'+
			       '<select class="simple-select2" multiple="multiple" placeholder="Required" is_mandatory="'+value.mandatory+'" id="'+value.attrName+'" name="zAttrStr_'+value.attrName+'" ' +
			       'style="' + width +'""></select></td></tr>');
				   
				  var $select = $("#"+value.attrName);
				   
				} else {
					$("#"+tableId+" tbody").append('<tr><td>' +  value.displayName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
				    'data-placement="right" title="'+value.description+'"></i></td><td>'+
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
			    		$select.select2().val(attrValModifiedList).trigger('change');
			    	} else {
			    		$select.select2().trigger('change');
			    	}
  	
		   } else if(value.isVisible != true && value.attrName.indexOf("access_group") == -1) {
					   			   
			    var placeholder = value.mandatory == true ? 'Required' : "";
			    var attrVal = value.attrValue != null ? value.attrValue : "";
			    
				   if(tableId == 'assetBulkMetadataTable' && value.attrName != 'asset_name' && value.attrName !='asset_type' &&
					        value.attrName !='asset_identifier') {
					   $("#"+tableId+" tbody").append('<tr><td>' +  value.displayName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
					        	'data-placement="right" title="'+value.description+'"></i></td><td>'+
					        	'<input type="text" is_mandatory="'+value.mandatory+'"  value = "' + attrVal + '" class="bulkAssetTextbox" placeholder="'+placeholder+'" aria-label="value of meta data" name="zAttrStr_'+value.attrName+'"' +
					        	'></td></tr>');
					} else if(tableId != 'assetBulkMetadataTable') {
						$("#"+tableId+" tbody").append('<tr><td>' +  value.displayName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
					        	'data-placement="right" title="'+value.description+'"></i></td><td>'+
					        	'<input type="text" is_mandatory="'+value.mandatory+'" value = "' + attrVal + '" placeholder="'+placeholder+'" aria-label="value of meta data" name="zAttrStr_'+value.attrName+'"' +
					        	'style="width:70%;"></td></tr>');
					}
				    
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

$(document).on('click', '.dataTargetCollapse', function() {
	if ($(this).parent().parent().find('div.dataDivCollapse').is(":visible")) {
		$(this).parent().css('margin-bottom', '-1px');
		$(this).parent().parent().find('div.dataDivCollapse').css('display', 'none');
		$(this).parent().parent().find('.filterSearchBox').css('display', 'none');
		$(this).attr('src', '/images/AccordionUp.svg');
		$(this).parent().parent().find(".css-17rpx5x").hide();
		$(this).parent().parent().find(".css-17rpx5xLess").hide();
	} else {
		$(this).parent().css('margin-bottom', '15px');
		$(this).parent().parent().find('div.dataDivCollapse').css('display', 'block');
		$(this).parent().parent().find('.filterSearchBox').css('display', 'none');
		$(this).attr('src', '/images/AccordionDown.svg');
		$(this).parent().parent().find('.css-17rpx5x').show();
		$(this).parent().parent().find('.showMore').show();
		showFirstFewFields($(this).parent().parent(), 'Less');
	}
});

$(document).on('click', '.searchCheckBoxlist', function() {
	if ($(this).parent().parent().find('.filterSearchBox').is(":visible")) {
		$(this).parent().parent().find('.filterSearchBox').css('display', 'none');
	} else {
		$(this).parent().parent().find('.filterSearchBox').css('display', 'block');
	}
});

$(document).on('keyup', '.filterSearchBox', function() {
	var query = $(this).val().toLowerCase();
	$(this).parent().find('.filteritem').each(function(i, elem) {
		var x = $(this).val().toLowerCase();
		if (x.indexOf(query) != -1) {
			$(this).parent().parent().show();

		} else {
			$(this).parent().parent().hide();
		}
	});
	
	showFirstFewFields($(this).parent(),'searchFilter');
});

$(document).on('click', '#clearFilters', function() {
	$(".filterGroupDiv").each(function(e) {
		$(this).show();
		$(this).find('.showMorefields').show();
		$(this).find('.filteritem').prop('checked', false);
		$(this).find('span').css('color', '#212529');
	});
	showFirstFewFields();
	populateSearchCriteria('simpleSearch');
});

$(document).on('click', '.clearMetadata', function() {
	$(this).parent().find("input[type='text']").val("");
});

$(document).on('change', '.filteritem', function() {

	if ($(this).is(':checked')) {
		$(this).parent().find('span').css('color', '#2E76ED');
	} else {
		$(this).parent().find('span').css('color', '#212529');
	}
	var attrName = $(this).parent().parent().attr('id');

	populateSearchCriteria('simpleSearch');
	
	// based on child selection, search at parent level and check the
	// parent checkbox
	$(this).closest('.filterComponentDiv').prevAll().find('.attributeLabel').each(function(e) {
		filterPrev($(this), attrName);
	});

	// always filter the metadata on the children level
	// do not remove parent based on child selection
	$(this).closest('.filterComponentDiv').nextAll().find('.attributeLabel').each(function(e) {
		filterNext($(this), attrName);
	});

	
});

$(document).on('click', '.showMore', function() {

	$(this).parent().hide();
	$(this).parent().parent().find(".css-17rpx5xLess").show();
	showFirstFewFields($(this).parent().parent(), 'More');
});

$(document).on('click', '.showLess', function() {

	$(this).parent().hide();
	$(this).parent().parent().find(".css-17rpx5x").show();
	$(this).parent().parent().find(".showMore").show();
	showFirstFewFields($(this).parent().parent(), 'Less');
});

function filterNext($this, attributeTypeName) {

	var attributeName = $this.find('label').text();

	var rowId = 1;
	var d = {};
	var attrNames = [];
	var attrValues = [];
	var isExcludeParentMetadata = [];
	var rowIds = [];
	var operators = [];

	// filter a list based on the parent level selection
	$this.closest('.filterComponentDiv').prevAll().find(".filteritem:checked").each(function() {
		var attrName = $(this).parent().parent().attr('id');
		var attrVal = $(this).val();
		if (attrName != attributeName) {
			attrNames.push(attrName);
			attrValues.push(attrVal);
			rowIds.push(rowId);
			isExcludeParentMetadata.push(false);
			operators.push("EQUAL");
			rowId = rowId + 1;
		}
	});

	d.attrName = attrNames.join();
	d.attrValuesString = attrValues.join('@@');
	d.isExcludeParentMetadata = isExcludeParentMetadata.join();
	d.rowId = rowIds.join();
	d.operator = operators.join();
	d.searchName = attributeName;

	$.ajax({
		url : '/getFilterList',
		type : 'GET',
		contentType : 'application/json',
		dataType : 'text',
		data : d,
		success : function(data, status) {
			var list = JSON.parse(data);
			var len = list.length;

			$this.parent().find('.filterGroupDiv').each(function(e) {
				var val = $(this).find('.filteritem').val();
				if (list.indexOf(val) != -1) {
					$(this).show();
					$(this).find('.showMorefields').show();
				} else {
					$(this).hide();
					$(this).find('.filteritem').prop("checked", false);
					$(this).find('span').css('color', '#212529');
				}
			});

			if (len && len > 4) {
				$this.parent().find('.css-17rpx5x').show();
				$this.parent().find('.showMore').show();
				$this.parent().find('.css-17rpx5xLess').hide();
				showFirstFewFields($this.parent(), 'Less');
			} else {
				$this.parent().find('.css-17rpx5xLess').hide();
				$this.parent().find('.css-17rpx5x').hide();
			}

		},
		error : function(data, status, error) {
			console.log("===> status: ", status);
			console.log("===> error: ", error);
			console.log("===> data: ", data);
		}
});
}

function filterPrev($this, attributeTypeName) {
	var attributeName = $this.find('label').text();

	var rowId = 1;
	var d = {};
	var attrNames = [];
	var attrValues = [];
	var isExcludeParentMetadata = [];
	var rowIds = [];
	var operators = [];
	var url;

	// filter a list based on the parent level selection
	$this.closest('.filterComponentDiv').nextAll().find(".filteritem:checked").each(function() {
		var attrName = $(this).parent().parent().attr('id');
		var attrVal = $(this).val();
		if (attrName != attributeName) {
			attrNames.push(attrName);
			attrValues.push(attrVal);
			rowIds.push(rowId);
			isExcludeParentMetadata.push(false);
			operators.push("EQUAL");
			rowId = rowId + 1;
		}
	});

	d.attrName = attrNames.join();
	d.attrValuesString = attrValues.join('@@');
	d.isExcludeParentMetadata = isExcludeParentMetadata.join();
	d.rowId = rowIds.join();
	d.operator = operators.join();
	d.searchName = attributeName;
	if(attributeName == 'Program Name' || attributeName == 'Study Name') {
		url = '/getFilterList?retrieveParent=true';
	} else  {
		url = '/getFilterList';
	}

	$.ajax({
		url : url,
		type : 'GET',
		contentType : 'application/json',
		dataType : 'text',
		data : d,
		beforeSend : function() {
			$("#spinner").show();
			$("#dimmer").show();
		},
		success : function(data, status) {
			var list = JSON.parse(data);
			var len = list.length;

			$this.parent().find('.filterGroupDiv').each(function(e) {
				var val = $(this).find('.filteritem').val();
				if (list.indexOf(val) != -1) {
					$(this).show();
					$(this).find('.showMorefields').show();
				} else {
					$(this).hide();
					$(this).find('.filteritem').prop("checked", false);
					$(this).find('span').css('color', '#212529');
				}
			});

			if (len && len > 4) {
				$this.parent().find('.css-17rpx5x').show();
				$this.parent().find('.showMore').show();
				$this.parent().find('.css-17rpx5xLess').hide();
				showFirstFewFields($this.parent(), 'Less');
			} else {
				$this.parent().find('.css-17rpx5xLess').hide();
				$this.parent().find('.css-17rpx5x').hide();
			}
		},
		error : function(data, status, error) {
			console.log("===> status: ", status);
			console.log("===> error: ", error);
			console.log("===> data: ", data);
		}
	});
}

function showFirstFewFields($this, oper) {
	if ($this) {
		if (oper == 'More') {
			$this.find('.filterGroupDiv:visible').each(function(index) {
				$(this).find('.showMorefields').show();

			});
		} else if (oper == 'Less') {
			var len = $this.find('.filterGroupDiv:visible').length;
			if (len > 4) {
				var modifiedSize = len - 4;
				$this.find('.filterGroupDiv:visible').each(function(index) {
					if (index > 3) {
						$(this).find('.showMorefields').hide();
					} else {
						$(this).find('.showMorefields').show();
					}
				});
				$this.find(".showMore").text(modifiedSize + " More ..");
			} else {
				$this.find('.css-17rpx5x').hide();				
			}
		} else if( oper =='searchFilter') {
			var len = $this.find('.filterGroupDiv:visible').length;
			if (len > 4) {
				var modifiedSize = len - 4;
				$this.find('.css-17rpx5x').show();
				$this.find(".showMore").text(modifiedSize + " More ..");
			} else {
				$this.find('.css-17rpx5x').hide();				
			}
		}

	} else {
		$(".dataDivCollapse").each(function(e) {
			var len = $(this).find('.filterGroupDiv:visible').length;
			var modifiedSize = len - 4;
			if (len > 4) {
				$(this).parent().find('.showMore').text(modifiedSize + " More..");
				$(this).parent().find('.showMore').show();
				$(this).parent().find('.css-17rpx5x').show();
				$(this).parent().find('.css-17rpx5xLess').hide();
				$(this).find('.filterGroupDiv:visible').each(function(index) {
					if (index > 3) {
						$(this).find(".showMorefields").hide();
					} else {
						$(this).find(".showMorefields").show();
					}

				});
			} else {
				$(this).parent().find('.css-17rpx5x').hide();
				$(this).parent().find('.css-17rpx5xLess').hide();
			}

		});
	}
}