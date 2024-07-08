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
					var width = 'width:95%;';
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
			    	inputType = "search";
			    }
			    
				   if(tableId == 'assetBulkMetadataTable') {
					   $("#"+tableId+" tbody").append('<tr><td>' +  value.displayName + '&nbsp;&nbsp;' + infoHtml + '</td><td>'+
					        	'<input type = "' + inputType + '" is_mandatory = "'+ value.mandatory+'"  value = "' + attrVal + '" class="bulkAssetTextbox" placeholder="'+placeholder+'" aria-label="value of meta data" name="zAttrStr_'+value.attrName+'"' +
					        	'></td></tr>');
					} else {
						$("#"+tableId+" tbody").append('<tr><td>' +  value.displayName + '&nbsp;&nbsp;' + infoHtml + '</td><td>'+
					        	'<input type = "' + inputType + '" is_mandatory = "' + value.mandatory+'" value = "' + attrVal + '" placeholder="'+placeholder+'" aria-label="value of meta data" name="zAttrStr_'+value.attrName+'"' +
					        	'style="width: 95%; border-radius: 8px;border: 1px solid #6B7294;height: 36px;""></td></tr>');
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