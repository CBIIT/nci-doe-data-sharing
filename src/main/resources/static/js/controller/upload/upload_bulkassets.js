$(document).ready(
	function () {

		$("#assetSelectionGlobusButton").click(function (e) {
			var d = {};
			d.programPath = $("#programList").val();
			d.studyPath = $("#studyList").val();
			d.uploadType = "assetBulkUpload";
			invokeAjax('/upload', 'GET', d, postUploadGlobusFunction, postFailureFunction, null, 'text');
		});
		

		$('#registerAssetSelect').on('change', function () {
			var selectedOption = $("#registerAssetSelect option:selected").text();
			$("#assetUploadDiv").hide();

			if (selectedOption !== 'Select Asset Type') {
				$("#assetType").val(selectedOption);
				$("#uploadSectionDiv").hide();
				$("#uploadHeader").hide();

				$("#registerCollectionForm").show()
				createCollectionDiv('Asset', 'Study', 'studyList');
				$("#registerAssetPicker").show();
			}

		});

		$('#uploadAssetSelect').on('change', function () {
		
			var selectedOption = $("#uploadAssetSelect option:selected").text();

			if (selectedOption == 'Upload Assets from Globus Endpoint') {
				$("#registerCollectionForm").hide();
				$("#assetUploadDiv").show();
				$("#assetUploadDiv").addClass('show');
				$("#globusDetailsDiv").show();
				$("#S3DetailsDiv").hide();
				$("#googleCloudDiv").hide();

				var folderLength = $("#assetSelectedFolders ul li").length ;
				var selectedAssetFiles = $("#assetSelectedFiles ul li").length;

				
				resetAssetBulkUploadChoiceOptions();
				var globusEndpointId = $("#globusEndpointId").val();

				if (!globusEndpointId) {
					$("#bulkAssetOptionsDiv").hide();
				} else {
					$("#bulkAssetOptionsDiv").show();
					if (folderLength && folderLength > 1) {

						$("#multipleFoldersUploadDiv").hide();
						$("#uploadCsvFile").show();
						$("#formAssetSelection").hide();
						$("#addMetadataDiv").hide();
					} else {
						$("#multipleFoldersUploadDiv").show();
						$("#uploadCsvFile").hide();
					}

					if(folderLength == 0  || selectedAssetFiles != 0 ){
						$(".registerErrorMsg").html("Select Only Folders from Globus");
		                $(".registerMsgErrorBlock").show();
						$("#registerBulkAssets").hide();
						$("#assetGlobusEndpointId").hide();
					}
				}
			} else if (selectedOption == 'Upload Assets from AWS S3') {
				$("#registerCollectionForm").hide();

				$("#assetUploadDiv").show();
				$("#assetUploadDiv").addClass('show');
				$("#globusDetailsDiv").hide();
				$("#googleCloudDiv").hide();
				$("#registerBulkAssets").prop("disabled", false);
				$("#S3DetailsDiv").show();
				$("#bulkAssetOptionsDiv").show();
				$("#uploadCsvFile").hide();
				$("#multipleFoldersUploadDiv").show();
				$("#registerBulkAssets").show()
				resetAssetBulkUploadChoiceOptions();

			} else if (selectedOption == 'Upload Assets from Google Cloud') {
				$("#registerCollectionForm").hide();
				$("#assetUploadDiv").show();
				$("#assetUploadDiv").addClass('show');
				$("#S3DetailsDiv").hide();
				$("#globusDetailsDiv").hide();
				$("#googleCloudDiv").show();
				var gcCloudAuthorized = $("#gcCloudAuthorized").val();

				// Add logic after GC login
				if (gcCloudAuthorized === "true") {
					$("#bulkAssetOptionsDiv").show();
					$("#uploadCsvFile").hide();
				    $("#multipleFoldersUploadDiv").show();
				    $("#registerBulkAssets").prop("disabled", false);
				}
				else {
					$("#bulkAssetOptionsDiv").hide();
					$("#uploadCsvFile").hide();
					$("#multipleFoldersUploadDiv").hide();
				}
			}

		});



		$('input[type=radio][name=registerAssetRadio]').change(function () {
			if (this.value == 'Create Asset') {
				$('#registerAssetSelect').prop('disabled', false);
				$('#uploadAssetSelect').prop('disabled', true);


				// Reset uploadAssetSelect selection 
				$('#uploadAssetSelect').prop('selectedIndex', 0);
				$('#uploadAssetSelect').val('').trigger('change');
			    $("#assetUploadDiv").hide();


			} else if (this.value == 'Upload Asset') {
				$('#registerAssetSelect').prop('disabled', true);
				$('#uploadAssetSelect').prop('disabled', false);
				// Reset registerAssetSelect selection 
				$('#registerAssetSelect').prop('selectedIndex', 0);
				$('#registerAssetSelect').val('').trigger('change');
				$("#registerCollectionForm").hide()

			}

		});

		$("#authorizeGoogleCloud").click(function (e) {
			var d = {};
			d.programPath = $("#programList").val();
			d.studyPath = $("#studyList").val();
			d.uploadType = "assetBulkUpload";
			d.action = "cloud";
			invokeAjax('/upload', 'GET', d, postUploadGlobusFunction, postFailureFunction, null, 'text');

		});
		

	});

function retrieveAssetTypeDiv(data) {
	if (data.value != 'Select') {
		var params1 = {
			collectionType: 'Asset',
			controllerValue: data.value,
			controllerAttribute: 'asset_type'
		};
		invokeAjax('/addCollection', 'GET', params1, constructAssetTypeBulkDiv, null, null, null);
	} else {
		$("#addMetadataDiv").hide();
		$("#assetBulkMetadataTable tbody").html("");
	}
}

function constructAssetTypeBulkDiv(data, status) {
	$("#addMetadataDiv").show();
	$("#assetBulkMetadataTable tbody").html("");
	$
		.each(
			data,
			function (key, value) {

				var placeholderValue = value.mandatory == true ? 'Required' : "";
				var infoHtml = "";
				if (value.description) {
					infoHtml = '<img src="images/infoIcon.svg" class="icon" data-toggle="tooltip"'
						+ 'data-placement="right" title="'
						+ value.description
						+ '"></img>';
				} 
				 if (value.attrName == 'access_group') {

						$("#assetBulkMetadataTable tbody")
							.append(
								'<tr><td>'
								+ value.displayName
								+ '&nbsp;&nbsp;' + infoHtml + '</td><td>'
								+ '<div class="mt-2"><select class="simple-select2" is_mandatory="' + value.mandatory + '" multiple="multiple" id="bulkAccessGrpSelect" name="zAttrStr_'
								+ value.attrName
								+ '"'
								+ 'style="width: 95%; border-radius: 8px;border: 1px solid #6B7294;height: 36px;"></select></div>'
								+ '</td></tr>');
						
						loadJsonData('/metaDataPermissionsList', $("#bulkAccessGrpSelect"), false, null, loadDefaultBulkAccessGrp,
							null, "key", "value");

				} else if (value.validValues != null && value.attrName != 'asset_type' && value.isVisibleOnUplaodPage != false) {

					$("#assetBulkMetadataTable tbody")
						.append(
							'<tr><td>'
							+ value.displayName
							+ '&nbsp;&nbsp;' + infoHtml + '</td><td>'
							+ '<select class="simple-select2" is_mandatory="'
							+ value.mandatory
							+ '" style="width:99%;" onChange="onChangeForMetadata(registerBulkAssetForm, true,'
							+ value.controllerAttribute + ',assetBulkMetadataTable, '
							+ value.attrName + ');"' + 'id="' + value.attrName
							+ '" name="zAttrStr_' + value.attrName + '" value="'
							+ value.attrValue + '"></select></td></tr>');

					var $select = $("#" + value.attrName);
					if (value.attrValue) {
						$select.append($('<option></option>').attr('value', value.attrValue).text(
							value.attrValue));
					} else if (!value.defaultValue) {
						$select.append($('<option></option>').attr('value', 'Select').text('Select'));
					}

					for (var i = 0; i < value.validValues.length; i++) {
						if ($select.find("option[value='" + value.validValues[i].key + "']").length == 0) {
							$select.append($('<option></option>').attr('value', value.validValues[i].key).text(
								value.validValues[i].value));
						}
					}

					if (value.defaultValue) {
						$select.select2().val(value.defaultValue);
					} else {
						$select.select2();
					}

				} else if (value.attrName && value.attrName != 'asset_type'
					       && value.isVisibleOnUplaodPage != false) {
					if (value.attrName == 'description') {
						$("#assetBulkMetadataTable tbody")
							.append(
								'<tr><td>'
								+ value.displayName
								+ '&nbsp;&nbsp;<img src="images/infoIcon.svg" class="icon" data-toggle="tooltip"'
								+ 'data-placement="right" title="Please note that this description will be applied to all Assets. You may edit the description of each Asset separately after it is uploaded."></img></td><td>'
								+ '<textarea rows="4" placeholder ="Required" is_mandatory="'
								+ value.mandatory
								+ '" aria-label="value of meta data"  name="zAttrStr_'
								+ value.attrName
								+ '"'
								+ 'style="width:100%;padding-left: 10px;font-family:inter_regular;"></textarea></td></tr>');
					} else {
						$("#assetBulkMetadataTable tbody").append(
							'<tr><td>' + value.displayName
							+ '&nbsp;&nbsp;' + infoHtml + '</td><td>' + '<input type="search" placeholder="'
							+ placeholderValue + '" class="bulkAssetTextbox" is_mandatory="'
							+ value.mandatory
							+ '" aria-label="value of meta data"  name="zAttrStr_' + value.attrName
							+ '"' + '></td></tr>');
					}
				}

			});
}

function registerBulkAssets() {

	var studyPath = $("#studyPath").val();
	if (!studyPath) {
		studyPath = $("#studyList").val();
	}
	$("#registerBulkAssetForm").attr('bulkDatafilePath', studyPath);
	var uploadType = $("#uploadAssetSelect option:selected").val();
	$("#registerBulkAssetForm").attr('uploadType', uploadType);
	var form = $('#registerBulkAssetForm')[0];
	var data = new FormData(form);
	data.append('bulkDatafilePath', studyPath);
	data.append('uploadType', uploadType);
	data.append('assetType', $("#assetTypeSelect").val())
	var isValidated = true;
	var errorMsg = "";
	var usermetaDataEntered = true;
	var isFormBulkAssetUpload;

	$('table#assetBulkMetadataTable input[type="search"]').each(function () {
		var name = $(this).val();
		var ismandatory = $(this).attr('is_mandatory');
		if (!name && ismandatory && ismandatory != "false") {
			isValidated = false;
			usermetaDataEntered = false;
		}
	});

	$("table#assetBulkMetadataTable").find("textarea").each(function () {
		var ismandatory = $(this).attr('is_mandatory');
		if (!$(this).val() && ismandatory && ismandatory != "false") {
			isValidated = false;
			usermetaDataEntered = false;
		}
	});

	$("table#assetBulkMetadataTable").find(".simple-select2").each(function () {
		var ismandatory = $(this).attr('is_mandatory');
		var isMultiSelect = $(this).prop('multiple');
		var name = $(this).val();
		if (isMultiSelect) {
			name = $(this).select2("val");
		}
		if (!isMultiSelect && ismandatory && ismandatory != "false" && name && name == 'Select') {
			isValidated = false;
			usermetaDataEntered = false;
		} else if (isMultiSelect && ismandatory && ismandatory != "false" && name.length == 0) {
			isValidated = false;
			usermetaDataEntered = false;
		}
	});

	if (!usermetaDataEntered) {
		errorMsg = "Enter values for all required metadata.";
	}

	if (uploadType == 'globus') {

		if (!$("#assetGlobusEndpointId").length || !$("#assetGlobusEndpointPath").length) {
			isValidated = false;
			errorMsg = "Select Assets from Globus.";

		} else if (!$("#assetSelectedFolders").length) {
			isValidated = false;
			errorMsg = "Select Folders from Globus.";

		} else if ($("#assetSelectedFiles").length) {
			isValidated = false;
			errorMsg = "Select folders only."

		}

	} else if (uploadType == 'S3') {

		$('#S3DetailsDiv input[type="search"]').each(function () {
			var ismandatory = $(this).attr('is_mandatory');
			if ($(this).is(":visible") && !$(this).val() && ismandatory && ismandatory != "false") {
				isValidated = false;
			}
		});

		if (!isValidated) {
			errorMsg = "Enter all the required fields."
		}
	} else if (uploadType == 'cloud') {

		$('#googleCloudDiv input[type="search"]').each(function () {
			var ismandatory = $(this).attr('is_mandatory');
			if ($(this).is(":visible") && !$(this).val() && ismandatory && ismandatory != "false") {
				isValidated = false;
			}
		});

		if (!isValidated) {
			errorMsg = "Enter all the required fields."
		}
	}

	/* once all the above validation passes, do the following additional validations */

	if (isValidated) {
		if ($('input[name="assetUploadType"]').is(":visible") && $('input[name="assetUploadType"]:checked').length == 0) {
			isValidated = false;
			errorMsg = "Select your choice of upload.";

		} else if ($("input[name='assetUploadType']:checked").val() == 'No' && $("#assetTypeSelect").val() == 'Select') {
			isValidated = false;
			errorMsg = "Select Asset Type.";

		} else if ($("#doeMetadataFile").is(":visible") && !$("#doeMetadataFile").val()) {
			isValidated = false;
			errorMsg = "Upload CSV Metadata file.";

		}
	}

	if (errorMsg) {
		$(".registerMsg").html("");
		$(".registerMsgBlock").hide();
		$(".registerErrorMsg").html(errorMsg);
		$(".registerMsgErrorBlock").show();
		$('body,html').animate({
				scrollTop : 0
		}, 500);

	} else if (isValidated) {

		/* all the validations have passed, call the ajax to perform bulk asset upload */

		if ($("input[name='assetUploadType']:checked").val() == 'No') {
			isFormBulkAssetUpload = true;
		} else if ($("input[name='assetUploadType']:checked").val() == 'Yes') {
			isFormBulkAssetUpload = false;
		} else if (uploadType == 'globus' && $("#doeMetadataFile").is(":visible") && $("#doeMetadataFile").val()) {
			isFormBulkAssetUpload = false;
		}
		$.ajax({
			type: "POST",
			enctype: "multipart/form-data",
			url: "/addbulk?isFormBulkAssetUpload=" + isFormBulkAssetUpload,
			data: data,
			processData: false,
			contentType: false,
			beforeSend: function () {
				$("#spinner").show();
				$("#dimmer").show();
			},
			success: function (msg) {
				$("#spinner").hide();
				$("#dimmer").hide();
				console.log('SUCCESS: ', msg);

				if (!msg) {
					msg = "Error in bulk upload";
				}
				
				if (msg && msg.indexOf("Your bulk data file registration request has the following task ID") != -1) {
					$(".registerMsgErrorBlock").hide();
					$(".registerErrorMsg").html("");
					$(".registerMsg").html(msg);
					$(".registerMsgBlock").show();
				} else {
					console.log('ERROR: ', msg);
					$(".registerMsg").html("");
					$(".registerMsgBlock").hide();
					$(".registerErrorMsg").html(msg);
					$(".registerMsgErrorBlock").show();
				}
				
				$('body,html').animate({
						scrollTop : 0
				}, 500);

				
			},
			error: function (e) {
				$("#spinner").hide();
				$("#dimmer").hide();
				console.log('ERROR: ', e);
				var error = "";
				if (e && e.responseText) {
					error = e.responseText
				} else {
					error = e;
				}
				
				$(".registerMsg").html("");
				$(".registerMsgBlock").hide();
				$(".registerErrorMsg").html(error);
				$(".registerMsgErrorBlock").show();
				$('body,html').animate({
					scrollTop: 0
				}, 500);

			}
		});
	}
}

function displayAssetTpeSelection(data) {
	if (data == 'Yes') {
		$("#uploadCsvFile").show();
		$("#formAssetSelection").hide();
		$("#assetTypeSelect").val("Select").trigger('change');
		$("#addMetadataDiv").hide();
	} else if (data == 'No') {
		$("#doeMetadataFile").val("");
		$("#uploadCsvFile").hide();
		$("#formAssetSelection").show();
	}
}

function displayBulkAssetSelection(data) {

	if (data == 'Globus') {
		$("#assetUploadDiv").addClass('show');
		$("#globusDetailsDiv").show();
		$("#S3DetailsDiv").hide();
		var bulkUploadCollection = $("#bulkUploadCollection").val();
		var folderLength = $("#assetSelectedFolders ul li").length;
		resetAssetBulkUploadChoiceOptions();
		if (!bulkUploadCollection) {
			$("#bulkAssetOptionsDiv").hide();
		} else {
			$("#bulkAssetOptionsDiv").show();
			if (folderLength && folderLength > 1) {

				$("#multipleFoldersUploadDiv").hide();
				$("#uploadCsvFile").show();
				$("#formAssetSelection").hide();
				$("#addMetadataDiv").hide();
			} else {
				$("#multipleFoldersUploadDiv").show();
				$("#uploadCsvFile").hide();
			}

		}
	} else if (data == 'S3') {
		$("#globusDetailsDiv").hide();
		$("#assetUploadDiv").addClass('show');
		$("#registerBulkAssets").prop("disabled", false);
		$("#S3DetailsDiv").show();
		$("#bulkAssetOptionsDiv").show();
		$("#uploadCsvFile").hide();
		$("#multipleFoldersUploadDiv").show();
		resetAssetBulkUploadChoiceOptions();

	} else if (data == 'emptyAsset') {
		$("#assetUploadDiv").removeClass('show');
	}
}

function resetAssetBulkUploadChoiceOptions() {

	$('input[name="assetUploadType"]').prop('checked', false);
	$("#uploadCsvFile").hide();
	$("#formAssetSelection").hide();
	$("#assetTypeSelect").val("Select").trigger("change");
}


function displayEmptyAssetScreen(data) {
	$("#uploadSectionDiv").hide();
	$("#uploadHeader").hide();
	$("#registerCollectionForm").hide();
	
	// show create fragment
	$(".registerMsg").html("");
	$(".registerMsgBlock").hide();
	$(".registerMsgErrorBlock").hide();
	$(".registerErrorMsg").html("");
	$("#uploadRegisterCollectionFragment").show();
	$("#registerAssetPicker").hide();
	$("#uploadSectionDiv").hide();
	$("#uploadHeader").hide();
	$("#assetUploadDiv").hide();
	
	
	$("#registerAssetPicker").show();

	// Reset registerAssetSelect radio buttons
	$('input[type=radio][name=registerAssetRadio]').prop('checked', false);

	// Reset registerAssetSelect selection 
	$('#registerAssetSelect').prop('selectedIndex', 0);
	$('#registerAssetSelect').prop('disabled', true);
	$('#registerAssetSelect').val('').trigger('change');

	// Reset uploadAssetSelect selection 
	$('#uploadAssetSelect').prop('selectedIndex', 0);
	$('#uploadAssetSelect').prop('disabled', true);
	$('#uploadAssetSelect').val('').trigger('change');

}

function loadDefaultBulkAccessGrp(data, status) {
 	$("#bulkAccessGrpSelect").select2({
   		 placeholder: "Required"
	}).val(defaultGroup).trigger("change");
    
}