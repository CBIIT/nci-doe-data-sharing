$(document).ready(
		function() {

			$("#assetSelectionGlobusButton").click(function(e) {
				var d = {};
				d.institutionPath = $("#instituteList").val();
				d.studyPath = $("#studyList").val();
				d.uploadType = "assetBulkUpload";

				invokeAjax('/upload', 'GET', d, postUploadGlobusFunction, postFailureFunction, null, 'text');
			});


});

function retrieveAssetTypeDiv(data) {
	if (data.value != 'Select') {
		var params1 = {
			collectionType : 'Asset',
			controllerValue : data.value,
			controllerAttribute : 'asset_type'
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

	$("#assetBulkMetadataTable tbody")
			.append(
					'<tr><td style="width: 24%;">Asset Group Identifier&nbsp;&nbsp;<i class="fas fa-info-square" data-toggle="tooltip"'
							+ 'data-placement="right" title="The system uses the Asset Group Identifier to create a unique Asset Identifier for each asset, in the format <group name>_<integer>"></i></td><td>'
							+ '<input type="search" placeholder="Required" class="bulkAssetTextbox" is_mandatory="true" aria-label="value of meta data"  name="assetGroupIdentifier"'
							+ '></td></tr>');

	$
			.each(
					data,
					function(key, value) {

						var placeholderValue = value.mandatory == true ? 'Required' : "";
						var infoHtml = "";
						if(value.description) {
						infoHtml = '<i class="fas fa-info-square" data-toggle="tooltip"'
														+ 'data-placement="right" title="'
														+ value.description
														+ '"></i>';
						}

						if (value.validValues != null && value.attrName != 'asset_type' && value.isVisibleOnUplaodPage != false) {

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

						} else if (value.attrName && value.attrName != 'asset_name' && value.attrName != 'asset_type'
								&& value.attrName != 'asset_identifier' && value.attrName != 'access_group' && value.isVisibleOnUplaodPage != false) {
							if (value.attrName == 'description') {
								$("#assetBulkMetadataTable tbody")
										.append(
												'<tr><td>'
														+ value.displayName
														+ '&nbsp;&nbsp;<i class="fas fa-info-square" data-toggle="tooltip"'
														+ 'data-placement="right" title="Please note that this description will be applied to all Assets. You may edit the description of each Asset separately after it is uploaded."></i></td><td>'
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
	$("#registerBulkAssetForm").attr('bulkDatafilePath', studyPath);
	$("#registerBulkAssetForm").attr('uploadType', 'globus');
	var form = $('#registerBulkAssetForm')[0];
	var data = new FormData(form);
	data.append('bulkDatafilePath', studyPath);
	data.append('uploadType', 'globus');
	data.append('assetType', $("#assetTypeSelect").val())
	var isValidated = true;
	var usermetaDataEntered = true;
	var isFormBulkUpload;

	$('table#assetBulkMetadataTable input[type="search"]').each(function() {
		var name = $(this).val();
		var ismandatory = $(this).attr('is_mandatory');
		if (!name && ismandatory && ismandatory != "false") {
			isValidated = false;
			usermetaDataEntered = false;
		}
	});

	$("table#assetBulkMetadataTable").find("textarea").each(function() {
		var ismandatory = $(this).attr('is_mandatory');
		if (!$(this).val() && ismandatory && ismandatory != "false") {
			isValidated = false;
			usermetaDataEntered = false;
		}
	});

	$("table#assetBulkMetadataTable").find(".simple-select2").each(function() {
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

	if (!$("#assetGlobusEndpointId").length || !$("#assetGlobusEndpointPath").length) {
		isValidated = false;
		bootbox.dialog({
			message : 'Select Assets from Globus.'
		});
	} else if (!$("#assetSelectedFolders").length) {
		isValidated = false;
		bootbox.dialog({
			message : 'Select Folders from Globus.'
		});
	} else if ($("#assetSelectedFiles").length) {
		isValidated = false;
		bootbox.dialog({
			message : 'Select folders only.'
		});
	} else if ($('input[name="assetUploadType"]:checked').length == 0) {
		isValidated = false;
		bootbox.dialog({
			message : 'Select your choice of upload.'
		});
	} else if ($("input[name='assetUploadType']:checked").val() == 'No' && $("#assetTypeSelect").val() == 'Select') {
		isValidated = false;
		bootbox.dialog({
			message : 'Select Asset Type.'
		});
	} else if ($("input[name='assetUploadType']:checked").val() == 'Yes' && !$("#doeMetadataFile").val()) {
		isValidated = false;
		bootbox.dialog({
			message : 'Upload CSV Metadata file.'
		});
	} else if (!usermetaDataEntered) {
		bootbox.dialog({
			message : 'Enter values for all required metadata.'
		});
	}

	if (isValidated) {
		if ($("input[name='assetUploadType']:checked").val() == 'No') {
			isFormBulkUpload = true;
		} else if ($("input[name='assetUploadType']:checked").val() == 'Yes') {
			isFormBulkUpload = false;
		}
		$.ajax({
			type : "POST",
			enctype : "multipart/form-data",
			url : "/addbulk?isFormBulkUpload=" + isFormBulkUpload,
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
					msg = "Error in bulk upload";
				}
				bootbox.dialog({
					message : msg,
					onEscape : function() {
						location.replace("/addbulk");
					}
				});
			},
			error : function(e) {
				$("#spinner").hide();
				$("#dimmer").hide();
				console.log('ERROR: ', e);
				bootbox.dialog({
					message : msg,
					onEscape : function() {
						location.replace("/addbulk");
					}
				});

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