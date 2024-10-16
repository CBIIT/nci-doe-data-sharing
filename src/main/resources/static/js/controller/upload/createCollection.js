$(document).ready(
	function () {

		$(document).on('keyup', '.identifier_validation', function () {
			var identifierVal = $(this).val();
			var pattern = /^[A-Za-z0-9_-]*$/;

			if (!pattern.test(identifierVal)) {
				$(this).val(identifierVal.replace(/[^A-Za-z0-9_-]/g, ''));
			}
		});

	});


function constructNewCollectionMetaDataSet(data, status) {

	$("#newMetaDataTable tbody").html("");
	var parentAccessgrp = $("#parentAccessGroup").val();
	var assetType = $("#assetType").val();
	var collectionType = $("#collectionType").val();

	$
		.each(
			data,
			function (key, value) {
				var controllerAttribute = value.controllerAttribute;
				var infoHtml = "";
				if (value.description) {
					infoHtml = '<img src="images/infoIcon.svg" class="icon" data-toggle="tooltip"'
						+ 'data-placement="right" title="'
						+ value.description
						+ '"></img>';
				}
				if (value.attrName == 'access_group') {

					if (!parentAccessgrp || (parentAccessgrp && parentAccessgrp == "public")) {
						$("#newMetaDataTable tbody")
							.append(
								'<tr><td>'
								+ value.displayName
								+ '&nbsp;&nbsp;' + infoHtml + '</td><td>'
								+ '<div class="mt-2"><select class="simple-select2" multiple="multiple" id="accessGroupSelect" name="zAttrStr_'
								+ value.attrName
								+ '"'
								+ 'style="width: 95%; border-radius: 8px;border: 1px solid #6B7294;height: 36px;"></select></div>'
								+ '</td></tr>');
						
						loadJsonData('/metaDataPermissionsList', $("#accessGroupSelect"), false, null, loadDefaultAccessGroup,
							null, "key", "value");

					} else {

						$("#newMetaDataTable tbody")
							.append(
								'<tr><td>'
								+ value.displayName
								+ '&nbsp;&nbsp;' + infoHtml + '</td><td>'
								+ '<input type="search" placeholder="Required" aria-label="value of meta data" name="zAttrStr_'
								+ value.attrName
								+ '" value ="'
								+ parentAccessgrp
								+ '"'
								+ 'disabled="disabled" style="width: 95%; border-radius: 8px;border: 1px solid #6B7294;height: 36px;background-color: #dddddd;"><input type="hidden" name="zAttrStr_'
								+ value.attrName
								+ '" value ="'
								+ parentAccessgrp
								+ '"/> &nbsp;&nbsp;<img src="images/infoIcon.svg" class="icon"><span>Access group inherited from parent.</span></img></td></tr>');
					}
				} else if (value.attrName == 'asset_type') {

					$("#newMetaDataTable tbody")
						.append(
							'<tr><td>'
							+ value.displayName
							+ '&nbsp;&nbsp;' + infoHtml + '</td><td>'
							+ '<input type="search" disabled="disabled" aria-label="value of meta data" value ="'
							+ assetType + '" name="zAttrStr_' + value.attrName + '"'
							+ 'style="width: 95%; border-radius: 8px;border: 1px solid #6B7294;height: 36px;"><input type="hidden" name="zAttrStr_'
							+ value.attrName + '" value ="' + assetType + '"/> </td></tr>');

				} else if (value.validValues != null && value.isVisibleOnUplaodPage != false) {
					$("#newMetaDataTable tbody").append(
						"<tr><td>" + value.displayName
						+ "&nbsp;&nbsp;" + infoHtml + "</td><td>"
						+ "<select class='simple-select2' is_mandatory='" + value.mandatory
						+ "' onChange='onChangeForMetadata(registerCollectionForm, true, "
						+ value.controllerAttribute + ",newMetaDataTable, " + value.attrName
						+ ");' style='width: 95%; border-radius: 8px;border: 1px solid #6B7294;height: 36px;' id='" + value.attrName + "' name='zAttrStr_"
						+ value.attrName + "' value='" + value.attrValue + "'></select></td></tr>");

					
					var $select = $("#newMetaDataTable select[id='" + value.attrName + "']");

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

				} else if (value.attrValue && value.isVisibleOnUplaodPage != false) {
					$("#newMetaDataTable tbody").append(
						'<tr><td>' + value.displayName
						+ '&nbsp;&nbsp;' + infoHtml + '</td><td>'
						+ '<input type="search" is_mandatory="' + value.mandatory
						+ '" placeholder="Required" aria-label="value of meta data" value="'
						+ value.attrValue + '" name="zAttrStr_' + value.attrName + '"'
						+ 'style="width: 95%; border-radius: 8px;border: 1px solid #6B7294;height: 36px;"></td></tr>');
				} else if (value.attrName.indexOf("_identifier") != -1) {
					var placeholderValue = value.mandatory == true ? 'Required' : "";
					$("#newMetaDataTable tbody").append(
						'<tr><td>' + value.displayName
						+ '&nbsp;&nbsp;' + infoHtml + '</td><td>' + '<input type="search" is_mandatory="'
						+ value.mandatory + '" class="identifier_validation" pattern = "[A-Za-z0-9_-]*" placeholder= "' + placeholderValue
						+ '" aria-label="value of meta data" name="zAttrStr_' + value.attrName
						+ '"' + 'style="width: 95%; border-radius: 8px;border: 1px solid #6B7294;height: 36px;"></td></tr>');
				}
				else {
					var placeholderValue = value.mandatory == true ? 'Required' : "";
					if (value.attrName.indexOf("description") != -1) {
						$("#newMetaDataTable tbody").append(
							'<tr><td>' + value.displayName
							+ '&nbsp;&nbsp;' + infoHtml + '</td><td>' + '<textarea rows="5" is_mandatory="'
							+ value.mandatory + '" placeholder="' + placeholderValue
							+ '" aria-label="value of meta data" name="zAttrStr_' + value.attrName
							+ '"' + 'style="width: 95%; border-radius: 8px;border: 1px solid #6B7294;height: 36px;"></textarea></td></tr>');
					} else if (value.isVisibleOnUplaodPage != false) {
						$("#newMetaDataTable tbody").append(
							'<tr><td>' + value.displayName
							+ '&nbsp;&nbsp;' + infoHtml + '</td><td>' + '<input type="search" is_mandatory="'
							+ value.mandatory + '" placeholder="' + placeholderValue
							+ '" aria-label="value of meta data" name="zAttrStr_' + value.attrName
							+ '"' + 'style="width: 95%; border-radius: 8px;border: 1px solid #6B7294;height: 36px;"></td></tr>');
					}
				}
			});
}


function createCollectionDiv(collectionType, parentCollectionType, selectTarget, folderPath) {

	var selectedIndexPathVal = $("#" + selectTarget).val();
	var displayCollectionType;
	var parentName = $("#" + selectTarget + " option:selected").text();
	if (folderPath) {
		selectedIndexPathVal = folderPath;
		var folderParentPath = selectedIndexPathVal.split('/').at(-2);
		parentName = folderParentPath;
	}
	if (selectTarget == 'basePath') {
		$(".parentCollectionDiv").hide();
	} else {
		$(".parentCollectionDiv").show();
		$("#parentCollectionName").text(parentName);
	}
	if (selectTarget == 'dataList') {
		$(".permissionsLabel").hide();
		$("#parentCollectionNameDiv").css('margin-left', '0px');
	} else {
		$(".permissionsLabel").show();
		$("#parentCollectionNameDiv").css('margin-left', '0px');
	}
	$("#collectionPath").val(selectedIndexPathVal);
	$("#newMetaDataTable tbody").html("");
	$(".registerMsg").html("");
	$("#newMetaDataTable tbody").html("");
	$(".registerMsgBlock").hide();
	$(".registerMsgErrorBlock").hide();
	$(".registerErrorMsg").html("");
	
	
	$('#parentCollectionLabel').text(parentCollectionType.toUpperCase() + " COLLECTION IDENTIFIER");
	

	if (collectionType == 'Folder') {
		displayCollectionType = 'Asset Subcollection';
		$(".folderDiv").show();
	} else {
		displayCollectionType = collectionType;
		$(".folderDiv").hide();
	}
	$("#parentCollectionType").val(parentCollectionType);
	
	$("#collectionType").val(collectionType);
	
	// Tp get the button stylings
	$("#registerCollectionBtn").attr("class", "btn btn-primary mb-2 mr-2 register" + collectionType)
	$("#registerCollectionBtn").html("Register " + "<br>" + displayCollectionType + `<img class="arrowright"
	src='/images/white_right_arrow.svg' style="width: 16px;
	transform: translate(64px, -8px);"/>`);
	$("#collectionMetaDataLabel").text(displayCollectionType + " Metadata");
	$("#registerModalTitle").html("Register " + displayCollectionType);
	
	$("#addNewMetaData")
		.html(
			"<img src='images/addIcon.svg' th:src='@{/images/addIcon.png}' class='metadataIcon' alt='add metadata'>&nbsp;Add Metadata");
	
	var params = {
		parent: selectedIndexPathVal
	};
	
	invokeAjax('/addCollection/parentAccessGroup', 'GET', params, retrieveCollectionList, null, null, null);
	// loadJson for permissions list
	loadJsonData('/metaDataPermissionsList', $("#metaDataPermissionsList"), false, null, null, null, "key", "value");

	// show create fragment
	$("#uploadRegisterCollectionFragment").show();
	$("#registerAssetPicker").hide();
	$("#uploadSectionDiv").hide();
	$("#uploadHeader").hide();
	$("#assetUploadDiv").hide();

}



function retrieveCollectionList(data, status) {
	var parentAccessGrp;	
	$.each(data, function (key, val) {
		if (val.key == "parentAccessGroup")
			parentAccessGrp = val.value;
	});

	$("#parentAccessGroup").val(parentAccessGrp);

	var collectionPath = $("#collectionPath").val();
	var collectionType = $("#collectionType").val();
	var assetType = $("#assetType").val();
	
	if (collectionType && collectionPath) {
		var params = {
			selectedPath: collectionPath,
			collectionType: collectionType,
			controllerValue: assetType,
			controllerAttribute: 'asset_type'
		};
		invokeAjax('/addCollection', 'GET', params, constructNewCollectionMetaDataSet, null, null, null);
	}
}

function computeWidthForAssetCollectionName(ele) {
	const textboxEle = document.getElementById(ele);
	textboxEle.style.width = ((textboxEle.value.length + 5) * 10) + 'px';
}

function registerCollection() {

	$(".registerErrorMsg").html("");
	$(".registerMsgErrorBlock").hide();
	var collectionPath = $("#collectionPath").val();
	var collectionType = $("#collectionType").val();

	var newCollectionPath;
	var collectionName;
	var validate = true;
	var usermetaDataEntered = true;

	$('table#newMetaDataTable input[type="search"]').each(function () {
		var name = $(this).val();
		var ismandatory = $(this).attr('is_mandatory');
		if (!name && ismandatory && ismandatory != "false") {
			usermetaDataEntered = false;
		}

		if (name && ($(this).attr('name') == ('zAttrStr_' + collectionType.toLowerCase() + '_' + 'identifier'))) {
			var modifiedName = name.replace(/ /g, "_");
			collectionName = modifiedName;
		}
	});

	if (collectionType && collectionType == 'Folder') {

		var folderName = $("#folder_name").val();
		var folderNameModifief = folderName.replace(/ /g, "_");
		collectionName = folderNameModifief;
		if (!folderName) {
			validate = false;
			$(".registerErrorMsg").append("Enter Folder Name");
			$(".registerMsgErrorBlock").show();
			$('body,html').animate({
				scrollTop: 0
			}, 500);
		}
	}

	$("textarea").each(function () {
		var ismandatory = $(this).attr('is_mandatory');
		if (!$(this).val() && ismandatory && ismandatory != "false") {
			usermetaDataEntered = false;
		}
	});

	$("table#newMetaDataTable .simple-select2").each(function () {
		var isMultiSelect = $(this).prop('multiple');
		var ismandatory = $(this).attr('is_mandatory');
		var name = $(this).val();

		if (isMultiSelect) {
			name = $(this).select2("val");
		}

		if (!isMultiSelect && ismandatory && ismandatory != "false" && name && name == 'Select') {
			usermetaDataEntered = false;
		} else if (isMultiSelect && ismandatory && ismandatory != "false" && name.length == 0) {
			usermetaDataEntered = false;
		}

	});

	let accessGroupError = true;

	if ($("#accessGroupSelect").is(":visible")  && $("#accessGroupSelect").val().length == 0) {
			accessGroupError = false;
	}
	

	if (!usermetaDataEntered || !accessGroupError) {
		validate = false;
		$(".registerErrorMsg").append("Enter values for all required metadata.");
		$(".registerMsgErrorBlock").show();
		$('body,html').animate({
			scrollTop: 0
		}, 500);
	}

	if (collectionPath && collectionName) {
		newCollectionPath = collectionPath + "/" + collectionName.trim();
		$("#newCollectionPath").val(newCollectionPath);
	}

	if (validate && newCollectionPath) {
		var data = $('#registerCollectionForm').serialize();
		$.ajax({
			type: "POST",
			url: "/addCollection",
			data: data,
			beforeSend: function () {
				$("#spinner").show();
				$("#dimmer").show();
			},
			success: function (msg) {
				$("#spinner").hide();
				$("#dimmer").hide();
				console.log('SUCCESS: ', msg);
				if (msg && msg == 'Not Authorized') {
					location.replace("/loginTab");
				}
				postSuccessRegisterCollection(msg, collectionType);
				$('body,html').animate({
					scrollTop: 0
				}, 500);

			},
			error: function (e) {
				$("#spinner").hide();
				$("#dimmer").hide();
				console.log('ERROR: ', e);
				$(".registerErrorMsg").html(e.responseText);
				$(".registerMsgErrorBlock").show();
				$('body,html').animate({
					scrollTop: 0
				}, 500);
			}
		});
	}
}


function postSuccessRegisterCollection(data, collectionType) {
	if (data.indexOf("Collection is created") != -1) {
		if (collectionType == 'Program') {
			var params = {
				selectedPath: $("#basePath").val(),
				refreshNode: 'true'
			};
			loadJsonData('/collectionList', $("#programList"), true, params, displaySuccessMsg, null, "key",
				"value");
			resetOnChangeofSelectCollection("programList", null);
		} else if (collectionType == 'Study') {
			var params = {
				selectedPath: $("#programList").val(),
				refreshNode: 'true'
			};
			loadJsonData('/collectionList', $("#studyList"), true, params, displaySuccessMsg, null, "key", "value");
			resetOnChangeofSelectCollection("studyList", null);
		} else if (collectionType == 'Asset') {
			var params = {
				selectedPath: $("#studyList").val(),
				refreshNode: 'true'
			};
			loadJsonData('/collectionList', $("#dataList"), true, params, displaySuccessMsg, null, "key", "value");
			resetAssetsSelection();
			$("#assetUploadDiv").removeClass('show');
		} else if (collectionType == 'Folder') {
			constructAssetFileAndFoldersDiv($("#dataList").val());
			displaySuccessMsg();
		}
	} else {
		$(".registerMsg").html("");
		$(".registerMsgBlock").hide();
		$(".registerErrorMsg").html("Error creating collection: " + data);
		$(".registerMsgErrorBlock").show();

	}
}


function displaySuccessMsg(data, status) {
	$(".registerErrorMsg").html("");
	$(".registerMsgErrorBlock").hide();
	$(".registerMsg").html("Collection created successfully.");
	$(".registerMsgBlock").show();
}

function loadDefaultAccessGroup(data, status) {    
    $("#accessGroupSelect").select2({
   		 placeholder: "Required"
	}).val(defaultGroup).trigger("change");
}
