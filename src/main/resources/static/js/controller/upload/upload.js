$(document).ready(
	function () {

		loadUploadTab();
		$('body').tooltip({
			selector: '[data-toggle="tooltip"]'
		});

		$("#bulkAssetGlobusRadiobtn").click(function (e) {
			resetAssetsSelection();
		});

		$(".backToUploadTab").click(function (e) {
			$("#uploadRegisterCollectionFragment").hide();
			$("#registerAssetPicker").hide();
			$("#uploadSectionDiv").show();
			$("#uploadHeader").show();
			$("#uploadDataFilesTab").hide();
			$("#editCollectionFragment").hide();
			$('#registerCollectionForm').show();
			resetSelectionsForBackToUploadTab();
		});
	});


function loadUploadTab() {

	var program = $("#programPath").val();
	var stu = $("#studyPath").val();
	var data = $("#datafilePath").val();
	var bulkUploadCollection = $("#bulkUploadCollection").val();
	var gcCloudAuthorized = $("#gcCloudAuthorized").val();
	var uploadPath = $("#uploadPath").val();
	var uploadAsyncType = $("#uploadAsyncType").val();

	if (program) {
		$("input[name=selectProgram][value='Select Program']").prop("checked", true);
		showSelect('Program', 'true');
		// $("#studyListDiv").show();
		$("#studyListDiv").removeClass("disable-pointer-events");
		$("#deleteProgram").show();
		$("#editProgram").show();
		if (stu) {
			$("input[name=selectStudy][value='Select Study']").prop("checked", true);
			showSelect('Study', program);
			// $("#dataSetListDiv").show();
			$("#dataSetListDiv").removeClass("disable-pointer-events");

			$("#deleteStudy").show();
			$("#editStudy").show();
			$("#bulkAssetGlobusRadiobtn").show();
			
			if (bulkUploadCollection) {
				$("input[name=selectAsset][value='Register Asset']").click();
				// Reset registerAssetSelect radio buttons
				$("input[name=registerAssetRadio][value='Upload Asset']").click();
				$('#uploadAssetSelect').prop('disabled', false);
				$("#registerBulkAssets").prop("disabled", false);
				
				if (uploadAsyncType && uploadAsyncType == "cloud") {
				   $('#uploadAssetSelect').val('cloud').trigger('change');
				} else  {
				   $('#uploadAssetSelect').val('globus').trigger('change');	
				}
				
			}

			
			if (data) {
				if (!bulkUploadCollection) {
					showSelect('Asset', stu);
				}
				$("input[name=selectAsset][value='Select Asset']").prop("checked", true);
				$("#deleteDataSet").show();
				$("#editAsset").show();
				constructAssetFileAndFoldersDiv(data);
				$("#uploadAndRegisterFiles").show();
				$("#uploadDataFilesTab").show();
				$('input[name=datafileTypeUpload]:checked').val();
				$("#bulkFileUploadSection").show();
				$("#registerFileBtnsDiv").show();
				$("#bulkDataFilePathCollection").val(uploadPath);
				$(".registerBulkDataFileSuccess").hide();
				$(".registerBulkDataFile").html("");
				if ($("#fileNamesDiv").is(":visible") || $("#folderNamesDiv").is(":visible")) {
					$("#registerBulkDataFileBtn").prop("disabled", false);
				} else {
					$("#registerBulkDataFileBtn").prop("disabled", true);
				}

				if (uploadAsyncType && uploadAsyncType == 'drive') {
					$("#datafileTypeDriveUpload").prop("checked", true);
					$("#displayGlobusUploadDiv").hide();
					$("#displayDriveUploadDiv").show();
					$("#displayCloudUploadDiv").hide();
				} else if (uploadAsyncType && uploadAsyncType == 'cloud') {
					$("#datafileTypeCloudUpload").prop("checked", true);
					$("#displayGlobusUploadDiv").hide();
					$("#displayDriveUploadDiv").hide();
					$("#displayCloudUploadDiv").show();
					$("#registerBulkDataFileBtn").prop("disabled", false);
				} else {
					$("#datafileTypeGlobusUpload").prop("checked", true);
					$("#displayGlobusUploadDiv").show();
					$("#displayDriveUploadDiv").hide();
					$("#displayCloudUploadDiv").hide();
				}
			}
		}

		if (!bulkUploadCollection && $("#showUploadDataFilesTab").val() == 'true') {
			$("#uploadSectionDiv").hide();
			$("#uploadHeader").hide();
			$("#uploadDataFilesTab").show();
			$("#uploadCollectionPath").val(uploadPath);
			computeWidthForCollectionPath('uploadCollectionPath');
		}
	}

}


function addNewMetaDataCollection(tableName) {
	var rowId = $("#" + tableName + " tbody").length;
	rowId = rowId + 1;
	$("#" + tableName + " tbody")
		.append(
			'<tr id="addRow'
			+ rowId
			+ '"><td><input type="search" placeholder="Required" style="width: 95%; border-radius: 8px;border: 1px solid #6B7294;height: 36px;" '
			+ 'name="_addAttrName'
			+ rowId
			+ '" aria-label="add new row" id="_addAttrName'
			+ rowId
			+ '"></td><td><input type="search" placeholder="Required" style="width: 88%; border-radius: 8px;border: 1px solid #6B7294;height: 36px;margin: 6px 0px !important" id="_addAttrValue'
			+ rowId
			+ '" name="_addAttrValue'
			+ rowId
			+ '" >'
			+ '&nbsp;&nbsp;<img src="images/deleteIcon.svg" th:src="@{/images/deleteIcon.png}" class="metadataIcon" alt="delete metadata" onclick="removeCollectionRow(\'addRow'
			+ rowId + '\')"></td></tr>');

}

function showSelect(collection, selection) {

	var isEmptyOption;
	var seclectedValue;

	if (selection) {
		isEmptyOption = false;
	} else {
		isEmptyOption = true;
	}

	if (collection == 'Program') {

		if (isEmptyOption) {
			$("#institutePath").val("");
		}

		loadJsonData('/programList', $("#programList"), isEmptyOption, null, null, null, "key", "value");
		$("#showSelectProgramDiv").show();
		resetOnChangeofSelectCollection("programList", null);

	} else if (collection == 'Study') {

		if (isEmptyOption) {
			$("#studyPath").val("");
		}

		if (selection) {
			seclectedValue = selection;
		} else {
			seclectedValue = $("#programList").val();
		}

		var params = {
			selectedPath: seclectedValue,
		};

		loadJsonData('/collectionList', $("#studyList"), isEmptyOption, params, null, null, "key", "value");

		$("#showSelectStudyDiv").show();
		resetOnChangeofSelectCollection("studyList", null);
	} else if (collection == 'Asset') {

		if (isEmptyOption) {
			$("#datafilePath").val("");
		}

		if (selection) {
			seclectedValue = selection;
		} else {
			seclectedValue = $("#studyList").val();
		}

		var params = {
			selectedPath: seclectedValue,
			refreshNode: 'true'
		};
		loadJsonData('/collectionList', $("#dataList"), isEmptyOption, params, null, null, "key", "value");
		resetAssetsSelection(true);
		$("#showSelectAssetDiv").show();
		$("#showSubAssetSelectionDiv").hide();
		$("#assetUploadDiv").removeClass('show');
	} else if (collection == 'subAsset') {
		$("#showSubAssetSelectionDiv").show();
		$("#showSelectAssetDiv").hide();
		$("#uploadAndRegisterFiles").hide();
		$("#dataListDiv").hide();
		$("#assetUploadDiv").removeClass('show');
	}
}

function resetSelectionsForBackToUploadTab() {

	if ($("input[name=selectProgram]").is(":visible")) {
		if ($("#programList").is(":visible")) {
			$("input[name=selectProgram][value='Select Program']").prop("checked", true);
		} else {
			$("input[name=selectProgram][value='Select Program']").click();
		}
	}
	if ($("input[name=selectStudy]").is(":visible")) {
		if ($("#studyList").is(":visible")) {
			$("input[name=selectStudy][value='Select Study']").prop("checked", true);
		} else {
			$("input[name=selectStudy][value='Select Study']").click();
		}
	}
	if ($("input[name=selectAsset]").is(":visible")) {
		$("input[name=selectAsset][value='Select Asset']").prop("checked", true);
		if (!$("#dataList").is(":visible")) {
			showSelect('Asset');
		}
	}
}

function resetOnChangeofSelectCollection(selectTarget, selectedValue) {
	if (selectTarget == 'programList') {
		$("#uploadAndRegisterFiles").hide();
		$("#bulkAssetGlobusRadiobtn").hide();
		if (selectedValue && selectedValue != 'ANY') {
			// $("#studyListDiv").show();
			$("#studyListDiv").removeClass("disable-pointer-events");

			$("#deleteStudy").hide();
			$("#editStudy").hide();
			$("#deleteProgram").show();
			$("#editProgram").show();
			// $("#dataSetListDiv").hide();
			$("#dataSetListDiv").addClass("disable-pointer-events");

			$("#dataListDiv").hide();
			$("#showSelectStudyDiv").hide();
			$("input[name=selectStudy][value='Select Study']").prop("checked", false);
		} else {
			// $("#studyListDiv").hide();
			$("#studyListDiv").addClass("disable-pointer-events");

			// $("#dataSetListDiv").hide();
			$("#dataSetListDiv").addClass("disable-pointer-events");

			$("#dataListDiv").hide();
			$("#deleteProgram").hide();
			$("#editProgram").hide();
		}
	} else if (selectTarget == 'studyList') {
		$("#uploadAndRegisterFiles").hide();
		$("#dataListDiv").hide();
		if (selectedValue && selectedValue != 'ANY') {
			// $("#studyListDiv").show();
			$("#studyListDiv").removeClass("disable-pointer-events");

			// $("#dataSetListDiv").show();
			$("#dataSetListDiv").removeClass("disable-pointer-events");

			$("#deleteStudy").show();
			$("#editStudy").show();
			$("#bulkAssetGlobusRadiobtn").show();
			$("#deleteDataSet").hide();
			$("#editAsset").hide();
			$("#showSelectAssetDiv").hide();
			$("#showSubAssetSelectionDiv").hide();
			$("input[name=selectAsset]").prop("checked", false);

		} else {
			// $("#studyListDiv").show();
			$("#studyListDiv").removeClass("disable-pointer-events");

			// $("#dataSetListDiv").hide();
			$("#dataSetListDiv").addClass("disable-pointer-events");

			$("#deleteStudy").hide();
			$("#editStudy").hide();
			$("#bulkAssetGlobusRadiobtn").hide();
		}
	} else if (selectTarget == 'dataList') {
		// $("#studyListDiv").show();
		$("#studyListDiv").removeClass("disable-pointer-events");

		if (selectedValue && selectedValue != 'ANY') {
			$("#uploadAndRegisterFiles").show();
			constructAssetFileAndFoldersDiv(selectedValue);
			$("#deleteDataSet").show();
			$("#editAsset").show();

		} else {
			$("#dataListDiv").hide();
			$("#uploadAndRegisterFiles").hide();
			$("#deleteDataSet").hide();
			$("#editAsset").hide();
		}
	}
}


function resetAssetsSelection(reset) {
	if ($("#dataListDiv").is(":visible") || reset) {
		var data = {
			id: 'ANY',
			text: 'Select'
		};
		var newOption = new Option(data.text, data.id, true, true);
		if ($('#dataList').find("option[value='" + data.id + "']").length) {
			$('#dataList').val(data.id).trigger('change');
		} else {
			$('#dataList').append(newOption).trigger('change');
		}
	}
}


function computeWidthForCollectionPath(ele) {
	const textboxEle = document.getElementById(ele);
	textboxEle.style.width = ((textboxEle.value.length + 1) * 7) + 'px';
}

function postUploadGlobusFunction(data, status) {
	location.replace(data);
}