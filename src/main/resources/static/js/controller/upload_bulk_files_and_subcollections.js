$(document).ready(
		function() {
		
		   $("#doeDataFile").change(function(e) {
				appendFileName($(this));
				$("#registerBulkDataFileBtn").prop("disabled", false);
			});
			
			$("#registerBulkDataFileBtn").click(function(e) {
				registerBulkDataFile();
			});

			$("#assetSelectionGlobusButton").click(function(e) {
				var d = {};
				d.programPath = $("#programList").val();
				d.studyPath = $("#studyList").val();
				d.uploadType = "assetBulkUpload";

				invokeAjax('/upload', 'GET', d, postUploadGlobusFunction, postFailureFunction, null, 'text');
			});


			$("#primaryGlobusButton").click(function(e) {

				var d = {};
				d.programPath = $("#programList").val();
				d.studyPath = $("#studyList").val();
				d.dataSetPath = $("#dataList").val();
				d.uploadPath = $("#bulkDataFilePathCollection").val();

				invokeAjax('/upload', 'GET', d, postUploadGlobusFunction, postFailureFunction, null, 'text');
			});

			$("#driveUploadAuthlink").click(function(e) {
				var d = {};
				d.programPath = $("#programList").val();
				d.studyPath = $("#studyList").val();
				d.dataSetPath = $("#dataList").val();
				d.uploadPath = $("#bulkDataFilePathCollection").val();
				d.action = "drive";
				invokeAjax('/upload', 'GET', d, postUploadGlobusFunction, postFailureFunction, null, 'text');
			});
			
			$("#cloudUploadAuthlink").click(function(e){
				var d = {};
				d.programPath = $("#programList").val();
				d.studyPath = $("#studyList").val();
				d.dataSetPath = $("#dataList").val();
				d.uploadPath = $("#bulkDataFilePathCollection").val();
				d.action = "cloud";
				invokeAjax('/upload', 'GET', d, postUploadGlobusFunction, postFailureFunction, null, 'text');
				
			});
			
			$(document).on('click', '.uploadDataSet', function() {
				var folderPath = $(this).parent().find('a').attr('data-name');
				$("#uploadSectionDiv").hide();
				$("#uploadHeader").hide();
				$("#uploadDataFilesTab").show();
				$("#registerBulkDataFileBtn").prop("disabled", true);
				openBulkDataRegistration(folderPath);
			});

			$("#addBulkDataFiles").click(function(e) {
				$("#uploadSectionDiv").hide();
				$("#uploadHeader").hide();
				$("#uploadDataFilesTab").show();
				$("#registerBulkDataFileBtn").prop("disabled", true);
				openBulkDataRegistration();
			});

});


function addNewMetaDataRowsForDataFile($this) {
	var rowId = $this.parent().find('div').length;
	rowId = rowId + 1;

	$this
			.parent()
			.append(
					'&nbsp;&nbsp;<div id="addDataRow'
							+ rowId
							+ '"><input type="search" style="width:40%;" '
							+ 'name="_addAttrName'
							+ rowId
							+ '" aria-label="add new row" id="_addAttrName'
							+ rowId
							+ '">&nbsp;<input type="search" style="width:40%;" id="_addAttrValue'
							+ rowId
							+ '" name="_addAttrValue'
							+ rowId
							+ '" >'
							+ '&nbsp;&nbsp;<input class="btn btn-primary pull-right" type="button" value="X" onclick="removeCollectionRow(\'addDataRow'
							+ rowId + '\')"></div>');

}


function openBulkDataRegistration(folderPath) {
	var datafilePath = $("#dataList").val();
	if (folderPath) {
		datafilePath += "/" + folderPath;
	}
	$("#bulkDataFilePath").val(datafilePath);
	$("#bulkDataFilePathCollection").val(datafilePath);
	$(".registerBulkDataFileSuccess").hide();
	$(".registerBulkDataFile").html("");
	$(".uploadBulkDataError").hide();
	$(".uploadBulkDataErrorMsg").html("");
	$("#uploadCollectionPath").val(datafilePath);
	computeWidthForCollectionPath('uploadCollectionPath');
	clearRegisterDataDiv();
}

function clearRegisterDataDiv() {
	$('input[name=datafileTypeUpload]').prop('checked', false);
	$("#doeDataFile").val("");
	$("#newMetaDataTableForSingleFile tbody").html("");
	$("#singleFileDataUploadSection").hide();
	$("#bulkFileUploadSection").hide();
	$("#registerFileBtnsDiv").hide();
	$("#displayGlobusUploadDiv").hide();
	$("#displayS3UploadDiv").hide();
	$("#folderNamesDiv").html("");
	$("#fileNamesDiv").html("");
	$("#globusEndPointInformation").html("");
}

function registerBulkDataFile() {
	var uploadType = $('input[name=datafileTypeUpload]:checked').val();
	var usermetaDataEntered = true;

	if (uploadType == 'singleData') {
		var file = $("#doeDataFile").val();
		var dataFilePath = $("#dataFilePath").val();

		$('table#newMetaDataTableForSingleFile input[type="search"]').each(function() {
			var name = $(this).val();
			if (!name) {
				usermetaDataEntered = false;
			}
		});

		if (!file || !dataFilePath) {
			$(".uploadBulkDataError").show();
			$(".uploadBulkDataErrorMsg").html("Upload data source file.")
		} else if (!usermetaDataEntered) {
			$(".uploadBulkDataError").show();
			$(".uploadBulkDataErrorMsg").html("Enter the values for all file metadata.");
		} else if (dataFilePath && file) {
			$("#registerDataFileForm").attr('dataFilePath', dataFilePath);
			var form = $('#registerDataFileForm')[0];
			var data = new FormData(form);
			data.append('dataFilePath', dataFilePath);
			$.ajax({
				type : "POST",
				enctype : "multipart/form-data",
				url : "/addDatafile",
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
					if (msg && msg == 'Not Authorized') {
						location.replace("/loginTab");
					}
					if (msg && msg.indexOf("The system has registered your file") != -1) {
						console.log('SUCCESS: ', msg);
						$('body,html').animate({
							scrollTop : 0
						}, 500);
						$(".uploadBulkDataError").hide();
						$(".uploadBulkDataErrorMsg").html("");
						$(".registerBulkDataFile").html(msg);
						$(".registerBulkDataFileSuccess").show();
						cancelAndReturnToUploadTab();
					} else {
						console.log('ERROR: ', msg);
						$(".uploadBulkDataError").show();
						$(".uploadBulkDataErrorMsg").html(msg);
					}

				},
				error : function(e) {
					$("#spinner").hide();
					$("#dimmer").hide();
					console.log('ERROR: ', e);
					$(".uploadBulkDataError").show();
					$(".uploadBulkDataErrorMsg").html(e);
				}
			});
		}

	} else {
		var dataFilePath = $("#bulkDataFilePathCollection").val();
		var bulkUploadType = $('input[name=datafileTypeUpload]:checked').val();
		var validate = true;
		$('form#registerBulkDataForm input[type="search"]').each(function() {
			if ($(this).is(":visible") && !$(this).val()) {
				validate = false;
			}
		});

		if ((bulkUploadType == 's3' || bulkUploadType == 'cloud') && !validate) {
			$(".uploadBulkDataError").show();
			$(".uploadBulkDataErrorMsg").html("Enter all the required fields.")
		} else if (bulkUploadType == 'globus' && !$("#globusEndPointInformation").length) {
			$(".uploadBulkDataError").show();
			$(".uploadBulkDataErrorMsg").html("Select globus end point information.")
		} else if (bulkUploadType == 'drive' && (!$("#fileNamesDiv").length || !$("#folderNamesDiv").length)) {
			$(".uploadBulkDataError").show();
			$(".uploadBulkDataErrorMsg").html("Select google drive information.")
		}  else if (dataFilePath) {
			$("#uploadType").val(bulkUploadType);
			$("#bulkDatafilePath").val(dataFilePath);
			var data = $('#registerBulkDataForm').serialize();
			$
					.ajax({
						type : "POST",
						url : "/addbulk",
						data : data,
						beforeSend : function() {
							$("#spinner").show();
							$("#dimmer").show();
						},
						success : function(msg) {
							$("#spinner").hide();
							$("#dimmer").hide();
							if (msg && msg == 'Not Authorized') {
								location.replace("/loginTab");
							}
							if (msg
									&& msg
											.indexOf("Your bulk data file registration request has the following task ID") != -1) {
								console.log('SUCCESS: ', msg);
								$('body,html').animate({
									scrollTop : 0
								}, 500);
								$(".uploadBulkDataError").hide();
								$(".uploadBulkDataErrorMsg").html("");
								$(".registerBulkDataFile").html(msg);
								$(".registerBulkDataFileSuccess").show();
								cancelAndReturnToUploadTab();
							} else {
								$(".uploadBulkDataError").show();
								$(".uploadBulkDataErrorMsg").html(msg);
							}

						},
						error : function(e) {
							$("#spinner").hide();
							$("#dimmer").hide();
							console.log('ERROR: ', e);
							if (e == 'Not Authorized') {
								location.replace("/loginTab");
							}
							$(".uploadBulkDataError").show();
							$(".uploadBulkDataErrorMsg").html(e);
						}
					});
		}
	}

}

function appendFileName($this) {
	// Append the file name to the data file path
	var filename = $this.val().replace(/^C:\\fakepath\\/, "")
	var value = $("#bulkDataFilePathCollection").val() + "/" + filename;
	$("#dataFilePath").val(value);

}

function displayDataFileSection(value) {
	$(".registerBulkDataFileSuccess").hide();
	$(".registerBulkDataFile").html("");
	$("#registerFileBtnsDiv").show();

	if (value == 'singleData') {
		$("#singleFileDataUploadSection").show();
		$("#bulkFileUploadSection").hide();
		$("#doeDataFile").val("");
		$("#registerBulkDataFileBtn").prop("disabled", true);

	} else if (value == 'globus') {
		$("#singleFileDataUploadSection").hide();
		$("#bulkFileUploadSection").show();
		$("#displayGlobusUploadDiv").show();
		$("#displayS3UploadDiv").hide();
		$("#fileNamesDiv").show();
		$("#folderNamesDiv").show();
		$("#displayDriveUploadDiv").hide();
		$("#displayCloudUploadDiv").hide();
		$("#registerBulkDataFileBtn").prop("disabled", true);
		$("#fileNamesDiv").html("");
		$("#folderNamesDiv").html("");
		$("#globusEndPointInformation").html("");
	} else if (value == 's3') {
		$("#singleFileDataUploadSection").hide();
		$("#bulkFileUploadSection").show();
		$("#displayGlobusUploadDiv").hide();
		$("#displayS3UploadDiv").show();
		$("#fileNamesDiv").hide();
		$("#folderNamesDiv").hide();
		$("#displayDriveUploadDiv").hide();
		$("#displayCloudUploadDiv").hide();
		$("#registerBulkDataFileBtn").prop("disabled", false);

	} else if (value == 'drive') {
		$("#driveUploadAuthlink").prop("disabled",false);
		$("#singleFileDataUploadSection").hide();
		$("#bulkFileUploadSection").show();
		$("#displayGlobusUploadDiv").hide();
		$("#displayS3UploadDiv").hide();
		$("#fileNamesDiv").hide();
		$("#folderNamesDiv").hide();
		$("#displayDriveUploadDiv").show();
		$("#displayCloudUploadDiv").hide();
		$("#driveDiv").hide();
		$("#driveAuthorisedMsg").hide();
		$("#cloudAuthorisedMsg").hide();
		$("#fileNamesDiv").html("");
		$("#folderNamesDiv").html("");
		$("#registerBulkDataFileBtn").prop("disabled", true);
	} else if (value == 'cloud') {
		$("#cloudUploadAuthlink").prop("disabled",false);
		$("#singleFileDataUploadSection").hide();
		$("#bulkFileUploadSection").show();
		$("#displayGlobusUploadDiv").hide();
		$("#displayS3UploadDiv").hide();
		$("#fileNamesDiv").hide();
		$("#folderNamesDiv").hide();
		$("#displayDriveUploadDiv").hide();
		$("#displayCloudUploadDiv").show();
		$("#driveDiv").hide();
		$("#driveAuthorisedMsg").hide();
		$("#cloudAuthorisedMsg").hide();
		$("#fileNamesDiv").html("");
		$("#folderNamesDiv").html("");
		$("#registerBulkDataFileBtn").prop("disabled", true);
	}

}

function cancelAndReturnToUploadTab() {
	constructAssetFileAndFoldersDiv($("#dataList").val());
}