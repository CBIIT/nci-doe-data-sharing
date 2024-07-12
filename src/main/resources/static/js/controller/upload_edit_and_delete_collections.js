$(document).ready(
		function() {			
			$(document).on(
					'click',
					'.editFolderMetadata',
					function() {
						
						var selectedPath = $(this).attr('metadata_path');
						var collectionId = $(this).attr('collectionId');
						var collectionType = "Folder";
						
						$("#editCollectionFragment").show();
						$("#uploadSectionDiv").hide();
						$("#uploadHeader").hide();
						$(".backToUploadTab").show();
						$(".backToUplaodTab_editCollection").show();
						$(".backToSearchBtn").hide();
						$(".backToSearch_editCollection").hide();

						$("#userMetaData tbody").html("");
						$("#path").val(selectedPath);
						$(".editCollectionSuccess").hide();
						$(".editCollectionMsg").html("");
						$(".editCollectionError").hide();
						$(".editCollectionErrorMsg").html("");
						$("#collectionId").val(collectionId);
						$("#isDataObject").val(false);
						var params = {
							selectedPath : selectedPath,
							collectionType : collectionType,
							refresh : false
						};
						invokeAjax('/addCollection', 'GET', params, constructEditCollectionMetadata, null, null, null);
		     });

			/*
			 * This function is to delete collection of type Folders and sub
			 * folders
			 */
			$(document).on(
					'click',
					'.deleteFolderCollectionBtn',
					function() {
						var path = $(this).attr('coll_path');
						var name = $(this).attr('coll_name');
						deleteFolderCollection(name, path, postSuccessDeleteCollectionFunctionOnUpload);
			});
});

function postSuccessDeleteCollectionFunctionOnUpload(data, status) {
	if (data != "SUCCESS") {
		return bootbox.alert(data);
	} else {
		constructAssetFileAndFoldersDiv($("#dataList").val());
	}
}

function functionEditCollection($this, collectionType) {
	var selectedValue = $("#" + $this + " option:selected").text();
	var textvalue = $("#" + $this + " option:selected").val();
	if (textvalue && textvalue != "ANY") {
		var params = {
			selectedPath : textvalue,
			collectionType : collectionType
		};
		invokeAjax('/collection/canEdit', 'GET', params, postSuccessEditCollectionMetadata, null, null, null);
	}
}

function postSuccessEditCollectionMetadata(data, status) {
	if (data && data.permissionRole == 'No Permissions') {
		bootbox.alert("No permissions to edit this collection");

	} else {
		$("#editCollectionFragment").show();
		$("#uploadSectionDiv").hide();
		$("#uploadHeader").hide();
		$(".backToUploadTab").show();
		$(".backToUplaodTab_editCollection").show();
		$(".backToSearchBtn").hide();
		$(".backToSearch_editCollection").hide();
		$("#userMetaData tbody").html("");
		$("#path").val(data.collectionPath);
		$(".editCollectionSuccess").hide();
		$(".editCollectionMsg").html("");
		$(".editCollectionError").hide();
		$(".editCollectionErrorMsg").html("");
		$("#collectionId").val(data.collectionId);
		$("#isDataObject").val(false);
		$("#editUserMetadataFileName").html(data.collectionName);

		if (data && data.permissionRole == 'Owner') {
			$("#updatePermissions").show();
		} else {
			$("#updatePermissions").hide();
		}

		var params1 = {
			selectedPath : data.collectionPath,
			collectionType : data.collectionType,
			refresh : false
		};
		invokeAjax('/addCollection', 'GET', params1, constructEditCollectionMetadata, null, null, null);
	}

}

/*
 * This function is to delete collections of type Program, Study and Asset
 */
function functionDelete($this, collectionType) {
	var selectedValue = $("#" + $this + " option:selected").text();
	var textvalue = $("#" + $this + " option:selected").val();
	if (textvalue && textvalue != "ANY") {
		bootbox.confirm({
			message : "Are you sure you want to delete " + selectedValue + "?",
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
						collPath : textvalue
					};
					$.ajax({
						type : "POST",
						url : "/deleteCollection",
						contentType : 'application/x-www-form-urlencoded; charset=UTF-8',
						dataType : 'text',
						data : params,
						beforeSend : function() {
							$("#spinner").show();
							$("#dimmer").show();
						},
						success : function(msg) {
							$("#spinner").hide();
							$("#dimmer").hide();
							console.log('SUCCESS: ', msg);
							if (msg && msg == 'Not Authorized') {
								location.replace("/loginTab");
							}
							postSuccessDeleteCollection(msg, collectionType);

						},
						error : function(e) {
							$("#spinner").hide();
							$("#dimmer").hide();
							console.log('ERROR: ', e);
							returnErrorMessage(e);
						}
					});
				}
			}
		});
	} else {
		bootbox.alert("Select a collection to delete.");
	}
}

function postSuccessDeleteCollection(data, collectionType) {
	if (data && data != "SUCCESS") {
		return bootbox.alert(data);
	} else if (data == 'SUCCESS') {
		// refresh select drop down
		if (collectionType == 'Program') {
			var params = {
				selectedPath : $("#basePath").val(),
				refreshNode : 'true'
			};
			loadJsonData('/browse/collection', $("#instituteList"), true, params, null, null, "key", "value");
			retrieveCollections('instituteList', 'ANY', 'deleteAction');
		} else if (collectionType == 'Study') {
			var params = {
				selectedPath : $("#instituteList").val(),
				refreshNode : 'true'
			};
			loadJsonData('/browse/collection', $("#studyList"), true, params, null, null, "key", "value");
			retrieveCollections('studyList', 'ANY', 'deleteAction');

		} else if (collectionType == 'Asset') {
			var params = {
				selectedPath : $("#studyList").val(),
				refreshNode : 'true'
			};
			loadJsonData('/browse/collection', $("#dataList"), true, params, null, null, "key", "value");
			retrieveCollections('dataList', 'ANY', 'deleteAction');
		}
	}
}

function retrieveCollections($this, selectedIndex, action) {

	$("#assetUploadDiv").removeClass('show');
	var selectTarget = $this;
	var params;
	var selectedValue;
	if (action == 'onChange') {
		selectedValue = selectedIndex.value;
		params = {
			selectedPath : selectedIndex.value
		};
		if (selectedIndex && selectedIndex.value != 'ANY') {
			$("#" + selectTarget + " option[value='ANY']").remove();
		}
	} else {
		selectedValue = selectedIndex;
		params = {
			selectedPath : selectedIndex
		};
	}
	resetOnChangeofSelectCollection(selectTarget, selectedValue);
}