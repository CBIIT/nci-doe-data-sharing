var scope = [ 'https://www.googleapis.com/auth/drive.file' ];
var pickerApiLoaded = false;

function loadUploadPicker() {
	gapi.load('picker', {
		'callback' : onUploadPickerApiLoad
	});
	gapi.load('client', start);
}

function onUploadPickerApiLoad() {
	pickerApiLoaded = true;
	createUploadPicker();
}

function start() {
	gapi.client.setToken({
		access_token : oauthToken
	})
}

function createUploadPicker() {
	if (pickerApiLoaded && oauthToken) {
		var view = new google.picker.DocsView(google.picker.ViewId.DOCS);
		view.setIncludeFolders(true);
		view.setSelectFolderEnabled(true);
		view.setParent("root");
		var picker = new google.picker.PickerBuilder()
		.setOAuthToken(oauthToken).addView(view)
		.enableFeature(google.picker.Feature.MULTISELECT_ENABLED)
		.setCallback(uploadPickerCallback).setTitle(
				"Select Files/Folders").build();
		picker.setVisible(true);
	}
}

// Callback implementation for download picker.
function uploadPickerCallback(data) {
	if (data.action == google.picker.Action.PICKED) {
		// get all selected files
        var files = data[google.picker.Response.DOCUMENTS];
        
        // loop over selected files 
        for (var i = 0; i < files.length; i++) {
            populateSelection(files[i][google.picker.Document.ID])
        }
	}
}

/**
 * Populate selected file/folder.
 *
 * @param {String} fileId ID of the file/folder.
 */
function populateSelection(fileId) {
	gapi.client.load('drive', 'v3', function() {
		var request = gapi.client.drive.files.get({
			'fileId' : fileId
		});
		
		var filesNamesDivHtml = '<div class="form-group col-4 assetBulkGlobusLabels">'
								 + '<span class="assetGlobusSelectionFields" style="padding-left: 10px;">SELECTED FILES'
								 + '</span>'
								 + '</div>'
								 + '<div class="form-group col-lg-6 col-md-6" id="googledriveFilesContent" style="margin-top: 9px;margin-left: 3px;"></div>';
		var folderNamesDivHtml = '<br /><div class="form-group col-4 assetBulkGlobusLabels">'
								 + ' <span class="assetGlobusSelectionFields" style="padding-left: 10px;">SELECTED FOLDERS'
								 + '</span></div>'
								 + '<div class="form-group col-lg-6 col-md-6" id="googledriveFolderContent" style="margin-top: 9px;margin-left: 3px;"></div>';				 
								 
		$("#fileNamesDiv").html(filesNamesDivHtml);
		$("#folderNamesDiv").html(folderNamesDivHtml);
		$("#fileNamesDiv").css({   
		    "margin-top": "45px", 
		    "background" : "#EEF4F6",
		    "display": "none"         
		});
		$("#folderNamesDiv").css({   
		    "margin-top": "3px",
		    "background" : "#EEF4F6" ,
		    "display": "none"        
		});
		
		request.execute(function(resp) {
			if (resp.mimeType == 'application/vnd.google-apps.folder') {
				console.log('Folder: ' + resp.name);
				var folderHtml = '<span class="googledrive_labels" id="' + resp.id + '_label"></span><input type="hidden" id="' + resp.id + '" name="folderNames">'
				+ '<input type="hidden" name="folderIds" value="' + resp.id + '"><br/>';
				$("#googledriveFolderContent").append(folderHtml);
				$("#folderNamesDiv").show();
			} else {
				console.log('File: ' + resp.name);
				var fileHtml = '<span class="googledrive_labels" id="' + resp.id + '_label"></span><input type="hidden" id="' + resp.id + '" name="fileNames">'
				+ '<input type="hidden" name="fileIds" value="' + resp.id + '"><br/>';
				$("#googledriveFilesContent").append(fileHtml);
				$("#fileNamesDiv").show();
			}
			$("#registerBulkDataFileBtn").prop("disabled",false);
			constructPath(resp.id, "", resp.id);
		});
	});
}

/**
 * Construct full path using file's parents.
 *
 * @param {String} fileId ID of the file/folder.
 * @param {String} path Child path to prepend to.
 */
function constructPath(fileId, path, origFileId) {
	gapi.client.load('drive', 'v3', function() {
		var request = gapi.client.drive.files.get({
			'fileId' : fileId
		});
		request.execute(function(resp) {
			console.log('Folder: ' + resp.name);
			var folder = resp.name;
			request = gapi.client.drive.files.get({
				'fileId' : fileId,
				'fields' : 'parents'
			});
			request.execute(function(resp) {
			if (resp) {
				if (resp.parents) {
					if (fileId == origFileId)
						path = folder;
					else
						path = folder + '/' + path;
					constructPath(resp.parents[0], path, origFileId)
				} else {
					console.log('The user selected: ' + path);
					$("#" + origFileId + "_label").html(path);
					$("#" + origFileId).val(path);
				}
			}
			});
		});
	});
}