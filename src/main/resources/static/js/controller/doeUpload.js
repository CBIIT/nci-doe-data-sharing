
// The Browser API key obtained from the Google API Console.
// Replace with your own Browser API key, or your own key.
var developerKey = 'AIzaSyDUlmQM6zthuhrlH9J0GaKQpk49XRWGPkA';

// The Client ID obtained from the Google API Console. Replace with your own Client ID.
var clientId = "380839902969-444n7bnntgi67d76mk6o61q0ok0tdsb0.apps.googleusercontent.com"

// Replace with your own project number from console.developers.google.com.
// See "Project number" under "IAM & Admin" > "Settings"
var appId = "380839902969";
var scope = [ 'https://www.googleapis.com/auth/drive.file' ];
var pickerApiLoaded = false;
var oauthToken;

function loadUploadPicker() {
	gapi.load('auth', {'callback': onAuthApiLoad});
	gapi.load('picker', {
		'callback' : onUploadPickerApiLoad
	});
	gapi.load('client');
}

function onAuthApiLoad() {
    window.gapi.auth.authorize(
        {
          'client_id': clientId,
          'scope': scope,
          'immediate': false
        },
        handleAuthResult);
  }

function onUploadPickerApiLoad() {
	pickerApiLoaded = true;
	createUploadPicker();
}

function handleAuthResult(authResult) {
    if (authResult && !authResult.error) {
      oauthToken = authResult.access_token;
      $("#accessToken").val(oauthToken);
      createUploadPicker();
    }
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
				"Select Files or Folder").build();
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
	gapi.client.load('drive', 'v2', function() {
		var request = gapi.client.drive.files.get({
			'fileId' : fileId
		});
		$("#fileNamesDiv").html('<label for="fileName"><b>Selected Files:</b></label>');
		$("#folderNamesDiv").html('<br /><label for="folderName"><b>Selected Folders:</b></label>');
		request.execute(function(resp) {
			if (resp.mimeType == 'application/vnd.google-apps.folder') {
				console.log('Folder: ' + resp.title);
				var folderHtml = '<div><label id="' + resp.id + '_label"></label><input type="hidden" id="' + resp.id + '" name="folderNames"></div>'
				+ '<input type="hidden" name="folderIds" value="' + resp.id + '">';
				$("#folderNamesDiv").append(folderHtml);
				$("#folderNamesDiv").show();
			} else {
				console.log('File: ' + resp.title);
				var fileHtml = '<div><label id="' + resp.id + '_label"></label><input type="hidden" id="' + resp.id + '" name="fileNames"></div>'
				+ '<input type="hidden" name="fileIds" value="' + resp.id + '">';
				$("#fileNamesDiv").append(fileHtml);
				$("#fileNamesDiv").show();
			}
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
	gapi.client.load('drive', 'v2', function() {
		var request = gapi.client.drive.files.get({
			'fileId' : fileId
		});
		request.execute(function(resp) {
			console.log('Folder: ' + resp.title);
			if (fileId == origFileId)
				path = resp.title;
			else
				path = resp.title + '/' + path
			if (resp.parents[0]) {
				if (resp.parents[0].isRoot) {
					console.log('The user selected: ' + path);
					$("#" + origFileId + "_label").html(path);
					$("#" + origFileId).val(path);
				} else {
					constructPath(resp.parents[0].id, path, origFileId)
				}
			}
		});
	});
}