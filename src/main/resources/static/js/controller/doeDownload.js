
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

function loadDownloadPicker() {
	gapi.load('auth', {'callback': onAuthApiLoad});
	gapi.load('picker', {
		'callback' : onDownloadPickerApiLoad
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

function onDownloadPickerApiLoad() {
	pickerApiLoaded = true;
	createDownloadPicker();
}

function handleAuthResult(authResult) {
    if (authResult && !authResult.error) {
      oauthToken = authResult.access_token;
      $("#accessToken").val(oauthToken);
      createUploadPicker();
    }
  }


function createDownloadPicker() {
	if (pickerApiLoaded && oauthToken) {
		var view = new google.picker.DocsView(google.picker.ViewId.FOLDERS);
		view.setIncludeFolders(true);
		view.setSelectFolderEnabled(true);
		view.setParent("root");
		var picker = new google.picker.PickerBuilder()
		.setOAuthToken(oauthToken).addView(view)
		.setCallback(downloadPickerCallback).setTitle(
				"Select Folder").build();
		picker.setVisible(true);
	}
}

// Callback implementation for download picker.
function downloadPickerCallback(data) {
	if (data.action == google.picker.Action.PICKED) {
		var fileId = data.docs[0].id;
		constructPath(fileId, "");
	}
}

/**
 * Construct full path using file's parents.
 *
 * @param {String} fileId ID of the file/folder.
 * @param {String} path Child path to prepend to.
 */
function constructPath(fileId, path) {
	gapi.client.load('drive', 'v2', function() {
		var request = gapi.client.drive.files.get({
			'fileId' : fileId
		});
		request.execute(function(resp) {
			console.log('Folder: ' + resp.title);
			path = resp.title + '/' + path
			if (resp.parents[0]) {
				if (resp.parents[0].isRoot) {
					console.log('The user selected: ' + path);
					if(downloadType == "data_object")
						path = path + downloadFileName;
					$("#drivePath").val(path);
				} else {
					constructPath(resp.parents[0].id, path)
				}
			}
		});
	});
}