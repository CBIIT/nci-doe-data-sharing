$(document).ready(function () {
	    console.log("initialize dirty checking");
	    $('form.dirty-check').areYouSure();
	    
	    // Auto-lowercase Email Username (Usernames are always stored in lowercase)
	    $('#username').blur(function() {
	    	$(this).val($(this).val().trim().toLowerCase());
	    });
	    
	    $("#id_user_email").blur(function() {
	    	$(this).val($(this).val().trim().toLowerCase());
	    });
	    
	    $(".loginFieldsTextBox").keyup(function() {
	    	if($(this).val()) {
	    		$(this).parent().find('.TextField-floatingLabel-qefpP').show();
	    	} else {
	    		$(this).parent().find('.TextField-floatingLabel-qefpP').hide();
	    	}
	    });
	    

	    $(window).scroll(function () {
	        if ($(this).scrollTop() >= 50) {        // If page is scrolled more than 50px
	            $('#return-to-top').show(200);    // Fade in the arrow
	        } else {
	            $('#return-to-top').hide(200);   // Else fade out the arrow
	        }
	    });
	    
	    $('#return-to-top').click(function () {      // When arrow is clicked
	        $('body,html').animate({
	            scrollTop: 0                       // Scroll to top of body
	        }, 500);
	    });

	    $('.simple-select2').select2({
	        theme: 'bootstrap4',
	        allowClear: false
	    });

	    $('.simple-select2-sm').select2({
	        theme: 'bootstrap4',
	        containerCssClass: ':all:',
	        placeholder: "",
	        allowClear: false,
	    });
	    
	    $('[data-toggle="tooltip"]').tooltip();
	    
	    loadJsonData('/search/search-list', $("#metadatalist"), true, null, null, null, "key", "value");
	   


	    
$("#searchBtn").click(function(e){
	e.preventDefault();	
	//$(".errorFilterCriteria").hide();
	populateSearchCriteria('simpleSearch');
	/*if(!$("#attributeVal").val()) {
		$(".keywordError").show();
	} else {
		$(".keywordError").hide();
		
	}*/
});

$("#displayAllResults").click(function(e){
	e.preventDefault();
	//$(".keywordError").hide();
	//$(".errorFilterCriteria").hide();
   populateSearchCriteria('displayAllResults');
});

$("#advSearchBtn").click(function(e){
	e.preventDefault();
	//$(".keywordError").hide();
	var advLength = $("div#metadatalisting .filteritem").length;
		  /* if (advLength == 0){
			$(".errorFilterCriteria").show();
		} else { 
			 $(".errorFilterCriteria").hide();
			
		}*/	
	 populateSearchCriteria('advSearchBtn');
});

$("#downloadSelected").click(function(e){
	var selectedPaths = [];
    $("#searchResultTable tbody input[type=checkbox]:checked").each(function () {
    	selectedPaths.push($(this).attr('id'));
    });
    if(selectedPaths.length == 0) {
    	$("#searchResultTable tbody input[type=radio]:checked").each(function () {
	    	selectedPaths.push($(this).attr('id'));
	    });
    }
    
    if(selectedPaths.length == 1) {
  	location.replace('/downloadTab?selectedPaths='+selectedPaths+'&&downloadAsyncType=collection');
  } else  {
  	$("#downloadType").val("collectionfiles");
  	location.replace('/downloadTab?selectedPaths='+selectedPaths+'&&downloadAsyncType=collectionfiles');
  }
    
    
});


var selectedPathsString = $("#selectedPathsString").val();
var downloadType = $("#downloadType").val();
var downloadFileName = $("#downloadFileName").val();
var asyncSearchType = $("#asyncSearchType").val();

  if(selectedPathsString && downloadType && (downloadType == 'collection' || downloadType == 'collectionfiles')) {
	  $("#syncRadioSet").hide();	
	  $("#SyncDiv").hide();
	  if(asyncSearchType) {
		  $("input[name=searchType][value="+asyncSearchType+"]").click();
	  }
	  
	  var selectedPaths = selectedPathsString.split(',');
    $("#selectedFilesList").val(selectedPaths);
    
    $.each(selectedPaths, function(index, value) {
    	$(".selectedFilesListDisplay").append("<p>"+value+"</p>");
    });
    $(".selectedFilesDiv").show();
    
    if(selectedPaths.length == 1) {
	  $("#destinationPathId").val(selectedPaths);
     }
    
	$("#informationalText").html("This page allows you to download the " +
			"selected data files " +
			"asynchronously to a Globus endpoint location, or an S3 bucket, or Google Drive.");
} else if(selectedPathsString && downloadType && (downloadType == 'data_object' || downloadType == 'datafiles')) {

	if(downloadFileName && downloadFileName != "null") {
		$("#syncRadioSet").show();
		$(".selectedFilesListDisplay").append("<p>"+selectedPathsString+"</p>");
		$(".selectedFilesDiv").show();
		$("#destinationPathId").val(selectedPathsString);
		$("#drivePath").val(downloadFileName);
		$("#informationalText").html("This page allows you to download the " +
				"selected data file either synchronously to your computer or asynchronously " +
				"to Globus endpoint location, or an S3 bucket, or Google Drive.");
		if(asyncSearchType) {
			 $("input[name=searchType][value="+asyncSearchType+"]").click();
		  } else {
			  $("#searchTypeSync").click(); 
		  }
		
	} else {
		 $("#syncRadioSet").hide();	
		  $("#SyncDiv").hide();
		  if(asyncSearchType) {
			  $("input[name=searchType][value="+asyncSearchType+"]").click();
		  }
		$("#informationalText").html("This page allows you to download the " +
				"selected data files " +
				"asynchronously to a Globus endpoint location, or an S3 bucket, or Google Drive.");
		
		  var selectedPaths = selectedPathsString.split(',');
		    $("#selectedFilesList").val(selectedPaths);
		    
		    $.each(selectedPaths, function(index, value) {
		    	$(".selectedFilesListDisplay").append("<p>"+value+"</p>");
		    });
		    $(".selectedFilesDiv").show();
		    
		    if(selectedPaths.length == 1) {
			  $("#destinationPathId").val(selectedPaths);
		     }
	} 
	
}

$("#download-btn").click(function(e){
	e.preventDefault();
	var selectedFiles = $("#selectedFilesList").val();
	var searchType = $('input[name=searchType]:checked').val();
	var d = {};
	d.searchType = searchType;
	d.destinationPath = $("#destinationPathId").val();
	d.downloadType = $("#downloadType").val();
	d.downloadFileName = $("#downloadFileNameVal").val();
	$("#message").hide();
	var validate = true;
	
	if(!searchType) {
		 $("#download-modal").find('.downloadErrorMsg').html("Enter the download type.");
		 $("#download-modal").find("#message").show();
	}
	
	else if(searchType == 's3' || searchType == 'async' || searchType == 'drive'  || selectedFiles) {
		d.bucketName = $("#downloadBucketName").val();
		d.s3Path = $("#downloadS3Path").val();
	    d.accessKey = $("#downloadAccessKey").val();
	    d.secretKey = $("#downloadSecretKey").val();
	    d.region = 	$("#downloadRegion").val();	
	    d.endPointName = $("#endPointName").val();
		d.endPointLocation = $("#endPointLocation").val();
		d.drivePath = $("#drivePath").val();
		
		var url;
		if(selectedFiles) {
			url = "/downloadfiles/download";
			d.selectedPaths = selectedFiles;
		} else {
			url = "/download";
			
		}
		if(searchType == 'async') {
			$('div#AsyncDiv input[type="text"]').each(function(){
		        if(!$(this).val()){
		        	validate = false;
		        }          
		    });
		} else if(searchType == 's3') {
			$('div#s3Div input[type="text"]').each(function(){
		        if(!$(this).val()){
		        	validate = false;
		        }          
		    });
		} else if(searchType == 'drive') {
			$('div#driveDiv input[type="text"]').each(function(){
		        if(!$(this).val()){
		        	validate = false;
		        }          
		    });
		}
	
		if(!validate) {
			 $('.downloadErrorMsg').html("Enter all the criteria.");
			 $("#message").show();
		} else {
					   								
			$.ajax({
				type : "POST",
			     url : url,
			     contentType : 'application/json',
				 data : JSON.stringify(d),
				 beforeSend: function () {
			    	   $("#spinner").show();
			           $("#dimmer").show();
			       },
				 success : function(msg) {
					 $("#spinner").hide();
			         $("#dimmer").hide();
					 console.log('SUCCESS: ', msg);
					 
					 if(msg && msg.message && msg.message.indexOf("Download request is not successful:") != -1) {
						 $('.downloadErrorMsg').html(msg.message);
						 $("#message").show();
						 $('.downloadSuccessMsg').html("");
						 $("#successBlockDownload").hide();
					 } else {
						 $('.downloadSuccessMsg').html(msg.message);
						 $("#successBlockDownload").show();
						 $('.downloadErrorMsg').html("");
						 $("#message").hide();
					 }
					
					
				 },
				error : function(e) {
					 console.log('ERROR: ', e);
					 $("#spinner").hide();
			         $("#dimmer").hide();
					 $('#downloadErrorMsg').html(e.message);
					 $("#message").show();
				}
			});
	   }
	} else {
		$('div#SyncDiv input[type="text"]').each(function(){
	        if(!$(this).val()){
	        	validate = false;
	        }          
	    });
		if(!validate) {
			 $('.downloadErrorMsg').html("Enter all the criteria.");
			 $("#message").show();
		} else {
		  $('#downloadSyncForm').attr('action', '/downloadsync');
		  $("#downloadSyncForm").submit();
		}
	}
		
	
});

$("#btnRegister").click(function(e){
	callRegisterFormValidation();
	
});	

$("#loginButton").click(function(e){
	validateUserLogin();
	
});	

$("#btnforgotPassword").click(function(e){	
	validateForgotPassword();
});

$("#btnSubmitPswdLink").click(function(e){
	var params= {emailAddr:$('#forgotPasswordLightbox').find("#txtResetPswdLink").val()};
	
	invokeAjax('/resetPasswordLink','GET',params,postResetLinkFunction,null,null,'text');
});


$("#logout").click(function(e){
	invokeAjax('/logOut','POST',null,postLogOutFunction,null,null,'text');
});

$("#register-tab").click(function(e){
	$("#registrationTab").show();
	$("#loginSubTab").hide();
});

$("#returnToLoginForm").click(function(e){
	$("#registrationTab").hide();
	$("#loginSubTab").show();
});

invokeAjax('/user-info','GET',{emailAddr:$("#emailAddrTxt").text()},postGetUserInfoFunction,null,null,'text');


$("#btnUpdateProfile").click(function(e){
	var d= {};
	d.firstName = $("#firstNameTxt").val();
	d.lastName = $("#lastNameTxt").val();
	d.institution = $("#institutionTxt").val();
	d.emailAddrr = $("#emailAddrTxt").text();
	
	invokeAjax('/user-info','POST',JSON.stringify(d),postUpdateUserFunction,null,null,'text');
});


$("#resetAdvSearchBtn").click(function(e){
	//$(".keywordError").hide();
	//$(".errorFilterCriteria").hide();
	 $('#metadatalisting').empty();
	 $("#searchResultsDiv").hide();
	 $("#advSearchDiv").hide();
	  loadJsonData('/search/search-list', $("#metadatalist"), true, null, null, null, "key", "value");
	 
});

$("#resetBtn").click(function(e){
	//$(".keywordError").hide();
	//$(".errorFilterCriteria").hide();
	$("#attributeVal").val("");
	 $("#searchResultsDiv").hide();
	 $("#advSearchDiv").hide();
	$('#metadatalisting').empty();

});
	
$(".backToSearch").click(function(e){
	$("#searchFragmentDiv").show();
    $("#dataSetFragment").hide();
	$("#editCollectionFragment").hide();
});

$("#backtoSearch").click(function(e){
	location.replace("/searchTab?returnToSearch=true");
});

$('body').on('click', 'a.button.closeBtn', function () {
    $(this).closest('div.popover').popover('hide');
});


$('body').on('click', function (e) {
    $('[data-toggle=popover]').each(function () {
        if (!$(this).is(e.target) && $(this).has(e.target).length === 0 && $('.popover').has(e.target).length === 0) {
            (($(this).popover('hide').data('bs.popover') || {}).inState || {}).click = false;
        }
    });
});

$("#addMetaData").click(function(e){
	addCollectionMetaDataRows();
});


$("#updateMetaData").click(function(e){
	updateMetaDataCollection();
});

$("#registerCollectionBtn").click(function(e){
	registerCollection();
});


$("#registerDataFileBtn").click(function(e){
	registerDataFile();
});


$("#doeDataFile").change(function (e) {	
	appendFileName($(this));

});

$("#addBulkDataFiles").click(function(e){
	$("#uploadDataFilesTab").show();
	openBulkDataRegistration();
});

$("#cancelBulkRegister").click(function(e){
	$("#uploadDataFilesTab").hide();
	$("#uploadSubFragmentTab").show();
		cancelAndReturnToUploadTab();
});

$("#registerBulkDataFileBtn").click(function(e){
	registerBulkDataFile();
});

$("#bulkDoeDataFile").change(function (e) {	
	appendBulkFileName($(this));

});


$("#driveAuthlink").click(function(e){
	var params= {type:$("#downloadType").val(),downloadFilePath:$("#selectedFilesList").val()}

	 invokeAjax('/download','GET',params,postGoogleDriveFunction,null,null,'text');
});

$("#primaryGlobusButton").click(function(e){
	
	var d = {};
	d.institutionPath =$("#instituteList").val();
	d.studyPath = $("#studyList").val();
	d.dataSetPath = $("#dataList").val();

	 invokeAjax('/upload','GET',d,postUploadGlobusFunction,null,null,'text');
});

$("#driveUploadAuthlink").click(function(e){
	var d = {};
	d.institutionPath =$("#instituteList").val();
	d.studyPath = $("#studyList").val();
	d.dataSetPath = $("#dataList").val();
    d.action ="Drive";
	 invokeAjax('/upload','GET',d,postUploadGlobusFunction,null,null,'text');
});

function postGoogleDriveFunction(data,status) {
	location.replace(data);
}
$(".addNewMetaDataForDataFiles").click(function(e){
	addNewMetaDataRowsForDataFile($(this));
});

$("#updatePermissions").click(function(e){
	editPermissionsOpenModal();
});

$("#btnUpdatePermissions").click(function(e) {
	updatePermissionsFunction();
});
$("#btnUpdateAccessGroup").click(function(e){
	updateAccessGroupsFunction();
});

$(".select2-selection").removeAttr("role");
$(".select2-search__field").removeAttr("role");
$(".select2-search__field").attr("aria-label", "textbox");
$(".select2-search__field").attr("type", "textbox");

$(document).ajaxStop(function () {
	   console.log("Last ajax call completed");
	   $(".select2-selection").removeAttr("role");
	   $(".select2-search__field").removeAttr("role");
	   $(".select2-search__field").attr("aria-label", "textbox");
	   $(".select2-search__field").attr("type", "textbox");
});

});

$(document).on('change', '#publicAccess', function() {
	if($(this).is(":checked")) {
		$("#registerCollectionModal").find("#accessGroupSelect").next(".select2-container").hide();
		
	} else {
		$("#registerCollectionModal").find("#accessGroupSelect").next(".select2-container").show();
	}

});

$(document).on('change', '#editPublicAccess', function() {
	if($(this).is(":checked")) {
		$("#updateAccessPermissionsModal").find("#updateAccessGroupsList").next(".select2-container").hide();
		$("#updateAccessPermissionsModal").find("#updateAccessGroupsList").val("").trigger('change');
		
	} else {
		$("#updateAccessPermissionsModal").find("#updateAccessGroupsList").next(".select2-container").show();
	}

});

$(document).on('click', '.notifyUsersLink', function(){
	notifyUsersFunction($(this).attr('notify_permissions'));
});

$(document).on('click', '#pickerUploadLink', function(){
	loadUploadPicker();
});
$(document).on('click', '#pickerLink', function(){
	loadDownloadPicker();
});