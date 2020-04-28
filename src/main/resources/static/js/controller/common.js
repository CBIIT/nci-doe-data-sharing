$(document).ready(function () {
	
	var uploadtabIniatialize = false;
	    console.log("initialize dirty checking");
	    $('form.dirty-check').areYouSure();
	    
	    // Auto-lowercase Email Username (Usernames are always stored in lowercase)
	    $('#username').blur(function() {
	    	$(this).val($(this).val().trim().toLowerCase());
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
	    
	    loadJsonData('/search/adv-search-list', $("#metadatalist"), true, null, null, null, "displayName", "displayName");
	   


	    
$("#searchBtn").click(function(e){
	e.preventDefault();
	$("#searchResultsDiv").show();
	 populateSearchCriteria('simpleSearch');
	refreshDataTable();
	
});

$("#displayAllResults").click(function(e){
	e.preventDefault();
	$("#searchResultsDiv").show();
	 populateSearchCriteria('displayAllResults');
	refreshDataTable();
});

$("#advSearchBtn").click(function(e){
	e.preventDefault();
	$("#searchResultsDiv").show();
	 populateSearchCriteria('advSearchBtn');
	refreshDataTable();
});

$("#downloadSelected").click(function(e){
	$("#download-modal").find("input[type=radio]").prop("checked", "").end();
	$("#download-modal").find(".selectedFilesListDisplay").html("");
	$("#download-modal").find('.downloadErrorMsg').html("");
	$("#download-modal").find("#message").hide();
	$("#download-modal").find("#AsyncDiv").hide();
	$("#download-modal").find("#s3Div").hide();
    $("#download-modal").find("div#AsyncDiv input[type='text']").val("");
    $("#download-modal").find("div#s3Div input[type='text']").val("");
	var selectedPaths = [];
	    $("#searchResultTable tbody input[type=checkbox]:checked").each(function () {
	    	selectedPaths.push($(this).attr('id'));
	    });
	    $("#download-modal").find("#selectedFilesList").val(selectedPaths);
	    
	    $.each(selectedPaths, function(index, value) {
	    	$("#download-modal").find(".selectedFilesListDisplay").append("<p>"+value+"</p>");
	    });
	    
	    if(selectedPaths.length == 1) {
	    	  $("#download-modal").find("#destinationPathId").val(selectedPaths);
	    	  $("#download-modal").find("#downloadType").val("collection");
	    } else  {
	    	$("#download-modal").find("#downloadType").val("collectionfiles");
	    }
	    $("#download-modal").find("#SyncDiv").hide();
		$("#download-modal").find("#syncRadioSet").hide();		
		$("#download-modal").find(".selectedFilesDiv").show();
	    $("#download-modal").modal('show');
});



$("#download-btn").click(function(e){
	e.preventDefault();
	var selectedFiles = $(".selectedFilesList").val();
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
	
	else if(searchType == 's3' || searchType == 'async' || selectedFiles) {
		d.bucketName = $("#bucketName").val();
		d.s3Path = $("#s3Path").val();
	    d.accessKey = $("#accessKey").val();
	    d.secretKey = $("#secretKey").val();
	    d.region = 	$("#region").val();	
	    d.endPointName = $("#endPointName").val();
		d.endPointLocation = $("#endPointLocation").val();
		
		var url;
		if(selectedFiles && selectedFiles.len > 1) {
			url = "/downloadfiles/download";
			d.selectedFiles = selectedFiles;
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
		}
	
		if(!validate) {
			 $("#download-modal").find('.downloadErrorMsg').html("Enter all the criteria.");
			 $("#download-modal").find("#message").show();
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
					 $("#download-modal").find('.downloadErrorMsg').html(msg.message);
					 $("#download-modal").find("#message").show();
				 },
				error : function(e) {
					 console.log('ERROR: ', e);
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
			 $("#download-modal").find('.downloadErrorMsg').html("Enter all the criteria.");
			 $("#download-modal").find("#message").show();
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

$("#myaccountTab").click(function(e){
	$("#landingDiv").hide();
	$("#myAccount").show();
	$("#changePassword").hide();
	var params= {emailAddr:$("#emailAddrTxt").text()};
	invokeAjax('/user-info','GET',params,postGetUserInfoFunction,null,null,'text');
	
});

$("#cancelAccntUpdate").click(function(e){
	$("#landingDiv").show();
	$("#myAccount").hide();
	$("#changePassword").hide();
});


$("#changePswdTab").click(function(e){
	$("#landingDiv").hide();
	$("#myAccount").hide();
	$("#changePassword").show();
	$(".successBlock").hide();
	$(".errorBlock").hide();
});

$("#landing-tab").click(function(e){
	$("#landingDiv").show();
	$("#myAccount").hide();
	$("#changePassword").hide();
});

$("#cancelResetPswdBtn").click(function(e){
	$("#landingDiv").show();
	$("#myAccount").hide();
	$("#changePassword").hide();
});



$("#btnUpdateProfile").click(function(e){
	var d= {};
	d.firstName = $("#firstNameTxt").val();
	d.lastName = $("#lastNameTxt").val();
	d.institution = $("#institutionTxt").val();
	d.emailAddrr = $("#emailAddrTxt").text();
	
	invokeAjax('/user-info','POST',JSON.stringify(d),postUpdateUserFunction,null,null,'text');
});


$("#resetAdvSearchBtn").click(function(e){
	 $('#metadatalisting').empty();
	 $("#searchResultsDiv").hide();
	 
});

$("#resetBtn").click(function(e){
	$("#attributeVal").val("");
	 $("#searchResultsDiv").hide();
	$('#metadatalisting').empty();

});
	
$(".backToSearch").click(function(e){
	$("#searchFragmentDiv").show();
    $("#dataSetFragment").hide();
	$("#editCollectionFragment").hide();
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

$("#primaryGlobusButton").click(function(e){
	
	var d = {};
	d.institutionPath =$("#instituteList").val();
	d.studyPath = $("#studyList").val();
	d.dataSetPath = $("#dataList").val();

	 invokeAjax('/upload','GET',d,postUploadGlobusFunction,null,null,'text');
});

$(".addNewMetaDataForDataFiles").click(function(e){
	addNewMetaDataRowsForDataFile($(this));
});

loadUploadTab();
});
