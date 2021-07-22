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
	    
	    $("#txtResetPswdLink").blur(function() {
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
	    
	    
$("#searchBtn").click(function(e){
	e.preventDefault();	
	populateSearchCriteria('simpleSearch');
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
  	location.replace('/downloadTab?selectedPaths='+selectedPaths+'&&downloadAsyncType=collection&&returnToSearch=true');
  } else  {
  	$("#downloadType").val("collectionfiles");
  	location.replace('/downloadTab?selectedPaths='+selectedPaths+'&&downloadAsyncType=collectionfiles&&returnToSearch=true');
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
			"asynchronously to a Globus endpoint location, an S3 bucket, or Google Drive.");
} else if(selectedPathsString && downloadType && (downloadType == 'data_object' || downloadType == 'datafiles')) {

	if(downloadFileName && downloadFileName != "null" && downloadType == 'data_object') {
		$("#syncRadioSet").show();
		$(".selectedFilesListDisplay").append("<p>"+selectedPathsString+"</p>");
		$(".selectedFilesDiv").show();
		$("#destinationPathId").val(selectedPathsString);
		$("#drivePath").val(downloadFileName);
		$("#informationalText").html("This page allows you to download the " +
				"selected data file either synchronously to your computer or asynchronously " +
				"to Globus endpoint location, an S3 bucket, or Google Drive.");
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
				"asynchronously to a Globus endpoint location, an S3 bucket, or Google Drive.");
		
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
	var searchType =$('input[name=searchType]:checked:visible').val();
	var d = {};
	d.searchType = searchType;
	d.destinationPath = $("#destinationPathId").val();
	d.downloadType = $("#downloadType").val();
	d.downloadFileName = $("#downloadFileNameVal").val();
	$("#message").hide();
	var validate = true;
	
	if(!searchType) {
		 $('.downloadErrorMsg').html("Select a download destination.");
		 $("#message").show();
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
			 $('.downloadErrorMsg').html("Enter the destination information.");
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
			 $('.downloadErrorMsg').html("Enter the destination information.");
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

$("#forgotPswdLink").click(function(e){
	$("#forgotPasswordLightbox").find(".forgotPswdErrorMsg").html("");
	$("#forgotPasswordLightbox").find(".forgorPswdErrorBlock").hide();
	$("#forgotPasswordLightbox").find(".forgotPswdSuccessMsg").html("");
	$("#forgotPasswordLightbox").find(".forgorPswdSuccessBlock").hide();
	$("#forgotPasswordLightbox").find("#txtResetPswdLink").val("");
	$("#forgotPasswordLightbox").modal('show');
	
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
	validateUpdateProfile();
});


$("#resetBtn").click(function(e){
	$("#attributeVal").val("");
	 $("#searchResultsDiv").hide();
});
	
$(".backToSearchBtn").click(function(e){
	$("#searchFragmentDiv").show();
    $("#dataSetFragment").hide();
	$("#editCollectionFragment").hide();
});

$(".backToAssetDetailsBtn").click(function(e){
	 $("#assetDetailsFragment").show();
     $("#editCollectionFragment").hide();
});

$(".backtoAssetFromDwnldBtn").click(function(e){
	var assetIdentifier= $("#assetIdentifier").val();
	location.replace('/assetDetails?assetIdentifier='+assetIdentifier+'&&returnToSearch=true');
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

$("#registerBulkAssets").click(function(e){
	registerBulkAssets();
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

$("#btnSelectAssetType").click(function(e){
	var assetType = $("#createAssetModal").find("#createAssetCollectionType option:selected").val();
	$("#registerCollectionModal").find("#assetType").val(assetType);
	openUploadModal('studyList');
});

$("#driveAuthlink").click(function(e){
	var params= {type:$("#downloadType").val(),
			downloadFilePath:$("#selectedFilesList").val(),
			action:"Drive",assetIdentifier:$("#assetIdentifier").val(),
			returnToSearch:$("#returnToSearch").val()}

	 invokeAjax('/download','GET',params,postGoogleDriveFunction,postFailureFunction,null,'text');
});

$("#downloadGlobuslink").click(function(e){
	var params= {type:$("#downloadType").val(),downloadFilePath:$("#selectedFilesList").val(),action:"Globus",
			assetIdentifier:$("#assetIdentifier").val(),
			returnToSearch:$("#returnToSearch").val()}

	 invokeAjax('/download','GET',params,postGoogleDriveFunction,postFailureFunction,null,'text');
});

$("#primaryGlobusButton").click(function(e){
	
	var d = {};
	d.institutionPath =$("#instituteList").val();
	d.studyPath = $("#studyList").val();
	d.dataSetPath = $("#dataList").val();

	 invokeAjax('/upload','GET',d,postUploadGlobusFunction,postFailureFunction,null,'text');
});

$("#assetSelectionGlobusButton").click(function(e){
	var d = {};
	d.institutionPath =$("#instituteList").val();
	d.studyPath = $("#studyList").val();

	 invokeAjax('/upload','GET',d,postUploadGlobusFunction,postFailureFunction,null,'text');
});

$("#driveUploadAuthlink").click(function(e){
	var d = {};
	d.institutionPath =$("#instituteList").val();
	d.studyPath = $("#studyList").val();
	d.dataSetPath = $("#dataList").val();
    d.action ="Drive";
	 invokeAjax('/upload','GET',d,postUploadGlobusFunction,postFailureFunction,null,'text');
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

$('.asset-attributes').click(function(){
	  if($(this).hasClass('collapsed')){
	      $(this).find('img').attr('src','/images/arrow.collapse-open.svg')
	  } else {
		  $(this).find('img').attr('src','/images/arrow.collapse.svg')
	  }
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

$(document).on('click', '#pickerUploadLink', function(){
	loadUploadPicker();
});
$(document).on('click', '#pickerLink', function(){
	loadDownloadPicker();
});


$(document).on('click', '.dataTargetCollapse', function() {
	   if($(this).parent().parent().find('div.dataDivCollapse').is(":visible")){
	  	  $(this).parent().css('margin-bottom','-1px');
		  $(this).parent().parent().find('div.dataDivCollapse').css('display','none');
		  $(this).parent().parent().find('.filterSearchBox').css('display','none');
		  $(this).attr('src','/images/AccordionUp.svg')
	  } else {
		  $(this).parent().css('margin-bottom','15px');
	  	  $(this).parent().parent().find('div.dataDivCollapse').css('display','block');
	  	  $(this).parent().parent().find('.filterSearchBox').css('display','none');
		  $(this).attr('src','/images/AccordionDown.svg');
	  }
});

$(document).on('click', '.searchCheckBoxlist', function() {
	if($(this).parent().parent().find('.filterSearchBox').is(":visible")){
		$(this).parent().parent().find('.filterSearchBox').css('display','none');
	} else {
		$(this).parent().parent().find('.filterSearchBox').css('display','block');
	}
});

$(document).on('keyup', '.filterSearchBox', function() {
    var query = $(this).val().toLowerCase();
    $(this).parent().find('.filteritem').each(function(i,elem){
    	var x = $(this).val().toLowerCase();;
	  if (x.indexOf(query) != -1) {
         $(this).parent().show();

      } else{
         $(this).parent().hide();
     }
});  
});

$(document).on('click', '#clearFilters', function() {
	$(".filterGroupDiv").each(function(e){
		$(this).show();
	    $(this).find('.filteritem').prop('checked',false);
	    $(this).find('span').css('color','#212529');
	});
	$("#searchResultsDiv").hide();
});

$(document).on('click','.clearMetadata',function(){
	$(this).parent().find("input[type='text']").val("");
});

$(document).on('change', '.filteritem', function() {
	
	if($(this).is(':checked')) {
		$(this).parent().find('span').css('color','#2E76ED');
	} else {
		$(this).parent().find('span').css('color','#212529');
	}
	var attrName = $(this).parent().attr('id');
	
	//based on child selection, search at parent level and check the parent checkbox
	$(this).closest('.filterComponentDiv').prev().find('.attributeLabel').each(function(e){
		filterPrev($(this),attrName);
    });
	
	//always filter the metadata on the children level
	//do not remove parent based on child selection
	$(this).closest('.filterComponentDiv').next().find('.attributeLabel').each(function(e){
		filterNext($(this),attrName);
    });
	
	
	populateSearchCriteria('simpleSearch');
});

function filterNext($this,attributeTypeName) {

	var attributeName = $this.find('label').text();

	var rowId = 1;
	var d = {};
	var attrNames = [];
	var attrValues = [];
	var isExcludeParentMetadata = [];
	var rowIds = [];
	var operators = [];

	// filter a list based on the parent level selection
	$this.closest('.filterComponentDiv').prevAll().find(".filteritem:checked")
			.each(function() {
				var attrName = $(this).parent().attr('id');
				var attrVal = $(this).val();
				if (attrName != attributeName) {
					attrNames.push(attrName);
					attrValues.push(attrVal);
					rowIds.push(rowId);
					isExcludeParentMetadata.push(false);
					operators.push("EQUAL");
					rowId = rowId + 1;
				}
			});

	d.attrName = attrNames.join();
	d.attrValuesString = attrValues.join('@@');
	d.isExcludeParentMetadata = isExcludeParentMetadata.join();
	d.rowId = rowIds.join();
	d.operator = operators.join();
	d.searchName = attributeName;

	$.ajax({
		url : '/getFilterList',
		type : 'GET',
		async : false,
		contentType : 'application/json',
		dataType : 'text',
		data : d,
		success : function(data, status) {
			var list = JSON.parse(data);
			$this.parent().find('.filterGroupDiv').each(function(e) {
				var val = $(this).find('.filteritem').val();
				if (list.indexOf(val) != -1) {
					$(this).show();
				} else {
					$(this).hide();
					$(this).find('.filteritem').prop("checked", false);
					$(this).find('span').css('color','#212529');
				}
			});
		},
		error : function(data, status, error) {
			console.log("===> status: ", status);
			console.log("===> error: ", error);
			console.log("===> data: ", data);
		}
	}).done(
			function(e) {
				$this.closest('.filterComponentDiv').next().find('.attributeLabel').each(function(e){
					filterNext($(this));
				});
			});
}

function filterPrev($this,attributeTypeName) {
	var attributeName = $this.find('label').text();

	var rowId = 1;
	var d = {};
	var attrNames = [];
	var attrValues = [];
	var isExcludeParentMetadata = [];
	var rowIds = [];
	var operators = [];
	var url;

	// filter a list based on the parent level selection
	$this.closest('.filterComponentDiv').nextAll().find(".filteritem:checked")
			.each(function() {
				var attrName = $(this).parent().attr('id');
				var attrVal = $(this).val();
				if (attrName != attributeName) {
					attrNames.push(attrName);
					attrValues.push(attrVal);
					rowIds.push(rowId);
					isExcludeParentMetadata.push(false);
					operators.push("EQUAL");
					rowId = rowId + 1;
				}
			});

	d.attrName = attrNames.join();
	d.attrValuesString = attrValues.join('@@');
	d.isExcludeParentMetadata = isExcludeParentMetadata.join();
	d.rowId = rowIds.join();
	d.operator = operators.join();
	d.searchName = attributeName;
     if(attributeTypeName  == 'Asset Type') {
	    url = '/getFilterList';
     } else {
	    url  = '/getFilterList?retrieveParent=true';
     }
     
	$.ajax({
		url : url,
		type : 'GET',
		async : false,
		contentType : 'application/json',
		dataType : 'text',
		data : d,
		beforeSend: function () {
	    	$("#spinner").show();
	        $("#dimmer").show();
	    },
		success : function(data, status) {
			var list = JSON.parse(data);
			$this.parent().find('.filterGroupDiv').each(function(e) {
				var val = $(this).find('.filteritem').val();
				if (list.indexOf(val) != -1) {
					$(this).find('.filteritem').prop("checked", true);
					$(this).show();
					$(this).find('span').css('color','#2E76ED');
				}
			});
		},
		error : function(data, status, error) {
			console.log("===> status: ", status);
			console.log("===> error: ", error);
			console.log("===> data: ", data);
		}
	}).done(
			function(e) {
				$this.closest('.filterComponentDiv').prev().find(
						'.attributeLabel').each(function(e) {
							filterPrev($(this));
				});
			});
}