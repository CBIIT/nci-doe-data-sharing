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
	$("#download-modal").find(".selectedFilesListDisplay").html("");
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
	
	if(searchType == 's3' || searchType == 'async' || selectedFiles) {
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
	} else {
		$('#downloadSyncForm').attr('action', '/downloadsync');
		$("#downloadSyncForm").submit();
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

$("#manageTasks-tab").click(function(e){
	refreshTaskManagerDataTable();
});

$("#addMetaData").click(function(e){
	addCollectionMetaDataRows();
});


$("#updateMetaData").click(function(e){
	updateMetaDataCollection();
});

/*$("#upload-tab").click(function(e){
	if(!uploadtabIniatialize) {
		uploadtabIniatialize = true;
		loadUploadTab();
	}
});*/

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
	//$("#uploadSubFragmentTab").hide();
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

	 invokeAjax('/upload','GET',d,postUploadGlobusFunction,null,null,null);
});

$(".addNewMetaDataForDataFiles").click(function(e){
	addNewMetaDataRowsForDataFile($(this));
});

loadUploadTab();
});


function populateSearchCriteria(searchType) {
	
	
	search_criteria_json.detailed = true;
	search_criteria_json.searchType = "collection";	
	
	var attrNames = [] ;
	var attrValues = [];
	var levelValues= [];
	var isExcludeParentMetadata = [];
	var rowIds = [];
	var operators = [];

	attrNames.push("collection_type");
	attrValues.push("Data_Set");
	levelValues.push("ANY");
	isExcludeParentMetadata.push(true);
	rowIds.push(1);
	operators.push("EQUAL");
			
		
	 if($("#attributeVal").val()) {
		    attrNames.push("ANY");
			attrValues.push('%' + $("#attributeVal").val() + '%');
			levelValues.push("ANY");
			isExcludeParentMetadata.push(false);
			rowIds.push(2);
			operators.push("LIKE");

	} 
	 if(searchType == 'advSearchBtn')  {
		var rowId = 3;
		$(".filteritem").each(function(){
			var attrName = $(this).find("div.filtertext").text();
			var attrVal = $(this).find("input[type=text]").val();
			attrNames.push(attrName);
			attrValues.push('%' + attrVal + '%' );
			rowIds.push(rowId);
			isExcludeParentMetadata.push(false);
			operators.push("LIKE");
			rowId =  rowId + 1 ;
		});	
	}
	 
		search_criteria_json.attrName = attrNames.join();
		search_criteria_json.attrValue = attrValues.join();
		search_criteria_json.rowId = rowIds.join();	
		search_criteria_json.level = levelValues.join();
		search_criteria_json.isExcludeParentMetadata = isExcludeParentMetadata.join();
		search_criteria_json.operator = operators.join();
}

function refreshDataTable() {
	var isVisible = (loggedOnUserInfo ? true:false);
    console.log("refresh datatable");
    if (!$.fn.DataTable.isDataTable('#searchResultTable')) {
        dataTableInit(isVisible);
    } else {
        var t = $('#searchResultTable').DataTable();
        console.log(t);
        t.ajax.reload(null, false);
    }
}

function dataTableInit(isVisible) {
    $('#searchResultTable').DataTable({
        "paging": true,
        "ordering": false,
        "info": true,
        "pageLength": 25,
        oLanguage: {
            "sSearch": "Filter:"
        },
        "ajax": {
            "url": "/search",
            "type": "GET",
            "data": function (d) {
            	d.searchType = search_criteria_json.searchType;
                d.detailed = search_criteria_json.detailed;
                d.level = search_criteria_json.level;
               d.attrName =search_criteria_json.attrName;
               d.attrValue =search_criteria_json.attrValue;
               d.rowId = search_criteria_json.rowId;
               d.isExcludeParentMetadata = search_criteria_json.isExcludeParentMetadata;
               d.operator = search_criteria_json.operator;
            },
            "dataSrc": function (data) {
                return data;
            },
            "error": function (xhr, error, thrown) {
                console.log("Response status: " + xhr.status + " (" + xhr.statusText + ")");
                console.log(error + ": " + thrown + " [" + xhr.status + " (" + xhr.statusText + ")]");
                console.log(xhr.responseText);
                console.log(xhr);
                $("#spinner").hide();
                $("#dimmer").hide();
            },

            "beforeSend": function () {
                $("#spinner").show();
                $("#dimmer").show();
            },

            "complete": function () {
                $("#spinner").hide();
                $("#dimmer").hide();
            }
        },

        "initComplete": function (settings, json) {

        },

        "drawCallback": function (settings) {
        	$("#searchResultTable thead").remove();
        	
           /*$(".downloadLink").click(function(e){
        	   var path = $(this).attr('data-path');
        	   var fileName = $(this).attr('data-fileName');  
        	   $("#download-modal").find(".selectedFilesDiv").hide();
               downloadFunction(path,fileName);
             });*/
        	
           $(".dataSetFragment").click(function(e) {
        	   $("#searchFragmentDiv").hide();
        	   $("#dataSetFragment").show();
        	   $("#editCollectionFragment").hide();
        	   var datsetPath = $(this).attr('data_set_path');
        	   var metadata = $(this).attr('metadata_type');
        		refreshDataSetDataTable(datsetPath,metadata);
           });
           
           $(".editCollectionMetadata").click(function(e){
        	   $("#searchFragmentDiv").hide();
        	   $("#dataSetFragment").hide();
        	   $("#editCollectionFragment").show();
        	   var metaData = $(this).attr('metadata_set');
        	   var metaDataPath = $(this).attr('metadata_path');
        	   constructCollectionMetData(metaData,metaDataPath,false);
           });
           
           $(".selectCheckboxForIns").click(function(e){
        	   var table = $(e.target).closest('table').attr('id');
        	   var len = $('#' + table).find("input[type=checkbox]:checked").length;
        	   if ($(this).is(':checked')) {
        		   $(e.target).closest('table tr').find('.selectCheckBoxForStudy').prop('checked', true);        		   
        	   } else {
        		   $(e.target).closest('table tr').find('.selectCheckBoxForStudy').prop('checked', false);
        	   }
        	   if (len >= 1) {
        		   $("#downloadSelected").prop("disabled", false);
        	   } else {
        		   $("#downloadSelected").prop("disabled", true);
        	   }
        	   
           });
           $(".selectCheckBoxForStudy").click(function(e){
        	   var table = $(e.target).closest('table').attr('id');
        	   var len = $('#' + table).find("input[type=checkbox]:checked").length;        	   
        	   if (len >= 1) {
        		   $("#downloadSelected").prop("disabled", false);
        	   } else {
        		   $("#downloadSelected").prop("disabled", true);
        	   }
        	   
           });
           
           initializeToolTips();
           initializePopover();
           displayPopover();
        },

        "columns": [
            {"data": "path", "render": function (data, type, row) {
                    return renderPath(data, type, row);
                },
        },

        ],
        "dom": '<"top"lip>rt<"bottom"p>',

        "lengthMenu": [[10, 25, 50, 100], [10, 25, 50, 100]],

        "language": {
            "zeroRecords": "Nothing found to display",
            "info": "&nbsp; (Displaying _START_ to _END_ of _TOTAL_ )",
            sLengthMenu: "_MENU_",
            "infoEmpty": " No records to display"
        }
    });
}

function renderPath(data, type, row) {
	var html = "";
	
	var study;
	var ins;
	var data;
	
	study = JSON.stringify(row.studyUserMetadata);
	ins = JSON.stringify(row.instituteUserMetadata);
	data = JSON.stringify(row.selfMetadata);
	
	html += "<div class='col-md-10' style='font-size:16px;margin-top:20px;'><div class='row'><div class='col-md-12'><input type='checkbox' id=" + row.dataSetPath + " " +
			"class='selectCheckboxForIns'/>&nbsp;&nbsp;&nbsp;<span class='cil_14_bold_no_color'>" + row.dataSetName + "</span>" +
			"&nbsp&nbsp;<span class='editCollectionMetadata' metadata_path  = '" + row.dataSetPath+ "' metadata_set = '" + data  + "'><i class='fa fa-edit' data-toggle='tooltip' data-content='Edit Data Set Metadata'></i></span></div><div class='col-md-12'></div>" +
			"<div class='col-md-12' style='margin-left:22px;'><a href='#' class='dataSetFragment' metadata_type = '" + data  + "' data_set_path = " + row.dataSetPath + ">" +
					"<span class='cil_12_bold_no_color'>" + row.dataSetDescription + "</span>" +
			"</a><br></div><div class='col-md-12'><br></div><div class='col-md-12' style='margin-left:22px;'>" +
			"<span class='cil_12_bold_no_color'>Study: </span><a class='cil_12_no_color button2a' metadata_type = '" + study  + "' tabindex='0'" +
			" data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' data-popover-content='#a01'>" + row.studyName + "</a>" +
					"&nbsp&nbsp;<span class='editCollectionMetadata' metadata_path  = '" + row.studyPath+ "'  metadata_set = '" + study  + "'><i class='fa fa-edit' data-toggle='tooltip' data-content='Edit Study Metadata'></i></span></div>" +
			"<div class='col-md-12 top-buffer' style='margin-left:22px;'>" +
			"<span class='cil_12_bold_no_color'>Institute: </span><a class='cil_12_no_color button2a' metadata_type = '" + ins  + "' tabindex='0'" +
			" data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' data-popover-content='#a01'>" + row.instituteName + "</a>" +
					"&nbsp&nbsp;<span class='editCollectionMetadata' metadata_path  = '" + row.institutePath+ "' metadata_set = '" + ins  + "'><i class='fa fa-edit' data-toggle='tooltip' data-content='Edit Institute Metadata'></i></span></div></div></div>" +
			"<div class='row' style='margin-left: 25px;margin-bottom:10px;'><div class='col-sm-1 resultsStatsContainer' title='Number of Files'>" +
			"<i class='fas fa-folder-open' title='Number of Files' style='font-size: 18px;'></i>" +
			"<span class='resultsStatsText'> &nbsp;&nbsp;" + row.numOfDataSets + " </span></div></div>";

	
    return html;
}


function display(value) {
	var asyncDiv = document.getElementById("AsyncDiv");
	var syncDiv = document.getElementById("SyncDiv");
	var s3Div = document.getElementById("s3Div");
	if (value == "async") {
		$("#AsyncDiv").show();
		$("#SyncDiv").hide();
		$("#s3Div").hide();
	} else if (value == "sync") {
		$("#SyncDiv").show();
		$("#AsyncDiv").hide();
		$("#s3Div").hide();
	} else {
		$("#SyncDiv").hide();
		$("#AsyncDiv").hide();
		$("#s3Div").show();
	}
}




function addValueToSelected(optionVal) {
    if (($(optionVal).attr('value')) === '')
        return;
    var $metadatalist = $('#metadatalisting');
    var fieldPath = $(optionVal).attr('value');
    var mdIdentifier = fieldPath.replace(new RegExp('\\.|/|\\s|\\[|\\]', 'g'), '_');
    var rowId = 'filterItemList_' + mdIdentifier;
    var $rowdiv = $('<div class="row filteritem" style="margin-top: 9px;margin-bottom: 10px;" id="' + rowId + '"/>');
    $metadatalist.append($rowdiv);
    var $coldiv = $('<div class="col-sm-5" />');
    $rowdiv.append($coldiv);
    var $selectdiv = $('<div class="filtertext">' + $(optionVal).text() + '</div>');
    $coldiv.append($selectdiv);

    var $inputColumn = $('<div class="col-sm-7">');
    var $inputGroup = $('<div class="input-group" />');
    $inputColumn.append($inputGroup);
    $rowdiv.append($inputColumn);
    var advancedSearchInput = $('<input type="text" data-value="' + fieldPath + '" data-type="'
        + $(optionVal).attr('data-type') + '" id="metadatasearch_' + mdIdentifier
        + '" class="form-control" placeholder="Enter a keyword..."'
        + ' title="Enter a Search Keyword or Phrase" aria-label="Enter a Search Keyword or Phrase"'
        + ' inputtype="textval"/>');
    $inputGroup.append(advancedSearchInput);
    var $inputGroupButton = $('<div class="input-group-btn" />');
    $inputGroup.append($inputGroupButton);
    $inputGroupButton.append('<input class="btn btn-primary pull-right" type="button" ' +
        'value="X" onclick="removeRowAddOption(\'' + rowId + '\')"/>');
    if (/.*Date.*/.test($(optionVal).text())) {
        advancedSearchInput.attr(
            'placeholder', 'Enter a date range (e.g. \'June 2017\' or \'May through September\')');
    }

    removeOptionFromAdvancedSearchSelector(rowId, $(optionVal).attr('value'));
}



function removeOptionFromAdvancedSearchSelector(rowId, optionVal) {
    var optionToHide = $("#metadatalist option[value=\"" + optionVal + "\"]");
    advancedSearchHiddenOptions[rowId] = optionToHide;
    optionToHide.remove();
}

function removeRowAddOption(rowVal) {
    var $metadatalist = $('#metadatalist');
    $metadatalist.append(advancedSearchHiddenOptions[rowVal]);
    sortOption();
    removeRow(rowVal);
}

$.fn.sortOptions = function () {
    $(this).each(function () {
        var op = $(this).children('option');
        op.sort(function (a, b) {
            if (a.value === '')
                return -1;
            if (b.value === '')
                return 1;
            return a.text > b.text ? 1 : -1;
        })
        return $(this).empty().append(op);
    });
}

function sortOption() {
    $('#metadatalist').sortOptions();
    $("#metadatalist").val($("#metadatalist option:first").val());
}

function removeRow(rowId) {
    $('#' + rowId).remove();
}

function initializeToolTips() {
    $('[data-toggle="tooltip"]').tooltip();
}

function initializePopover() {
    $("[data-toggle=popover]").popover({
        html: true,
        trigger: 'manual',
        container: 'body',
        placement: 'left',
        delay: {show: 50, hide: 50},
        content: function () {
            var content = $(this).attr("data-popover-content");
            return $(content).children(".popover-body").html();
        },
        title: function () {
            var title = $(this).attr("data-popover-content");
            return $(title).children(".popover-heading").html();
        }
    });
}


function displayPopover() {
    $('.button2a').on('click', function (e) {
        openPopOver($(this));
    });
    $('.button2a').on('keypress', function (e) {
        if (e.which == 13 || e.keyCode == 13) {//Enter key pressed
            openPopOver($(this));
        }
    });
}

function openPopOver($this) {
    var pop = $this;
    $('.button2a').not($this).popover('hide'); 
    var metadata = $this.attr('metadata_type');
    
    var list = JSON.parse(metadata);
    
      var ind = "<div id=\"a01\" class=\"col-md-12 hidden\"> <div class=\"popover-heading\">" +
                "User MetaData <a class=\"button closeBtn float-right\" href=\"javascript:void(0);\"><i class=\"fa fa-times\"></i></a> </div>" +
                "<div class='popover-body'> <div class='divTable' style='width: 100%;border: 1px solid #000;'>" +
                "<div class='divTableBody'><div class='divTableRow'>" +
                "<div class='divTableHead'>Attribute</div>" + 
                "<div class='divTableHead'>Value</div></div>";

            var content = "";

            $.each(list, function( key, value ) {	
                content += "<div class='divTableRow'><div class='divTableCell'>" + value.key + "</div>" +
                        "<div class='divTableCell'>" + value.value + "</div></div>";
                });
            
            var table = ind + content + "</div> </div></div> </div>";
            $("#a01").remove();
            pop.after(table);
            pop.data('bs.popover').setContent();
            pop.popover('show');
        
   
}
