$(document).ready(function () {
	
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
	    
	    loadJsonData('/search/adv-search-list', $("#metadatalist"), true, null, null, null, "displayName", "displayName");
	   


	    
$("#searchBtn").click(function(e){
	e.preventDefault();
	$("#searchResultsDiv").show();
	 populateSearchCriteria('simpleSearch');
	refreshDataTable();

	   /* var eGridDiv = document.querySelector('#myGrid');
	    new agGrid.Grid(eGridDiv, gridOptions);
	  
		$.ajax({
			type : "GET",
		     url : "/search",
		     data: search_criteria_json,
			 success : function(data) {
				 console.log('SUCCESS: ', data);
				 gridOptions.api.setRowData(data);
			 },
			error : function(e) {
				 console.log('ERROR: ', e);
			}
		});*/
	
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
	    $("#download-modal").find(".selectedFilesList").val(selectedPaths);
	    
	    $.each(selectedPaths, function(index, value) {
	    	$("#download-modal").find(".selectedFilesListDisplay").append("<p>"+value+"</p>");
	    });
	    
	    $("#download-modal").find("#SyncDiv").hide();
		$("#download-modal").find("#syncRadioSet").hide();
		$("#download-modal").find("#downloadType").val("collectionfiles");
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
		if(selectedFiles) {
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
				 success : function(msg) {
					 console.log('SUCCESS: ', msg);
					 $('#downloadErrorMsg').html(msg.message);
					 $("#message").show();
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
	invokeAjax('/user-info','GET',params,postGetUserInfoFunction,null,null,null);
	
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
	
	invokeAjax('/user-info','POST',JSON.stringify(d),null,null,null,null);
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
	
$("#backToSearch").click(function(e){
	$("#searchFragmentDiv").show();
    $("#dataSetFragment").hide();
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

});

var gridOptions = {
	    columnDefs: [
	        {headerName:'Path', width: 300,cellRenderer: 'medalCellRenderer'	            
	         },
	        {headerName: 'download', field: 'download'},
	    ],

	    defaultColDef: {
	        width: 300
	    },
	    defaultColGroupDef: {
	        marryChildren: true
	    },
	    components: {
	        'medalCellRenderer': MedalCellRenderer
	    },
	    columnTypes: {
	        numberColumn: {width: 300}
	    },

	    rowData: null
	};

function MedalCellRenderer() {}


MedalCellRenderer.prototype.init = function(params) {
    
	var html = "";
	var metadata = params.data.metadataEntries;
		var parentData = metadata.parentMetadataEntries;
		var selfMetadata = metadata.selfMetadataEntries;
		
		html += "<input type='checkbox' name='selectCheckboxForPath'/><label>Institue</label><br/>";

		$.each(parentData, function( key, value ) {
			
				html += "<div><span>" + value.attribute +"</span> - <span>" + value.value +"</span></div>";
			
		});
		
		html += "<input type='checkbox' name='selectCheckboxForPath'/><label>Study</label><br/>";
		$.each(selfMetadata, function( key, value ) {
			html += "<div><span>" + value.attribute +"</span> - <span>" + value.value +"</span></div>";
		});

		html += "<div><a href='#' class='dataSetFragment'>Data Set</a></div>";
	
		this.eGui.innerHTML = html;
};

MedalCellRenderer.prototype.getGui = function() {
    return this.eGui;
};


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
        	
           $(".downloadLink").click(function(e){
        	   var path = $(this).attr('data-path');
        	   var fileName = $(this).attr('data-fileName');  
        	   $("#download-modal").find(".selectedFilesDiv").hide();
               downloadFunction(path,fileName);
             });
           $(".dataSetFragment").click(function(e) {
        	   $("#searchFragmentDiv").hide();
        	   $("#dataSetFragment").show();
        	   var datsetPath = $(this).attr('data_set_path');
        	  /* dataset_criteria_json.searchType = "data_Set";
        	   dataset_criteria_json.detailed = true;        	   
        	   dataset_criteria_json.attrName = 'ANY';
        	   dataset_criteria_json.attrValue = '%';
        	   dataset_criteria_json.rowId = 1;
        	   dataset_criteria_json.level = 'Data_Set';
        	   dataset_criteria_json.isExcludeParentMetadata = false;
        	   dataset_criteria_json.operator = 'LIKE';*/
        		refreshDataSetDataTable(datsetPath);
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
	html += "<div class='col-md-10' style='font-size:16px;margin-top:20px;'><div class='row'><div class='col-md-12'><input type='checkbox' id=" + row.dataSetPath + " class='selectCheckboxForIns'/>&nbsp;&nbsp;&nbsp;<span class='cil_14_bold_no_color'>" + row.dataSetName + "</span>" +
			"</div><div class='col-md-12'></div><div class='col-md-12' style='margin-left:22px;'><a href='#' class='dataSetFragment' data_set_path = " + row.dataSetPath + "><span class='cil_12_bold_no_color'>" + row.dataSetDescription + "</span>" +
			"</a><br></div><div class='col-md-12'><br></div><div class='col-md-12' style='margin-left:22px;'>" +
			"<span class='cil_12_bold_no_color'>Study: </span><a class='cil_12_no_color button2a' metadata_type = 'Study' tabindex='0'" +
			" data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' data-popover-content='#a01'>" + row.studyName + "</a></div>" +
			"<div class='col-md-12 top-buffer' style='margin-left:22px;'>" +
			"<span class='cil_12_bold_no_color'>Institute: </span><a class='cil_12_no_color button2a' metadata_type = 'Institute' tabindex='0'" +
			" data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' data-popover-content='#a01'>" + row.instituteName + "</a></div></div></div>" +
			"<div class='row' style='margin-left: 25px;margin-bottom:10px;'><div class='col-sm-1 resultsStatsContainer' title='Number of Files'>" +
			"<i class='fas fa-folder-open' title='Number of Files' style='font-size: 18px;'></i>" +
			"<span class='resultsStatsText'> &nbsp;&nbsp;" + row.numOfDataSets + " </span></div></div>";
       
	studyMetadata = row.studyUserMetadata;
	instituteMetaData = row.instituteUserMetadata;
	
    return html;
}
/*function renderPath(data, type, row) {
	
	var html = "";
	var downdloadFileName = null;
	var path = row.path;
	if(search_criteria_json.searchType != 'collection') {
		var n = path.lastIndexOf("/");
		downdloadFileName = path.substring(n+1);		
	}
	
	var data = row.metadataEntries;
		var parentData = data.parentMetadataEntries;
		var selfMetadata = data.selfMetadataEntries;
		
       html += "<label style='color: #00458F;'><input type='checkbox' class='selectCheckboxForIns'> &nbsp;&nbsp;Institute <a id='downloadlink' class='btn btn-link btn-sm downloadLink' " +
       		"href='javascript:void(0);' data-toggle='modal' data-backdrop='static' data-keyboard='false' " +
       		"data-filename=" + downdloadFileName + " data-path=" + row.download + " data-target='#download-modal'" +
       		" style=''><i class='fa fa-download' aria-hidden='true'></i></a></label><br/><div class='divTable' style='margin-left: 71px; " +
       		"width: 50%; border: 1px solid #000; display: table;'><div class='divTableBody'>";
		
       
       $.each(parentData, function( key, value ) {	
			html += "<div class='divTableRow'><div class='divTableHead'><span>" + value.attribute +"</span> " +
					"</div> <div class='divTableHead'> <span>" + value.value +"</span></div></div>";
		
		});
		
       html += "</div></div>";
       
		html += "<label style='margin-left: 91px;color: #00458F;'><input type='checkbox' class='selectCheckBoxForStudy'> &nbsp;&nbsp; Study <a id='downloadlink' " +
				"class='btn btn-link btn-sm downloadLink' " +
       		"href='javascript:void(0);' data-toggle='modal' data-backdrop='static' data-keyboard='false' " +
       		"data-filename=" + downdloadFileName + " data-path=" + row.download + " data-target='#download-modal'" +
       		" style=''><i class='fa fa-download' aria-hidden='true'></i></a></label><br/><div class='divTable'" +
				"style='margin-left: 143px;width: 50%; border: 1px solid #000; display: table;'><div class='divTableBody'>";
		
		$.each(selfMetadata, function( key, value ) {
			html += "<div class='divTableRow'><div class='divTableHead'><span>" + value.attribute +"</span>" +
					" </div> <div class='divTableHead'> <span>" + value.value +"</span></div></div>";
		});

		 html += "</div></div>";
		 
		html += "<div style='margin-left:30px;margin-top:10px;'><a href='#' class='dataSetFragment'>Data Set</a></div>";
	
	return html;
}*/



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

function downloadFunction(path,fileName) {
	
	$("#download-modal").find("#destinationPathId").val(path);	
	$("#download-modal").find("#message").hide();
	
	if(fileName && fileName != "null") {
		$("#download-modal").find("#syncRadioSet").show();
		$("#download-modal").find("#SyncDiv").show();
		$("#download-modal").find("#searchTypeSync").click();
		$("#download-modal").find("#downloadType").val("data_object");
		$("#download-modal").find("#downloadFileNameVal").val(fileName);
		$("#download-modal").find("#downloadFileName").val(fileName);
	} else {
		$("#download-modal").find("#SyncDiv").hide();
		$("#download-modal").find("#syncRadioSet").hide();
		$("#download-modal").find("#downloadType").val("collection");
	}
	
	$("#download-modal").find("#transferType").val("globus");
	$("#download-modal").find("#source").val("");
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
    var type = $this.attr('metadata_type');
    var list = "";
    if(type == 'Study') {
    	list = studyMetadata;
    } else {
    	list = instituteMetaData;
    }
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
