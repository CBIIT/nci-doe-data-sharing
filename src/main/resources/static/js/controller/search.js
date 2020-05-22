function populateSearchCriteria(searchType) {
	
	$("#searchResultsDiv").show();
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
		refreshDataTable();
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
        	if(isVisible) {
        		$("#downloadSelected").show();
        	} else {
        		$("#downloadSelected").hide();
        	}

           $(".dataSetFragment").click(function(e) {
        	   $("#searchFragmentDiv").hide();
        	   $("#dataSetFragment").show();
        	   $("#editCollectionFragment").hide();
        	   var datsetPath = $(this).attr('data_set_path');
        	   var metadata = $(this).attr('metadata_type');
        	   var acessGrps = $(this).attr('access_grp');
        		refreshDataSetDataTable(datsetPath,metadata,acessGrps);
           });
           
           $(".editCollectionMetadata").click(function(e){
        	   $("#searchFragmentDiv").hide();
        	   $("#dataSetFragment").hide();
        	   $("#editCollectionFragment").show();
        	   var metaData = $(this).attr('metadata_set');
        	   var metaDataPath = $(this).attr('metadata_path');
        	   var permissionsRole = $(this).attr('permissions_role');
        	   var collectionId = $(this).attr('collectionId');
        	   constructCollectionMetData(metaData,metaDataPath,false,permissionsRole,collectionId);
           });
           
           $(".editAccessGroupPermissions").click(function(e){
         	   var collectionId = $(this).attr('collectionId');
         	  var access_groups = $(this).attr('access_groups');
         	 var metaDataPath = $(this).attr('metadata_path');
        	   editAccessPermissions(collectionId,access_groups,metaDataPath);
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
	var isLoggedOnuserExists = (loggedOnUserInfo ? true:false);
	
	study = JSON.stringify(row.studyUserMetadata);
	ins = JSON.stringify(row.instituteUserMetadata);
	data = JSON.stringify(row.selfMetadata);
	
	if(isLoggedOnuserExists) {
		var editDataSetHtml = "";
		var editStudySetHtml = "";
		var editProgramSetHtml = "";
		
		if(row.dataSetPermissionRole && row.dataSetPermissionRole != 'No Permissions') {
			editDataSetHtml = "<span class='editCollectionMetadata' collectionId  = '" + row.dataSetCollectionId + "' " +
			"permissions_role = '" + row.dataSetPermissionRole + "'" +
			" metadata_path  = '" + row.dataSetPath+ "' metadata_set = '" + data  + "'>" +
           "<i class='fa fa-edit' data-toggle='tooltip' data-content='Edit Data Set Metadata'></i></span>";
			if(row.dataSetPermissionRole == 'Owner') {
				editDataSetHtml += "&nbsp;&nbsp;<span class='editAccessGroupPermissions' collectionId  = '" + row.dataSetCollectionId + "' " +
			    " access_groups  = '" + row.dataLevelAccessGroups+ "' metadata_path  = '" + row.dataSetPath+ "'>" +
                 "<i class='fa fa-users' data-toggle='tooltip' data-content='Edit Data Set Access Permissions'></i></span>";
			}
		}
		
		if(row.dataSetPermissionRole && row.dataSetPermissionRole != 'No Permissions') {
			editStudySetHtml = "<span class='editCollectionMetadata' collectionId  = '" + row.studyCollectionId + "'" +
							" permissions_role = '" + row.studyPermissionRole + "' metadata_path  = '" + row.studyPath+ "' " +
									" metadata_set = '" + study  + "'>" +
			"<i class='fa fa-edit' data-toggle='tooltip' data-content='Edit Study Metadata'></i></span>";
			if(row.studyPermissionRole == 'Owner') {
				editStudySetHtml += "&nbsp;&nbsp;<span class='editAccessGroupPermissions' collectionId  = '" + row.studyCollectionId + "' " +
			    " access_groups  = '" + row.studyLevelAccessGroups+ "' metadata_path  = '" + row.studyPath+ "'>" +
                 "<i class='fa fa-users' data-toggle='tooltip' data-content='Edit Study Access Permissions'></i></span>";
			}
		}
		
		if(row.programPermissionRole && row.programPermissionRole != 'No Permissions') {
			editProgramSetHtml = "<span class='editCollectionMetadata' collectionId  = '" + row.programCollectionId + "'" +
							" permissions_role = '" + row.programPermissionRole + "' metadata_path  = '" + row.institutePath+ "' " +
									"metadata_set = '" + ins  + "'>" +
			"<i class='fa fa-edit' data-toggle='tooltip' data-content='Edit Program Metadata'></i></span>"; 
			
			if(row.programPermissionRole == 'Owner') {
				editProgramSetHtml += "&nbsp;&nbsp;<span class='editAccessGroupPermissions' collectionId  = '" + row.programCollectionId + "' " +
			    " access_groups  = '" + row.programLevelAccessGroups+ "' metadata_path  = '" + row.institutePath+ "'>" +
                 "<i class='fa fa-users' data-toggle='tooltip' data-content='Edit Program Access Permissions'></i></span>";
			}
		}
	
		html += "<div class='col-md-10' style='font-size:16px;margin-top:20px;'><div class='row'><div class='col-md-12'><input type='checkbox' id=" + row.dataSetPath + " " +
			"class='selectCheckboxForIns'/>&nbsp;&nbsp;&nbsp;<span class='cil_14_bold_no_color'>" + row.dataSetName + "</span>" +
			"&nbsp&nbsp;" + editDataSetHtml + "</div><div class='col-md-12'></div>" +
			"<div class='col-md-12' style='margin-left:22px;'><a href='#' class='dataSetFragment' access_grp ='"+row.dataLevelAccessGroups +"' metadata_type = '" + data  + "' data_set_path = " + row.dataSetPath + ">" +
					"<span class='cil_12_bold_no_color'>" + row.dataSetDescription + "</span>" +
			"</a><br></div><div class='col-md-12'><br></div><div class='col-md-12' style='margin-left:22px;'>" +
			"<span class='cil_12_bold_no_color'>Study: </span><a class='cil_12_no_color button2a' metadata_type = '" + study  + "' tabindex='0'" +
			" data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' data-popover-content='#a01'>" + row.studyName + "</a>" +
					"&nbsp&nbsp;"+editStudySetHtml+"</div>" +
			"<div class='col-md-12 top-buffer' style='margin-left:22px;'>" +
			"<span class='cil_12_bold_no_color'>Program: </span><a class='cil_12_no_color button2a' metadata_type = '" + ins  + "' tabindex='0'" +
			" data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' data-popover-content='#a01'>" + row.programName + "</a>" +
					"&nbsp&nbsp;"+editProgramSetHtml+"</div></div></div>";

	} else {
		html += "<div class='col-md-10' style='font-size:16px;margin-top:20px;'><div class='row'><div class='col-md-12'>"+
		"&nbsp;&nbsp;&nbsp;<span class='cil_14_bold_no_color'>" + row.dataSetName + "</span>" +
		"&nbsp&nbsp;</div><div class='col-md-12'></div>" +
		"<div class='col-md-12' style='margin-left:22px;'><a href='#' class='dataSetFragment' access_grp ='"+row.dataLevelAccessGroups +"' metadata_type = '" + data  + "' data_set_path = " + row.dataSetPath + ">" +
				"<span class='cil_12_bold_no_color'>" + row.dataSetDescription + "</span>" +
		"</a><br></div><div class='col-md-12'><br></div><div class='col-md-12' style='margin-left:22px;'>" +
		"<span class='cil_12_bold_no_color'>Study: </span><a class='cil_12_no_color button2a' metadata_type = '" + study  + "' tabindex='0'" +
		" data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' data-popover-content='#a01'>" + row.studyName + "</a>" +
				"&nbsp&nbsp;</div>" +
		"<div class='col-md-12 top-buffer' style='margin-left:22px;'>" +
		"<span class='cil_12_bold_no_color'>Program: </span><a class='cil_12_no_color button2a' metadata_type = '" + ins  + "' tabindex='0'" +
		" data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' data-popover-content='#a01'>" + row.programName + "</a>" +
				"&nbsp&nbsp;</div></div></div>";
	}
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
    if ($(optionVal).attr('value') === '' || $(optionVal).attr('value') === 'ANY')
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
            return a.value > b.value ? 1 : -1;
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
        if (e.which == 13 || e.keyCode == 13) {
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
