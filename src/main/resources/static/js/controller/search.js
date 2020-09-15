$(document).ready(function () {
	$("#landing-tab").removeClass('active');
	$("#search-tab").addClass('active');
	
	 $('.dt-responsive tbody').on('click', 'td', function (e) {
		 initializeToolTips();
		
	 });
	 $('body').tooltip({selector: '[data-toggle="tooltip"]'});
	 var doeIdentifier = $("#doeIdentifier").val();
	 if(doeIdentifier) {
		 populateSearchCriteria('datasetUrl');
	 }
});
function populateSearchCriteria(searchType) {
	
	$("#searchResultsDiv").show();
	search_criteria_json.detailed = true;
	search_criteria_json.searchType = "dataobject";	
	
	var attrNames = [] ;
	var attrValues = [];
	var levelValues= [];
	var isExcludeParentMetadata = [];
	var rowIds = [];
	var operators = [];

	attrNames.push("collection_type");
	attrValues.push("Dataset");
	levelValues.push("ANY");
	isExcludeParentMetadata.push(false);
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
			  if (/.*Date.*/.test(attrName)) {
				  var dateFromAttr = $(this).find(".fromDate").val();
				  var dateToAttr = $(this).find(".toDate").val();
				  if(dateFromAttr) {
					  attrNames.push(attrName);
					  levelValues.push("ANY");
						attrValues.push('%' + dateFromAttr + '%' );
						rowIds.push(rowId);
						isExcludeParentMetadata.push(false);
						operators.push("TIMESTAMP_GREATER_OR_EQUAL");
						rowId =  rowId + 1 ;  
				  }
				  
				  if(dateToAttr) {
					  attrNames.push(attrName);
					  levelValues.push("ANY");
						attrValues.push('%' + dateToAttr + '%' );
						rowIds.push(rowId);
						isExcludeParentMetadata.push(false);
						operators.push("TIMESTAMP_LESS_OR_EQUAL");
						rowId =  rowId + 1 ;  
				  }
				  
			  } else {
				  var attrVal = $(this).find("input[type=text]").val();
					attrNames.push(attrName);
					levelValues.push("ANY");
					attrValues.push('%' + attrVal + '%' );
					rowIds.push(rowId);
					isExcludeParentMetadata.push(false);
					operators.push("LIKE");
					rowId =  rowId + 1 ;
			  }
			
		});	
	}
	 
	 if(searchType == 'datasetUrl') {
		 var rowId = 3;
		 var attrVal = $("#doeIdentifier").val();
		 attrNames.push('dme_data_id');
			attrValues.push('%' + attrVal + '%' );
			levelValues.push("Dataset");
			rowIds.push(rowId);
			isExcludeParentMetadata.push(false);
			operators.push("LIKE");
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
        	$('body').tooltip({selector: '[data-toggle="tooltip"]'});
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
        	   var permissions = $(this).attr('permissions_role');
        	   var collections = $(this).attr('collections');
        		refreshDataSetDataTable(datsetPath,metadata,acessGrps,permissions,collections);
           });
           
           $(".editCollectionMetadata").click(function(e){
        	   $("#searchFragmentDiv").hide();
        	   $("#dataSetFragment").hide();
        	   $("#editCollectionFragment").show();
        	   var metaDataPath = $(this).attr('metadata_path');
        	   var permissionsRole = $(this).attr('permissions_role');
        	   var collectionId = $(this).attr('collectionId');
        	   var fileName = $(this).attr('data-fileName');
        	   var selectedCollection = $(this).attr('selectedCollection');
        	   var params= {selectedPath:metaDataPath,levelName:selectedCollection,isDataObject:false};
        	   
   			$.ajax({
				type : "GET",
			     url : '/browse/metaData',
			     contentType : 'application/json',
				 data : params,
				 beforeSend: function () {
			    	   $("#spinner").show();
			           $("#dimmer").show();
			       },
				 success : function(msg) {
					 $("#spinner").hide();
			         $("#dimmer").hide();
			         constructCollectionMetData(msg,metaDataPath,false,permissionsRole,collectionId,fileName);
				 },
				error : function(e) {
					 console.log('ERROR: ', e);
					 $("#spinner").hide();
			         $("#dimmer").hide();
				}
			});
        	   
           });
           
           $(".editAccessGroupPermissions").click(function(e){
         	   var collectionId = $(this).attr('collectionId');
         	 var metaDataPath = $(this).attr('metadata_path');
         	var selectedCollection = $(this).attr('selectedCollection');
         	var collectionName = $(this).attr('collection_name');
     	   var params= {selectedPath:metaDataPath,levelName:selectedCollection};
    	   
  			$.ajax({
				type : "GET",
			     url : '/browse/getAccessgroups',
			     contentType : 'application/json',
				 data : params,
				 beforeSend: function () {
			    	   $("#spinner").show();
			           $("#dimmer").show();
			       },
				 success : function(msg) {
					 $("#spinner").hide();
			         $("#dimmer").hide();
			         editAccessPermissions(collectionId,metaDataPath,msg,selectedCollection,collectionName);
				 },
				error : function(e) {
					 console.log('ERROR: ', e);
					 $("#spinner").hide();
			         $("#dimmer").hide();
				}
			});
        	   
           });
           
           $(".selectCheckboxForIns").click(function(e){
        	   var table = $(e.target).closest('table').attr('id');
        	   var len = $('#' + table).find("input[type=checkbox]:checked").length;
        	   if (len >= 1) {
        		   $("#downloadSelected").prop("disabled", false);
        	   } else {
        		   $("#downloadSelected").prop("disabled", true);
        	   }
        	   
           });
           
           $(".selectRadioForDataSet").click(function(e){
        	   var table = $(e.target).closest('table').attr('id');
        	   var len = $('#' + table).find("input[type=radio]:checked").length;
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
                return renderDataSetName(data, type, row);
            },
        	},
            {"data": "path", "render": function (data, type, row) {
                    return renderPath(data, type, row);
                },
        },

        ],
        "columnDefs": [
            {className: "td_class_3", "targets": [0]},
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

    
function renderDataSetName(data, type, row){
	var html = "";
	
	var study;
	var ins;
	var permissions ={};
	var collections = {};
	var isLoggedOnuserExists = (loggedOnUserInfo ? true:false);
	
	study = JSON.stringify(row.studyUserMetadata);
	ins = JSON.stringify(row.instituteUserMetadata);
	data = JSON.stringify(row.selfMetadata);
	permissions.dataLevelAccessGroups = row.dataLevelAccessGroups;
	permissions.studyLevelAccessGroups = row.studyLevelAccessGroups;
	permissions.programLevelAccessGroups = row.programLevelAccessGroups;
	permissions.dataCollectionId = row.dataSetCollectionId;
	permissions.studyCollectionId = row.studyCollectionId;
	permissions.progCollectionId = row.programCollectionId;
	collections.datasetName = row.dataSetName;
	collections.studyName = row.studyName;
	collections.programName = row.programName;
	
	if(isLoggedOnuserExists) {
		var editDataSetHtml = "";
		var editStudySetHtml = "";
		var editProgramSetHtml = "";
		var checkboxHtml ="";
		
		if(isUploader && isUploader == true) {
			checkboxHtml +="<input aria-label='checkbox' type='checkbox' id=" + row.dataSetPath + " class='selectCheckboxForIns'/>";
		} else {
			checkboxHtml += "<input aria-label='radio' type='radio' id=" + row.dataSetPath + " class='selectRadioForDataSet'/>";
		}
		if(row.dataSetPermissionRole && row.dataSetPermissionRole != 'No Permissions') {
			editDataSetHtml = "<span class='editCollectionMetadata' selectedCollection = 'Dataset' data-fileName = '" + row.dataSetName + "' collectionId  = '" + row.dataSetCollectionId + "' " +
			"permissions_role = '" + row.dataSetPermissionRole + "'" +
			" metadata_path  = '" + row.dataSetPath+ "' metadata_set = '" + data  + "'>" +
           "<i class='fa fa-edit' data-toggle='tooltip' title='Edit Dataset Metadata'></i></span>";
			if(row.dataSetPermissionRole == 'Owner') {
				editDataSetHtml += "&nbsp;&nbsp;<span class='editAccessGroupPermissions' collection_name = '" + row.dataSetName + "' collectionId  = '" + row.dataSetCollectionId + "' " +
			    "permissions_groups ='"+ JSON.stringify(permissions) + "'  selectedCollection = 'Dataset' access_groups  = '" + row.dataLevelAccessGroups+ "' metadata_path  = '" + row.dataSetPath+ "'>" +
                 "<i class='fa fa-users' data-toggle='tooltip' title='Edit Dataset Access Permissions'></i></span>";
			}
		}
			
		html += "<div class='col-md-10' style='font-size:16px;margin-top:20px;'><div class='row'><div class='col-md-12'>" +
				""+checkboxHtml+"&nbsp;&nbsp;&nbsp;" +
						"<a href='#' class='dataSetFragment' " +
			"permissions_role = '" + row.dataSetPermissionRole + "'  collections = '" + JSON.stringify(collections)+ "' access_grp ='"+row.dataLevelAccessGroups +"'" +
					" metadata_type = '" + data  + "' data_set_path = " + row.dataSetPath + ">" +
							"<span class='cil_14_bold_no_color'>" + row.dataSetName + "</span></a>" +
			"&nbsp&nbsp;" + editDataSetHtml + "</div></div></div>";

	} else {
		html += "<div class='col-md-10' style='font-size:16px;margin-top:20px;'><div class='row'><div class='col-md-12'>"+
		"&nbsp;&nbsp;&nbsp;<a href='#' class='dataSetFragment' collections = '" + JSON.stringify(collections)+ "' permissions_role = '" + row.dataSetPermissionRole + "' access_grp ='"+row.dataLevelAccessGroups +"' metadata_type = '" + data  + "' data_set_path = " + row.dataSetPath + "><span class='cil_14_bold_no_color'>" + row.dataSetName + "</span></a>" +
		"&nbsp&nbsp;</div></div></div>";
	}
    return html;	
 }

function renderPath(data, type, row) {
	var html = "";
	
	var study;
	var ins;
	var permissions ={};
	var collections = {};
	var isLoggedOnuserExists = (loggedOnUserInfo ? true:false);
	
	study = JSON.stringify(row.studyUserMetadata);
	ins = JSON.stringify(row.instituteUserMetadata);
	data = JSON.stringify(row.selfMetadata);
	permissions.dataLevelAccessGroups = row.dataLevelAccessGroups;
	permissions.studyLevelAccessGroups = row.studyLevelAccessGroups;
	permissions.programLevelAccessGroups = row.programLevelAccessGroups;
	permissions.dataCollectionId = row.dataSetCollectionId;
	permissions.studyCollectionId = row.studyCollectionId;
	permissions.progCollectionId = row.programCollectionId;
	collections.datasetName = row.dataSetName;
	collections.studyName = row.studyName;
	collections.programName = row.programName;
	
	if(isLoggedOnuserExists) {
		var editDataSetHtml = "";
		var editStudySetHtml = "";
		var editProgramSetHtml = "";
		var checkboxHtml ="";
				
		if(row.studyPermissionRole && row.studyPermissionRole != 'No Permissions') {
			editStudySetHtml = "<span class='editCollectionMetadata' selectedCollection = 'study' data-fileName = '" + row.studyName + "' collectionId  = '" + row.studyCollectionId + "'" +
							" permissions_role = '" + row.studyPermissionRole + "' metadata_path  = '" + row.studyPath+ "' " +
									" metadata_set = '" + study  + "'>" +
			"<i class='fa fa-edit' data-toggle='tooltip' title='Edit Study Metadata'></i></span>";
			if(row.studyPermissionRole == 'Owner') {
				editStudySetHtml += "&nbsp;&nbsp;<span class='editAccessGroupPermissions' collection_name ='" +row.studyName + "' collectionId  = '" + row.studyCollectionId + "' " +
			    " permissions_groups ='"+ JSON.stringify(permissions) + "'   selectedCollection = 'study'  access_groups  = '" + row.studyLevelAccessGroups+ "' metadata_path  = '" + row.studyPath+ "'>" +
                 "<i class='fa fa-users' data-toggle='tooltip' title='Edit Study Access Permissions'></i></span>";
			}
		}
		
		if(row.programPermissionRole && row.programPermissionRole != 'No Permissions') {
			editProgramSetHtml = "<span class='editCollectionMetadata' selectedCollection = 'program' data-fileName = '" + row.programName + "' collectionId  = '" + row.programCollectionId + "'" +
							" permissions_role = '" + row.programPermissionRole + "' metadata_path  = '" + row.institutePath+ "' " +
									"metadata_set = '" + ins  + "'>" +
			"<i class='fa fa-edit' data-toggle='tooltip' title='Edit Program Metadata'></i></span>"; 
			
			if(row.programPermissionRole == 'Owner') {
				editProgramSetHtml += "&nbsp;&nbsp;<span class='editAccessGroupPermissions' collection_name ='" +row.programName + "' collectionId  = '" + row.programCollectionId + "' " +
			    "permissions_groups ='"+ JSON.stringify(permissions) + "'   selectedCollection = 'program'  access_groups  = '" + row.programLevelAccessGroups+ "' metadata_path  = '" + row.institutePath+ "'>" +
                 "<i class='fa fa-users' data-toggle='tooltip' title='Edit Program Access Permissions'></i></span>";
			}
		}
	
		html += "<div class='col-md-10' style='font-size:16px;margin-top:20px;'><div class='row'>" +
				"<div class='col-md-12'></div>" +
			"<div class='col-md-12' style='margin-left:22px;'>" +
					"<span class='cil_12_bold_no_color'>" + row.dataSetDescription + "</span>" +
			"<br></div><div class='col-md-12'><br></div><div class='col-md-12' style='margin-left:22px;'>" +
			"<span class='cil_12_bold_no_color'>Study: </span><a class='cil_12_no_color button2a' metadata_type = '" + study  + "' tabindex='0'" +
			" data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' data-popover-content='#a01'>" + row.studyName + "</a>" +
					"&nbsp&nbsp;"+editStudySetHtml+"</div>" +
			"<div class='col-md-12 top-buffer' style='margin-left:22px;'>" +
			"<span class='cil_12_bold_no_color'>Program: </span><a class='cil_12_no_color button2a' metadata_type = '" + ins  + "' tabindex='0'" +
			" data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' data-popover-content='#a01'>" + row.programName + "</a>" +
					"&nbsp&nbsp;"+editProgramSetHtml+"</div></div></div>";

	} else {
		html += "<div class='col-md-10' style='font-size:16px;margin-top:20px;'><div class='row'>" +
				"<div class='col-md-12'></div>" +
		"<div class='col-md-12' style='margin-left:22px;'>" +
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
    
    if (/.*Date.*/.test($(optionVal).text())) {
    	var $dateColumn = $('<div class="form-row><div class="form-group" style="padding-right: 50px;">'
    			+'<label>From: <input type="date" class="fromDate" id="from_' + fieldPath + '" > </label></div> <div class="form-group" '
    			+'style=" padding-left: 50px;"> <label>To: <input type="date" class="toDate" id="To_' + fieldPath + '"> </label></div></div>');
    	$inputGroup.append($dateColumn);
    } else {
    	var advancedSearchInput = $('<input type="text" data-value="' + fieldPath + '" data-type="'
    	        + $(optionVal).attr('data-type') + '" id="metadatasearch_' + mdIdentifier
    	        + '" class="form-control" placeholder="Enter a keyword..."'
    	        + ' title="Enter a Search Keyword or Phrase" aria-label="Enter a Search Keyword or Phrase"'
    	        + ' inputtype="textval"/>');
    	    $inputGroup.append(advancedSearchInput);
    }
    	    var $inputGroupButton = $('<div class="input-group-btn" />');
    	    $inputGroup.append($inputGroupButton);
    	    $inputGroupButton.append('<input class="btn btn-primary pull-right" style="background-color: #7C7C7C !important;color: #fff;" type="button" ' +
    	        'value="X" onclick="removeRowAddOption(\'' + rowId + '\')"/>');
    	    
    $("#advSearchDiv").show();

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
    $('body').tooltip({selector: '[data-toggle="tooltip"]'});
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
    var headerTest = $this.attr('metadata_context');
    if(!headerTest) {
    	headerTest = 'User Metadata';
    } else {
    	headerTest = 'System Metadata';
    }
    var list = JSON.parse(metadata);
    
      var ind = "<div id=\"a01\" class=\"col-md-12 hidden\"> <div class=\"popover-heading\">" +
                "" + headerTest +" <a class=\"button closeBtn float-right\" href=\"javascript:void(0);\"><i class=\"fa fa-times\"></i></a> </div>" +
                "<div class='popover-body'> <div class='divTable' style='width: 100%;border: 1px solid #000;'>" +
                "<div class='divTableBody'><div class='divTableRow'>" +
                "<div class='divTableHead'>Attribute</div>" + 
                "<div class='divTableHead'>Value</div></div>";

            var content = "";

            $.each(list, function( key, value ) {	
                content += "<div class='divTableRow'><div class='divTableCell'>" + value.displayName + "</div>" +
                        "<div class='divTableCell'>" + value.value + "</div></div>";
                });
            
            var table = ind + content + "</div> </div></div> </div>";
            $("#a01").remove();
            pop.after(table);
            pop.data('bs.popover').setContent();
            pop.popover('show');
        
   
}
