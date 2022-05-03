$(document).ready(function () {
	$("#landing-tab").removeClass('active');
	$("#search-tab").addClass('active');
	if($(".backToStatusTabLink").is(':visible')) {
		$("#search-tab").removeClass('active');
		$("#manageTasks-tab").addClass('active');
	} else {
		$("#search-tab").addClass('active');
		$("#manageTasks-tab").removeClass('active');
	}
	
	 $('.dt-responsive tbody').on('click', 'td', function () {
		 initializeToolTips();
		
	 });
	 $('body').tooltip({selector: '[data-toggle="tooltip"]'});
	 var dmeDataId = $("#dmeDataId").val();
	 var doiId = $("#doi").val();
	 var returnToSearch = $("#returnToSearch").val();
	 if(dmeDataId || doiId) {
		 populateSearchCriteria('datasetUrl');
	 } else if(returnToSearch) {
		 var search = $("#searchQuery").val();
		 var list= JSON.parse(search);
		 var isShowMyCollection = list.isShowMyCollection;
		 if(isShowMyCollection) {
			 $("#returnToSearchMyCollection").val(isShowMyCollection);
		 }
		 for (var i = 1; i < list.attrName.length; i++) {
			 var attrVal = list.attrValuesString.split('@@');
			 var iskeyWordSearch = list.iskeyWordSearch[i];
			 if(iskeyWordSearch == true) {
				 var attrval = attrVal[i];
				 var newAttrVal = attrval.replaceAll('%', '');
				 $("#attributeVal").val(newAttrVal);
				 $("#resetBtn").show();
			 } else {
				 var attrName = list.attrName[i];
				 var attrVal = attrVal[i];
				 $(".attrName").each(function(){
					 if($(this).text() == attrName) {
						 $(this).parent().parent().find(".filteritem").each(function(){
							 if($(this).val() == attrVal) {
								 $(this).prop("checked",true);
								 $(this).trigger('change');
							 }
						 })
					 }
				 })
			 }
		 }
		 populateSearchCriteria(null);
	 } else {
		 populateSearchCriteria(null);
	 }
	 
	 showFirstFewFields();
	 
	 $(document).keypress(function(event){	
			var keycode = (event.keyCode ? event.keyCode : event.which);
			if(keycode == '13'){
				 event.preventDefault();
				 populateSearchCriteria('displayAllResults');
			}

		});
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
	var iskeyWordSearch = [];
	
	attrNames.push("collection_type");
	attrValues.push("Asset");
	levelValues.push("ANY");
	isExcludeParentMetadata.push(false);
	rowIds.push(1);
	operators.push("EQUAL");
	iskeyWordSearch.push(false);

			
	 if($("#attributeVal").val()) {
		    attrNames.push("ANY");
			attrValues.push('%' + $("#attributeVal").val().trim() + '%');
			levelValues.push("ANY");
			isExcludeParentMetadata.push(false);
			rowIds.push(2);
			operators.push("LIKE");
			iskeyWordSearch.push(true);
	} 

		var rowId = 3;
		$(".filteritem:checked").each(function () {
			var attrName = $(this).parent().parent().attr('id');
	        var attrVal = $(this).val();					
	        attrNames.push(attrName);
			levelValues.push("ANY");
			attrValues.push(attrVal);
			rowIds.push(rowId);
			isExcludeParentMetadata.push(false);
			operators.push("EQUAL");
			iskeyWordSearch.push(false);
			rowId =  rowId + 1 ;
			
		});	

	 if(searchType == 'datasetUrl') {
		 var rowId = 3;
		 var attrVal = $("#dmeDataId").val();
		 var attrVal1 = $("#doi").val();
		 if(attrVal) {
			 attrNames.push('dme_data_id');
			 attrValues.push('%' + attrVal + '%' );
		 } else if(attrVal1) {
			 attrNames.push('doi');
			 attrValues.push('%' + attrVal1 + '%' );
		 }
		    iskeyWordSearch.push(false);
			levelValues.push("Asset");
			rowIds.push(rowId);
			isExcludeParentMetadata.push(false);
			operators.push("LIKE");
	 }
	 
		 
	 
		search_criteria_json.attrName = attrNames.join();
		search_criteria_json.attrValuesString = attrValues.join("@@");
		search_criteria_json.rowId = rowIds.join();	
		search_criteria_json.level = levelValues.join();
		search_criteria_json.isExcludeParentMetadata = isExcludeParentMetadata.join();
		search_criteria_json.iskeyWordSearch = iskeyWordSearch.join();
		search_criteria_json.operator = operators.join();
		var myCollection = $("#returnToSearchMyCollection").val();
		search_criteria_json.isShowMyCollection = myCollection;
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
        t.ajax.reload(null, true);
       
    }
    if(isVisible && !$("#myCollections").is(':visible')) {
    	var myCollection = $("#returnToSearchMyCollection").val();
    	if(myCollection && myCollection == "true") {
    		 $("div.toolbar").prepend('<div style="float: left;">'+
              	   '<label><input type="checkbox" checked="true" id="myCollections" style="transform: translateY(1.5px);">'+
             	   '&nbsp;&nbsp;Collections I Can Edit</label></div>');
    	} else {
    		$("div.toolbar").prepend('<div style="float: left;">'+
               	   '<label><input type="checkbox" id="myCollections" style="transform: translateY(1.5px);">'+
              	   '&nbsp;&nbsp;Collections I Can Edit</label></div>');
    	} 
    	
    	
    	
    }
   
}

function dataTableInit(isVisible) {
    $('#searchResultTable').DataTable({
        "paging": true,
        "ordering": false,
        "info": true,
        "pageLength": 25,
        "ajax": {
            "url": "/search",
            "type": "GET",
            "data": function (d) {
               d.searchType = search_criteria_json.searchType;
               d.detailed = search_criteria_json.detailed;
               d.level = search_criteria_json.level;
               d.attrName =search_criteria_json.attrName;
               d.attrValuesString =search_criteria_json.attrValuesString;
               d.rowId = search_criteria_json.rowId;
               d.isExcludeParentMetadata = search_criteria_json.isExcludeParentMetadata;
               d.iskeyWordSearch = search_criteria_json.iskeyWordSearch;
               d.operator = search_criteria_json.operator;
               d.isShowMyCollection = search_criteria_json.isShowMyCollection;
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

        "initComplete": function () {
        	$('body').tooltip({selector: '[data-toggle="tooltip"]'});
        },

        "drawCallback": function () {
        	
			$("#myCollections").on('change', function() {
				if ($(this).is(':checked')) {
					$(this).prop('checked', true);
					$("#returnToSearchMyCollection").val("true");
				} else {
					$(this).prop('checked', false);
					$("#returnToSearchMyCollection").val("false");
				}
				populateSearchCriteria('displayAllResults');
			});
						
        	$("#searchResultTable thead").remove();
        	if(isVisible) {
        		$("#downloadSelected").show();
        	} else {
        		$("#downloadSelected").hide();
        	}

           $(".dataSetFragment").click(function() {
        	   var dmeDataId = $(this).attr('dme_data_id');
        	   location.replace('/assetDetails?returnToSearch=true&&dme_data_id='+dmeDataId);
           });
           
           $(".editCollectionMetadata").click(function(){
        	   $("#searchFragmentDiv").hide();
        	   $("#dataSetFragment").hide();
        	   $("#editCollectionFragment").show();
        	   $(".backToAssetDetailsBtn").hide();
   	           $(".backToSearchBtn").show();
        	   var metaDataPath = $(this).attr('metadata_path');
        	   var permissionsRole = $(this).attr('permissions_role');
        	   var collectionId = $(this).attr('collectionId');
        	   var fileName = $(this).attr('data-fileName');
        	   var selectedCollection = $(this).attr('selectedCollection');
        	   var assetType =  $(this).attr('asset_type');
        	   var isReferenceDataset = $(this).attr('is_reference_dataset');

   				$("#userMetaData tbody").html("");
   				$("#path").val(metaDataPath);
   				$(".editCollectionSuccess").hide();
   				$(".editCollectionMsg").html("");
   				$(".editCollectionError").hide();
   				$(".editCollectionErrorMsg").html("");
   				$("#collectionId").val(collectionId);
   				$("#isDataObject").val(false);
   				$("#editUserMetadataFileName").html(fileName);
   			 
   				if(permissionsRole && permissionsRole == 'Owner') {
   					$("#updatePermissions").show();
   				} else {
   					$("#updatePermissions").hide();
   				}
   				var controllerAttributeList = [];
   				var controllerValueList = [];
   				
   				controllerAttributeList.push("is_reference_dataset");
   				controllerAttributeList.push("asset_type");
   				controllerValueList.push(isReferenceDataset);
   				controllerValueList.push(assetType);
   				
   				var params1= {selectedPath:metaDataPath,collectionType:selectedCollection,controllerValue:controllerValueList.join(),refresh:false,controllerAttribute:controllerAttributeList.join()};
   				invokeAjax('/addCollection','GET',params1,constructEditCollectionMetadata,null,null,null);
        	   
           });
           
           $(".editAccessGroupPermissions").click(function(){
         	 var collectionId = $(this).attr('collectionId');
         	 var metaDataPath = $(this).attr('metadata_path');
         	 var selectedCollection = $(this).attr('selectedCollection');
         	 var collectionName = $(this).attr('collection_name');
     	     var params= {selectedPath:metaDataPath,levelName:selectedCollection};
    	   
  			$.ajax({
				type : "GET",
			     url : '/getAccessgroups',
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
 
        	   var clipboard = new ClipboardJS('.share-link-copy-button');

        	   clipboard.on('success', function(e) {
          	     console.log(e);
          	     $(e.trigger).tooltip('hide').attr('data-original-title', 'Copied').tooltip('show');
          	     setTimeout(function() {
                       $(e.trigger).tooltip('hide');
                       $(e.trigger).attr('data-original-title', 'Copy to Clipboard');
                         }, 2000);
          	  
          	   });

        	   clipboard.on('error', function(e) {
        	     console.log(e);
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
            {className: "td_class_7", "targets": [0]},
            {className: "td_class_9", "targets": [1]},
        ],
        	   
        "dom": '<"toolbar top"lip>rt<"bottom"ip>',
        
        "pagingType": "simple",

        "lengthMenu": [[10, 25, 50, 100], [10, 25, 50, 100]],

        "language": {
        	"sLoadingRecords": "Loading...",
        	"lengthMenu": "ROWS PER PAGE &nbsp;&nbsp; _MENU_",
            "zeroRecords": "Nothing found to display",
            "paginate": {
            	 next: '<i style="color:#000;font-size:17px;" class="fas fa-caret-right"></i>',
                 previous: '<i style="color:#000;font-size:17px;" class="fas fa-caret-left"></i>'
              }
        }
    });
}

    
function renderDataSetName(data, type, row){
	
	var html = "";
	var isLoggedOnuserExists = (loggedOnUserInfo ? true:false);


	if(isLoggedOnuserExists) {
		var editDataSetHtml = "";
		var checkboxHtml = "";
		
		if(row.isBulkAsset == false) {
		   if(isUploader && isUploader == true) {
			checkboxHtml += "<input aria-label='checkbox' type='checkbox' id=" + row.dataSetPath + " " +
					        "class='selectCheckboxForIns'/>";
		    } else {
			checkboxHtml += "<input aria-label='radio' type='radio' id=" + row.dataSetPath + " class='selectRadioForDataSet'/>";
		    }
		} else  {
			checkboxHtml += "<input aria-label='checkbox' style='display:none;' type='checkbox' id=" + row.dataSetPath + " " +
	        "class='selectCheckboxForIns'/>";
		}
		
		if(row.dataSetPermissionRole && row.dataSetPermissionRole != 'No Permissions') {
			editDataSetHtml = "<span class='editCollectionMetadata' asset_type = " + row.assetType + " is_reference_dataset = " + row.isReferenceDataset + " selectedCollection = 'Asset' " +
					          "data-fileName = '" + row.dataSetName + "' collectionId  = '" + row.dataSetCollectionId + "' " +
			                  "permissions_role = '" + row.dataSetPermissionRole + "'" +
			                  " metadata_path  = '" + row.dataSetPath+ "'>" +
                              "<img src='images/Search_EditMetaData.svg' data-toggle='tooltip' title='Edit Asset Metadata' th:src='@{/images/Search_EditMetaData.svg}' " +
			                  "style='width:15px;' alt='edit collection'></span>";
			
			if(row.dataSetPermissionRole == 'Owner') {
				editDataSetHtml += "&nbsp;&nbsp;<span class='editAccessGroupPermissions' collection_name = '" + row.dataSetName + "' " +
						            "collectionId  = '" + row.dataSetCollectionId + "' " +
			                        " selectedCollection = 'Asset' " +
			    		            "metadata_path  = '" + row.dataSetPath+ "'>" +
                                    "<img src='images/Search_AccessGroups.svg' data-toggle='tooltip' " +
                                    "title='Edit Asset Access Permissions' " +
                                    "th:src='@{/images/Search_AccessGroups.svg}' " +
			                        "style='width:15px;' alt='Edit Asset Access Permissions'></span>";
			}
		}
			
		html += "<div class='col-md-12' style='font-size:16px;margin-top:10px;'><div class='row'><div class='col-md-12'>" +
				""+checkboxHtml+"&nbsp;&nbsp;&nbsp;" +
				"<a href='#' class='dataSetFragment' " +
			    "dme_data_id  = '" + row.dmeDataId + "' permissions_role = '" + row.dataSetPermissionRole + "' " +
				"data_set_path = " + row.dataSetPath + ">" +
				"<span class='cil_14_bold_no_color'>" + row.dataSetName + "</span></a>" +
			    "&nbsp&nbsp;" + editDataSetHtml + "</div></div></div>";

	} else {
		html += "<div class='col-md-12' style='font-size:16px;margin-top:10px;'><div class='row'><div class='col-md-12'>"+
		        "&nbsp;&nbsp;&nbsp;<a href='#' class='dataSetFragment' " +
				"dme_data_id  = '" + row.dmeDataId + "' permissions_role = '" + row.dataSetPermissionRole + "'" +
				"data_set_path = " + row.dataSetPath + ">" +
				"<span class='cil_14_bold_no_color'>" + row.dataSetName + "</span></a>" +
		        "&nbsp&nbsp;</div></div></div>";
	}
	
	html+= "<div class='col-md-12' style='margin-left: 25px;'><span class='sharableLink'>Sharable Link: " +
			"<i class='fas fa-share'></i></span><p style='display: none;' class ='sharableLinkDiv'><input type='text' " +
			"id= 'colId"+row.dataSetCollectionId+"' value='"+ row.dataSetdmeDataId + "' readonly='true'/> &nbsp; " +
			"<button type='button' class='share-link-copy-button' data-toggle='tooltip' data-placement='bottom' " +
			"title='Copy to clipboard' data-clipboard-target='#colId"+row.dataSetCollectionId+"'>" +
			"<img src='images/clippy.svg' width='13' alt='Copy to clipboard'/></button></p></div>";
    
    return html;	
 }

function renderPath(data, type, row) {
	
	var html = "";
	var isLoggedOnuserExists = (loggedOnUserInfo ? true:false);
	
	if(isLoggedOnuserExists) {
		var editStudySetHtml = "";
		var editProgramSetHtml = "";
		
				
		if(row.studyPermissionRole && row.studyPermissionRole != 'No Permissions') {
			editStudySetHtml = "<span class='editCollectionMetadata' selectedCollection = 'Study' " +
					           "data-fileName = '" + row.studyName + "' collectionId  = '" + row.studyCollectionId + "' " +
					           "permissions_role = '" + row.studyPermissionRole + "' metadata_path  = '" + row.studyPath+ "'> " +
					           "<img src='images/Search_EditMetaData.svg' data-toggle='tooltip' title='Edit Study Metadata'" +
					           "th:src='@{/images/Search_EditMetaData.svg}' style='width:15px;' alt='edit collection'></span>";
			
			
		if(row.studyPermissionRole == 'Owner') {
			editStudySetHtml += "&nbsp;&nbsp;<span class='editAccessGroupPermissions' collection_name ='" +row.studyName + "'" +
						        " collectionId  = '" + row.studyCollectionId + "' " +
			                    " selectedCollection = 'Study'  " +
			    		        "metadata_path  = '" + row.studyPath+ "'>" +
                                "<img src='images/Search_AccessGroups.svg' data-toggle='tooltip' title='Edit Study Access Permissions' " +
                                "th:src='@{/images/Search_AccessGroups.svg}' " +
			                    "style='width:15px;' alt='Edit Study Access Permissions'</span>";
			}
		}
		
		if(row.programPermissionRole && row.programPermissionRole != 'No Permissions') {
			editProgramSetHtml = "<span class='editCollectionMetadata' selectedCollection = 'Program' " +
					"data-fileName = '" + row.programName + "' collectionId  = '" + row.programCollectionId + "'" +
					" permissions_role = '" + row.programPermissionRole + "' " +
					"metadata_path  = '" + row.institutePath+ "' >" +
			        "<img src='images/Search_EditMetaData.svg' data-toggle='tooltip' title='Edit Program Metadata' " +
			        "th:src='@{/images/Search_EditMetaData.svg}' " +
			        "style='width:15px;' alt='edit collection'></span>"; 
			
			if(row.programPermissionRole == 'Owner') {
				editProgramSetHtml += "&nbsp;&nbsp;<span class='editAccessGroupPermissions' " +
				"collection_name ='" +row.programName + "' collectionId  = '" + row.programCollectionId + "' " +
			    "selectedCollection = 'Program'  " +
			    "metadata_path  = '" + row.institutePath+ "'>" +
                "<img src='images/Search_AccessGroups.svg' data-toggle='tooltip' title='Edit Program Access Permissions' " +
                "th:src='@{/images/Search_AccessGroups.svg}' " +
			    "style='width:15px;' alt='Edit Program Access Permissions'</span>";
			}
		}
	
		html += "<div class='col-md-10' style='font-size:16px;margin-top:10px;margin-bottom: 10px;'><div class='row'>" +
				"<div class='col-md-12'></div>" +
			    "<div class='col-md-12 cil_12_bold_no_color_dataset'>" +
				"<span style='word-break: break-all;'>" + row.dataSetDescription + "</span>" +
			    "<br></div><div class='col-md-12' style='margin-left:22px;margin-top: 10px;'>" +
			    "<span style='color: #747474;' class='cil_12_bold_no_color'>STUDY: </span>" +
			    "<a class='cil_12_no_color button2a' " +
			    "selected_path='"+row.studyPath+"' collection_type='Study' tabindex='0'" +
			    " data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' " +
			    "data-popover-content='#a01'>" + row.studyName + "</a>" +
				"&nbsp&nbsp;"+editStudySetHtml+"</div>" +
			    "<div class='col-md-12 top-buffer' style='margin-left:22px;'>" +
			    "<span style='color: #747474;' class='cil_12_bold_no_color'>PROGRAM: </span><a class='cil_12_no_color button2a' " +
				" selected_path='"+row.institutePath+"' collection_type='Program' tabindex='0'" +
			    " data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' " +
			    "data-popover-content='#a01'>" + row.programName + "</a>" +
				"&nbsp&nbsp;"+editProgramSetHtml+"</div></div></div>";

	} else {
		html += "<div class='col-md-10' style='font-size:16px;margin-top:10px;margin-bottom: 10px;'><div class='row'>" +
				"<div class='col-md-12'></div>" +
		        "<div class='col-md-12 cil_12_bold_no_color_dataset' >" +
				"<span style='word-break: break-all;'>" + row.dataSetDescription + "</span>" +
		        "</a><br></div><div class='col-md-12' style='margin-left:22px;margin-top: 10px;'>" +
		        "<span style='color: #747474;' class='cil_12_bold_no_color'>STUDY: </span><a class='cil_12_no_color button2a'" +
		        "selected_path='"+row.studyPath+"' collection_type='Study' tabindex='0'" +
		        " data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' " +
		        "data-popover-content='#a01'>" + row.studyName + "</a>" +
				"&nbsp&nbsp;</div>" +
		        "<div class='col-md-12 top-buffer' style='margin-left:22px;'>" +
		        "<span style='color: #747474;' class='cil_12_bold_no_color'>PROGRAM: </span><a class='cil_12_no_color button2a' " +
				"selected_path='"+row.institutePath+"' collection_type='Program' tabindex='0'" +
		        " data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' " +
		        "data-popover-content='#a01'>" + row.programName + "</a>" +
				"&nbsp&nbsp;</div></div></div>";
	}
    return html;
}


function display(value) {
	if (value == "async") {
		$("#AsyncDiv").show();
		$("#SyncDiv").hide();
		$("#s3Div").hide();
		$("#driveDiv").hide();
		$("#download-btn").prop("disabled",false);
	} else if (value == "sync") {
		$("#SyncDiv").show();
		$("#AsyncDiv").hide();
		$("#s3Div").hide();
		$("#driveDiv").hide();
		$("#download-btn").prop("disabled",false);
	} else if (value == "drive") {
		$("#SyncDiv").hide();
		$("#AsyncDiv").hide();
		$("#s3Div").hide();
		$("#driveDiv").show();
		$("#download-btn").prop("disabled",true);
	} else {
		$("#SyncDiv").hide();
		$("#AsyncDiv").hide();
		$("#s3Div").show();
		$("#driveDiv").hide();
		$("#download-btn").prop("disabled",false);
	}
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
    $('.button2a').on('click', function () {
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
    var headerTest = 'User Metadata';
    var selectedPath = $this.attr('selected_path');
    var collection_type = $this.attr('collection_type');
    
  if(collection_type != 'DataObject') {
     var params= {selectedPath:selectedPath,collectionType:collection_type,refresh:false};	
		$.ajax({
		       url: '/addCollection',
		       type: 'GET',
		       contentType: 'application/json',
		       dataType: 'json',
		       data: params,
		       beforeSend: function () {
		    	   $("#spinner").show();
		           $("#dimmer").show();
		       },
		       success: function (data, status) {
		    	   $("#spinner").hide();
		           $("#dimmer").hide();
                   var table ="";
                   
				   if(data.length > 0) {
		    	      
		    	      var ind = "<div id=\"a01\" class=\"col-md-12 hidden\"> <div class=\"popover-heading\">" +
		                "" + headerTest +" <a class=\"button closeBtn float-right\" href=\"javascript:void(0);\"><i class=\"fa fa-times\"></i></a> </div>" +
		                "<div class='popover-body'> <div class='divTable' style='width: 100%;border: 1px solid #000;'>" +
		                "<div class='divTableBody'><div class='divTableRow'>" +
		                "<div class='divTableHead'>ATTRIBUTE</div>" + 
		                "<div class='divTableHead'>VALUE</div></div>";

		               var content = "";

		             $.each(data, function( key, value ) {	
		            	var attrVal = value.attrValue;
		            	if(attrVal && (attrVal.startsWith('https') || attrVal.startsWith('http'))) {
		            		content += "<div class='divTableRow'><div class='divTableCell'>" + value.displayName + "</div>" +
		                    "<div class='divTableCell'><a target='_blank' href=" + attrVal + ">" + attrVal + "</a></div></div>";
		            	} else if(value.attrName.indexOf("access_group") == -1){
		            		content += "<div class='divTableRow'><div class='divTableCell'>" + value.displayName + "</div>" +
		                    "<div class='divTableCell'>" + attrVal + "</div></div>";
		            	}
		                
		              });
					  table = ind + content + "</div> </div></div> </div>";
					} else {
						table = "<div id=\"a01\" class=\"col-md-12 hidden\">" 
						+"<div class=\"popover-heading\"> No User Metadata &nbsp;&nbsp;" +
		                "<a class=\"button closeBtn float-right\" href=\"javascript:void(0);\"><i class=\"fa fa-times\"></i></a> </div>" +
		                "<div class='popover-body'></div></div>";
					}
		            
		            
		            $("#a01").remove();
		            pop.after(table);
					initializePopover();
		            pop.data('bs.popover').setContent();
		            pop.popover('show');
		       },
		       error: function (data, status, error) {
		    	   $("#spinner").hide();
		           $("#dimmer").hide();
				   console.log("===> data: ", data);
		    	   console.log("===> status: ", status);
		    	   console.log("===> error: ", error);
		       }

		   }); 
		} else {
			openDataObjectPopOver($this);
		}
}


function openDataObjectPopOver($this) {
	var pop = $this;
	$('.button2a').not($this).popover('hide');
	var fileName = $this.attr('file_name');
	var selectedPath = $this.attr('selected_path');

	var ind = "<div id=\"a01\" class=\"col-md-12 hidden\"><div class=\"popover-heading\">" + "Metadata for " + fileName
		+ "<a class=\"button closeBtn float-right\" href=\"javascript:void(0);\">"
		+ "<i class=\"fa fa-times\"></i></a> </div><div class='popover-body'>";
	var table = "";
	var content = "";

	var params= {selectedPath:selectedPath};
	
	$.ajax({
		url: '/getDataObjects/getMetadata',
		type: 'GET',
		contentType: 'application/json',
		dataType: 'json',
		data: params,
		beforeSend: function() {
			$("#spinner").show();
			$("#dimmer").show();
		},
		success: function(data, status) {
			$("#spinner").hide();
			$("#dimmer").hide();
			console.log("success status: ", status);
			
			if (data.selfMetadata.length > 0) {

				ind += "<p><b>User Metadata </b></p><div class='divTable' style='width: 100%;border: 1px solid #000;'>"
					+ "<div class='divTableBody'><div class='divTableRow'>"
					+ "<div class='divTableHead rowAttribute'>ATTRIBUTE</div>"
					+ "<div class='divTableHead'>VALUE</div></div>";

				$.each(data.selfMetadata, function(key, value) {
					if (value.value.startsWith('https') || value.value.startsWith('http')) {
						content += "<div class='divTableRow'><div class='divTableCell'>" + value.displayName + "</div>"
							+ "<div class='divTableCell'><a target='_blank' href=" + value.value + ">" + value.value
							+ "</a></div></div>";
					} else {
						content += "<div class='divTableRow'><div class='divTableCell'>" + value.displayName + "</div>"
							+ "<div class='divTableCell'>" + value.value + "</div></div>";
					}

				});
				content += "</div> </div><br/>";

			}

			if (data.systemMetadata.length > 0) {
				content += "<p><b>Key System Metadata </b></p><div class='divTable' style='width: 100%;border: 1px solid #000;'>"
					+ "<div class='divTableBody'><div class='divTableRow'>"
					+ "<div class='divTableHead rowAttribute'>ATTRIBUTE</div>"
					+ "<div class='divTableHead'>VALUE</div></div>";

				$.each(data.systemMetadata, function(key, value) {
					content += "<div class='divTableRow'><div class='divTableCell'>" + value.displayName + "</div>"
						+ "<div class='divTableCell'>" + value.value + "</div></div>";
				});

				content += "</div> </div>";

			}

			table += ind + content + "</div> </div></div> </div>";
			$("#a01").remove();
			pop.after(table);
			initializePopover();
			pop.data('bs.popover').setContent();
			pop.popover('show');

		},
		error: function(data, status, error) {
			$("#spinner").hide();
			$("#dimmer").hide();
			console.log("===> data: ", data);
			console.log("===> status: ", status);
			console.log("===> error: ", error);
		}

	});

}