$(document).ready(function() {
	if (screen.width > 990) {
		$("#mobileKeyworkSearchDiv").hide();
	} else {
		$("#filterSectionDiv").removeClass('col-lg-3 col-md-8');
		$("#mobileKeyworkSearchDiv").show();
	}
	$(".landing-tab").removeClass('active');
	$(".search-tab").addClass('active');
	if ($(".backToStatusTabLink").is(':visible')) {
		$(".search-tab").removeClass('active');
		$("#manageTasks-tab").addClass('active');
	} else {
		$(".search-tab").addClass('active');
		$("#manageTasks-tab").removeClass('active');
	}

	$('.dt-responsive tbody').on('click', 'td', function() {
		initializeToolTips();

	});
	$('body').tooltip({
		selector : '[data-toggle="tooltip"]'
	});
	
	var dmeDataId = $("#dmeDataId").val();
	var doiId = $("#doi").val();
	var returnToSearch = $("#returnToSearch").val();
	if (dmeDataId || doiId) {
		populateSearchCriteria('datasetUrl');
	} else if (returnToSearch) {
		var search = $("#searchQuery").val();
		var list = JSON.parse(search);
		var isShowMyCollection = list.isShowMyCollection;
		if (isShowMyCollection) {
			$("#returnToSearchMyCollection").val(isShowMyCollection);
		}
		for (var i = 1; i < list.attrName.length; i++) {
			var attrVal = list.attrValuesString.split('@@');
			var iskeyWordSearch = list.iskeyWordSearch[i];
			if (iskeyWordSearch == true) {
				var attrval = attrVal[i];
				var newAttrVal = attrval.replaceAll('%', '');

				if (screen.width > 990) {
					$("#attributeVal").val(newAttrVal);
					$("#resetBtn").show();
				} else {
					$("#mobileKeyworkSearchDiv").find("#attributeVal").val(newAttrVal);
					$("#resetBtnMobile").show();
				}

			} else {
				var attrName = list.attrName[i];
				var attrVal = attrVal[i];
				$(".attrName").each(function() {
					if ($(this).text() == attrName) {
						$(this).parent().parent().find(".filteritem").each(function() {
							if ($(this).val() == attrVal) {
								$(this).prop("checked", true);
								$(this).trigger('change');
							}
						})
					}
				})
			}
		}
		populateSearchCriteria(null);
	} else if ($("#keyWord").val()) {
		if (screen.width > 990) {
			$("#attributeVal").val($("#keyWord").val());
			$("#resetBtn").show();
		} else {
			$("#mobileKeyworkSearchDiv").find("#attributeVal").val($("#keyWord").val());
			$("#resetBtnMobile").show();
		}

		populateSearchCriteria(null);
	} else if (screen.width > 990) {
		populateSearchCriteria(null);

	}
	showFirstFewFields();

	$(document).keypress(function(event) {
		var keycode = (event.keyCode ? event.keyCode : event.which);
		if (keycode == '13') {
			if ($("#searchBtn").is(':visible')) {
				event.preventDefault();
				populateSearchCriteria('displayAllResults');
			} else if ($("#searchMobileBtn").is(':visible')) {
				$("#searchMobileBtn").click();
			}
		}
	});
	
	$("#collapseFilters").click(function() {
	
	});
	
	$("#expandFilters").click(function() {
	
	});
});

function populateSearchCriteria(searchType) {

	if (screen.width > 990) {
		$("#searchResultsDiv").show();
		$("#searchResultsMobileDiv").hide();
	} else {
		$("#searchResultsDiv").hide();
		$("#mobileKeyworkSearchDiv").hide();
		$("#filterSectionDiv").hide();
		$("#searchResultsMobileDiv").show();
	}

	search_criteria_json.detailed = true;
	var attrNames = [];
	var attrValues = [];
	var levelValues = [];
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
	var attributeVal;

	if (screen.width > 990) {
		attributeVal = $("#attributeVal").val();
	} else {
		attributeVal = $("#mobileKeyworkSearchDiv").find("#attributeVal").val();
	}

	if (attributeVal) {
		attrNames.push("ANY");
		attrValues.push('%' + attributeVal.trim() + '%');
		levelValues.push("ANY");
		isExcludeParentMetadata.push(false);
		rowIds.push(2);
		operators.push("LIKE");
		iskeyWordSearch.push(true);
	}

	var rowId = 3;
	$(".filteritem:checked").each(function() {
		var attrName = $(this).parent().parent().attr('id');
		var attrVal = $(this).val();
		attrNames.push(attrName);
		levelValues.push("ANY");
		attrValues.push(attrVal);
		rowIds.push(rowId);
		isExcludeParentMetadata.push(false);
		operators.push("EQUAL");
		iskeyWordSearch.push(false);
		rowId = rowId + 1;

	});

	if (searchType == 'datasetUrl') {
		var rowId = 3;
		var attrVal = $("#dmeDataId").val();
		var attrVal1 = $("#doi").val();
		if (attrVal) {
			attrNames.push('dme_data_id');
			attrValues.push('%' + attrVal + '%');
		} else if (attrVal1) {
			attrNames.push('doi');
			attrValues.push('%' + attrVal1 + '%');
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

	if (screen.width > 990) {
		refreshDataTable();
	} else {
		refreshDataTableMobile();
	}

}

function refreshDataTable() {
	var isVisible = (loggedOnUserInfo ? true : false);
	console.log("refresh datatable");
	if (!$.fn.DataTable.isDataTable('#searchResultTable')) {
		dataTableInit(isVisible);
		$("div.toolbar").prepend(
					'<div id="show_search_count"></div><div id="sortingDiv" >'
							+ '<span id="ascSpan"><img src="images/search_ascending.svg"/></span>&nbsp;&nbsp;'
							+ '<span id="descSpan"><img src="images/search_descending.svg"/></span>'
							+ '</div>');
		$("div.toolbar").after (
				'<div class="col-lg-12 col-md-12 col-sm-12 float-right">'
				 + '<button id="downloadSelected" type="button"'
				 + 'class="btn btn-primary float-right mb-2 mr-2" disabled>'
				 + 'DOWNLOAD <br />SELECTED ASSETS <img class="arrow_right_download_selected"' 
				 + 'src="/images/white_right_arrow.svg"/>'
				 + '</button></div>');
	} else {
		var t = $('#searchResultTable').DataTable();
		console.log(t);
		t.ajax.reload(null, true);

	}
	
}

// Function to get the total search results count
 function getTotalSearchResultsCount() {
 		var dataTable = $('#searchResultTable').DataTable();
        return dataTable.rows({ search: 'applied' }).data().length;
 }
      
function dataTableInit(isVisible) {
	$('#searchResultTable').DataTable({
		"paging" : true,
		"info" : true,
		"pageLength" : 25,
		"responsive": true,
		"order": [[0, 'asc']],
		"ajax" : {
			"url" : "/search",
			"type" : "GET",
			"data" : function(d) {
				d.detailed = search_criteria_json.detailed;
				d.level = search_criteria_json.level;
				d.attrName = search_criteria_json.attrName;
				d.attrValuesString = search_criteria_json.attrValuesString;
				d.rowId = search_criteria_json.rowId;
				d.isExcludeParentMetadata = search_criteria_json.isExcludeParentMetadata;
				d.iskeyWordSearch = search_criteria_json.iskeyWordSearch;
				d.operator = search_criteria_json.operator;
				d.isShowMyCollection = search_criteria_json.isShowMyCollection;
			},
			"dataSrc" : function(data) {
				return data;
			},
			"error" : function(xhr, error, thrown) {
				console.log("Response status: " + xhr.status + " (" + xhr.statusText + ")");
				console.log(error + ": " + thrown + " [" + xhr.status + " (" + xhr.statusText + ")]");
				console.log(xhr.responseText);
				console.log(xhr);
				$("#spinner").hide();
				$("#dimmer").hide();
			},

			"beforeSend" : function() {
				$("#spinner").show();
				$("#dimmer").show();
			},

			"complete" : function() {
				$("#spinner").hide();
				$("#dimmer").hide();
			}
		},

		"initComplete" : function() {
			$('body').tooltip({
				selector : '[data-toggle="tooltip"]'
			});
		},

		"drawCallback" : function() {			

			var table = $('#searchResultTable').DataTable();
			$("#searchResultTable thead").remove();
			if (isVisible) {
				$("#downloadSelected").show();
			} else {
				$("#downloadSelected").hide();
			}

			$(".dataSetFragment").click(function() {
				var dmeDataId = $(this).attr('dme_data_id');
				location.replace('/assetDetails?returnToSearch=true&&dme_data_id=' + dmeDataId);
			});
			
			 		  
			 $("#descSpan").click(function() {
				    table.order([[0, 'desc']]).draw();
				    table.ajax.reload(null, true);
			  });
  
			 $('#ascSpan').on('click', function () {				
				    table.order([[0, 'asc']]).draw();
				    table.ajax.reload(null, true);
			  });
			  $("#show_search_count").html("Showing " + getTotalSearchResultsCount() + " Results");

			$(".editCollectionMetadata").click(function() {
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

				$("#userMetaData tbody").html("");
				$("#path").val(metaDataPath);
				$(".editCollectionSuccess").hide();
				$(".editCollectionMsg").html("");
				$(".editCollectionError").hide();
				$(".editCollectionErrorMsg").html("");
				$("#collectionId").val(collectionId);
				$("#isDataObject").val(false);
				$("#editUserMetadataFileName").html(fileName);

				if (permissionsRole && permissionsRole == 'Owner') {
					$("#updatePermissions").show();
				} else {
					$("#updatePermissions").hide();
				}

				var params1 = {
					selectedPath : metaDataPath,
					collectionType : selectedCollection,
					refresh : false
				};
				invokeAjax('/addCollection', 'GET', params1, constructEditCollectionMetadata, null, null, null);

			});

			$(".editAccessGroupPermissions").click(function() {
				var collectionId = $(this).attr('collectionId');
				var metaDataPath = $(this).attr('metadata_path');
				var selectedCollection = $(this).attr('selectedCollection');
				var collectionName = $(this).attr('collection_name');
				var params = {
					selectedPath : metaDataPath,
					levelName : selectedCollection
				};

				$.ajax({
					type : "GET",
					url : '/getAccessgroups',
					contentType : 'application/json',
					data : params,
					beforeSend : function() {
						$("#spinner").show();
						$("#dimmer").show();
					},
					success : function(msg) {
						$("#spinner").hide();
						$("#dimmer").hide();
						editAccessPermissions(collectionId, metaDataPath, msg, selectedCollection, collectionName);
					},
					error : function(e) {
						console.log('ERROR: ', e);
						$("#spinner").hide();
						$("#dimmer").hide();
					}
				});

			});

			$(".selectCheckboxForIns").click(function(e) {
				var table = $(e.target).closest('table').attr('id');
				var len = $('#' + table).find("input[type=checkbox]:checked").length;
				if (len >= 1) {
					$("#downloadSelected").prop("disabled", false);
				} else {
					$("#downloadSelected").prop("disabled", true);
				}

			});

			$(".selectRadioForDataSet").click(function(e) {
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

		"columns" : [ {
			"data" : "dataSetName",
			"render" : function(data, type, row) {
				return renderDataSetName(data, type, row);
			},
		},
		],

		"dom" : '<"toolbar top"lip>rt<"bottom"ip>',

		"pagingType" : "simple",

		"lengthMenu" : [ [ 10, 25, 50, 100 ], [ 10, 25, 50, 100 ] ],

		"language" : {
			"sLoadingRecords" : "Loading...",
			"lengthMenu" : "Results per Page: _MENU_",
			"zeroRecords" : "Nothing found to display",
			"paginate" : {
				next : '<i style="color:#000;font-size:17px;" class="fas fa-caret-right"></i>',
				previous : '<i style="color:#000;font-size:17px;" class="fas fa-caret-left"></i>'
			}
		}
	});
}

function renderDataSetName(data, type, row) {

	var html = "";
	var isLoggedOnuserExists = (loggedOnUserInfo ? true : false);
	var checkboxHtml = "";
	var editDataSetHtml = "";
	var editStudySetHtml = "";
	var editProgramSetHtml = "";
		      
	if (isLoggedOnuserExists) {	
		
		if (row.isBulkAsset == false) {
			if (isUploader && isUploader == true) {
				checkboxHtml += "<input aria-label='checkbox' type='checkbox' id=" + row.dataSetPath + " "
						+ "class='selectCheckboxForIns'/>";
			} else {
				checkboxHtml += "<input aria-label='radio' type='radio' id=" + row.dataSetPath
						+ " class='selectRadioForDataSet'/>";
			}
		} else {
			checkboxHtml += "<input aria-label='checkbox' style='display:none;' type='checkbox' id=" + row.dataSetPath
					+ " " + "class='selectCheckboxForIns'/>";
		}
		
		if (row.dataSetPermissionRole && row.dataSetPermissionRole != 'No Permissions') {
			editDataSetHtml = "&nbsp;<span class='editCollectionMetadata' selectedCollection = 'Asset' "
					+ "data-fileName = '"
					+ row.dataSetName
					+ "' collectionId  = '"
					+ row.dataSetCollectionId
					+ "' "
					+ "permissions_role = '"
					+ row.dataSetPermissionRole
					+ "'"
					+ " metadata_path  = '"
					+ row.dataSetPath
					+ "'>"
					+ "<img class='editCollectionImg' src='images/Search_EditMetaData.svg' data-toggle='tooltip' title='Edit Asset Metadata' th:src='@{/images/Search_EditMetaData.svg}' "
					+ "alt='edit collection'></span>&nbsp;";

			if (row.dataSetPermissionRole == 'Owner') {
				editDataSetHtml += "&nbsp;<span class='editAccessGroupPermissions' collection_name = '"
						+ row.dataSetName + "' " + "collectionId  = '" + row.dataSetCollectionId + "' "
						+ " selectedCollection = 'Asset' " + "metadata_path  = '" + row.dataSetPath + "'>"
						+ "<img class='collectionAccessGrpImg' src='images/Search_AccessGroups.svg' data-toggle='tooltip' "
						+ "title='Edit Asset Access Permissions' " + "th:src='@{/images/Search_AccessGroups.svg}' "
						+ "alt='Edit Asset Access Permissions'></span>&nbsp;";
			}
		}
		

		if (row.studyPermissionRole && row.studyPermissionRole != 'No Permissions') {
			editStudySetHtml = "&nbsp;&nbsp;<span class='editCollectionMetadata' selectedCollection = 'Study' "
					+ "data-fileName = '" + row.studyName + "' collectionId  = '" + row.studyCollectionId + "' "
					+ "permissions_role = '" + row.studyPermissionRole + "' metadata_path  = '" + row.studyPath + "'> "
					+ "<img src='images/Search_EditMetaData.svg' data-toggle='tooltip' title='Edit Study Metadata'"
					+ "th:src='@{/images/Search_EditMetaData.svg}' class='editCollectionImg' alt='edit collection'></span>";

			if (row.studyPermissionRole == 'Owner') {
				editStudySetHtml += "&nbsp;&nbsp;<span class='editAccessGroupPermissions' collection_name ='"
						+ row.studyName
						+ "'"
						+ " collectionId  = '"
						+ row.studyCollectionId
						+ "' "
						+ " selectedCollection = 'Study'  "
						+ "metadata_path  = '"
						+ row.studyPath
						+ "'>"
						+ "<img class='collectionAccessGrpImg' src='images/Search_AccessGroups.svg' data-toggle='tooltip' title='Edit Study Access Permissions' "
						+ "th:src='@{/images/Search_AccessGroups.svg}' "
						+ "alt='Edit Study Access Permissions'</span>";
			}
		}

		if (row.programPermissionRole && row.programPermissionRole != 'No Permissions') {
			editProgramSetHtml = "&nbsp;&nbsp;<span class='editCollectionMetadata' selectedCollection = 'Program' "
					+ "data-fileName = '" + row.programName + "' collectionId  = '" + row.programCollectionId + "'"
					+ " permissions_role = '" + row.programPermissionRole + "' " + "metadata_path  = '"
					+ row.institutePath + "' >"
					+ "<img src='images/Search_EditMetaData.svg' data-toggle='tooltip' title='Edit Program Metadata' "
					+ "th:src='@{/images/Search_EditMetaData.svg}' "
					+ "class='editCollectionImg' alt='edit collection'></span>";

			if (row.programPermissionRole == 'Owner') {
				editProgramSetHtml += "&nbsp;&nbsp;<span class='editAccessGroupPermissions' "
						+ "collection_name ='"
						+ row.programName
						+ "' collectionId  = '"
						+ row.programCollectionId
						+ "' "
						+ "selectedCollection = 'Program'  "
						+ "metadata_path  = '"
						+ row.institutePath
						+ "'>"
						+ "<img src='images/Search_AccessGroups.svg' data-toggle='tooltip' title='Edit Program Access Permissions' "
						+ "th:src='@{/images/Search_AccessGroups.svg}' "
						+ "class='collectionAccessGrpImg' alt='Edit Program Access Permissions'</span>";
			}
	   }
    }
		html += "<div class='ms-card-expanded-1 ms-card-expanded-4'>"
		  		+ "<div class='col-lg-12 col-md-12 col-sm-12 flex-row-9'>"
		  		+ checkboxHtml
		   		+ "<div class='overlap-group2-1'>"
		     	+ " <div class='frame-14'>"
		     	+ " <div class='atom-modeling-pipeli poppins-medium-congress-blue-25px'>"
		        + " <span class='poppins-medium-congress-blue-25px'>" + row.dataSetName + "</span>"
		        + "&nbsp;&nbsp;"
		         + editDataSetHtml
		         + "<button type='button' class='share-link-copy-button' data-toggle='tooltip' data-placement='bottom' "
			     + " title='Copy to clipboard' data-clipboard-text='" + row.dataSetdmeDataId + "'>"
			     + "<img src='images/copy_to_clipboard.svg' width='20' alt='Copy to clipboard'/></button>"
		         + " </div>"
		         + " </div>"
		         + " </div>"
		         + "<div class='ml-auto'>"
		         + "<button type='button' class='expand_card_btn'  data-toggle='collapse' href='#collapse" + row.dmeDataId + "'"
		         + "><img src='images/expand_card.png' data-toggle='tooltip' title= 'Expand and Collapse Card'" 
		         + "alt='Collapse asset details'></button>"
		         + "<button type='button' class='view_asset_details_btn dataSetFragment' data-toggle='tooltip' data-placement='bottom' title='View details of this card'"
		         + " dme_data_id = '" + row.dmeDataId + "'><img src='images/view_details_arrow.png'" 
		         + "alt='View Asset Details'></button>"
		         + "</div>"
		         + "</div>"
		         + "<div id='collapse" + row.dmeDataId + "' class='col-lg-12 col-md-12 col-sm-12'>"
		         + " <img src='images/search_line.png' class='line_divider' alt='asset name divider'/>"
		         + "<div class='overlap-group'><div class='asset-description opensans-bold-midnight-blue-13px'>"
                 + "<span class='opensans-bold-midnight-blue-13px'>ASSET DESCRIPTION: &nbsp;&nbsp;</span>"
                 + "<span class='inter-normal-congress-blue-16px'>" + row.dataSetDescription + "</span></div></div>"               
                 + "<div class='study-container'><div class='study opensans-bold-midnight-blue-13px'>"
                 + "<span class='opensans-bold-midnight-blue-13px'>STUDY: &nbsp;&nbsp;</span>"
                 + "<a class='button2a' style='text-decoration:underline;' collection_name = '" + row.studyName +"' selected_path = '" + row.studyPath + "' collection_type='Study' tabindex='0'"
				 + " data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' "
				 + "data-popover-content='#a01'>"
                 + "<span class='inter-medium-green-blue-15px'>" + row.studyName + " </span></a>"
                 + editStudySetHtml
                 + "</div></div>"
                 + "<div class='program-container'>"
                 + "<div class='program opensans-bold-midnight-blue-13px'>"
                 + "<span class='opensans-bold-midnight-blue-13px'>PROGRAM: &nbsp;&nbsp;</span>"
                 + "<a class='button2a' style='text-decoration:underline;' collection_name = '" + row.programName + "' selected_path = '" + row.institutePath + "' collection_type='Program' tabindex='0'"
				 + "data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' "
				 + "data-popover-content='#a01'>"
                 + "<span class='inter-medium-green-blue-15px'>" + row.programName + " </span></a>"
                 + editProgramSetHtml
                 + "</div></div>"  
                 + "</div>"            
		         + "</div>";		      

	return html;
}



function display(value) {
	if (value == "async") {
		$("#AsyncDiv").show();
		$("#SyncDiv").hide();
		$("#s3Div").hide();
		$("#driveDiv").hide();
		$("#googleCloudDiv").hide();
		$("#download-btn").prop("disabled", false);
		$("#download-btn").css('cursor', 'pointer');
	} else if (value == "sync") {
		$("#SyncDiv").show();
		$("#AsyncDiv").hide();
		$("#s3Div").hide();
		$("#driveDiv").hide();
		$("#googleCloudDiv").hide();
		$("#download-btn").prop("disabled", false);
		$("#download-btn").css('cursor', 'pointer');
	} else if (value == "drive") {
		$("#SyncDiv").hide();
		$("#AsyncDiv").hide();
		$("#s3Div").hide();
		$("#driveDiv").show();
		$("#googleCloudDiv").hide();
		var googleDriveIsAuthorized = $("#googleDriveIsAuthorized").val();
		if (googleDriveIsAuthorized) {
			$("#download-btn").prop("disabled", false);
			$("#download-btn").css('cursor', 'pointer');
			$("#driveAuthlink").css('pointer-events', 'none');
			$("#driveAuthlink").css('opacity', '0.65');
		} else {
			$("#download-btn").prop("disabled", true);
			$("#download-btn").css('cursor', 'default');
			$("#driveAuthlink").css('pointer-events', 'auto');
			$("#driveAuthlink").css('opacity', '1');
		}

	} else if (value == 'cloud') {
		$("#SyncDiv").hide();
		$("#AsyncDiv").hide();
		$("#s3Div").hide();
		$("#driveDiv").hide();
		$("#googleCloudDiv").show();
		var googleCloudIsAuthorized = $("#googleCloudIsAuthorized").val();
		if (googleCloudIsAuthorized) {
			$("#download-btn").prop("disabled", false);
			$("#download-btn").css('cursor', 'pointer');
			$("#googleCloudAuthlink").css('pointer-events', 'none');
			$("#googleCloudAuthlink").css('opacity', '0.65');
		} else {
			$("#download-btn").prop("disabled", true);
			$("#download-btn").css('cursor', 'default');
			$("#googleCloudAuthlink").css('pointer-events', 'auto');
			$("#googleCloudAuthlink").css('opacity', '1');
		}
	} else {
		$("#SyncDiv").hide();
		$("#AsyncDiv").hide();
		$("#s3Div").show();
		$("#driveDiv").hide();
		$("#googleCloudDiv").hide();
		$("#download-btn").prop("disabled", false);
		$("#download-btn").css('cursor', 'pointer');
	}
}

function initializeToolTips() {
	$('[data-toggle="tooltip"]').tooltip();
	$('body').tooltip({
		selector : '[data-toggle="tooltip"]'
	});
}

function initializePopover() {
	$("[data-toggle=popover]").popover({
		html : true,
		trigger : 'manual',
		container : 'body',
		placement : 'left',
		delay : {
			show : 50,
			hide : 50
		},
		content : function() {
			var content = $(this).attr("data-popover-content");
			return $(content).children(".popover-body").html();
		},
		title : function() {
			var title = $(this).attr("data-popover-content");
			return $(title).children(".popover-heading").html();
		}
	});
}

function displayPopover() {
	$('.button2a').on('click', function() {
		openPopOver($(this));

	});
	$('.button2a').on('keypress', function(e) {
		if (e.which == 13 || e.keyCode == 13) {
			openPopOver($(this));
		}
	});
}

function openPopOver($this) {
	var pop = $this;
	$('.button2a').not($this).popover('hide');
	var selectedPath = $this.attr('selected_path');
	var collection_type = $this.attr('collection_type');
	var collection_name = $this.attr('collection_name');

	var headerName = "<div class='popoverHeader'><p class='popoverInfo'>Metadata for " + collection_name +"</p>"	+
	                  "<p class='popoverSubInfo'>" + collection_type+ " Metadata</p></div>";

	if (collection_type != 'DataObject') {
		var params = {
			selectedPath : selectedPath,
			collectionType : collection_type,
			refresh : false
		};
		$
				.ajax({
					url : '/addCollection',
					type : 'GET',
					contentType : 'application/json',
					dataType : 'json',
					data : params,
					beforeSend : function() {
						$("#spinner").show();
						$("#dimmer").show();
					},
					success : function(data, status) {
						$("#spinner").hide();
						$("#dimmer").hide();
						var table = "";

						if (data.length > 0) {

							var ind = "<div id=\"a01\" class=\"col-md-12 hidden\"> <div class=\"popover-heading\"><a class=\"button closeBtn float-right\" href=\"javascript:void(0);\"><img src='images/close_metadata_popover.svg' width='16' style='margin-top: -8px;'/></a>"
									+ headerName
									+ "</div>"
									+ "<div class='popover-body'> <div class='divTable' style='width: 100%;border: 1px solid #000;'>"
									+ "<div class='divTableBody'><div class='divTableRow'>"
									+ "<div class='divTableHead'>ATTRIBUTE</div>"
									+ "<div class='divTableHead'>VALUE</div></div>";

							var content = "";

							$
									.each(
											data,
											function(key, value) {
												var attrVal = value.attrValue;
												if (attrVal
														&& (attrVal.startsWith('https') || attrVal.startsWith('http'))) {
													content += "<div class='divTableRow divTableContent'><div class='divTableCell divAttrName'>"
															+ value.displayName.toUpperCase()
															+ "</div>"
															+ "<div class='divTableCell divAttrVal'><a target='_blank' href="
															+ attrVal + ">" + attrVal + "</a></div></div>";
												} else if (value.attrName.indexOf("access_group") == -1) {
													content += "<div class='divTableRow divTableContent'><div class='divTableCell divAttrName'>"
															+ value.displayName.toUpperCase()
															+ "</div>"
															+ "<div class='divTableCell divAttrVal'>"
															+ attrVal
															+ "</div></div>";
												}

											});
							table = ind + content + "</div> </div></div> </div>";
						} else {
							table = "<div id=\"a01\" class=\"col-md-12 hidden\">"
									+ "<div class=\"popover-heading\"> NO USER METADATA &nbsp;&nbsp;"
									+ "<a class=\"button closeBtn float-right\" href=\"javascript:void(0);\"><img src='images/close_metadata_popover.svg' width='16' style='margin-top: -8px;'/></a> </div>"
									+ "<div class='popover-body'></div></div>";
						}

						$("#a01").remove();
						pop.after(table);
						initializePopover();
						pop.data('bs.popover').setContent();
						pop.popover('show');
					},
					error : function(data, status, error) {
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

	var headerName = "<div class='popoverHeader'><p class='popoverInfoDatObj'>Metadata for " + fileName + "</p></div>";

	var ind = "<div id=\"a01\" class=\"col-md-12 hidden\"><div class=\"popover-heading\"><a class=\"button closeBtn float-right\" href=\"javascript:void(0);\">"
			+ "<img src='images/close_metadata_popover.svg' width='16' style='margin-top: -8px;'/></a>"
			+ headerName
			+ " </div><div class='popover-body'>";

	var table = "";
	var content = "";

	var params = {
		selectedPath : selectedPath
	};

	$
			.ajax({
				url : '/getDataObjects/getMetadata',
				type : 'GET',
				contentType : 'application/json',
				dataType : 'json',
				data : params,
				beforeSend : function() {
					$("#spinner").show();
					$("#dimmer").show();
				},
				success : function(data, status) {
					$("#spinner").hide();
					$("#dimmer").hide();
					console.log("success status: ", status);

					if (data.selfMetadata.length > 0) {

						ind += "<p class='divDataObjUserMetadata'>User Metadata</p><div class='divTable' style='width: 100%;border: 1px solid #000;'>"
								+ "<div class='divTableBody'><div class='divTableRow'>"
								+ "<div class='divTableHead rowAttribute'>ATTRIBUTE</div>"
								+ "<div class='divTableHead'>VALUE</div></div>";

						$
								.each(
										data.selfMetadata,
										function(key, value) {
											if (value.value.startsWith('https') || value.value.startsWith('http')) {
												content += "<div class='divTableRow divTableContent'><div class='divTableCell divAttrName'>"
														+ value.displayName.toUpperCase()
														+ "</div>"
														+ "<div class='divTableCell divAttrVal'><a target='_blank' href="
														+ value.value + ">" + value.value + "</a></div></div>";
											} else {
												content += "<div class='divTableRow divTableContent'><div class='divTableCell divAttrName'>"
														+ value.displayName.toUpperCase()
														+ "</div>"
														+ "<div class='divTableCell divAttrVal'>"
														+ value.value
														+ "</div></div>";
											}

										});
						content += "</div> </div><br/>";

					}

					if (data.systemMetadata.length > 0) {
						content += "<p class='divDataObjSysMetadata'>Key System Metadata</p><div class='divTable' style='width: 100%;border: 1px solid #000;'>"
								+ "<div class='divTableBody'><div class='divTableRow'>"
								+ "<div class='divTableHead rowAttribute'>ATTRIBUTE</div>"
								+ "<div class='divTableHead'>VALUE</div></div>";

						$
								.each(
										data.systemMetadata,
										function(key, value) {
											content += "<div class='divTableRow divTableContent'><div class='divTableCell divAttrName'>"
													+ value.displayName.toUpperCase()
													+ "</div>"
													+ "<div class='divTableCell divAttrVal'>"
													+ value.value
													+ "</div></div>";
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
				error : function(data, status, error) {
					$("#spinner").hide();
					$("#dimmer").hide();
					console.log("===> data: ", data);
					console.log("===> status: ", status);
					console.log("===> error: ", error);
				}

			});

}