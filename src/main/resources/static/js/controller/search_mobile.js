function refreshDataTableMobile() {
	console.log("refresh search table for Mobile");
	if (!$.fn.DataTable.isDataTable('#searchResultMobileTable')) {
		dataTableInitForMobile();
	} else {
		var t = $('#searchResultMobileTable').DataTable();
		console.log(t);
		t.ajax.reload(null, true);

	}

}

function dataTableInitForMobile() {
	$('#searchResultMobileTable').DataTable({
		"paging" : true,
		"ordering" : false,
		"info" : true,
		"pageLength" : 25,
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

			$("#searchResultMobileTable thead").remove();
			$(".dataSetFragment").click(function() {
				var dmeDataId = $(this).attr('dme_data_id');
				location.replace('/assetDetails?returnToSearch=true&&dme_data_id=' + dmeDataId);
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
			displayPopoverMobile();
		},

		"columns" : [ {
			"data" : "path",
			"render" : function(data, type, row) {
				return renderSearchAssetRow(data, type, row);
			},
		}, ],
		"dom" : '<"toolbar top">rt<"bottom"lip>',

		"pagingType" : "simple",

		"lengthMenu" : [ [ 10, 25, 50, 100 ], [ 10, 25, 50, 100 ] ],

		"language" : {
			"sLoadingRecords" : "Loading...",
			"lengthMenu" : "ROWS PER PAGE &nbsp;&nbsp; _MENU_",
			"zeroRecords" : "Nothing found to display",
			"paginate" : {
				next : '<i style="color:#000;font-size:17px;" class="fas fa-caret-right"></i>',
				previous : '<i style="color:#000;font-size:17px;" class="fas fa-caret-left"></i>'
			}
		}
	});
}

function renderSearchAssetRow(data, type, row) {

	var html = "";

	html += "<div class='col-md-12' style='margin-top:10px;display: inline-flex;'>"
			+ "&nbsp;&nbsp;&nbsp;<a href='#' style='width: 85%;' class='dataSetFragment' " + "dme_data_id  = '" + row.dmeDataId
			+ "' permissions_role = '" + row.dataSetPermissionRole + "'" + "data_set_path = " + row.dataSetPath + ">"
			+ "<div class='cil_14_bold_no_color_mobile' style='margin-left:-1rem;'>" + row.dataSetName + "</div></a>"
			+ "&nbsp&nbsp;";

	html += "<button type='button' style='float:right; width:15%;' class='share-link-copy-button' data-toggle='tooltip' data-placement='bottom' "
			+ "title='Copy to clipboard' data-clipboard-text='"
			+ row.dataSetdmeDataId
			+ "'>"
			+ "<img src='images/Search.Shareable_Link.svg' width='18' alt='Copy to clipboard'/></button></div>";

	html += "<div class='col-md-12 cil_12_bold_no_color_mobile_dataset' >" + "<span style='word-break: break-all;'>"
			+ row.dataSetDescription
			+ "</span>"
			+ "</a><hr style='background-color: #A7A7A7;height: 3px;width: 27px;margin-left: 2px;'></div><div class='col-md-12' style='margin-top: -10px;'>"
			+ "<span style='color: #747474;' class='cil_12_bold_no_color'>STUDY: </span><a class='cil_12_no_color button2a'"
			+ "selected_path='"
			+ row.studyPath
			+ "' collection_type='Study' tabindex='0'"
			+ " data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' "
			+ "data-popover-content='#a01'>"
			+ row.studyName
			+ "</a>"
			+ "&nbsp&nbsp;</div>"
			+ "<div class='col-md-12 top-buffer'>"
			+ "<span style='color: #747474;' class='cil_12_bold_no_color'>PROGRAM: </span><a class='cil_12_no_color button2a' "
			+ "selected_path='" + row.institutePath + "' collection_type='Program' tabindex='0'"
			+ " data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' "
			+ "data-popover-content='#a01'>" + row.programName + "</a>" + "&nbsp&nbsp;</div>";

	return html;
}

function displayPopoverMobile() {
	$('.button2a').on('click', function() {
		openPopOverMobile($(this));

	});
	$('.button2a').on('keypress', function(e) {
		if (e.which == 13 || e.keyCode == 13) {
			openPopOverMobile($(this));
		}
	});
}

function openPopOverMobile($this) {
	var pop = $this;
	$('.button2a').not($this).popover('hide');
	var selectedPath = $this.attr('selected_path');
	var collection_type = $this.attr('collection_type');
	var headerName = "<div class='popoverHeaderMobile'><p class='popoverInfoMobile'>" + collection_type.toUpperCase()
			+ " METADATA</p></div>";

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

						var ind = "<div id=\"a01\" class=\"col-md-12 hidden\"> <div class=\"popover-heading\"><a class=\"button closeBtn float-right\" href=\"javascript:void(0);\"><img src='images/close_metadata_popover.svg' style='margin-top: -8px;' width='13' /></a>"
								+ headerName
								+ "</div>"
								+ "<div class='popover-body'> <div class='divTable' style='width: 100%;border: 1px solid #000;'>"
								+ "<div class='divTableBody'>";

						var content = "";

						$
								.each(
										data,
										function(key, value) {
											var attrVal = value.attrValue;
											if (attrVal && (attrVal.startsWith('https') || attrVal.startsWith('http'))) {
												content += "<div class='divTableRowMobile divTableContent'><div class='divTableCell divAttrNameMobile'><span class='popoverMobileAttrName'>ATTRIBUTE: </span>&nbsp;&nbsp;"
														+ value.displayName
														+ "<br/><span class='popoverMobileAttrVal'>VALUE: </span>&nbsp;&nbsp;<a target='_blank' href="
														+ attrVal + ">" + attrVal + "</a></div></div>";
											} else if (value.attrName.indexOf("access_group") == -1) {
												content += "<div class='divTableRowMobile divTableContent'><div class='divTableCell divAttrNameMobile'><span class='popoverMobileAttrName'>ATTRIBUTE: </span>&nbsp;&nbsp;"
														+ value.displayName
														+ "<br/><span class='popoverMobileAttrVal'>VALUE: </span>&nbsp;&nbsp;"
														+ attrVal + "</div></div>";
											}

										});
						table = ind + content + "</div> </div></div> </div>";
					} else {
						table = "<div id=\"a01\" class=\"col-md-12 hidden\">"
								+ "<div class=\"popover-heading\"> NO USER METADATA &nbsp;&nbsp;"
								+ "<a class=\"button closeBtn float-right\" href=\"javascript:void(0);\"><i class=\"fa fa-times\"></i></a> </div>"
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

}