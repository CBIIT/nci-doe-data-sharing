$(document).ready(function() {
	var dataSetTable;
	refreshTaskDatatable('dataSetTable');

	$("#assetFilesTab").click(function(e) {
		$(this).css('background-color', '#194F85');
		$("#generatePredictionsTab").css('background-color', 'rgb(128 128 128 / 33%)');
		$("#generatePredTable").hide();
		$("#dataSetTable").show();
		refreshTaskDatatable('dataSetTable');
	});

	$("#generatePredictionsTab").click(function(e) {
		$(this).css('background-color', '#194F85');
		$("#assetFilesTab").css('background-color', 'rgb(128 128 128 / 33%)');
		$("#generatePredTable").show();
		$("#dataSetTable").hide();
		refreshTaskDatatable('generatePredTable');
	});

});

function refreshTaskDatatable(table) {
	var isVisible = (loggedOnUserInfo ? true : false);
	console.log("refresh datatable");
	if (!$.fn.DataTable.isDataTable('#' + table)) {
		if (table == 'dataSetTable') {
			$('#generatePredTable').dataTable().fnDestroy();
			dataTableInitDataSet(isVisible);
		} else {
			$('#dataSetTable').dataTable().fnDestroy();
			generatePredTable(isVisible);
		}

	} else {
		var t = $('#' + table).DataTable();
		console.log(t);
		t.ajax.reload(null, false);
	}
}

function generatePredTable(isVisible) {
	$('#generatePredTable').DataTable({
		"paging" : true,
		"ordering" : true,
		"info" : true,
		"pageLength" : 25,
		"ajax" : {
			"url" : "/getDataObjects/getPredictionFiles",
			"type" : "GET",
			"data" : {
				path : $("#assetPath").val()
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

		"initComplete" : function(settings, json) {
			$('body').tooltip({
				selector : '[data-toggle="tooltip"]'
			});
		},

		"drawCallback" : function(settings) {

			$("#downloadSelectedDataSet").prop("disabled", true);
			$("#downloadSelectedMetadata").hide();

			if (isVisible) {
				$("#downloadSelectedDataSet").show();
			} else {
				$("#downloadSelectedDataSet").hide();
			}

			$(".selectAll").change(function(e) {
				var table = $(e.target).closest('table');
				var tableId = table.attr('id');
				var row_count = $('#' + tableId + "> tbody").children().length;
				$(".downloadModelAnalysisLink").prop("disabled", false);
				if ($(this).is(':checked')) {
					$('td input:checkbox', table).prop('checked', true);
					if (row_count >= 1) {
						$("#downloadSelectedDataSet").prop("disabled", false);
					}

				} else {
					$('td input:checkbox', table).prop('checked', false);
					if (row_count > 1) {
						$("#downloadSelectedDataSet").prop("disabled", true);
					}
				}
			});

			$(".selectIndividualCheckbox").click(function(e) {
				var table = $(e.target).closest('table').attr('id');
				if (!$(this).is(':checked')) {
					$("#" + table).find(".selectAll").prop('checked', false);
					$(this).closest("tr").find('a.downloadModelAnalysisLink').prop("disabled", false);
				}
				var len = $('#' + table).find('.selectIndividualCheckbox:checked').length;
				$(".downloadModelAnalysisLink").prop("disabled", false);
				if (len >= 1) {
					$("#downloadSelectedDataSet").prop("disabled", false);
				} else {
					$("#downloadSelectedDataSet").prop("disabled", true);
				}
			});

			$(".downloadModelAnalysisLink").click(function(e) {
				onClickOfModelAnlysisBulkDownloadBtn($(this));
			});

			$("#downloadSelectedDataSet").click(function(e) {
				onClickOfModelAnlysisBulkDownloadBtn();
			});

			var clipboard = new ClipboardJS('.share_path_copy');

			clipboard.on('success', function(e) {
				console.log(e);
				$(e.trigger).tooltip('hide').attr('data-original-title', 'Copied').tooltip('show');
				setTimeout(function() {
					$(e.trigger).tooltip('hide');
					$(e.trigger).attr('data-original-title', 'Copy File Path');
				}, 2000);

			});

			clipboard.on('error', function(e) {
				console.log(e);
			});

			initializeToolTips();
			initializePopover();
			displayPopoverDataSet();
		},

		"columns" : [ {
			"data" : "inputDatasetPath",
			"render" : function(data, type, row) {
				return renderBatchSelect(data, type, row);
			},
			responsivePriority : 4
		},

		{
			"data" : "inputDatasetName",
			"render" : function(data, type, row) {
				return renderInputDatasetName(data, type, row);
			},
			responsivePriority : 1
		},

		{
			"data" : "predictionsName",
			"render" : function(data, type, row) {
				return renderPredictionsName(data, type, row);
			},
			responsivePriority : 2
		},

		{
			"data" : "download",
			"render" : function(data, type, row) {
				return renderGeneratePredDownload(data, type, row);
			},
			responsivePriority : 3
		},

		],
		columnDefs : [ {
			orderable : false,
			className : 'select-checkbox',
			headerHtml : 'batch select',
			blurable : true,
			targets : 0,
			"visible" : isVisible,
		}, {
			"targets" : 0,
			"orderable" : false
		}, {
			"targets" : -1,
			"orderable" : false
		}, {
			"visible" : isVisible,
			"targets" : -1
		} ],

		"dom" : '<"top"lip>frt<"bottom"ip>',
		"pagingType" : "simple",

		"lengthMenu" : [ [ 10, 25, 50, 100 ], [ 10, 25, 50, 100 ] ],

		"language" : {
			"sSearch" : "Filter:",
			"lengthMenu" : "ROWS PER PAGE &nbsp;&nbsp; _MENU_",
			"sLoadingRecords" : "Loading...",
			"zeroRecords" : "Nothing found to display",
			"paginate" : {
				next : '<i style="color:#000;font-size:17px;" class="fas fa-caret-right"></i>',
				previous : '<i style="color:#000;font-size:17px;" class="fas fa-caret-left"></i>'
			}
		}
	});
}

function dataTableInitDataSet(isVisible) {
	dataSetTable = $('#dataSetTable').DataTable({
		"paging" : true,
		"ordering" : true,
		"info" : true,
		"pageLength" : 25,
		"ajax" : {
			"url" : "/getDataObjects",
			"type" : "GET",
			"data" : {
				path : $("#assetPath").val()
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

		"initComplete" : function(settings, json) {
			$('body').tooltip({
				selector : '[data-toggle="tooltip"]'
			});
		},

		"drawCallback" : function(settings) {

			$("#downloadSelectedDataSet").prop("disabled", true);
			$("#downloadSelectedMetadata").prop("disabled", true);

			if (isVisible) {
				$("#downloadSelectedDataSet").show();
				$("#downloadSelectedMetadata").show();
			} else {
				$("#downloadSelectedDataSet").hide();
				$("#downloadSelectedMetadata").hide();
			}

			$(".selectAll").change(function(e) {
				var table = $(e.target).closest('table');
				var tableId = table.attr('id');
				var row_count = $('#' + tableId + "> tbody").children().length;
				$(".downloadLink").prop("disabled", false);
				if ($(this).is(':checked')) {
					$('td input:checkbox', table).prop('checked', true);
					if (row_count >= 1) {
						$("#downloadSelectedDataSet").prop("disabled", false);
						$("#downloadSelectedMetadata").prop("disabled", false);
					}

				} else {
					$('td input:checkbox', table).prop('checked', false);
					if (row_count > 1) {
						$("#downloadSelectedDataSet").prop("disabled", true);
						$("#downloadSelectedMetadata").prop("disabled", true);
					}
				}
			});

			$(".selectIndividualCheckbox").click(function(e) {
				var table = $(e.target).closest('table').attr('id');
				if (!$(this).is(':checked')) {
					$("#" + table).find(".selectAll").prop('checked', false);
					$(this).closest("tr").find('a.downloadLink').prop("disabled", false);
				}
				var len = $('#' + table).find('.selectIndividualCheckbox:checked').length;
				$(".downloadLink").prop("disabled", false);
				if (len >= 1) {
					$("#downloadSelectedMetadata").prop("disabled", false);
					$("#downloadSelectedDataSet").prop("disabled", false);
				} else {
					$("#downloadSelectedMetadata").prop("disabled", true);
					$("#downloadSelectedDataSet").prop("disabled", true);
				}
			});

			$("#downloadSelectedDataSet").click(function(e) {
				onClickOfBulkDownloadBtn();
			});

			var clipboard = new ClipboardJS('.share_path_copy');

			clipboard.on('success', function(e) {
				console.log(e);
				$(e.trigger).tooltip('hide').attr('data-original-title', 'Copied').tooltip('show');
				setTimeout(function() {
					$(e.trigger).tooltip('hide');
					$(e.trigger).attr('data-original-title', 'Copy File Path');
				}, 2000);

			});

			clipboard.on('error', function(e) {
				console.log(e);
			});

			var clipboard1 = new ClipboardJS('.share-assetLink-copy-button');

			clipboard1.on('success', function(e) {
				console.log(e);
				$(e.trigger).tooltip('hide').attr('data-original-title', 'Copied').tooltip('show');
				setTimeout(function() {
					$(e.trigger).tooltip('hide');
					$(e.trigger).attr('data-original-title', 'Copy to Clipboard');
				}, 2000);

			});

			clipboard1.on('error', function(e) {
				console.log(e);
			});

			$('#downloadSelectedMetadata').unbind('click').bind('click', function() {
				exportDataObjectMetadata();
			});
			initializeToolTips();
			initializePopover();
		},

		"columns" : [ {
			"data" : "path",
			"render" : function(data, type, row) {
				return renderBatchSelectForAssetFiles(data, type, row);
			},
			responsivePriority : 2
		},

		{
			"data" : "name",
			"render" : function(data, type, row) {
				return renderDataSetPath(data, type, row);
			},
			responsivePriority : 1
		},

		{
			"data" : "fileSize",
			"render" : function(data, type, row) {
				return renderFileSize(data, type, row);
			},
			responsivePriority : 4
		},

		{
			"data" : "download",
			"render" : function(data, type, row) {
				return renderDownload(data, type, row);
			},
			responsivePriority : 3
		},

		],
		columnDefs : [ {
			orderable : false,
			className : 'select-checkbox',
			headerHtml : 'batch select',
			blurable : true,
			targets : 0,
			"visible" : isVisible,
		}, {
			"targets" : 0,
			"orderable" : false
		}, {
			"targets" : -1,
			"orderable" : false
		}, {
			"targets" : 2,
			"type" : "file-size"
		}, {
			"visible" : isVisible,
			"targets" : 3
		} ],

		"dom" : '<"top"lip>frt<"bottom"ip>',
		"pagingType" : "simple",

		"lengthMenu" : [ [ 10, 25, 50, 100 ], [ 10, 25, 50, 100 ] ],

		"language" : {
			"sSearch" : "Filter:",
			"lengthMenu" : "ROWS PER PAGE &nbsp;&nbsp; _MENU_",
			"sLoadingRecords" : "Loading...",
			"zeroRecords" : "Nothing found to display",
			"paginate" : {
				next : '<i style="color:#000;font-size:17px;" class="fas fa-caret-right"></i>',
				previous : '<i style="color:#000;font-size:17px;" class="fas fa-caret-left"></i>'
			}
		}
	});
}

$.fn.dataTable.ext.type.order['file-size-pre'] = function(data) {
	var matches = data.match(/^(\d+(?:\.\d+)?)\s*([a-z]+)/i);
	var multipliers = {
		b : 1,
		bytes : 1,
		kb : 1000,
		kib : 1024,
		mb : 1000000,
		mib : 1048576,
		gb : 1000000000,
		gib : 1073741824,
		tb : 1000000000000,
		tib : 1099511627776,
		pb : 1000000000000000,
		pib : 1125899906842624
	};

	if (matches) {
		var multiplier = multipliers[matches[2].toLowerCase()];
		return parseFloat(matches[1]) * multiplier;
	} else {
		return -1;
	}
}

$('#dataSetTable tbody').on('click', '.button2a', function(e) {
	openPopOverDataSet($(this));
});

$('#dataSetTable tbody').on('keypress', '.button2a', function(e) {
	if (e.which == 13 || e.keyCode == 13) {
		openPopOverDataSet($(this));
	}
});

$('#dataSetTable tbody').on('click', '.downloadLink', function(e) {
	var path = $(this).attr('data-path');
	var fileName = $(this).attr('data-fileName');
	var isFolder =$(this).attr('is_folder');
	downloadFunction(path, fileName,isFolder);
});

$('#dataSetTable tbody').on('click', '.deleteDataFileBtn', function(e) {
	var path = $(this).attr('data-filePath');
	bootbox.confirm({
		message : "Are you sure you want to delete this?",
		buttons : {
			confirm : {
				label : 'Yes',
				className : 'btn-success'
			},
			cancel : {
				label : 'No',
				className : 'btn-danger'
			}
		},
		callback : function(result) {
			if (result == true) {
				var params = {
					deletepath : path
				};
				$.ajax({
					type : "POST",
					url : "/delete/datafile",
					contentType : 'application/x-www-form-urlencoded; charset=UTF-8',
					dataType : 'text',
					data : params,
					beforeSend : function() {
						$("#spinner").show();
						$("#dimmer").show();
					},
					success : function(msg) {
						$("#spinner").hide();
						$("#dimmer").hide();
						console.log('SUCCESS: ', msg);
						if (msg != "SUCCESS") {
							return bootbox.alert(msg);
						} else {
							refreshDataSetDataTable();
						}

					},
					error : function(e) {
						$("#spinner").hide();
						$("#dimmer").hide();
						console.log('ERROR: ', e);
						returnErrorMessage(e);
					}
				});
			}
		}
	});
});

$('#dataSetTable tbody').on(
		'click',
		'.downloadMetadata',
		function(e) {
			var selectedPath = $(this).attr('data_path');
			var assetIdentifier = $("#assetPath").val();
			var selectedPaths = [];
			selectedPaths.push(selectedPath);
			bootbox.confirm({
				message : "Do you wish to include parent metadata also?",
				buttons : {
					confirm : {
						label : 'Yes',
						className : 'btn-success'
					},
					cancel : {
						label : 'No',
						className : 'btn-danger'
					}
				},
				callback : function(result) {
					if (result == true) {
						window.open('/export?assetIdentifier=' + assetIdentifier + '&&isParent=true&&selectedPaths='
								+ selectedPaths, '_self');
					} else if (result == false) {
						window.open('/export?assetIdentifier=' + assetIdentifier + '&&isParent=false&&selectedPaths='
								+ selectedPaths, '_self');
					}
				}
			});

		});

$('#dataSetTable tbody').on('click', '.editCollectionMetadata', function(e) {
	$("#assetDetailsFragment").hide();
	$("#editCollectionFragment").show();
	$(".backToAssetDetailsBtn").show();
	$(".backToSearchBtn").hide();
	;
	var metaDataPath = $(this).attr('metadata_path');
	var collectionId = $(this).attr('collectionId');
	var fileName = $(this).attr('data-fileName');
	var selectedCollection = "Folder";

	$("#userMetaData tbody").html("");
	$("#path").val(metaDataPath);
	$(".editCollectionSuccess").hide();
	$(".editCollectionMsg").html("");
	$(".editCollectionError").hide();
	$(".editCollectionErrorMsg").html("");
	$("#collectionId").val(collectionId);
	$("#isDataObject").val(false);
	$("#editUserMetadataFileName").html(fileName);

	var params1 = {
		selectedPath : metaDataPath,
		collectionType : selectedCollection,
		refresh : false
	};
	invokeAjax('/addCollection', 'GET', params1, constructEditCollectionMetadata, null, null, null);
});

$('#dataSetTable tbody').on('click', '.editDataFileCollectionMetadata', function(e) {
	$("#assetDetailsFragment").hide();
	$("#editCollectionFragment").show();
	$(".backToAssetDetailsBtn").show();
	$(".backToSearchBtn").hide();
	var metaDataPath = $(this).attr('metadata_path');
	var fileName = $(this).attr('data-fileName');
	var params = {
		selectedPath : metaDataPath,
		levelName : 'DataObject',
		isDataObject : true
	};

	$.ajax({
		type : "GET",
		url : '/browse/metaData',
		contentType : 'application/json',
		data : params,
		beforeSend : function() {
			$("#spinner").show();
			$("#dimmer").show();
		},
		success : function(msg) {
			$("#spinner").hide();
			$("#dimmer").hide();
			constructCollectionMetData(msg, metaDataPath, true, fileName);
		},
		error : function(e) {
			console.log('ERROR: ', e);
			$("#spinner").hide();
			$("#dimmer").hide();
			bootbox.alert("Error in retrieving collection metadata.");
		}
	});

});

// expand and close folder row
$('#dataSetTable tbody')
		.on(
				'click',
				'a.detail-control',
				function() {
					var tr = $(this).closest('tr');
					var row = dataSetTable.row(tr);
					if (row.child.isShown()) {
						row.child.hide();
						tr.removeClass('shown');
						$(this).find("i.expand.far").toggleClass('fa-plus-circle fa-minus-circle');
					} else {						
						var x = row.data().filesList;
						var tableId = 'table'+row.data().name;
						var tableHtml= "<table class='table display" +
								" dt-responsive wrap subAssetsDataSetTable' id='" + tableId + "'role='grid'>" +
								"<tbody>";
						if (x && x.length > 0) {
							$.each(x,function(key, value) {

												var html = "";
												var userMetadata = "";
												var systemMetadata = "";

												if (value.systemMetadata && value.systemMetadata.length > 0) {
													systemMetadata = JSON.stringify(value.systemMetadata);
												}

												if (value.selfMetadata && value.selfMetadata.length > 0) {
													userMetadata = JSON.stringify(value.selfMetadata);
												}

												html += value.name
														+ "&nbsp;&nbsp;<a class='cil_12_no_color button2a' "
														+ "userMetadata = '"
														+ userMetadata
														+ "' file_name = '"
														+ value.name
														+ "' sys_metadata = '"
														+ systemMetadata
														+ "' "
														+ "tabindex='0'"
														+ " data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' "
														+ "data-popover-content='#a01'><i class='fas fa-info-circle' data-toggle='tooltip' title='Metadata'></i></a>";

												html += "&nbsp;&nbsp;&nbsp;<button type='button' style='border: transparent;margin-top: -6px;' class='btn btn-link btn-sm share_path_copy' data-toggle='tooltip' data-placement='top' "
														+ "title='Copy File Path' data-clipboard-text='"
														+ value.path
														+ "'>"
														+ "<img src='images/Copy-FilePath.png' th:src='@{/images/Copy-FilePath.png}' "
														+ "style='width:17px;' alt='copy file path'></button>";

												var downdloadFileName = null;
												var path = value.path;
												var html1 = "";
												var n = path.lastIndexOf("/");
												var accessgroups = $("#assetAccessGrp").val();
												var permissions = $("#assetPermission").val();
												downdloadFileName = path.substring(n + 1);

												if (permissions && permissions != 'No Permissions') {
													html1 += "<span style='border: transparent;' class='btn btn-link btn-sm editDataFileCollectionMetadata'  metadata_path  = '"
															+ path
															+ "'"
															+ "data-fileName = '"
															+ downdloadFileName
															+ "' >"
															+ "<img src='images/Edit-FileMetadata.png' data-toggle='tooltip' title='Edit File Metadata' th:src='@{/images/Edit-FileMetadata.png}' "
															+ "style='width:17px;' alt='edit collection'></span>";
												}

												html1 += "<a aria-label='download link' style='border: transparent;' class='btn btn-link btn-sm downloadMetadata'  data_path  = '"
														+ path
														+ "' href='javascript:void(0);' "
														+ "><img src='images/Download-Metadata.png' data-toggle='tooltip' title='Download File Metadata' th:src='@{/images/Download-Metadata.png}' "
														+ "style='width:17px;' alt='Download File Metadata'></a>";

												html1 += "<a aria-label='download link' style='border: transparent;' class='btn btn-link btn-sm downloadLink' href='javascript:void(0);' "
														+ "data-fileName = "
														+ downdloadFileName
														+ " data-path="
														+ value.path
														+ " "
														+ "><img src='images/Download.png' data-toggle='tooltip' title='Download File' th:src='@{/images/Download.png}' "
														+ "style='width:17px;' alt='download file'></a>";

												if (accessgroups && accessgroups.indexOf("public") == -1 && permissions
														&& permissions == 'Owner') {

													html1 += "<span style='border: transparent;' data-filePath = '"
															+ path
															+ "' class='btn btn-link btn-sm deleteDataFileBtn'>"
															+ "<img src='images/Delete.png' data-toggle='tooltip' title='Delete File' th:src='@{/images/Delete.png}' "
															+ "style='width:15px;' alt='Delete File'></span>";

												}
												
												tableHtml+=
														"<tr><td style='background-color: #d3d3d347 !important;width: 25%;'><input type='checkbox' style='margin-left:6px;' id='"
																+ value.path + "' "
																+ "class='dt-checkboxes selectIndividualCheckbox'"
																+ " aria-label='select'/></td>"
																+ "<td style='background-color: #d3d3d347 !important;width:28%'>" + html
																+ "</td><td style='background-color: #d3d3d347 !important;width:24%;'>" + value.fileSize
																+ "</td><td style='background-color: #d3d3d347 !important;'>" + html1 + "</td><tr>";
											});
							var table = tableHtml + "</tbody></table>";
							row.child(table, row.node().className + " folder_subrow").show();
						}
						$(this).find("i.expand.far").toggleClass('fa-plus-circle fa-minus-circle');
						tr.addClass('shown');
					}
				});

function renderBatchSelect(data, type, row) {
	var selectHtml = "<input type='checkbox' id='" + row.inputDatasetPath + "' dataset_path = '" + row.inputDatasetPath
			+ "' " + "pred_path ='" + row.predictionsPath + "' class='dt-checkboxes selectIndividualCheckbox'"
			+ " aria-label='select'/>";

	return selectHtml;
}

function renderBatchSelectForAssetFiles(data, type, row) {
	var selectHtml = "<input type='checkbox' id='" + row.path + "' class='dt-checkboxes selectIndividualCheckbox'"
			+ " aria-label='select'/>";

	if (row.isFolder && row.isFolder == true) {
		selectHtml += "&nbsp;&nbsp;<a class='detail-control'><i class='expand far fa-plus-circle'></i></a>";
	}

	return selectHtml;
}

function renderInputDatasetName(data, type, row) {
	var html = "";
	var userMetadata = "";
	var systemMetadata = "";

	if (row.inputDatasetSystemMetadata && row.inputDatasetSystemMetadata.length > 0) {
		systemMetadata = JSON.stringify(row.inputDatasetSystemMetadata);
	}

	if (row.inputDatasetSelfMetadata && row.inputDatasetSelfMetadata.length > 0) {
		userMetadata = JSON.stringify(row.inputDatasetSelfMetadata);
	}

	html += "&nbsp;&nbsp;&nbsp;"
			+ row.inputDatasetName
			+ "&nbsp;&nbsp;<a class='cil_12_no_color button2a' "
			+ "userMetadata = '"
			+ userMetadata
			+ "' file_name = '"
			+ row.inputDatasetName
			+ "' sys_metadata = '"
			+ systemMetadata
			+ "' "
			+ "tabindex='0'"
			+ " data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' "
			+ "data-popover-content='#a01'><i class='fas fa-info-circle' data-toggle='tooltip' title='Metadata'></i></a>";

	html += "&nbsp;&nbsp;&nbsp;<button type='button' style='border: transparent;margin-top: -6px;' class='btn btn-link btn-sm share_path_copy' data-toggle='tooltip' data-placement='top' "
			+ "title='Copy File Path' data-clipboard-text='"
			+ row.inputDatasetPath
			+ "'>"
			+ "<img src='images/Copy-FilePath.png' th:src='@{/images/Copy-FilePath.png}' "
			+ "style='width:17px;' alt='copy file path'></button>";
	return html;
}

function renderPredictionsName(data, type, row) {
	var html = "";
	var userMetadata = "";
	var systemMetadata = "";

	if (row.predictionsSystemMetadata && row.predictionsSystemMetadata.length > 0) {
		systemMetadata = JSON.stringify(row.predictionsSystemMetadata);
	}

	if (row.predictionsSelfMetadata && row.predictionsSelfMetadata.length > 0) {
		userMetadata = JSON.stringify(row.predictionsSelfMetadata);
	}

	html += "&nbsp;&nbsp;&nbsp;"
			+ row.predictionsName
			+ "&nbsp;&nbsp;<a class='cil_12_no_color button2a' "
			+ "userMetadata = '"
			+ userMetadata
			+ "' file_name = '"
			+ row.predictionsName
			+ "' sys_metadata = '"
			+ systemMetadata
			+ "' "
			+ "tabindex='0'"
			+ " data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' "
			+ "data-popover-content='#a01'><i class='fas fa-info-circle' data-toggle='tooltip' title='Metadata'></i></a>";

	html += "&nbsp;&nbsp;&nbsp;<button type='button' style='border: transparent;margin-top: -6px;' class='btn btn-link btn-sm share_path_copy' data-toggle='tooltip' data-placement='top' "
			+ "title='Copy File Path' data-clipboard-text='"
			+ row.predictionsPath
			+ "'>"
			+ "<img src='images/Copy-FilePath.png' th:src='@{/images/Copy-FilePath.png}' "
			+ "style='width:17px;' alt='copy file path'></button>";
	return html;
}

function renderDataSetPath(data, type, row) {

	var html = "";
	var userMetadata = "";
	var systemMetadata = "";
	var title = "";
	if (row.systemMetadata && row.systemMetadata.length > 0) {
		systemMetadata = JSON.stringify(row.systemMetadata);
	}

	if (row.selfMetadata && row.selfMetadata.length > 0) {
		userMetadata = JSON.stringify(row.selfMetadata);
	}

	if (row.isFolder && row.isFolder == true) {
		title = "Copy Folder Path";
	} else {
		title = "Copy File Path";
	}

	html += row.name
			+ "&nbsp;&nbsp;<a class='cil_12_no_color button2a' "
			+ "userMetadata = '"
			+ userMetadata
			+ "' file_name = '"
			+ row.name
			+ "' sys_metadata = '"
			+ systemMetadata
			+ "' "
			+ "tabindex='0'"
			+ " data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' "
			+ "data-popover-content='#a01'><i class='fas fa-info-circle' data-toggle='tooltip' title='Metadata'></i></a>";

	html += "&nbsp;&nbsp;&nbsp;<button type='button' style='border: transparent;margin-top: -6px;' class='btn btn-link btn-sm share_path_copy' data-toggle='tooltip' data-placement='top' "
			+ "title='"
			+ title
			+ "' data-clipboard-text='"
			+ row.path
			+ "'>"
			+ "<img src='images/Copy-FilePath.png' th:src='@{/images/Copy-FilePath.png}' "
			+ "style='width:17px;' alt='copy file path'></button>";
	return html;
}

function displayPopoverDataSet() {
	$('.button2a').on('click', function(e) {
		openPopOverDataSet($(this));
	});
	$('.button2a').on('keypress', function(e) {
		if (e.which == 13 || e.keyCode == 13) {
			openPopOverDataSet($(this));
		}
	});
}

function openPopOverDataSet($this) {
	var pop = $this;
	$('.button2a').not($this).popover('hide');
	var userMetadata = $this.attr('userMetadata');
	var sysMetadata = $this.attr('sys_metadata');
	var userMetadataList = "";
	var fileName = $this.attr('file_name');

	if (userMetadata) {
		userMetadataList = JSON.parse(userMetadata);
	}

	var sysMetadatalist = JSON.parse(sysMetadata);

	var ind = "<div id=\"a01\" class=\"col-md-12 hidden\"><div class=\"popover-heading\">" + "Metadata for " + fileName
			+ "<a class=\"button closeBtn float-right\" href=\"javascript:void(0);\">"
			+ "<i class=\"fa fa-times\"></i></a> </div><div class='popover-body'>";
	var table = "";
	var content = "";

	if (userMetadataList) {

		ind += "<p><b>User Metadata </b></p><div class='divTable' style='width: 100%;border: 1px solid #000;'>"
				+ "<div class='divTableBody'><div class='divTableRow'>"
				+ "<div class='divTableHead rowAttribute'>ATTRIBUTE</div>"
				+ "<div class='divTableHead'>VALUE</div></div>";

		$.each(userMetadataList, function(key, value) {
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

	if (sysMetadatalist) {
		content += "<p><b>Key System Metadata </b></p><div class='divTable' style='width: 100%;border: 1px solid #000;'>"
				+ "<div class='divTableBody'><div class='divTableRow'>"
				+ "<div class='divTableHead rowAttribute'>ATTRIBUTE</div>"
				+ "<div class='divTableHead'>VALUE</div></div>";

		$.each(sysMetadatalist, function(key, value) {
			content += "<div class='divTableRow'><div class='divTableCell'>" + value.displayName + "</div>"
					+ "<div class='divTableCell'>" + value.value + "</div></div>";
		});

		content += "</div> </div>";

	}

	table += ind + content + "</div> </div></div> </div>";
	$("#a01").remove();
	pop.after(table);
	pop.data('bs.popover').setContent();
	pop.popover('show');

}

function renderFileSize(data, type, row) {
	if (row.fileSize) {
		return row.fileSize;
	}
	return "";

}
function exportDataObjectMetadata() {
	var assetIdentifier = $("#assetPath").val();
	bootbox.confirm({
		message : "Do you wish to include parent metadata also?",
		buttons : {
			confirm : {
				label : 'Yes',
				className : 'btn-success'
			},
			cancel : {
				label : 'No',
				className : 'btn-danger'
			}
		},
		callback : function(result) {
			if (result == true) {
				var selectedPaths = [];
				$("#dataSetTable tbody input[type=checkbox]:checked").each(function() {
					selectedPaths.push($(this).attr('id'));
				});
				window.open('/export?assetIdentifier=' + assetIdentifier + '&&isParent=true&&selectedPaths='
						+ selectedPaths, '_self');
			} else if (result == false) {
				var selectedPaths = [];
				$("#dataSetTable tbody input[type=checkbox]:checked").each(function() {
					selectedPaths.push($(this).attr('id'));
				});
				window.open('/export?assetIdentifier=' + assetIdentifier + '&&isParent=false&&selectedPaths='
						+ selectedPaths, '_self');
			}
		}
	});

}

function renderDownload(data, type, row) {

	var downdloadFileName = null;
	var path = row.path;
	var html = "";
	var n = path.lastIndexOf("/");
	var accessgroups = $("#assetAccessGrp").val();
	var permissions = $("#assetPermission").val();
	downdloadFileName = path.substring(n + 1);

	var downloadFileTitle;
	var downloadMetadataTitle;

	if (permissions && permissions != 'No Permissions') {
		if (row.isFolder && row.isFolder == true) {
			html += "<span style='border: transparent;' class='btn btn-link btn-sm editCollectionMetadata'  metadata_path  = '"
					+ path
					+ "'"
					+ "collectionId = '"
					+ row.collectionId
					+ "' data-fileName = '"
					+ downdloadFileName
					+ "' >"
					+ "<img src='images/Edit-FileMetadata.png' data-toggle='tooltip' title='Edit Folder Metadata' th:src='@{/images/Edit-FileMetadata.png}' "
					+ "style='width:17px;' alt='edit collection'></span>";
		} else {
			html += "<span style='border: transparent;' class='btn btn-link btn-sm editDataFileCollectionMetadata'  metadata_path  = '"
					+ path
					+ "'"
					+ "data-fileName = '"
					+ downdloadFileName
					+ "' >"
					+ "<img src='images/Edit-FileMetadata.png' data-toggle='tooltip' title='Edit File Metadata' th:src='@{/images/Edit-FileMetadata.png}' "
					+ "style='width:17px;' alt='edit collection'></span>";
		}

	}

	if (row.isFolder && row.isFolder == true) {
		downloadFileTitle = "Download Folder";
	} else {
		downloadFileTitle = "Download File";
		downloadMetadataTitle = "Download File Metadata";
	}

	if (row.isFolder == false) {
		html += "<a aria-label='download link' style='border: transparent;' class='btn btn-link btn-sm downloadMetadata'  data_path  = '"
				+ path
				+ "' href='javascript:void(0);' "
				+ "><img src='images/Download-Metadata.png' data-toggle='tooltip' title= '"
				+ downloadMetadataTitle
				+ "' th:src='@{/images/Download-Metadata.png}' "
				+ "style='width:17px;' alt='Download File Metadata'></a>";
	}

	html += "<a aria-label='download link' style='border: transparent;' class='btn btn-link btn-sm downloadLink' href='javascript:void(0);' "
			+ "is_folder = '"+row.isFolder + "' data-fileName = "
			+ downdloadFileName
			+ " data-path="
			+ row.path
			+ " "
			+ "><img src='images/Download.png' data-toggle='tooltip' title='"
			+ downloadFileTitle
			+ "' th:src='@{/images/Download.png}' " + "style='width:17px;' alt='download file'></a>";

	if (accessgroups && accessgroups.indexOf("public") == -1 && permissions && permissions == 'Owner'
			&& row.isFolder == false) {

		html += "<span style='border: transparent;' data-filePath = '"
				+ path
				+ "' class='btn btn-link btn-sm deleteDataFileBtn'>"
				+ "<img src='images/Delete.png' data-toggle='tooltip' title='Delete File' th:src='@{/images/Delete.png}' "
				+ "style='width:15px;' alt='Delete File'></span>";

	}

	return html;

}

function renderGeneratePredDownload(data, type, row) {

	var html = "";
	html += "<a aria-label='download link' style='border: transparent;' class='btn btn-link btn-sm downloadModelAnalysisLink' href='javascript:void(0);' "
			+ "dataset_path = '"
			+ row.inputDatasetPath
			+ "' pred_path ='"
			+ row.predictionsPath
			+ "'"
			+ "><img src='images/Download.png' data-toggle='tooltip' title='Download Model Analysis Files' th:src='@{/images/Download.png}' "
			+ "style='width:17px;' alt='download file'></a>";

	return html;
}

function downloadFunction(path, fileName, isFolder) {
	var assetIdentifier = $("#assetIdentifier").val();
	
	if(isFolder && isFolder == "true") {
		location.replace('/downloadTab?selectedPaths=' + path
				+ '&&downloadAsyncType=collection&&assetIdentifier='+assetIdentifier+'&&returnToSearch=false');
	} else {
	location.replace('/downloadTab?selectedPaths=' + path + '&&fileName=' + fileName + '&&assetIdentifier='
			+ assetIdentifier + '&&downloadAsyncType=data_object&&returnToSearch=false');
	}
}

function onClickOfModelAnlysisBulkDownloadBtn($this) {
	var selectedPaths = [];
	var assetIdentifier = $("#assetIdentifier").val();

	if ($this) {
		selectedPaths.push($this.attr('dataset_path'));
		selectedPaths.push($this.attr('pred_path'));
	} else {
		$("#generatePredTable tbody input[type=checkbox]:checked").each(function() {
			selectedPaths.push($(this).attr('dataset_path'));
			selectedPaths.push($(this).attr('pred_path'));
		});
	}

	location.replace('/downloadTab?selectedPaths=' + selectedPaths + '&&assetIdentifier=' + assetIdentifier
			+ '&&downloadAsyncType=datafiles&&returnToSearch=false');

}

function onClickOfBulkDownloadBtn() {
	var selectedPaths = [];
	var assetIdentifier = $("#assetIdentifier").val();
	$("#dataSetTable tbody input[type=checkbox]:checked").each(function() {
		selectedPaths.push($(this).attr('id'));
	});

	if (selectedPaths.length == 1) {
		location.replace('/downloadTab?selectedPaths=' + selectedPaths + '&&assetIdentifier=' + assetIdentifier
				+ '&&downloadAsyncType=data_object&&returnToSearch=false');
	} else {
		location.replace('/downloadTab?selectedPaths=' + selectedPaths + '&&assetIdentifier=' + assetIdentifier
				+ '&&downloadAsyncType=datafiles&&returnToSearch=false');
	}
}