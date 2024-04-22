$(document).ready(function() {
	$.fn.dataTable.moment("MM/DD/YYYY HH:mm:ss");
	refreshTaskDatatable('dataSetTable');

	$("#assetFilesTab").click(function() {
		$(this).css('background-color', '#194F85');
		$("#generatePredictionsTab").css('background-color', 'rgb(128 128 128 / 33%)');
		$("#generatePredTable").hide();
		$("#dataSetTable").show();
		refreshTaskDatatable('dataSetTable');
	});

	$("#generatePredictionsTab").click(function() {
		$(this).css('background-color', '#194F85');
		$("#assetFilesTab").css('background-color', 'rgb(128 128 128 / 33%)');
		$("#generatePredTable").show();
		$("#dataSetTable").hide();
		refreshTaskDatatable('generatePredTable');
	});

	$("#btnPredictionAccessGrp").click(function() {
		updatePredictionAccessGroupsFunction();
	});
	

});

$(document).on('change', '#isPublicAccessForPrediction', function() {
	if ($(this).is(":checked")) {
		$("#editPredictionAccessModal").find("#updatePredictionAccessGrpList").next(".select2-container").hide();
		$("#editPredictionAccessModal").find("#updatePredictionAccessGrpList").val("").trigger('change');

	} else {
		$("#editPredictionAccessModal").find("#updatePredictionAccessGrpList").next(".select2-container").show();
	}

});

function filter() {
	$("#filterSearchBox").keyup(function() {		
		var filterVal = $(this).val().trim().toLowerCase();
		if(filterVal) {
			$(".clearAssetFilesFilter").show();
			$(".lensfilter").hide();
		} else  {
			$(".clearAssetFilesFilter").hide();
			$(".lensfilter").show();
		}
		
		$("#dataSetTable tbody tr").each(function() {
			var fileName = $(this).find('td').eq(1).text().trim().toLowerCase();
			if (fileName.indexOf(filterVal) != -1) {
				$(this).show();
			} else {
				$(this).hide();
			}
		});
	});
}

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
	if (isVisible && !$("#filterSearchBox").is(':visible')) {
		$("div.datasetToolbar").prepend(
				'<div style="float: left;margin-top: -10px;margin-bottom: 10px;margin-left: 1.5rem;">'
						+ '<label><input type="textbox" id="filterSearchBox" placeholder="Filter Files"/><img src="/images/filter_files_clear.png"/ class="clearAssetFilesFilter"> <img src="/images/lensIcon.svg" class="lensfilter"></label></div>');
	}
}

function generatePredTable(isVisible) {
	$('#generatePredTable').DataTable(
			{
				"paging" : true,
				"ordering" : true,
				"info" : true,
				"pageLength" : 25,
				"rowId" : 'staffId',
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

				"initComplete" : function() {
					$('body').tooltip({
						selector : '[data-toggle="tooltip"]'
					});
				},

				"drawCallback" : function() {

					$("#downloadSelectedDataSet").prop("disabled", true);
					$("#downloadSelectedMetadata").hide();

					if (isVisible) {
						$("#downloadSelectedDataSet").show();
					} else {
						$("#downloadSelectedDataSet").hide();
					}

					$(".selectAll").change(function(e) {
						var table = $(e.target).closest('table');
						$(".downloadModelAnalysisLink").prop("disabled", false);
						if ($(this).is(':checked')) {
							$('td input:checkbox', table).prop('checked', true);
							$("#downloadSelectedDataSet").prop("disabled", false);

						} else {
							$('td input:checkbox', table).prop('checked', false);
							$("#downloadSelectedDataSet").prop("disabled", true);

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

					$(".downloadModelAnalysisLink").click(function() {
						onClickOfModelAnlysisBulkDownloadBtn($(this));
					});

					$("#downloadSelectedDataSet").click(function() {
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

					$(".editPredAccess").click(
							function(e) {
								var predCollectionPath = $(this).attr('pred_collPath');
								var predCollectionId = $(this).attr('pred_collId');
								var predAccessGroups = $(this).attr('pred_accessGrps');
								var isPublic = $(this).attr('pred_is_public');

								$("#editPredictionAccessModal").find("#isPublic").val(isPublic);
								$("#editPredictionAccessModal").find("#predCollectionId").val(predCollectionId);
								$("#editPredictionAccessModal").find("#predCollectionPath").val(predCollectionPath);
								$("#editPredictionAccessModal").find("#predAccessGroups").val(
										predAccessGroups && predAccessGroups != "null" ? predAccessGroups : "");

								$("#editPredictionAccessModal").find(".updatePredGrpMsg").html("");
								$("#editPredictionAccessModal").find(".updatePredAccessSuccessBlock").hide();

								$("#editPredictionAccessModal").modal('show');

								loadJsonData('/metaDataPermissionsList', $("#editPredictionAccessModal").find(
										"#updatePredictionAccessGrpList"), false, null, postSuccessPredAccessFunction,
										null, "key", "value");

							});

					initializeToolTips();
					initializePopover();
					displayPopover();
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
					"data" : "outcomeFileName",
					"render" : function(data, type, row) {
						return renderOutcomeName(data, type, row);
					},
					responsivePriority : 3
				},

				{
					"data" : "taskId",
					"render" : function(data, type, row) {
						return rendertaskId(data, type, row);
					},
					responsivePriority : 1
				},

				{
					"data" : "fullName",
					"render" : function(data, type, row) {
						return renderSharedBy(data, type, row);
					},
					responsivePriority : 6
				},

				{
					"data" : "taskCompletedDate",
					"render" : function(data, type, row) {
						return renderTaskCompletedDate(data, type, row);
					},
					responsivePriority : 5
				},

				{
					"data" : "download",
					"render" : function(data, type, row) {
						return renderGeneratePredActions(data, type, row);
					},
					responsivePriority : 4
				},

				],
				columnDefs : [ {
					orderable : false,
					className : 'select-checkbox td_class_5_percent',
					headerHtml : 'batch select',
					blurable : true,
					targets : 0,
					"visible" : isVisible,
				}, {
					"targets" : 0,
					"orderable" : false
				}, {
					"targets" : 1,
					className : "td_class_5"
				}, {
					"targets" : 2,
					className : "td_class_5"
				}, {
					"targets" : 3,
					className : "td_class_5"
				}, {
					"targets" : 4,
					className : "td_class_15"
				}, {
					"targets" : 5,
					className : "td_class_15"
				}, {
					"visible" : isVisible,
					"targets" : -1,
					className : "td_class_5_percent",
					"orderable" : false,
				}, {
					type : "date",
					"targets" : [ -2 ]
				} ],

				"dom" : '<"top"lip>rt<"bottom">',
				"pagingType" : "simple",

				"lengthMenu" : [ [ 10, 25, 50, 100 ], [ 10, 25, 50, 100 ] ],

				"language" : {
					"sSearch" : "Filter:",
					"lengthMenu" : "ROWS PER PAGE &nbsp;&nbsp; _MENU_",
					"sLoadingRecords" : "Loading...",
					"zeroRecords" : "Nothing found to display",
					"paginate" : {
						next : '<img src="/images/pagination_right_assetfiles.png"/>',
						previous : '<img src="/images/paginate_left_assetFiles.png"/>'
					}
				}
			});
}

function dataTableInitDataSet(isVisible) {
	$('#dataSetTable').DataTable({
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

		"initComplete" : function() {
			$('body').tooltip({
				selector : '[data-toggle="tooltip"]'
			});
		},

		"drawCallback" : function() {

			$("#downloadSelectedDataSet").prop("disabled", true);
			$("#downloadSelectedMetadata").prop("disabled", true);

			$(".clearAssetFilesFilter").click(function() {
				$(this).parent().find("#filterSearchBox").val("");
				$(this).parent().find(".lensfilter").show;
				$(this).parent().find("#filterSearchBox").trigger('keyup');
				$(this).hide();
			});
			
			filter();
			if (isVisible) {
				$("#downloadSelectedDataSet").show();
				$("#downloadSelectedMetadata").show();
			} else {
				$("#downloadSelectedDataSet").hide();
				$("#downloadSelectedMetadata").hide();
			}

			$("#downloadSelectedDataSet").click(function() {
				onClickOfBulkDownloadBtn('dataSetTable');
			});

			$('#downloadSelectedMetadata').unbind('click').bind('click', function() {
				exportDataObjectMetadata();
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
				return renderActions(data, type, row);
			},
			responsivePriority : 3
		},

		],
		columnDefs : [ {
			orderable : false,
			className : 'select-checkbox td_class_15',
			headerHtml : 'batch select',
			blurable : true,
			targets : 0,
			"visible" : isVisible,
		}, {
			"targets" : 0,
			"orderable" : false,
			className : "td_class_15"
		}, {
			"targets" : -1,
			"orderable" : false,
			className : "td_class_4"
		}, {
			"targets" : 2,
			"type" : "file-size",
			className : "td_class_5"
		}, {
			"visible" : isVisible,
			"targets" : 3,
			className : "td_class_4"
		}, {
			"targets" : 1,
			className : "td_class_7"
		} ],

		"dom" : '<"datasetToolbar top"lip>rt<"bottom">',
		"pagingType" : "simple",

		"lengthMenu" : [ [ 10, 25, 50, 100 ], [ 10, 25, 50, 100 ] ],

		"language" : {
			"sSearch" : "Filter:",
			"lengthMenu" : "ROWS PER PAGE &nbsp;&nbsp; _MENU_",
			"sLoadingRecords" : "Loading...",
			"zeroRecords" : "Nothing found to display",
			"paginate" : {
				next : '<img src="/images/pagination_right_assetfiles.png"/>',
				previous : '<img src="/images/paginate_left_assetFiles.png"/>'
			}
		}
	});
}

$('#dataSetTable').on('click', '.selectAll', function(e) {
	var table = $(e.target).closest('table');
	var subtable = $('.subAssetsDataSetTable');
	if ($(this).is(':checked')) {
		$('td input:checkbox', table).prop('checked', true);
		$('td input:checkbox', subtable).prop('checked', false);
	} else {
		$('td input:checkbox', subtable).prop('checked', false);
		$('td input:checkbox', table).prop('checked', false);
	}
	var len = $('.selectIndividualCheckbox:checked').length;
	if (len >= 1) {
		$("#downloadSelectedDataSet").prop("disabled", false);
		$("#downloadSelectedMetadata").prop("disabled", false);
	} else {
		$("#downloadSelectedDataSet").prop("disabled", true);
		$("#downloadSelectedMetadata").prop("disabled", true);
	}
});

$('#dataSetTable tbody').on('click', '.selectIndividualCheckbox', function() {
	if (!$(this).is(':checked')) {
		$("#dataSetTable").find(".selectAll").prop('checked', false);
	}
	var len = $('.selectIndividualCheckbox:checked').length;
	if (len >= 1) {
		$("#downloadSelectedMetadata").prop("disabled", false);
		$("#downloadSelectedDataSet").prop("disabled", false);
	} else {
		$("#downloadSelectedMetadata").prop("disabled", true);
		$("#downloadSelectedDataSet").prop("disabled", true);
	}
});

$('#dataSetTable tbody').on('click', '.button2a', function() {
	openPopOver($(this));
});

$('#dataSetTable tbody').on('keypress', '.button2a', function(e) {
	if (e.which == 13 || e.keyCode == 13) {
		openPopOver($(this));
	}
});

$('#dataSetTable tbody').on('click', '.downloadLink', function() {
	var path = $(this).attr('data-path');
	var fileName = $(this).attr('data-fileName');
	var fileSize = $(this).closest('tr').find('td').eq(2).find('input[type="hidden"]').val().trim();
	downloadFunction(path, fileName, fileSize);
});

$('#dataSetTable tbody').on('click', '.downloadLinkFolder', function() {
	var path = $(this).attr('data-path');
	downloadFunction(path, null, null);
});

$('#dataSetTable tbody').on('click', '.editFolderMetadata', function() {
	$("#assetDetailsFragment").hide();
	$("#editCollectionFragment").show();
	$(".backToAssetDetailsBtn").show();
	$(".backToAssetDetails_editCollection").show();
	$(".backToSearchBtn").hide();
	$(".backToSearch_editCollection").hide();
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

$('#dataSetTable tbody').on('click', '.editDataFileCollectionMetadata', function() {
	$("#assetDetailsFragment").hide();
	$("#editCollectionFragment").show();
	$(".backToAssetDetailsBtn").show();
	$(".backToAssetDetails_editCollection").show();
	$(".backToSearchBtn").hide();
	$(".backToSearch_editCollection").hide();
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
					var leftCss = parseInt($(this).css('margin-left'));
					var widthCss = parseInt($("#dataSetTable").css('width'));
					var tableWidth = widthCss;
					leftCss += 25;
					var fileLeftCss = leftCss + 14;
					var thistr = $(this).closest('tr');
					var $this = $(this);

					if (thistr.hasClass('shown')) {
						$this.closest('tr').next('div.subFoldersDiv').remove();
						thistr.removeClass('shown');
						$this.find("i.expand.far").toggleClass('fa-folder fa-folder-open');
					} else {
						var name = $this.attr('data-name');
						var params = {
							path : $("#assetPath").val() + "/" + name
						};
						$
								.ajax({
									"url" : "/getDataObjects",
									"type" : "GET",
									data : params,
									beforeSend : function() {
										$("#spinner").show();
										$("#dimmer").show();
									},
									success : function(msg) {
										$("#spinner").hide();
										$("#dimmer").hide();
										var table;
										var tableId = 'table_' + name;
										var accessgroups = $("#assetAccessGrp").val();
										var permissions = $("#assetPermission").val();
										var tableHtml = "<div class='subFoldersDiv' style='width:200px;'><table style='width:"
												+ tableWidth
												+ "px;'"
												+ "class='table display"
												+ " dt-responsive wrap subAssetsDataSetTable' id='"
												+ tableId
												+ "'role='grid'>" + "<tbody>";

										$
												.each(
														msg,
														function(key, value) {
															var downdloadFileName = null;
															var path = value.path;
															var n = path.lastIndexOf("/");
															downdloadFileName = path.substring(n + 1);
															var nestedEditPermissionsHtml = "";
															var iconHtml = "";
															var selectHtml = "";
															var metadataInfoTitle = "File Metadata";
															var copyPathTitle = "Copy File Path";
															var collection_type = "";
															var fileSize = "";
															var fileSizeHtml = ""

															if (value.isFolder == false) {
																selectHtml = "<input type='checkbox' id='"
																		+ value.path
																		+ "' class='dt-checkboxes selectIndividualCheckbox'"
																		+ " aria-label='select'/>";
																iconHtml += "<span class='collectionName' style='margin-left:"
																		+ fileLeftCss
																		+ "px;'>"
																		+ value.name
																		+ "</span>";
																collection_type = "DataObject";
																fileSize = value.fileSize;
																fileSizeHtml = "<input type='hidden' class='fileSizeInBytes' value= '" + value.fileSizeInBytes+ "'/> " + fileSize + "";
															}
															if (value.isFolder == true) {
																iconHtml += "<a class='detail-control detail-control-sub-folder' style='margin-left:"
																		+ leftCss
																		+ "px;' "
																		+ "data-name = '"
																		+ name
																		+ "/"
																		+ value.name
																		+ "'>"
																		+ "<i class='expand far fa-folder'></i></a>"
																		+ value.name + "";
																metadataInfoTitle = "Folder Metadata";
																copyPathTitle = "Copy Folder Path";
																collection_type = "Folder";
															}

															iconHtml += "&nbsp;&nbsp;&nbsp;<button type='button' style='border: transparent;margin-top: -6px;"
																	+ "' class='btn btn-link btn-sm share_path_copy' data-toggle='tooltip' data-placement='top' "
																	+ "title='"
																	+ copyPathTitle
																	+ "' data-clipboard-text='"
																	+ value.path
																	+ "'>"
																	+ "<img src='images/copy_to_clipboard.svg' th:src='@{/images/copy_to_clipboard.svg}' "
																	+ "style='width:17px;' alt='copy file path'></button>";

															iconHtml += "<a class='cil_13_no_color button2a' selected_path='"
																	+ value.path
																	+ "' collection_type = "
																	+ collection_type
																	+ " file_name = '"
																	+ value.name
																	+ "'"
																	+ "tabindex='0'"
																	+ " data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' "
																	+ "data-popover-content='#a01'><img src='images/infoIcon.svg'"
																	+ "th:src='@{/images/infoIcon.svg}' class='infoMetadata' data-toggle='tooltip'"
																	+ "title='"
																	+ metadataInfoTitle
																	+ "' alt='Status info'></a>";

															if (permissions && permissions != 'No Permissions') {
																if (value.isFolder && value.isFolder == true) {
																	nestedEditPermissionsHtml += "<span style='border: transparent;' class='btn btn-link btn-sm editFolderMetadata'  metadata_path  = '"
																			+ value.path
																			+ "'"
																			+ "collectionId = '"
																			+ value.collectionId
																			+ "' data-fileName = '"
																			+ downdloadFileName
																			+ "' >"
																			+ "<img src='images/Edit-FileMetadata.png' data-toggle='tooltip' title='Edit Folder Metadata' th:src='@{/images/Edit-FileMetadata.png}' "
																			+ "style='width:17px;' alt='edit collection'></span>";
																} else {
																	nestedEditPermissionsHtml += "<span style='border: transparent;' class='btn btn-link btn-sm editDataFileCollectionMetadata'  metadata_path  = '"
																			+ value.path
																			+ "'"
																			+ "data-fileName = '"
																			+ downdloadFileName
																			+ "' >"
																			+ "<img src='images/Edit-FileMetadata.png' data-toggle='tooltip' title='Edit File Metadata' th:src='@{/images/Edit-FileMetadata.png}' "
																			+ "style='width:17px;' alt='edit collection'></span>";
																}

															}

															if (value.isFolder == false) {
																nestedEditPermissionsHtml += "<a aria-label='download link' style='border: transparent;' class='btn btn-link btn-sm downloadMetadata'  data_path  = '"
																		+ path
																		+ "' href='javascript:void(0);' "
																		+ "><img src='images/Download-Metadata.png' data-toggle='tooltip' title= 'Download File Metadata' th:src='@{/images/Download-Metadata.png}' "
																		+ "style='width:17px;' alt='Download File Metadata'></a>";

																nestedEditPermissionsHtml += "<a aria-label='download link' style='border: transparent;' class='btn btn-link btn-sm downloadLink' href='javascript:void(0);' "
																		+ "data-fileName = '" + downdloadFileName + "' data-path = '" + value.path + "'><img src='images/download_file.svg' data-toggle='tooltip' title='Download File' th:src='@{/images/download_file.svg}' "
																		+ "style='width:17px;' alt='download file'></a>";
															} else {
																downloadFileTitle = "Download Folder";
																nestedEditPermissionsHtml += "<a aria-label='download link' style='border: transparent;' class='btn btn-link btn-sm downloadLinkFolder' href='javascript:void(0);' "
																		+ "data-fileName = '" + downdloadFileName + "' data-path = '" + value.path + "'><img src='images/download_file.svg' data-toggle='tooltip' title = '" + downloadFileTitle + "' th:src='@{/images/download_file.svg}' "
																		+ "style='width:17px;' alt='download file'></a>";
															}

																							
															if (accessgroups && accessgroups.indexOf("public") == -1 && value.isFolder == false && permissions &&
	                                                                permissions != 'No Permissions' && (permissions == 'Owner' || isDeletePermission == true)) {

																nestedEditPermissionsHtml += "<span style='border: transparent;' data-filePath = '"
																		+ value.path
																		+ "' class='btn btn-link btn-sm deleteDataFileBtn'>"
																		+ "<img src='images/Delete.png' data-toggle='tooltip' title='Delete File' th:src='@{/images/Delete.png}' "
																		+ "style='width:15px;' alt='Delete File'></span>";
															}

															if (value.isFolder && value.isFolder == true) {
																nestedEditPermissionsHtml += "<span style='border: transparent;' coll_path = '"
																		+ value.path
																		+ "' coll_name='"
																		+ value.name
																		+ "' class='btn btn-link btn-sm deleteFolderCollectionBtn'>"
																		+ "<img src='images/Delete.png' data-toggle='tooltip' title='Delete Collection' th:src='@{/images/Delete.png}' "
																		+ "style='width:15px;' alt='Delete Collection'></span>";
															}

															if (loggedOnUserInfo) {
																tableHtml += "<tr><td style='background-color: #d3d3d347 !important;width: 15%;'>"
																		+ selectHtml
																		+ "</td>"
																		+ "<td style='background-color: #d3d3d347 !important;width:40%'>"
																		+ iconHtml
																		+ "</td><td style='background-color: #d3d3d347 !important;width:20%;'>"
																		+ fileSizeHtml
																		+ "</td>"
																		+ "<td style = 'background-color: #d3d3d347 !important;width:25%;' > "
																		+ nestedEditPermissionsHtml + "</td > <tr>";
															} else {
																tableHtml += "<tr>"
																		+ "<td style='background-color: #d3d3d347 !important;width:40%'>"
																		+ iconHtml
																		+ "</td><td style='background-color: #d3d3d347 !important;width:20%;'>"
																		+ fileSize + "</td><tr>";
															}

														});

										table = tableHtml + "</tbody></table></div>";
										thistr.after(table);
										$this.find("i.expand.far").toggleClass('fa-folder fa-folder-open');
										thistr.addClass('shown');

									},
									error : function(e) {
										console.log('ERROR: ', e);
										$("#spinner").hide();
										$("#dimmer").hide();

									}
								});

					}
				});

function postSuccessPredAccessFunction(data, status) {

	var isPublic = $("#editPredictionAccessModal").find("#isPublic").val();

	if (isPublic && isPublic == "true") {
		$("#editPredictionAccessModal").find("#isPublicAccessForPrediction").prop("checked", true);
		$("#editPredictionAccessModal").find("#isPublicAccessForPrediction").trigger('change');
	} else {
		$("#editPredictionAccessModal").find("#isPublicAccessForPrediction").prop("checked", false);
		$("#editPredictionAccessModal").find("#isPublicAccessForPrediction").trigger('change');
	}

	var grps = $("#editPredictionAccessModal").find("#predAccessGroups").val();
	var grpsList = grps.split(",");

	for (var i = 0; i < grpsList.length; i++) {
		$("#updatePredictionAccessGrpList option[value='" + grpsList[i] + "']").prop("selected", true);
		$("#editPredictionAccessModal").find("#updatePredictionAccessGrpList").trigger('change');
	}
}

function updatePredictionAccessGroupsFunction() {
	var selectedGrps = $("#editPredictionAccessModal").find("#updatePredictionAccessGrpList").val();
	var predCollPath = $("#editPredictionAccessModal").find("#predCollectionPath").val();
	var predCollId = $("#editPredictionAccessModal").find("#predCollectionId").val();
	var isPublic;

	if ($("#editPredictionAccessModal").find("#isPublicAccessForPrediction:visible").is(":checked")) {
		isPublic = true;
	} else {
		isPublic = false;
	}

	var params = {
		selectedGrps : selectedGrps,
		predCollId : predCollId,
		predCollPath : predCollPath,
		isPublic : isPublic
	};
	invokeAjax('/predictionAccessGroups', 'POST', params, postSuccessUpdatePredictionAccess, null,
			'application/x-www-form-urlencoded; charset=UTF-8', 'text');
}

function postSuccessUpdatePredictionAccess(data, status) {
	$("#editPredictionAccessModal").find(".updatePredGrpMsg").html("Prediction Access Groups Updated.");
	$("#editPredictionAccessModal").find(".updatePredAccessSuccessBlock").show();
	refreshTaskDatatable('generatePredTable');

}

function rendertaskId(data, type, row) {

	return row.taskId;
}

function renderSharedBy(data, type, row) {
	if (row.fullName) {
		return row.fullName;
	}

	return "";
}

function renderTaskCompletedDate(data, type, row) {
	if (data) {
		return moment(data).format("MM/DD/YYYY HH:mm:ss");
	}
	return "";

}

function renderBatchSelect(data, type, row) {
	var selectHtml = "<input type='checkbox' id='" + row.inputDatasetPath + "' dataset_path = '" + row.inputDatasetPath
			+ "' " + "pred_path ='" + row.predictionsPath + "' outcome_path = '" + row.outcomeFilePath
			+ "' class='dt-checkboxes selectIndividualCheckbox'" + " aria-label='select'/>";

	return selectHtml;
}

function renderBatchSelectForAssetFiles(data, type, row) {
	var selectHtml = "";

	if (row.isFolder == false) {
		var selectHtml = "<input type='checkbox' id='" + row.path + "' class='dt-checkboxes selectIndividualCheckbox'"
				+ " aria-label='select'/>";

	}

	return selectHtml;
}

function renderInputDatasetName(data, type, row) {
	var html = "";

	html += "&nbsp;&nbsp;&nbsp;"
			+ row.inputDatasetName
			+ "&nbsp;&nbsp;<a class='cil_13_no_color button2a' file_name = '"
			+ row.inputDatasetName
			+ "' "
			+ "tabindex='0' selected_path= '"
			+ row.inputDatasetPath
			+ "' "
			+ "collection_type= 'DataObject' file_name = '"
			+ row.inputDatasetName
			+ "' tabindex='0'"
			+ " data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' "
			+ "data-popover-content='#a01'><img src='images/infoIcon.svg' class='infoMetadata'"
			+ " th:src='@{/images/infoIcon.svg}' data-toggle='tooltip' title='File Metadata' alt='Status info'></i></a>";

	html += "&nbsp;&nbsp;&nbsp;<button type='button' style='border: transparent;margin-top: -6px;' "
			+ "class='btn btn-link btn-sm share_path_copy' data-toggle='tooltip' data-placement='top' "
			+ "title='Copy File Path' data-clipboard-text='" + row.inputDatasetPath + "'>"
			+ "<img src='images/copy_to_clipboard.svg' th:src='@{/images/copy_to_clipboard.svg}' "
			+ "style='width:17px;' alt='copy file path'></button>";
	return html;
}

function renderOutcomeName(data, type, row) {

	var html = "";

	if (row.outcomeFileName) {
		html += "&nbsp;&nbsp;&nbsp;" + row.outcomeFileName + "&nbsp;&nbsp;<a class='cil_13_no_color button2a'"
				+ " selected_path= '" + row.outcomeFilePath + "' collection_type= 'DataObject'" + "file_name = '"
				+ row.outcomeFileName + "'tabindex='0'"
				+ " data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' "
				+ "data-popover-content='#a01'><img src='images/infoIcon.svg' "
				+ "th:src='@{/images/infoIcon.svg}' data-toggle='tooltip' class='infoMetadata'"
				+ " title='File Metadata' alt='Status info'></a>";

		html += "&nbsp;&nbsp;&nbsp;<button type='button' style='border: transparent;margin-top: -6px;' "
				+ "class='btn btn-link btn-sm share_path_copy' data-toggle='tooltip' data-placement='top' "
				+ "title='Copy File Path' data-clipboard-text='" + row.outcomeFilePath + "'>"
				+ "<img src='images/copy_to_clipboard.svg' th:src='@{/images/copy_to_clipboard.svg}' "
				+ "style='width:17px;' alt='copy file path'></button>";
	}

	return html;

}

function renderPredictionsName(data, type, row) {
	var html = "";
	html += "&nbsp;&nbsp;&nbsp;" + row.predictionsName + "&nbsp;&nbsp;<a class='cil_13_no_color button2a'"
			+ " selected_path= '" + row.predictionsPath + "' collection_type= 'DataObject'" + "file_name = '"
			+ row.predictionsName + "'tabindex='0'"
			+ " data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' "
			+ "data-popover-content='#a01'><img src='images/infoIcon.svg' "
			+ "th:src='@{/images/infoIcon.svg}' data-toggle='tooltip' class='infoMetadata' "
			+ "title='File Metadata' alt='Status info'></a>";

	html += "&nbsp;&nbsp;&nbsp;<button type='button' style='border: transparent;margin-top: -6px;' "
			+ "class='btn btn-link btn-sm share_path_copy' data-toggle='tooltip' data-placement='top' "
			+ "title='Copy File Path' data-clipboard-text='" + row.predictionsPath + "'>"
			+ "<img src='images/copy_to_clipboard.svg' th:src='@{/images/copy_to_clipboard.svg}' "
			+ "style='width:17px;' alt='copy file path'></button>";
	return html;
}

function renderDataSetPath(data, type, row) {

	var html = "";
	var title = "";
	var metadatatitle = "";
	var collection_type = "";
	var marginLeftCss;
	var marginRightCss;
	if (loggedOnUserInfo) {
		marginLeftCss = -25
		marginRightCss = 0
	} else {
		marginLeftCss = 0
		marginRightCss = 10
	}

	if (row.isFolder && row.isFolder == true) {
		title = "Copy Folder Path";
		metadatatitle = "Folder Metadata";
		collection_type = "Folder";
		html += "<a class='detail-control' data-name = '" + row.name + "'" + "style='float:left;margin-right:"
				+ marginRightCss + "px;margin-left:" + marginLeftCss + "px;'>"
				+ "<i class='expand far fa-folder'></i></a>";
	} else {
		title = "Copy File Path";
		metadatatitle = "File Metadata";
		collection_type = "DataObject";
	}

	html += row.name + "&nbsp;&nbsp;";

	html += "<button type='button' style='border: transparent;margin-top: -6px;' class='btn btn-link btn-sm share_path_copy' "
			+ "data-toggle='tooltip' data-placement='top' "
			+ "title='"
			+ title
			+ "' data-clipboard-text='"
			+ row.path
			+ "'>"
			+ "<img src='images/copy_to_clipboard.svg' th:src='@{/images/copy_to_clipboard.svg}' "
			+ "style='width:17px;' alt='copy file path'></button>";

	html += "<a class='cil_13_no_color button2a' selected_path='" + row.path + "' collection_type= " + collection_type
			+ " " + "file_name = '" + row.name + "'" + "tabindex='0'"
			+ " data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' "
			+ "data-popover-content='#a01'><img src='images/infoIcon.svg'"
			+ "th:src='@{/images/infoIcon.svg}' class='infoMetadata' alt='metadata info' "
			+ "data-toggle='tooltip' title='" + metadatatitle + "'></a>";
	return html;
}

function renderFileSize(data, type, row) {
	if (row.fileSize) {	
		return "<input type='hidden' class='fileSizeInBytes' value= '" + row.fileSizeInBytes+ "'/> " + row.fileSize + " ";
	}
	return "";

}

function renderActions(data, type, row) {

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
			html += "<span style='border: transparent;' class='btn btn-link btn-sm editFolderMetadata'  metadata_path  = '"
					+ path
					+ "'"
					+ "collectionId = '"
					+ row.collectionId
					+ "' data-fileName = '"
					+ downdloadFileName
					+ "' >"
					+ "<img src='images/Edit-FileMetadata.png' data-toggle='tooltip' title='Edit Folder Metadata' "
					+ "th:src='@{/images/Edit-FileMetadata.png}' "
					+ "style='width:17px;' alt='edit collection'></span>";
		} else {
			html += "<span style='border: transparent;' class='btn btn-link btn-sm editDataFileCollectionMetadata'  metadata_path  = '"
					+ path
					+ "'"
					+ "data-fileName = '"
					+ downdloadFileName
					+ "' >"
					+ "<img src='images/Edit-FileMetadata.png' data-toggle='tooltip' title='Edit File Metadata'"
					+ " th:src='@{/images/Edit-FileMetadata.png}' "
					+ "style='width:17px;' alt='edit file metadata'></span>";
		}

	}

	if (row.isFolder == false) {
		downloadFileTitle = "Download File";
		downloadMetadataTitle = "Download File Metadata";
		html += "<a aria-label='download link' style='border: transparent;' class='btn btn-link btn-sm downloadMetadata'  data_path  = '"
				+ path
				+ "' href='javascript:void(0);' "
				+ "><img src='images/Download-Metadata.png' data-toggle='tooltip' title= '"
				+ downloadMetadataTitle
				+ "' th:src='@{/images/Download-Metadata.png}' "
				+ "style='width:17px;' alt='Download File Metadata'></a>";

		html += "<a aria-label='download link' style='border: transparent;' class='btn btn-link btn-sm downloadLink' "
				+ "href='javascript:void(0);' data-fileName = '" + downdloadFileName + "' data-path = '" + path + "'><img src='images/download_file.svg' data-toggle='tooltip' title='" + downloadFileTitle
				+ "' th:src='@{/images/download_file.svg}' " + "style='width:17px;' alt='download file'></a>";
	} else {
		downloadFileTitle = "Download Folder";
		html += "<a aria-label='download link' style='border: transparent;' class='btn btn-link btn-sm downloadLinkFolder' "
				+ "href='javascript:void(0);' "
				+ "data-fileName = '" + downdloadFileName + "' data-path = '" + row.path + "'><img src='images/download_file.svg' data-toggle='tooltip' title = '"
				+ downloadFileTitle
				+ "' th:src ='@{/images/download_file.svg}' " + "style ='width:17px;' alt ='download file'></a>";
	}

	if (accessgroups && accessgroups.indexOf("public") == -1 && row.isFolder == false && permissions &&
	       permissions != 'No Permissions' && (permissions == 'Owner' || isDeletePermission == true)) {

		html += "<span style='border: transparent;' data-filePath = '"
				+ path
				+ "' class='btn btn-link btn-sm deleteDataFileBtn'>"
				+ "<img src='images/Delete.png' data-toggle='tooltip' title='Delete File' th:src='@{/images/Delete.png}' "
				+ "style='width:15px;' alt='Delete File'></span>";
	}
	if (row.isFolder == true) {
		html += "<span style='border: transparent;' coll_path = '"
				+ path
				+ "' coll_name='"
				+ row.name
				+ "' class='btn btn-link btn-sm deleteFolderCollectionBtn'>"
				+ "<img src='images/Delete.png' data-toggle='tooltip' title='Delete Collection' th:src='@{/images/Delete.png}' "
				+ "style='width:15px;' alt='Delete Collection'></span>";
	}

	return html;

}

function renderGeneratePredActions(data, type, row) {

	var html = "";
	html += "<a aria-label='download link' style='border: transparent;' class='btn btn-link btn-sm downloadModelAnalysisLink' "
			+ "href='javascript:void(0);' "
			+ "dataset_path = '"
			+ row.inputDatasetPath
			+ "' pred_path ='"
			+ row.predictionsPath
			+ "'"
			+ " outcome_path = '"
			+ row.outcomeFilePath
			+ "'><img src='images/download_file.svg' data-toggle='tooltip' title='Download Model Analysis Files' th:src='@{/images/download_file.svg}' "
			+ "style='width:17px;' alt='download file'></a>";

	if (row.isOwner == true) {

		// add capability to edit the predictions access

		html += "<span style='border: transparent;'"
				+ "class='btn btn-link btn-sm editPredAccess' pred_is_public = '"
				+ row.isPublic
				+ "'pred_collPath = '"
				+ row.predictionFolderPath
				+ "' "
				+ "pred_collId = '"
				+ row.predCollId
				+ "' pred_accessGrps ='"
				+ row.predAccessGrps
				+ "'>"
				+ "<img src='images/Search_EditMetaData.svg' data-toggle='tooltip' title='Edit Prediction Access' th:src='@{/images/Search_EditMetaData.svg}' "
				+ "style='width:15px;' alt='Edit Prediction Access'></span>";

		// add capability to delete the predictions set

		html += "<span style='border: transparent;' dataset_path = '"
				+ row.inputDatasetPath
				+ "' pred_path ='"
				+ row.predictionsPath
				+ "'pred_collPath = '"
				+ row.predictionFolderPath
				+ "' "
				+ "is_reference_dataset = '"
				+ row.isReferenceDataset
				+ "' outcome_path = '"
				+ row.outcomeFilePath
				+ "'"
				+ " task_id = '"
				+ row.taskId
				+ "'class='btn btn-link btn-sm deletePredBtn'>"
				+ "<img src='images/Delete.png' data-toggle='tooltip' title='Delete Predictions' th:src='@{/images/Delete.png}' "
				+ "style='width:15px;' alt='Delete Predictions'></span>";

	}

	return html;
}

$('#dataSetTable tbody').on(
		'click',
		'.deleteFolderCollectionBtn',
		function() {
			var path = $(this).attr('coll_path');
			var name = $(this).attr('coll_name');
			bootbox.confirm({
				message : "Are you sure you want to delete " + name + "?",
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
							collPath : path
						};

						invokeAjax('/deleteCollection', 'POST', params, postSuccessDeleteCollectionFunction,
								postFailureDeleteCollectionFunction,
								'application/x-www-form-urlencoded; charset=UTF-8', 'text');

					}
				}
			});
		});

function postSuccessDeleteCollectionFunction(data, status) {
	if (data && data == 'Not Authorized') {
		location.replace("/loginTab");
	} else if (data != "SUCCESS") {
		return bootbox.alert(data);
	} else {
		refreshTaskDatatable('dataSetTable');
	}
}

$('#generatePredTable tbody')
		.on(
				'click',
				'.deletePredBtn',
				function() {

					var inputDataFilePath = $(this).attr('dataset_path');
					var predPath = $(this).attr('pred_path');
					var outcomePath = $(this).attr('outcome_path');
					var predCollectionPath = $(this).attr('pred_collPath');
					var taskId = $(this).attr('task_id');
					var isReferenceDataset = $(this).attr('is_reference_dataset');
					var paths = [];
					if (isReferenceDataset && isReferenceDataset == "false") {
						paths.push(inputDataFilePath);
						if (outcomePath && outcomePath != "null") {
							paths.push(outcomePath);
						}
					}

					paths.push(predPath);
					var deletePredModel = {};
					deletePredModel.deletepaths = paths.join();
					deletePredModel.taskId = taskId;
					deletePredModel.predCollectionPath = predCollectionPath;

					bootbox
							.confirm({
								message : "This will delete your input dataset file, prediction file, and outcome file. Are you sure?",
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

										invokeAjax('/deletePredictions', 'POST', JSON.stringify(deletePredModel),
												postSuccessDeletePredictionFunction,
												postFailureDeleteCollectionFunction, null, 'text');
									}
								}
							});
				});

function postSuccessDeletePredictionFunction(data, status) {
	if (data && data == 'Not Authorized') {
		location.replace("/loginTab");
	} else if (data != "SUCCESS") {
		return bootbox.alert(msg);
	} else {
		refreshTaskDatatable('generatePredTable');
	}
}

$('#dataSetTable tbody').on(
		'click',
		'.deleteDataFileBtn',
		function() {
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

						invokeAjax('/delete/datafile', 'POST', params, postSuccessDeleteCollectionFunction,
								postFailureDeleteCollectionFunction,
								'application/x-www-form-urlencoded; charset=UTF-8', 'text');
					}
				}
			});
		});

$('#dataSetTable tbody').on('click', '.downloadMetadata', function() {
	exportDataObjectMetadataResults($(this));

});

function exportDataObjectMetadata() {
	exportDataObjectMetadataResults();
}

function exportDataObjectMetadataResults($this) {
	var selectedPaths = [];
	if ($this) {
		var selectedPath = $this.attr('data_path');
		selectedPaths.push(selectedPath);
	} else {
		$("#dataSetTable tbody input[type=checkbox].selectIndividualCheckbox:checked").each(function() {
			selectedPaths.push($(this).attr('id'));
		});
	}
	var assetPath = $("#assetPath").val();
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
				window.open('/export?assetPath=' + assetPath + '&&isParent=true&&selectedPaths='
						+ selectedPaths, '_self');
			} else if (result == false) {

				window.open('/export?assetPath=' + assetPath + '&&isParent=false&&selectedPaths='
						+ selectedPaths, '_self');
			}
		}
	});
}

function onClickOfModelAnlysisBulkDownloadBtn($this) {
	var selectedPaths = [];
	var assetIdentifier = $("#assetIdentifier").val();

	if ($this) {
		selectedPaths.push($this.attr('dataset_path'));
		selectedPaths.push($this.attr('pred_path'));
		var outcomePath = $this.attr('outcome_path');
		if (outcomePath && outcomePath != "null") {
			selectedPaths.push(outcomePath);
		}

	} else {
		$("#generatePredTable tbody input[type=checkbox]:checked").each(function() {
			selectedPaths.push($(this).attr('dataset_path'));
			selectedPaths.push($(this).attr('pred_path'));
			var outcomePath = $(this).attr('outcome_path');
			if (outcomePath && outcomePath != "null") {
				selectedPaths.push(outcomePath);
			}

		});
	}

	location.replace('/downloadTab?selectedPaths=' + selectedPaths + '&&assetIdentifier=' + assetIdentifier
			+ '&&downloadAsyncType=datafiles&&returnToSearch=false');

}

function downloadFunction(path, fileName, fileSize) {

	var assetIdentifier = $("#assetIdentifier").val();
	if (!fileName) {
		location.replace('/downloadTab?selectedPaths=' + path + '&&assetIdentifier=' + assetIdentifier
				+ '&&downloadAsyncType=collection&&returnToSearch=false');
	} else {
		location.replace('/downloadTab?selectedPaths=' + path + '&&fileName=' + fileName + '&&assetIdentifier='
				+ assetIdentifier + '&&fileSize=' + fileSize +'&&downloadAsyncType=data_object&&returnToSearch=false');
	}

}

function onClickOfBulkDownloadBtn(tableName) {
	var selectedPaths = [];
	var assetIdentifier = $("#assetIdentifier").val();
	var fileName;
	var fileSize;
	
	var len = $("#" + tableName + " tbody input[type=checkbox].selectIndividualCheckbox:checked").length;
	$("#" + tableName + " tbody input[type=checkbox].selectIndividualCheckbox:checked").each(function() {
		if (len == 1) {
			fileName = $(this).closest('tr').find('td').eq(1).text().trim();
			fileSize = $(this).closest('tr').find('td').eq(2).find('input[type="hidden"]').val().trim();
		}
		selectedPaths.push($(this).attr('id'));
	});

	if (selectedPaths.length == 1) {

		location.replace('/downloadTab?selectedPaths=' + selectedPaths + '&&fileName=' + fileName
				+ '&&assetIdentifier=' + assetIdentifier + '&&fileSize=' + fileSize +'&&downloadAsyncType=data_object&&returnToSearch=false');
	} else {
		location.replace('/downloadTab?selectedPaths=' + selectedPaths + '&&assetIdentifier=' + assetIdentifier
				+ '&&downloadAsyncType=datafiles&&returnToSearch=false');
	}
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