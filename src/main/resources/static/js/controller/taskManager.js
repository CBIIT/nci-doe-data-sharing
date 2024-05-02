$(document).ready(function() {
	$.fn.dataTable.moment("MM/DD/YYYY HH:mm:ss");	
	var redirectToPredTab = $("#redirectToPredTab").val();
	if(redirectToPredTab) {
	  $("#modelAnalysisStatus").click();
	} else {
	  refreshTaskDatatable('manageTasksTable');
	}	
	$("#search").removeClass('active-nav');

});

$("#modelAnalysisStatus").click(function(e) {
		$(this).css('background-color', '#E2682C');
		$("#transferTaskStatus").css('background-color', '#848484');
		$("#manageTasksTable").hide();
		$("#inferencingTable").show();
		refreshTaskDatatable('inferencingTable');
});

$("#transferTaskStatus").click(function(e) {
		$(this).css('background-color', '#E2682C');
		$("#modelAnalysisStatus").css('background-color', '#848484');
		$("#inferencingTable").hide();
		$("#manageTasksTable").show();
		refreshTaskDatatable('manageTasksTable');
});

$("#showAll").on('change', function() {
	if ($(this).is(':checked')) {
		$("#showAll").val("true");
	} else {
		$("#showAll").val("false");
	}
	$('#manageTasksTable').dataTable().fnDestroy();
	$("#transferTaskStatus").click();
});

function refreshTaskDatatable(table) {
	console.log("refresh datatable");
	if (!$.fn.DataTable.isDataTable('#' + table)) {
		if (table == 'manageTasksTable') {
			$('#inferencingTable').dataTable().fnDestroy();
			dataTableInitTaskManager();
		} else {
			$('#manageTasksTable').dataTable().fnDestroy();
			dataTableInitInferenceTaskManager();
		}

	} else {
		var t = $('#' + table).DataTable();
		console.log(t);
		t.ajax.reload(null, false);
	}

	if (isAdmin == true && table == 'manageTasksTable') {
		$("#displayAllDiv").show();
	} else {
		$("#displayAllDiv").hide();

	}
}

function dataTableInitInferenceTaskManager() {
	$('#inferencingTable').DataTable({
		"paging" : true,
		"order" : [ [ 4, 'desc' ] ],
		"ordering" : true,
		"info" : true,
		"pageLength" : 25,
    "responsive": false,

		"ajax" : {
			"url" : "/performInferencing",
			"type" : "GET",
			"data" : {
				userId : loggedOnUserInfo
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
			initializeToolTips();
			initializePopover();
			displayPopoverTask();

			$(".downloadLink").click(function(e) {
				var dataPath = $(this).attr('data-path');
				downloadFunction(dataPath);
			});
		},

		"columns" : [ {
			"data" : "taskId",
			"render" : function(data, type, row) {
				return rendertaskId(data, type, row);
			},
			responsivePriority : 1
		}, {
			"data" : "assetIdentifer",
			"defaultContent" : "",
			responsivePriority : 2
		}, {
			"data" : "resultPath",
			"defaultContent" : "",
			responsivePriority : 2
		}, {
			"data" : "testDataSetPath",
			"defaultContent" : "",
			responsivePriority : 2
		}, {
			"data" : "startDate",
			"render" : function(data, type, row) {
				return renderTaskDate(data, type, row);
			},
			responsivePriority : 4
		},

		{
			"data" : "completedDate",
			"render" : function(data, type, row) {
				return renderTaskCompletedDate(data, type, row);
			},
			responsivePriority : 5
		},

		{
			"data" : "status",
			"render" : function(data, type, row) {
				return renderInferStatus(data, type, row);
			},
			responsivePriority : 3
		}, ],

		"columnDefs" : [ {
			className : "td_class_1",
			"targets" : [ 0 ]
		}, {
			className : "td_class_1",
			"targets" : [ 1 ]
		}, {
			className : "td_class_1",
			"targets" : [ 2 ]
		}, {
			className : "td_class_1",
			"targets" : [ 3 ]
		}, {
			className : "td_class_1",
			"targets" : [ -1 ]
		}, {
			type : "date",
			"targets" : [ 4, 5 ]
		} ],

		"dom" : '<"top"lpi>rt<"bottom"lpi>',

		"pagingType" : "simple",

		"lengthMenu" : [ [ 10, 25, 50, 100 ], [10, 25, 50, 100 ] ],

		"language" : {
			"lengthMenu" : "ROWS PER PAGE &nbsp;&nbsp; _MENU_",
			"sLoadingRecords" : "Loading...",
			"zeroRecords" : "Nothing found to display",
			"paginate" : {
				next : '<img src="/images/pagination_right_assetfiles.png"/>',
				previous : '<img src="/images/paginate_left_assetFiles.png"/>'
			},
		}
	});
}

function dataTableInitTaskManager() {
	var isVisible = $("#showAll").val() == "true" ? true : false;
	$('#manageTasksTable').DataTable({
		"paging" : true,
		"order" : [ [ 2, 'desc' ] ],
		"ordering" : true,
		"info" : true,
		"pageLength" : 25,
		"ajax" : {
			"url" : "/tasks",
			"type" : "GET",
			"data" : {
				showAll : isVisible
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
			initializeToolTips();
			initializePopover();
			displayPopoverTask();
		},

		"columns" : [ {
			"data" : "taskId",
			"render" : function(data, type, row) {
				return rendertaskId(data, type, row);
			},
			responsivePriority : 3
		},

		{
			"data" : "taskName",
			"render" : function(data, type, row) {
				return renderTaskName(data, type, row);
			},
			responsivePriority : 1
		}, {
			"data" : "taskCreatedDate",
			"render" : function(data, type, row) {
				return renderTaskDate(data, type, row);
			},
			responsivePriority : 4
		},

		{
			"data" : "taskCompletedDate",
			"render" : function(data, type, row) {
				return renderTaskCompletedDate(data, type, row);
			},
			responsivePriority : 5
		},

		{
			"data" : "userId",
			"render" : function(data, type, row) {
				return renderUserId(data, type, row);
			},
			responsivePriority : 7
		}, {
			"data" : "taskType",
			"defaultContent" : "",
			responsivePriority : 6
		}, {
			"data" : "transferStatus",
			"defaultContent" : "",
			responsivePriority : 2
		}, ],

		"columnDefs" : [ {
			className : "td_class_4",
			"targets" : [ 0 ]
		}, {
			className : "td_class_2",
			"targets" : [ 1 ]
		}, {
			className : "td_class_2",
			"targets" : [ 2 ]
		}, {
			className : "td_class_2",
			"targets" : [ 3 ]
		}, {
			className : "td_class_2",
			"targets" : [ 5 ]
		}, {
			className : "td_class_2",
			"targets" : [ 4 ],
			"visible" : isVisible
		}, {
			className : "td_class_5",
			"targets" : [ -1 ]
		}, {
			type : "date",
			"targets" : [ 2, 3 ]
		} ],

		"dom" : '<"top"lpi>rt<"bottom"lpi>',

		"pagingType" : "simple",

		"lengthMenu" : [ [ 10, 25, 50, 100 ], [ 10, 25, 50, 100 ] ],

		"language" : {
			"lengthMenu" : "ROWS PER PAGE &nbsp;&nbsp; _MENU_",
			"sLoadingRecords" : "Loading...",
			"zeroRecords" : "Nothing found to display",
			"paginate" : {
				next : '<img src="/images/pagination_right_assetfiles.png"/>',
				previous : '<img src="/images/paginate_left_assetFiles.png"/>'
			},
		}
	});
}

function renderUserId(data, type, row) {
	return row.userId;
}

function downloadFunction(path) {
	var fileName = path.substring(path.lastIndexOf("/") + 1, path.length);

	location.replace('/downloadTab?selectedPaths=' + path + '&&fileName=' + fileName
			+ '&&downloadAsyncType=data_object&&returnToStatus=true');
}

function renderInferStatus(data, type, row) {
	var html = "";
	if (data == 'COMPLETED' || data == 'Completed') {
		html += "Completed<a aria-label='download link' style='border: transparent;' class='btn btn-link btn-sm downloadLink' href='javascript:void(0);' "
				+ "data-path="
				+ row.resultPath
				+ " "
				+ "><img src='images/Download.png' data-toggle='tooltip' title='Download File' th:src='@{/images/Download.png}' "
				+ "style='width:17px;' alt='download file'></a>";
	} else if (data == 'Failed' || data == 'FAILED') {
		var error_msg = row.errorMessage.replace(/["']/g, "");
		html += "Failed&nbsp;&nbsp;<img style='width:17px;' data-toggle='tooltip'"
				+ "src='images/infoIcon.svg' alt='failed message' title= '" + error_msg + "'>";
	} else if (data == 'NOTSTARTED') {
		html += 'Not Started';
	} else if (data == 'INPROGRESS') {
		html += 'In Progress'
	} else {
		html += data;
	}

	return html;
}

function renderTaskDate(data, type, row) {
	if (data) {
		return moment(data).format("MM/DD/YYYY HH:mm:ss");
	}
	return "";
}

function renderTaskCompletedDate(data, type, row) {
	if (data) {
		return moment(data).format("MM/DD/YYYY HH:mm:ss");
	}
	return "";

}
function renderTaskName(data, type, row) {

	var html = "";

	if (row.taskName) {
		html += row.taskName
				+ "&nbsp;&nbsp;<span prog_name='"
				+ row.progName
				+ "' study_name = '"
				+ row.studyName
				+ "' "
				+ "datasetname='"
				+ row.dataSetName
				+ "' class='button3a' data-container='body' data-toggle='popover'"
				+ "data-placement='right' data-trigger='click' data-popover-content='#a02'>"
				+ "<img src='/images/infoIcon.svg' th:src='@{/images/infoIcon.svg}' style='width:17px;' alt='Status info'></a></span>";
	}

	return html;
}

function rendertaskId(data, type, row) {

	return "&nbsp;&nbsp;"+row.taskId;
}

function retryUpload(taskId, taskName) {
	var params = {
		taskId : taskId,
		taskName : taskName
	};
	$.ajax({
		type : "POST",
		url : "/uploadtask",
		data : params,
		beforeSend : function() {
			$("#spinner").show();
			$("#dimmer").show();
		},
		success : function(msg) {
			$("#spinner").hide();
			$("#dimmer").hide();
			bootbox.alert(msg);
			refreshTaskDatatable('manageTasksTable');
		},
		error : function(e) {
			$("#spinner").hide();
			$("#dimmer").hide();
			console.log('ERROR: ', e);
		}
	});
}

function retryDownload(taskId, taskName, taskType) {
	var params = {
		taskId : taskId,
		taskName : taskName,
		taskType : taskType
	};
	$.ajax({
		type : "POST",
		url : "/downloadtask",
		data : params,
		beforeSend : function() {
			$("#spinner").show();
			$("#dimmer").show();
		},
		success : function(msg) {
			$("#spinner").hide();
			$("#dimmer").hide();
			bootbox.alert(msg);
			refreshTaskDatatable('manageTasksTable');
		},
		error : function(e) {
			$("#spinner").hide();
			$("#dimmer").hide();
			console.log('ERROR: ', e);
		}
	});
}

function displayPopoverTask() {
	$('.button3a').on('click', function(e) {
		openPopOverDisplay($(this));
	});
	$('.button3a').on('keypress', function(e) {
		if (e.which == 13 || e.keyCode == 13) {
			openPopOverDisplay($(this));
		}
	});

	$('.button4a').on('click', function(e) {
		openPopOverTransferStatus($(this));
	});
	$('.button4a').on('keypress', function(e) {
		if (e.which == 13 || e.keyCode == 13) {
			openPopOverTransferStatus($(this));
		}
	});
}

function openPopOverTransferStatus($this) {
	var pop = $this;
	$('.button4a').not($this).popover('hide');
	var errorMsg = $this.attr('error_msg');

	var ind = "<div id=\"a02\" class=\"col-md-12 hidden\"> <div class=\"popover-heading\">"
			+ "<a class=\"button closeBtn float-right\" href=\"javascript:void(0);\"><i class=\"fa fa-times\"></i></a> </div>"
			+ "<div class='popover-body'> <div class='divTable' style='width: 100%;border: 1px solid #000;'>"
			+ "<div class='divTableBody'>";

	var content = "<div class='divTableRow'><div class='divTableCell'>" + errorMsg + "</div></div>";

	var table = ind + content + "</div> </div></div> </div>";
	$("#a02").remove();
	pop.after(table);
	pop.data('bs.popover').setContent();
	pop.popover('show');

}

function openPopOverDisplay($this) {
	var pop = $this;
	$('.button3a').not($this).popover('hide');
	var programName = $this.attr('prog_name');
	var studyName = $this.attr('study_name');
	var datasetName = $this.attr('datasetname');

	var ind = "<div id=\"a02\" class=\"col-md-12 hidden\"> <div class=\"popover-heading asset-identifier\">"
			+ "<a class=\"button closeBtn float-right\" href=\"javascript:void(0);\"><i class=\"fa fa-times\"></i></a> </div>"
			+ "<div class='popover-body'> <div class='divTable' style='width: 100%;border: 1px solid #000;'>"
			+ "<div class='divTableBody'>";

	var content = "<div class='divTableRow'><div class='divTableCell'><b>Program Identifier: </b></div>"
			+ "<div class='divTableCell'>" + programName + "</div></div><div class='divTableRow'>"
			+ "<div class='divTableCell'><b>Study Identifier: </b></div>" + "<div class='divTableCell'>" + studyName
			+ "</div></div><div class='divTableRow'>" + "<div class='divTableCell'><b>Asset Identifier: </b></div>"
			+ "<div class='divTableCell'>" + datasetName + "</div></div>";

	var table = ind + content + "</div> </div></div> </div>";
	$("#a02").remove();
	pop.after(table);
	pop.data('bs.popover').setContent();
	pop.popover('show');

}
