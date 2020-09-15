$(document).ready(function () {
	$("#landing-tab").removeClass('active');
	$("#manageTasks-tab").addClass('active');
	refreshTaskManagerDataTable();
});

function refreshTaskManagerDataTable() {
    console.log("refresh datatable");
    if (!$.fn.DataTable.isDataTable('#manageTasksTable')) {
    	dataTableInitTaskManager();
    } else {
        var t = $('#manageTasksTable').DataTable();
        console.log(t);
        t.ajax.reload(null, false);
    }
}

function retryUpload(taskId,taskName) {
	var params= {taskId:taskId,taskName:taskName};
	$.ajax({
		type : "POST",
	     url : "/uploadtask",
		 data : params,
		 beforeSend: function () {
	    	   $("#spinner").show();
	           $("#dimmer").show();
	       },
		 success : function(msg) {
			 $("#spinner").hide();
	         $("#dimmer").hide();
	         refreshTaskManagerDataTable();		 
		 },
		error : function(e) {
			$("#spinner").hide();
	         $("#dimmer").hide();
			 console.log('ERROR: ', e);				 
		}
	});
}

function retryDownload(taskId,taskName,taskType) {
	var params= {taskId:taskId,taskName:taskName,taskType:taskType};
	$.ajax({
		type : "POST",
	     url : "/downloadtask",
		 data : params,
		 beforeSend: function () {
	    	   $("#spinner").show();
	           $("#dimmer").show();
	       },
		 success : function(msg) {
			 $("#spinner").hide();
	         $("#dimmer").hide();
	         refreshTaskManagerDataTable();		 
		 },
		error : function(e) {
			$("#spinner").hide();
	         $("#dimmer").hide();
			 console.log('ERROR: ', e);				 
		}
	});
}

function dataTableInitTaskManager() {
    $('#manageTasksTable').DataTable({
        "paging": true,
        "ordering": true,
        "info": true,
        "pageLength": 25,
        oLanguage: {
            "sSearch": "Filter:"
        },
        "ajax": {
            "url": "/tasks",
            "type": "GET",
            "data": {userId:loggedOnUserInfo},
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
        },

        "columns": [
        	
        {"data": "taskId", "defaultContent": "",responsivePriority: 3},
        {"data": "taskName", "defaultContent": "",responsivePriority: 1},
        {"data": "taskDate", "defaultContent": "",responsivePriority: 4},
        {"data": "taskType", "defaultContent": "",responsivePriority: 5},
        {"data": "transferStatus", "defaultContent": "",responsivePriority: 2},
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

