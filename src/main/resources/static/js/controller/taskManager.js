$(document).ready(function () {
	$.fn.dataTable.moment("MM/DD/YYYY HH:mm:ss");
	$("#landing-tab").removeClass('active');
	$("#search-tab").removeClass('active');
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
        "order": [[2, 'desc']],
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
        	$('body').tooltip({selector: '[data-toggle="tooltip"]'});
        },

        "drawCallback": function (settings) {
        	initializeToolTips();
        	initializePopover();
            displayPopoverTask();
        },

        "columns": [
        	{"data": "taskId", "render": function (data, type, row) {
                return rendertaskId(data, type, row);
            },
            responsivePriority: 3
        },
        
        {"data": "taskName", "render": function (data, type, row) {
            return renderTaskName(data, type, row);
        },
        responsivePriority: 1
        },
        {"data": "taskCreatedDate", "render": function (data, type, row) {
            return renderTaskDate(data, type, row);
        },
        responsivePriority: 4
        },
        
        {"data": "taskCompletedDate", "render": function (data, type, row) {
            return renderTaskCompletedDate(data, type, row);
        },
        responsivePriority: 5
        },

        {"data": "taskType", "defaultContent": "",responsivePriority: 6},
        {"data": "transferStatus", "defaultContent": "",responsivePriority: 2},
        ],
       
        "columnDefs": [
            {className: "td_class_4", "targets": [0]},
            {className: "td_class_2", "targets": [1]},
            {className: "td_class_2", "targets": [2]},
            {className: "td_class_2", "targets": [3]},
            {className: "td_class_2", "targets": [4]},
            {className: "td_class_2", "targets": [5]},
            {className: "td_class_5", "targets": [-1]}
        ],
        
        "dom": '<"top"lip>rt<"bottom"ip>',

        "lengthMenu": [[10, 25, 50, 100], [10, 25, 50, 100]],

        "language": {
            "zeroRecords": "Nothing found to display",
            "info": "&nbsp; (Displaying _START_ to _END_ of _TOTAL_ )",
            sLengthMenu: "_MENU_",
            "infoEmpty": " No records to display"
        }
    });
}


function renderTaskDate(data, type, row) {
	if(data) {
		return "&nbsp&nbsp;"+ moment(data).format("MM/DD/YYYY HH:mm:ss") ;
	}
	return "";
}

function renderTaskCompletedDate(data, type, row) {
	if(data) {
		return "&nbsp&nbsp;"+ moment(data).format("MM/DD/YYYY HH:mm:ss");
	}
	return "";
	
}
function renderTaskName(data, type, row) {
    
	var html = "";
	
	if(row.taskName) {
	  html += "&nbsp&nbsp;"+ row.taskName + "&nbsp;&nbsp;<span prog_name='" + row.progName + "' study_name = '"+row.studyName + "' " +
			"datasetname='" + row.dataSetName + "' class='button3a' data-container='body' data-toggle='popover'" +
			"data-placement='right' data-trigger='click' data-popover-content='#a02'>" +
			"<img src='images/Status.info-tooltip.png' th:src='@{/images/Status.info-tooltip.png}' style='width:12px;' alt='Status info'></a></span>";
	}
	
	return html;
}

function rendertaskId(data, type, row) {
		
	return "&nbsp&nbsp;"+ row.taskId ;
}

function displayPopoverTask() {
    $('.button3a').on('click', function (e) {
        openPopOverDisplay($(this));
    });
    $('.button3a').on('keypress', function (e) {
        if (e.which == 13 || e.keyCode == 13) {
        	openPopOverDisplay($(this));
        }
    });
}

function openPopOverDisplay($this) {
    var pop = $this;
    $('.button3a').not($this).popover('hide'); 
    var programName = $this.attr('prog_name');
    var studyName = $this.attr('study_name');
    var datasetName = $this.attr('datasetname');

    
      var ind = "<div id=\"a02\" class=\"col-md-12 hidden\"> <div class=\"popover-heading\">" +
                "<a class=\"button closeBtn float-right\" href=\"javascript:void(0);\"><i class=\"fa fa-times\"></i></a> </div>" +
                "<div class='popover-body'> <div class='divTable' style='width: 100%;border: 1px solid #000;'>" +
                "<div class='divTableBody'>";

            var content = "<div class='divTableRow'><div class='divTableCell'><b>Program Identifier: </b></div>" +
                        "<div class='divTableCell'>" + programName + "</div></div><div class='divTableRow'>" +
                        "<div class='divTableCell'><b>Study Identifier: </b></div>" +
                        "<div class='divTableCell'>" + studyName + "</div></div><div class='divTableRow'>" +
                        "<div class='divTableCell'><b>Asset Identifier: </b></div>" +
                        "<div class='divTableCell'>" + datasetName + "</div></div>";
                
            
            var table = ind + content + "</div> </div></div> </div>";
            $("#a02").remove();
            pop.after(table);
            pop.data('bs.popover').setContent();
            pop.popover('show');
        
   
}

