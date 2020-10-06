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


function renderTaskName(data, type, row) {
    
	var html = "";
	
	html += row.taskName + "&nbsp;&nbsp;<span prog_name='" + row.progName + "' study_name = '"+row.studyName + "' " +
			"datasetname='" + row.dataSetName + "' class='button3a' data-container='body' data-toggle='popover'" +
			"data-placement='right' data-trigger='click' data-popover-content='#a02'><i class='fa fa-question-circle'></i></span>";
	
	return html;
}

function rendertaskId(data, type, row) {
		
	return row.taskId ;
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

            var content = "<div class='divTableRow'><div class='divTableCell'>Program Name</div>" +
                        "<div class='divTableCell'>" + programName + "</div></div><div class='divTableRow'>" +
                        "<div class='divTableCell'>Study Name</div>" +
                        "<div class='divTableCell'>" + studyName + "</div></div><div class='divTableRow'>" +
                        "<div class='divTableCell'>Dataset Name</div>" +
                        "<div class='divTableCell'>" + datasetName + "</div></div>";
                
            
            var table = ind + content + "</div> </div></div> </div>";
            $("#a02").remove();
            pop.after(table);
            pop.data('bs.popover').setContent();
            pop.popover('show');
        
   
}
