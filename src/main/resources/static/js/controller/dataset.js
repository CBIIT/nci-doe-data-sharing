function refreshDataSetDataTable(dataSetPath,metadata) {
	var isVisible = (loggedOnUserInfo ? true:false);
    console.log("refresh datatable");
    $("#dataSetTable").dataTable().fnDestroy();
    if (!$.fn.DataTable.isDataTable('#dataSetTable')) {
    	dataTableInitDataSet(isVisible,dataSetPath,metadata);
    } else {
        var t = $('#dataSetTable').DataTable();
        console.log(t);
        t.ajax.reload(null, false);
    }
}

function dataTableInitDataSet(isVisible,dataSetPath,metadata) {
    $('#dataSetTable').DataTable({
        "paging": true,
        "ordering": false,
        "info": true,
        "pageLength": 25,
        oLanguage: {
            "sSearch": "Filter:"
        },
        "ajax": {
            "url": "/getDataObjects",
            "type": "GET",
            "data": {path:dataSetPath},
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
        	$("#dataSetMetaData tbody").html("");
        	
        	var selfMetadata = JSON.parse(metadata);
        	$.each(selfMetadata, function(key, value) {	
                $("#dataSetMetaData tbody").append("<tr><td>" + value.key + "</td><td>" + value.value + "</td></tr>");
        	});

        	
        	 $(".selectAll").change(function (e) {
                 var table = $(e.target).closest('table');
                 var tableId = table.attr('id');
                 var row_count = $('#' + tableId + "> tbody").children().length;
                 if ($(this).is(':checked')) {
                     $('td input:checkbox', table).prop('checked', true);
                     if (row_count > 1) {
                         $("#downloadSelectedDataSet").prop("disabled", false);
                         $(".downloadLink").prop("disabled", true);
                     }

                 } else {
                     $('td input:checkbox', table).prop('checked', false);
                     if (row_count > 1) {
                         $("#downloadSelectedDataSet").prop("disabled", true);
                         $(".downloadLink").prop("disabled", false);
                     }
                 }
             });
        	 
        	  $(".selectIndividualCheckbox").click(function (e) {
                  var table = $(e.target).closest('table').attr('id');
                  if (!$(this).is(':checked')) {
                      $("#" + table).find(".selectAll").prop('checked', false);
                      $(this).closest("tr").find('a.downloadLink').prop("disabled", false);
                  }
                  var len = $('#' + table).find('.selectIndividualCheckbox:checked').length;
                  if (len > 1) {
                      $("#downloadSelectedDataSet").prop("disabled", false);
                      $("#" + table + " input[type=checkbox]:checked").each(function () {
                          $(this).closest("tr").find('a.downloadLink').prop("disabled", true);
                      });
                  } else {
                      $("#downloadSelectedDataSet").prop("disabled", true);
                      $(".downloadLink").prop("disabled", false);
                  }
              });
        	  
           $(".downloadLink").click(function(e){
        	   var path = $(this).attr('data-path');
        	   var fileName = $(this).attr('data-fileName');  
        	   $("#download-modal").find(".selectedFilesDiv").hide();
               downloadFunction(path,fileName);
             });
           
           $("#downloadSelectedDataSet").click(function(e){
        	   onClickOfBulkDownloadBtn();
           });
           
           $(".editDataFileCollectionMetadata").click(function(e){
        	   $("#searchFragmentDiv").hide();
        	   $("#dataSetFragment").hide();
        	   $("#editCollectionFragment").show();
        	   var metaData = $(this).attr('metadata_set');
        	   var metaDataPath = $(this).attr('metadata_path');
        	   constructCollectionMetData(metaData,metaDataPath,true);
           });
           
           
           initializeToolTips();
           initializePopover();
           displayPopover();
        },

        "columns": [
        	{"data": "path", "render": function (data, type, row) {
                return renderSelect(data, type, row);
            },
            responsivePriority: 2
        },
        
            {"data": "name", "render": function (data, type, row) {
                    return renderDataSetPath(data, type, row);
                },
                responsivePriority: 1
            },
            {"data": "download", "render": function (data, type, row) {
                return renderDownload(data, type, row);
            },
            responsivePriority: 3
        },

        ],
        columnDefs: [
            {
                orderable: false,
                className: 'select-checkbox',
                headerHtml: 'batch select',
                blurable: true,
                targets: 0,
            },],
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

function renderSelect(data, type, row) {
	var selectHtml = "<input type='checkbox' id='" + row.path + "' class='dt-checkboxes selectIndividualCheckbox' aria-label='select'/>";

    return selectHtml;
}

function renderDataSetPath(data, type, row) {
	
	
	if(row.selfMetadata && row.selfMetadata.length > 0) {
		var metadata = JSON.stringify(row.selfMetadata);
		var html = "<a class='cil_12_no_color button2a' metadata_type = '" + metadata  + "' tabindex='0'" +
		" data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' data-popover-content='#a01'>" + row.name + "</a>"
	} else {
		return row.name;
	}
	
	return html;
}

function renderDownload(data, type, row) {
	var downdloadFileName = null;
	var path = row.path;
	var html = "";
	if(search_criteria_json.searchType != 'collection') {
		var n = path.lastIndexOf("/");
		downdloadFileName = path.substring(n+1);		
	}
	
	
	html += "<a id='downloadlink' class='btn btn-link btn-sm downloadLink' href='javascript:void(0);' " +
	       "data-toggle='modal' data-backdrop='static' data-keyboard='false' data-fileName = " + downdloadFileName + " data-path=" + row.download + " " +
	        "data-target='#download-modal'><i class='fa fa-download' aria-hidden='true'></i></a>";

	if(row.selfMetadata && row.selfMetadata.length > 0) {
		var metadata = JSON.stringify(row.selfMetadata);
		
		html += "<span class='btn btn-link btn-sm editDataFileCollectionMetadata'  metadata_path  = '" + path + "' metadata_set = '" + metadata  + "' ><i class='fa fa-edit' data-toggle='tooltip' data-content='Edit Data Object Metadata'></i></span>";
	}
	
	return html;

}


function downloadFunction(path,fileName) {
	
	$("#download-modal").find("#destinationPathId").val(path);	
	$("#download-modal").find("#message").hide();
	
	if(fileName && fileName != "null") {
		$("#download-modal").find("#syncRadioSet").show();
		$("#download-modal").find("#SyncDiv").show();
		$("#download-modal").find("#searchTypeSync").click();
		$("#download-modal").find("#downloadType").val("data_object");
		$("#download-modal").find("#downloadFileNameVal").val(fileName);
		$("#download-modal").find("#downloadFileName").val(fileName);
	} 
}

function onClickOfBulkDownloadBtn() {
	$("#download-modal").find(".selectedFilesListDisplay").html("");
	 var selectedPaths = [];
	    $("#dataSetTable tbody input[type=checkbox]:checked").each(function () {
	    	selectedPaths.push($(this).attr('id'));
	    });
	    $("#download-modal").find(".selectedFilesList").val(selectedPaths);
	    
	    $.each(selectedPaths, function(index, value) {
	    	$("#download-modal").find(".selectedFilesListDisplay").append("<p>"+value+"</p>");
	    });
	    
	    if(selectedPaths.length == 1) {
	    	  $("#download-modal").find("#destinationPathId").val(selectedPaths);
	    	  $("#download-modal").find("#downloadType").val("data_object");
	    } else  {
	    	$("#download-modal").find("#downloadType").val("datafiles");
	    }
	    
	    $("#download-modal").find("#SyncDiv").hide();
		$("#download-modal").find("#syncRadioSet").hide();
		$("#download-modal").find(".selectedFilesDiv").show();
	    $("#download-modal").modal('show');
	    
}