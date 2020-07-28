function refreshDataSetDataTable(dataSetPath,metadata,accessgroups,permissions,collections) {
	var isVisible = (loggedOnUserInfo ? true:false);
    console.log("refresh datatable");
    $("#dataSetTable").dataTable().fnDestroy();
    if (!$.fn.DataTable.isDataTable('#dataSetTable')) {
    	dataTableInitDataSet(isVisible,dataSetPath,metadata,accessgroups,permissions,collections);
    } else {
        var t = $('#dataSetTable').DataTable();
        console.log(t);
        t.ajax.reload(null, false);
    }
}

function dataTableInitDataSet(isVisible,dataSetPath,metadata,accessgroups,permissions,collections) {
    $('#dataSetTable').DataTable({
        "paging": true,
        "ordering": false,
        "sorting":false,
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

        	$("#downloadSelectedDataSet").prop("disabled",true);
        	if(isVisible) {
        		$("#downloadSelectedDataSet").show();
        	} else {
        		$("#downloadSelectedDataSet").hide();
        	}
        	
        	$("#dataSetMetaData tbody").html("");
        	
        	var selfMetadata = JSON.parse(metadata);
        	$.each(selfMetadata, function(key, value) {	
                $("#dataSetMetaData tbody").append("<tr><td>" + value.displayName + "</td><td>" + value.value + "</td></tr>");
        	});

        	var collectionSet= JSON.parse(collections);
        	$("#selectedProgramName").text(collectionSet.programName);
        	$("#selectedStudyName").text(collectionSet.studyName);
        	$("#selectedDataSetName").text(collectionSet.datasetName);
        	
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
        	   $("#download-modal").find(".selectedFilesDiv").show();
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
        	   constructCollectionMetData(metaData,metaDataPath,true,null,null);
           });
           
           $(".deleteDataFileBtn").click(function(e){
        	   var path = $(this).attr('data-filePath');
        	   bootbox.confirm({
        		    message: "Are you sure you want to delete this?",
        		    buttons: {
        		        confirm: {
        		            label: 'Yes',
        		            className: 'btn-success'
        		        },
        		        cancel: {
        		            label: 'No',
        		            className: 'btn-danger'
        		        }
        		    },
        		    callback: function (result) {
        		    	if(result == true) {
        		           var params = {deletepath:path};
        		           $.ajax({
        						type : "POST",
        					     url : "/delete/datafile",
        						 contentType: 'application/x-www-form-urlencoded; charset=UTF-8',
        					       dataType: 'text',
        					       data: params,
        						 beforeSend: function () {
        					    	   $("#spinner").show();
        					           $("#dimmer").show();
        					       },
        						 success : function(msg) {
        							 $("#spinner").hide();
        					         $("#dimmer").hide();
        							 console.log('SUCCESS: ', msg);
        							 if(msg != "SUCCESS") {
        									return bootbox.alert(msg);
        								} else {
        									refreshDataSetDataTable(dataSetPath,metadata,accessgroups,
        											permissions,collections);
        								}
        							 
        						 },
        						error : function(e) {
        							$("#spinner").hide();
        					         $("#dimmer").hide();
        							 console.log('ERROR: ', e);
        							 bootbox.alert("Data file delete failed.");
        						}
        					});
        		    	}   
        		    }
        		}); 
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
            
            {"data": "name", "render": function (data, type, row) {
                return renderFileSize(data, type, row);
            },
            responsivePriority: 4
            },
        
            {"data": "download", "render": function (data, type, row) {
                return renderDownload(data, type, row,accessgroups,permissions);
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
                "visible": isVisible,
            },
            { "visible": isVisible, "targets": 3}],
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


function renderFileSize(data, type, row) {
	return row.fileSize;
	
}

function renderDownload(data, type, row,accessgroups,permissions) {
	var downdloadFileName = null;
	var path = row.path;
	var html = "";
	var metadata = "";
	var n = path.lastIndexOf("/");
	downdloadFileName = path.substring(n+1);	
	
	html += "<a aria-label='download link' class='btn btn-link btn-sm downloadLink' href='javascript:void(0);' " +
	       "data-toggle='modal' data-backdrop='static' data-keyboard='false' data-fileName = " + downdloadFileName + " data-path=" + row.download + " " +
	        "data-target='#download-modal'><i class='fa fa-download' aria-hidden='true'></i></a>";

	if(row.selfMetadata && row.selfMetadata.length > 0) {
		var metadata = JSON.stringify(row.selfMetadata);
	}	
   
	html += "<span class='btn btn-link btn-sm editDataFileCollectionMetadata'  metadata_path  = '" + path + "'" +
				" metadata_set = '" + metadata  + "' ><i class='fa fa-edit' data-toggle='tooltip'" +
				" data-content='Edit Data Object Metadata'></i></span>";
				
	if(accessgroups && accessgroups.indexOf("public") == -1 && permissions && permissions == 'Owner') {		
	 html+="&nbsp;&nbsp;<span data-filePath = '" + path + "' class='btn btn-link btn-sm deleteDataFileBtn'><i class='fas fa-trash'></i></span>";

	}
	return html;

}


function downloadFunction(path,fileName) {
	$("#download-modal").find(".selectedFilesListDisplay").html("");
	$("#download-modal").find(".selectedFilesListDisplay").append("<p>"+path+"</p>");
	 $("#download-modal").find("#selectedFilesList").val("");
	$("#download-modal").find("#destinationPathId").val(path);	
	$("#download-modal").find("#message").hide();
	$("#download-modal").find('.downloadErrorMsg').html("");
	
	if(fileName && fileName != "null") {
		$("#download-modal").find("#syncRadioSet").show();
		$("#download-modal").find("#informationalText").html("This page allows you to download the " +
				"selected data file either synchronously to your computer or asynchronously " +
				"to Globus endpoint location or an S3 bucket.");
		$("#download-modal").find("#SyncDiv").show();
		$("#download-modal").find("#searchTypeSync").click();
		$("#download-modal").find("#downloadType").val("data_object");
		$("#download-modal").find("#downloadFileNameVal").val(fileName);
		$("#download-modal").find("#downloadFileName").val(fileName);
	    $("#download-modal").find("div#AsyncDiv input[type='text']").val("");
	    $("#download-modal").find("div#s3Div input[type='text']").val("");
	} 
}

function onClickOfBulkDownloadBtn() {
	$("#download-modal").find(".selectedFilesListDisplay").html("");
	$("#download-modal").find("#message").hide();
	$("#download-modal").find('.downloadErrorMsg').html("");
	$("#download-modal").find("#informationalText").html("This page allows you to download the " +
			"selected data files " +
			"asynchronously to a Globus endpoint location or an S3 bucket.");
	 var selectedPaths = [];
	    $("#dataSetTable tbody input[type=checkbox]:checked").each(function () {
	    	selectedPaths.push($(this).attr('id'));
	    });
	    $("#download-modal").find("#selectedFilesList").val(selectedPaths);
	    
	    $.each(selectedPaths, function(index, value) {
	    	$("#download-modal").find(".selectedFilesListDisplay").append("<p>"+value+"</p>");
	    });
	    
	    if(selectedPaths.length == 1) {
	    	  $("#download-modal").find("#destinationPathId").val(selectedPaths);
	    	  $("#download-modal").find("#downloadType").val("data_object");
	    } else  {
	    	$("#download-modal").find("#downloadType").val("datafiles");
	    }
	    $("#download-modal").find("div#AsyncDiv input[type='text']").val("");
	    $("#download-modal").find("div#s3Div input[type='text']").val("");
	    $("#download-modal").find("#SyncDiv").hide();
		$("#download-modal").find("#syncRadioSet").hide();
		$("#download-modal").find(".selectedFilesDiv").show();
	    $("#download-modal").modal('show');
	    
}


function postSuccessDelete(data,status) {
	if(data != "SUCCESS") {
		return bootbox.alert(data);
	} else {
		 $("#dataSetTable").dataTable().fnDestroy();
		 var t = $('#dataSetTable').DataTable();
	        t.ajax.reload(null, false);
	}
}

function postFailureDeleteFunction() {
	return bootbox.alert("Data file delete failed.");
}