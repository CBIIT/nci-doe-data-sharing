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
         "ordering": true,
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
        	$('body').tooltip({selector: '[data-toggle="tooltip"]'});
        },

        "drawCallback": function (settings) {

        	$("#downloadSelectedDataSet").prop("disabled",true);
        	$("#downloadSelectedMetadata").prop("disabled",true);
        	
        	if(isVisible) {
        		$("#downloadSelectedDataSet").show();
        		$("#downloadSelectedMetadata").show();
        	} else {
        		$("#downloadSelectedDataSet").hide();
        		$("#downloadSelectedMetadata").hide();
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
        	 
        	  $(".selectIndividualCheckbox").click(function (e) {
                  var table = $(e.target).closest('table').attr('id');
                  if (!$(this).is(':checked')) {
                      $("#" + table).find(".selectAll").prop('checked', false);
                      $(this).closest("tr").find('a.downloadLink').prop("disabled", false);
                  }
                  var len = $('#' + table).find('.selectIndividualCheckbox:checked').length;
                  $(".downloadLink").prop("disabled", false);
                  if(len >= 1) {
                	  $("#downloadSelectedMetadata").prop("disabled", false);
                	  $("#downloadSelectedDataSet").prop("disabled", false); 
                  } else {
                	  $("#downloadSelectedMetadata").prop("disabled", true);
                	  $("#downloadSelectedDataSet").prop("disabled", true);
                  }
              });
        	  
           $(".downloadLink").click(function(e){
        	   var path = $(this).attr('data-path');
        	   var fileName = $(this).attr('data-fileName');  
        	   //$("#download-modal").find(".selectedFilesDiv").show();
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
        	   var fileName = $(this).attr('data-fileName');
        	   var params= {selectedPath:metaDataPath,levelName:'DataObject',isDataObject:true};
      			$.ajax({
    				type : "GET",
    			     url : '/browse/metaData',
    			     contentType : 'application/json',
    				 data : params,
    				 beforeSend: function () {
    			    	   $("#spinner").show();
    			           $("#dimmer").show();
    			       },
    				 success : function(msg) {
    					 $("#spinner").hide();
    			         $("#dimmer").hide();
    			         constructCollectionMetData(msg,metaDataPath,true,null,null,fileName);
    				 },
    				error : function(e) {
    					 console.log('ERROR: ', e);
    					 $("#spinner").hide();
    			         $("#dimmer").hide();
    				}
    			});
        	
           });
           
          // $("#downloadSelectedMetadata").click(function(e){
        $('#downloadSelectedMetadata').unbind('click').bind('click', function() {
        	   exportDataObjectMetadata();
        	  
           });
           
           $(".downloadMetadata").click(function(e){
        	   var selectedPath = $(this).attr('data_path');       	   
        	   var selectedPaths = [];
        	   selectedPaths.push(selectedPath);
        	   bootbox.confirm({
       		    message: "Do you wish to include parent metadata also?",
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
       		    		window.open('/export?isParent=true&&selectedPaths='+selectedPaths, '_self');
       		    	} else if(result == false) {
       		    		window.open('/export?isParent=false&&selectedPaths='+selectedPaths, '_self');
       		    	}   
       		    }
       		});
        		
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
            {"targets": 0, "orderable": false},
            {"targets": -1, "orderable": false},
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
	
	var html = "";
	if(row.systemMetadata && row.systemMetadata.length > 0) {
		if(row.selfMetadata && row.selfMetadata.length > 0) {
			var metadata = JSON.stringify(row.selfMetadata);
			 html+= "<a class='cil_12_no_color button2a' metadata_type = '" + metadata  + "' tabindex='0'" +
			" data-container='body' data-toggle='popover' data-placement='right' " +
			"data-trigger='click' data-popover-content='#a01'>" + row.name + "</a>";
			 var metadata1 = JSON.stringify(row.systemMetadata);
			 html+= "&nbsp;&nbsp;<a class='cil_12_no_color button2a' " +
			 		"metadata_context='system_metadata' metadata_type = '" + metadata1  + "' " +
			 				"tabindex='0'" +
				" data-container='body' data-toggle='popover' data-placement='right' " +
				"data-trigger='click' data-popover-content='#a01'><i class='fas fa-info-circle' data-toggle='tooltip' title='Key System Metadata'></i></a>";
		} else {
			var metadata = JSON.stringify(row.systemMetadata);
			 html+= row.name + "&nbsp;&nbsp;<a class='cil_12_no_color button2a' metadata_context='system_metadata' metadata_type = '" + metadata  + "' tabindex='0'" +
				" data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' data-popover-content='#a01'><i class='fas fa-info-circle' data-toggle='tooltip' title='Key System Metadata'></i></a>";
		}
	} else if(row.selfMetadata && row.selfMetadata.length > 0) {
		var metadata = JSON.stringify(row.selfMetadata);
		 html+= "<a class='cil_12_no_color button2a' metadata_type = '" + metadata  + "' tabindex='0'" +
		" data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' data-popover-content='#a01'>" + row.name + "</a>";
	} else {
		 html= row.name;
	}
	
	return html;
}


function displayPopoverDataSet() {
    $('.button2a').on('click', function (e) {
        openPopOverDataSet($(this));
    });
    $('.button2a').on('keypress', function (e) {
        if (e.which == 13 || e.keyCode == 13) {
        	openPopOverDataSet($(this));
        }
    });
}

function openPopOverDataSet($this) {
    var pop = $this;
    $('.button2a').not($this).popover('hide'); 
    var metadata = $this.attr('metadata_type');
    var usermetadata = $this.attr('sys_metadata');
    var headerTest = 'System Metadata';
    
    var list = JSON.parse(metadata);
    var list1 = JSON.parse(usermetadata);
    var ind = "";
    
    if(list) {
    	headerTest = 'User Metadata';
    	
       ind = "<div id=\"a01\" class=\"col-md-12 hidden\"> <div class=\"popover-heading\">" +
                "" + headerTest +" <a class=\"button closeBtn float-right\" href=\"javascript:void(0);\"><i class=\"fa fa-times\"></i></a> </div>" +
                "<div class='popover-body'> <div class='divTable' style='width: 100%;border: 1px solid #000;'>" +
                "<div class='divTableBody'><div class='divTableRow'>" +
                "<div class='divTableHead'>Attribute</div>" + 
                "<div class='divTableHead'>Value</div></div>";

            var content = "";

            $.each(list, function( key, value ) {	
                content += "<div class='divTableRow'><div class='divTableCell'>" + value.displayName + "</div>" +
                        "<div class='divTableCell'>" + value.value + "</div></div>";
                });
            
            var table = ind + content + "</div> </div></div> </div>";
           
    }
            
            if(list1) {
            	headerTest = 'System Metadata';
                 ind += "<div id=\"a01\" class=\"col-md-12 hidden\"> <div class=\"popover-heading\">" +
                "" + headerTest +" <a class=\"button closeBtn float-right\" href=\"javascript:void(0);\"><i class=\"fa fa-times\"></i></a> </div>" +
                "<div class='popover-body'> <div class='divTable' style='width: 100%;border: 1px solid #000;'>" +
                "<div class='divTableBody'><div class='divTableRow'>" +
                "<div class='divTableHead'>Attribute</div>" + 
                "<div class='divTableHead'>Value</div></div>";


            $.each(list1, function( key, value ) {	
                content += "<div class='divTableRow'><div class='divTableCell'>" + value.displayName + "</div>" +
                        "<div class='divTableCell'>" + value.value + "</div></div>";
                });
            
             table += ind + content + "</div> </div></div> </div>";
            
            }
            $("#a01").remove();
            pop.after(table);
            pop.data('bs.popover').setContent();
            pop.popover('show');
        
   
}

function renderFileSize(data, type, row) {
	return row.fileSize;
	
}
function exportDataObjectMetadata() {
	   bootbox.confirm({
 		    message: "Do you wish to include parent metadata also?",
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
 		    		 //exportDataObjectMetadata('true');
 		    		var selectedPaths = [];
 		    	    $("#dataSetTable tbody input[type=checkbox]:checked").each(function () {
 		    	    	selectedPaths.push($(this).attr('id'));
 		    	    });
 		    		window.open('/export?isParent=true&&selectedPaths='+selectedPaths, '_self');
 		    	} else if(result == false) {
 		    		// exportDataObjectMetadata('false');
 		    		var selectedPaths = [];
 		    	    $("#dataSetTable tbody input[type=checkbox]:checked").each(function () {
 		    	    	selectedPaths.push($(this).attr('id'));
 		    	    });
 		    		window.open('/export?isParent=false&&selectedPaths='+selectedPaths, '_self');
 		    	}   
 		    }
 		});
	
}

function renderDownload(data, type, row,accessgroups,permissions) {
	var downdloadFileName = null;
	var path = row.path;
	var html = "";
	var metadata = "";
	var n = path.lastIndexOf("/");
	downdloadFileName = path.substring(n+1);	
	
	html += "<a aria-label='download link' class='btn btn-link btn-sm downloadLink' href='javascript:void(0);' " +
	       "data-fileName = " + downdloadFileName + " data-path=" + row.download + " " +
	        "><i class='fa fa-download' data-toggle='tooltip' title='Download File' aria-hidden='true'></i></a>";

	if(row.selfMetadata && row.selfMetadata.length > 0) {
		var metadata = JSON.stringify(row.selfMetadata);
	}	
   
	html += "<span class='btn btn-link btn-sm editDataFileCollectionMetadata'  metadata_path  = '" + path + "'" +
				" metadata_set = '" + metadata  + "' data-fileName = '" + downdloadFileName + "' ><i class='fa fa-edit' data-toggle='tooltip'" +
				" title='Edit File Metadata'></i></span>";
				
	if(accessgroups && accessgroups.indexOf("public") == -1 && permissions && permissions == 'Owner') {		
	 html+="&nbsp;&nbsp;<span data-filePath = '" + path + "' class='btn btn-link btn-sm deleteDataFileBtn'><i class='fas fa-trash' data-toggle='tooltip' title='Delete File'></i></span>";

	}
	
	html += "<a aria-label='download link' class='btn btn-link btn-sm downloadMetadata'  data_path  = '" + path + "' href='javascript:void(0);' " +
     "><i class='fas fa-file-export' data-toggle='tooltip' title='Download Metadata'></i></a>";
	
	return html;

}


function downloadFunction(path,fileName) {
	location.replace('/downloadTab?selectedPaths='+path+'&&fileName='+fileName+'&&downloadAsyncType=data_object');
}

function onClickOfBulkDownloadBtn() {
	var selectedPaths = [];
    $("#dataSetTable tbody input[type=checkbox]:checked").each(function () {
    	selectedPaths.push($(this).attr('id'));
    });

    if(selectedPaths.length == 1) {
  	location.replace('/downloadTab?selectedPaths='+selectedPaths+'&&downloadAsyncType=data_object');
  } else  {
  	location.replace('/downloadTab?selectedPaths='+selectedPaths+'&&downloadAsyncType=datafiles');
  }	    
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