$(document).ready(function () {
	
	refreshDataSetDataTable();
});

function refreshDataSetDataTable() {
	var isVisible = (loggedOnUserInfo ? true:false);
    console.log("refresh datatable");
    if (!$.fn.DataTable.isDataTable('#dataSetTable')) {
    	dataTableInitDataSet(isVisible);
    } else {
        var t = $('#dataSetTable').DataTable();
        console.log(t);
        t.ajax.reload(null, false);
    }
}

function dataTableInitDataSet(isVisible) {
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
            "data": {path:$("#assetPath").val()},
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
                 downloadFunction(path,fileName);
              });
           
              $("#downloadSelectedDataSet").click(function(e){
        	       onClickOfBulkDownloadBtn();
              });
           

               $(".editDataFileCollectionMetadata").click(function(e){
        	        $("#assetDetailsFragment").hide();
        	        $("#editCollectionFragment").show();
        	        $(".backToAssetDetailsBtn").show();
        	        $(".backToSearchBtn").hide();
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
    			         constructCollectionMetData(msg,metaDataPath,true,fileName);
    				 },
    				 error : function(e) {
    					 console.log('ERROR: ', e);
    					 $("#spinner").hide();
    			         $("#dimmer").hide();
    			         bootbox.alert("Error in retrieving collection metadata.");
    				 }
    			   });
        	
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
        									refreshDataSetDataTable();
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
           displayPopoverDataSet();
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
            
            {"data": "fileSize", "render": function (data, type, row) {
                return renderFileSize(data, type, row);
            },
            responsivePriority: 4
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
                "visible": isVisible,
            },
            {"targets": 0, "orderable": false},
            {"targets": -1, "orderable": false},
            {"targets":2,"type":"file-size"},
            { "visible": isVisible, "targets": 3}],
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

$.fn.dataTable.ext.type.order['file-size-pre'] = function ( data ) {
    var matches = data.match( /^(\d+(?:\.\d+)?)\s*([a-z]+)/i );
    var multipliers = {
        b:  1,
        bytes: 1,
        kb: 1000,
        kib: 1024,
        mb: 1000000,
        mib: 1048576,
        gb: 1000000000,
        gib: 1073741824,
        tb: 1000000000000,
        tib: 1099511627776,
        pb: 1000000000000000,
        pib: 1125899906842624
    };

    if (matches) {
        var multiplier = multipliers[matches[2].toLowerCase()];
        return parseFloat( matches[1] ) * multiplier;
    } else {
        return -1;
    }
}


function renderSelect(data, type, row) {
	
	var selectHtml = "<input type='checkbox' id='" + row.path + "' class='dt-checkboxes selectIndividualCheckbox'" +
			         " aria-label='select'/>";

    return selectHtml;
}

function renderDataSetPath(data, type, row) {
	
	var html = "";
	var userMetadata = "";
	var systemMetadata = "";
		
	if(row.systemMetadata && row.systemMetadata.length > 0) {
		systemMetadata = JSON.stringify(row.systemMetadata);
	}
	
	if(row.selfMetadata && row.selfMetadata.length > 0) {
		userMetadata = JSON.stringify(row.selfMetadata);
	}
	
	html+= "&nbsp;&nbsp;&nbsp;" + row.name + "&nbsp;&nbsp;<a class='cil_12_no_color button2a' " +
		   "userMetadata = '" + userMetadata  + "' sys_metadata = '" + systemMetadata  + "' tabindex='0'" +
	       " data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' " +
	       "data-popover-content='#a01'><i class='fas fa-info-circle' data-toggle='tooltip' title='Metadata'></i></a>";	
			
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
    var userMetadata = $this.attr('userMetadata');
    var sysMetadata = $this.attr('sys_metadata');
    var userMetadataList = "";
    
    if(userMetadata) {
       userMetadataList = JSON.parse(userMetadata);
    }
    
    var sysMetadatalist = JSON.parse(sysMetadata);
    
    var ind = "<div id=\"a01\" class=\"col-md-12 hidden\"><div class=\"popover-heading\">" +
              "Metadata <a class=\"button closeBtn float-right\" href=\"javascript:void(0);\">" +
              "<i class=\"fa fa-times\"></i></a> </div><div class='popover-body'>";
    var table = "";
    var content = "";
    
    if(userMetadataList) {

       ind += "<p><b>User Metadata </b></p><div class='divTable' style='width: 100%;border: 1px solid #000;'>" +
              "<div class='divTableBody'><div class='divTableRow'>" +
              "<div class='divTableHead rowAttribute'>ATTRIBUTE</div>" + 
              "<div class='divTableHead'>VALUE</div></div>";

              $.each(userMetadataList, function( key, value ) {	
                content += "<div class='divTableRow'><div class='divTableCell'>" + value.displayName + "</div>" +
                        "<div class='divTableCell'>" + value.value + "</div></div>";
               });
            content += "</div> </div><br/>";
           
     }
            
    if(sysMetadatalist) {
       content += "<p><b>Key System Metadata </b></p><div class='divTable' style='width: 100%;border: 1px solid #000;'>" +
                  "<div class='divTableBody'><div class='divTableRow'>" +
                  "<div class='divTableHead rowAttribute'>ATTRIBUTE</div>" + 
                  "<div class='divTableHead'>VALUE</div></div>";

            $.each(sysMetadatalist, function( key, value ) {	
                content += "<div class='divTableRow'><div class='divTableCell'>" + value.displayName + "</div>" +
                        "<div class='divTableCell'>" + value.value + "</div></div>";
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
	return "&nbsp;&nbsp;&nbsp;" + row.fileSize;
	
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
 		    		var selectedPaths = [];
 		    	    $("#dataSetTable tbody input[type=checkbox]:checked").each(function () {
 		    	    	selectedPaths.push($(this).attr('id'));
 		    	    });
 		    		window.open('/export?isParent=true&&selectedPaths='+selectedPaths, '_self');
 		    	} else if(result == false) {
 		    		var selectedPaths = [];
 		    	    $("#dataSetTable tbody input[type=checkbox]:checked").each(function () {
 		    	    	selectedPaths.push($(this).attr('id'));
 		    	    });
 		    		window.open('/export?isParent=false&&selectedPaths='+selectedPaths, '_self');
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
	downdloadFileName = path.substring(n+1);	
	
	html += "<button type='button' style='border: transparent;' class='btn btn-link btn-sm share_path_copy' data-toggle='tooltip' data-placement='top' " +
	        "title='Copy File Path' data-clipboard-text='"+ row.path + "'>" +
	        "<img src='images/Copy-FilePath.png' th:src='@{/images/Copy-FilePath.png}' " +
			"style='width:17px;' alt='copy file path'></button>";
	
	if(permissions && permissions != 'No Permissions') {
	  html += "<span style='border: transparent;' class='btn btn-link btn-sm editDataFileCollectionMetadata'  metadata_path  = '" + path + "'" +
	        "data-fileName = '" + downdloadFileName + "' >" +
			"<img src='images/Edit-FileMetadata.png' data-toggle='tooltip' title='Edit File Metadata' th:src='@{/images/Edit-FileMetadata.png}' " +
			"style='width:17px;' alt='edit collection'></span>";
	}
	
	html += "<a aria-label='download link' style='border: transparent;' class='btn btn-link btn-sm downloadMetadata'  data_path  = '" + path + "' href='javascript:void(0);' " +
            "><img src='images/Download-Metadata.png' data-toggle='tooltip' title='Download File Metadata' th:src='@{/images/Download-Metadata.png}' " +
			"style='width:17px;' alt='Download File Metadata'></a>";
	
	html += "<a aria-label='download link' style='border: transparent;' class='btn btn-link btn-sm downloadLink' href='javascript:void(0);' " +
	        "data-fileName = " + downdloadFileName + " data-path=" + row.download + " " +
	        "><img src='images/Download.png' data-toggle='tooltip' title='Download File' th:src='@{/images/Download.png}' " +
			"style='width:17px;' alt='download file'></a>";
			
	if(accessgroups && accessgroups.indexOf("public") == -1 && permissions && permissions == 'Owner') {		
	
	    html += "<span style='border: transparent;' data-filePath = '" + path + "' class='btn btn-link btn-sm deleteDataFileBtn'>" +
	    	    "<img src='images/Delete.png' data-toggle='tooltip' title='Delete File' th:src='@{/images/Delete.png}' " +
	    			"style='width:15px;' alt='Delete File'></span>";

	}
	
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
    } else {
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