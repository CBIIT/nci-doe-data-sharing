$(document).ready(function () {
	$("#landing-tab").removeClass('active');
	$("#upload-tab").addClass('active');
	loadUploadTab();
});
function loadUploadTab() {	 
	 
	var ins = $("#institutePath").val();
	var stu= $("#studyPath").val();
	var data = $("#datafilePath").val();
	
	if(ins){
		loadJsonData('/browse', $("#instituteList"), true, null, postSuccessInsInitialize, null, "key", "value"); 
		loadJsonData('/browse/collection', $("#studyList"), true, {selectedPath:ins}, postSuccessStudyInitialize, null, "key", "value");
		if(stu) {
			loadJsonData('/browse/collection', $("#dataList"), true, {selectedPath:stu}, postSuccessDataSetInitialize, null, "key", "value");
			if(data) {
				invokeAjax('/browse/collection','GET',{selectedPath:data},contructDataListDiv,null,null,null);		
				$("#studyListDiv").show();
				$("#dataSetListDiv").show();
			    $("#dataListDiv").hide();
				$("#addBulkDataFiles").show();
				$("#uploadDataFilesTab").show();
				$('input[name=datafileTypeUpload]:checked').val();
				$("#singleFileDataUploadSection").hide();
				$("#bulkFileUploadSection").show();
				$("#registerFileBtnsDiv").show();		
				$(".registerBulkDataFileSuccess").hide();
				$(".registerBulkDataFile").html("");
				$("#registerBulkDataForm").show();
				$("#displayS3UploadDiv").hide();
				var uploadAsyncType = $("#uploadAsyncType").val();
				if(uploadAsyncType && uploadAsyncType == 'drive') {
					$("#datafileTypeDriveUpload").prop("checked", true);
					$("#displayGlobusUploadDiv").hide();
					$("#displayDriveUploadDiv").show();
				} else {
					$("#datafileTypeGlobusUpload").prop("checked", true);
					$("#displayGlobusUploadDiv").show();
					$("#displayDriveUploadDiv").hide();
				}
			}
			
		}
		
		
	} else {
		 loadJsonData('/browse', $("#instituteList"), true, null, null, null, "key", "value"); 
	}
  
}

function postSuccessInsInitialize(data,status) {
	$("#instituteList").val($("#institutePath").val());
}

function postSuccessStudyInitialize(data,status) {
	$("#studyList").val($("#studyPath").val());
}

function postSuccessDataSetInitialize(data,status) {
	$("#dataList").val($("#datafilePath").val());
	$("#bulkDataFilePathCollection").val($("#datafilePath").val());
	
}

function functionDelete($this,collectionType) {
	var selectedValue = $( "#" +$this+ " option:selected" ).text();
	var textvalue = $( "#" +$this+ " option:selected" ).val();
	   bootbox.confirm({
		    message: "Are you sure you want to delete " + selectedValue + "?",
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
		           var params = {collPath:textvalue};
		       	$.ajax({
					 type : "POST",
				     url : "/deleteCollection",
				     contentType: 'application/x-www-form-urlencoded; charset=UTF-8',
				     dataType: 'text',
					 data : params,
					 beforeSend: function () {
				    	   $("#spinner").show();
				           $("#dimmer").show();
				       },
					 success : function(msg) {
						 $("#spinner").hide();
				         $("#dimmer").hide();
						 console.log('SUCCESS: ', msg);
						 postSuccessDeleteCollection(msg,collectionType);
						 
					 },
					error : function(e) {
						$("#spinner").hide();
				         $("#dimmer").hide();
						 console.log('ERROR: ', e);				 
					}
				});
		    	}   
		    }
		});
}

function postSuccessDeleteCollection(data,collectionType) {
	if(data && data != "SUCCESS") {
		return bootbox.alert(data);
	} else if(data == 'SUCCESS'){
		// refresh select drop down
		if(collectionType  == 'Program') {
			var params= {selectedPath:$("#basePath").val(),refreshNode:'true'};
			loadJsonData('/browse/collection', $("#instituteList"), true, params, null, null, "key", "value");
			retrieveCollections('instituteList','ANY','deleteAction');
		} else if(collectionType == 'Study') {
			var params= {selectedPath:$("#instituteList").val(),refreshNode:'true'};
			loadJsonData('/browse/collection', $("#studyList"), true, params, null, null, "key", "value");
			retrieveCollections('studyList','ANY','deleteAction');
			
		} else if(collectionType == 'Asset') {
			var params= {selectedPath:$("#studyList").val(),refreshNode:'true'};
			 loadJsonData('/browse/collection', $("#dataList"), true, params, null, null, "key", "value");
				retrieveCollections('dataList','ANY','deleteAction');
		}
	}
}

function retrieveCollections($this, selectedIndex,action) {
	
	var selectTarget = $this;
	var params;
	var selectedValue;
	if(action  == 'onChange') {
		$("#" +selectTarget+ " option[value='ANY']").remove();
		 selectedValue = selectedIndex.value;
		 params = {selectedPath:selectedIndex.value};
	} else {
		 selectedValue = selectedIndex;
		 params= {selectedPath:selectedIndex};
	}
	
	
	if(selectTarget == 'instituteList') {
		$("#uploadDataFilesTab").hide();
		$("#addBulkDataFiles").hide();
		if(selectedValue && selectedValue != 'ANY') {
			loadJsonData('/browse/collection', $("#studyList"), true, params, null, null, "key", "value");
			$("#studyListDiv").show();
			$("#deleteProgram").show();
			$("#dataSetListDiv").hide();
			$("#dataListDiv").hide();
		} else {
			$("#studyListDiv").hide();
			$("#dataSetListDiv").hide();
			$("#dataListDiv").hide();
			$("#deleteProgram").hide();
		}
		
		
	} else if(selectTarget == 'studyList') {
		$("#addBulkDataFiles").hide();
		$("#uploadDataFilesTab").hide();
		 $("#dataListDiv").hide();
		if(selectedValue && selectedValue != 'ANY') {
			var params1= {selectedPath:selectedValue,refreshNode:'true'};			
		  loadJsonData('/browse/collection', $("#dataList"), true, params1, null, null, "key", "value");
		  $("#studyListDiv").show();
		  $("#dataSetListDiv").show();
		  $("#deleteStudy").show();
		} else {
			$("#studyListDiv").show();
			  $("#dataSetListDiv").hide();
			  $("#deleteStudy").hide();
		}
		
	} else if(selectTarget == 'dataList') {	
		$("#studyListDiv").show();
		$("#uploadDataFilesTab").hide();
		if(selectedValue && selectedValue != 'ANY') {
			var params1= {selectedPath:selectedValue,refreshNode:'true'};	
			invokeAjax('/browse/collection','GET',params1,contructDataListDiv,null,null,null);	
			if(action  == 'onChange') {
				var params2= {selectedPath:selectedValue};
				invokeAjax('/addbulk/canEdit','GET',params2,postSuccessCanEdit,null,null,null);
			} else {
				$("#addBulkDataFiles").show();
			
			}
			$("#deleteDataSet").show();
			
		} else {
			$("#dataListDiv").hide();
			$("#addBulkDataFiles").hide();
			$("#deleteDataSet").hide();
			
		}
		
	}
	
}

function clearRegisterDataDiv() {
	$('input[name=datafileTypeUpload]').prop('checked', false);
	$("#doeDataFile").val("");
	$("#newMetaDataTableForSingleFile tbody").html("");
	$("#singleFileDataUploadSection").hide();
	$("#bulkFileUploadSection").hide();
	$("#registerFileBtnsDiv").hide();			
	$("#registerBulkDataForm").hide();
	$("#displayGlobusUploadDiv").hide();
	$("#displayS3UploadDiv").hide();
	$("#folderNamesDiv").html("");
	$("#fileNamesDiv").html("");
	$("#globusEndPointInformation").html("");
}

function postSuccessCanEdit(data,status) {
	$("#addBulkDataFiles").show();
	if(data == false) {
		$("#addBulkDataFiles").prop("disabled",true);
		$("#addBulkDataFiles").prop("title","Insufficient permissions to add data.");
	} else {
			$("#addBulkDataFiles").prop("disabled",false);
		$("#addBulkDataFiles").prop("title","");
}
}

function contructDataListDiv(data,status) {
	$("#dataListing").html("");
	
	$.each(data, function(key, value) {	
		$("#dataListDiv").show();
		$("#dataListing").append('<li>'+value.value+'</li>');
	});
}

function constructNewCollectionMetaDataSet(data,status) {

	$("#newMetaDataTable tbody").html("");
	var parentAccessgrp = $("#registerCollectionModal").find("#parentAccessGroup").val();
	var assetType = $("#registerCollectionModal").find("#assetType").val();
	$.each(data, function(key, value) {	
		if(value.attrName  =='access_group') {
			
			if(!parentAccessgrp || (parentAccessgrp && parentAccessgrp == "public")) {	
		 	   $("#newMetaDataTable tbody").append('<tr><td>' + value.attrName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
        	   'data-placement="right" title="'+value.description+'"></i></td><td>'+
        	   '<select class="simple-select2" multiple="multiple" id="accessGroupSelect" name="zAttrStr_'+value.attrName+'"' +
        	   'style="width:70%;"></select> &nbsp;&nbsp;<input type="checkbox" id="publicAccess" checked="false" aria-label="public access" value="public access"/>&nbsp;&nbsp;Public</td></tr>');
		 	   loadJsonData('/metaDataPermissionsList', $("#registerCollectionModal").find("#accessGroupSelect"), false, null, null, null, "key", "value"); 
		
			} else {
				
				$("#newMetaDataTable tbody").append('<tr><td>' + value.attrName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
	        	'data-placement="right" title="'+value.description+'"></i></td><td>'+
	        	'<input type="text" placeholder="Required" aria-label="value of meta data" name="zAttrStr_'+value.attrName+'" value ="'+ parentAccessgrp+'"' +
	        	'disabled="disabled" style="width:70%;background-color: #dddddd;"><input type="hidden" name="zAttrStr_'+value.attrName+'" value ="'+ parentAccessgrp+'"/> &nbsp;&nbsp;<i class="fas fa-question-circle"><span>Access group inherited from parent.</span></i></td></tr>'); 
			}
	   } else if(value.attrName == 'asset_type') {
		   
		    $("#newMetaDataTable tbody").append('<tr><td>' + value.attrName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
       		'data-placement="right" title="'+value.description+'"></i></td><td>'+
       		'<input type="text" disabled="disabled" aria-label="value of meta data" value ="'+ assetType +'" name="zAttrStr_'+value.attrName+'"' +
       		'style="width:70%;"><input type="hidden" name="zAttrStr_'+value.attrName+'" value ="'+ assetType +'"/> </td></tr>');
	   
	   } else if(value.validValues != null){
		   
	    	$("#newMetaDataTable tbody").append('<tr><td>' + value.attrName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
        	'data-placement="right" title="'+value.description+'"></i></td><td>'+
        	'<select class="simple-select2" style="width:70%;" id="frameworkList" name="zAttrStr_'+value.attrName+'" value="'+value.attrValue+'"></select></td></tr>');
	    	
	    	  var $select = $("#registerCollectionModal").find("#frameworkList");
	    	  if(value.attrValue){
	    	 	$select.append($('<option></option>').attr('value', value.attrValue).text(value.attrValue));
	    	  } else {
	    	    $select.append($('<option></option>').attr('value', 'Select').text('Select'));
	    	  }
	    	  	    	  
	    	  for (var i = 0; i < value.validValues.length; i++) {
	    		   $select.append($('<option></option>').attr('value', value.validValues[i]).text(value.validValues[i]));
              }
               
	    	  $select.select2().trigger('change');
	    	
	   } else if(value.attrValue){		    
		 	$("#newMetaDataTable tbody").append('<tr><td>' + value.attrName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
        	'data-placement="right" title="'+value.description+'"></i></td><td>'+
        	'<input type="text" placeholder="Required" aria-label="value of meta data" value="'+value.attrValue+'" name="zAttrStr_'+value.attrName+'"' +
        	'style="width:70%;"></td></tr>');
	   } else {
			$("#newMetaDataTable tbody").append('<tr><td>' + value.attrName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
        	'data-placement="right" title="'+value.description+'"></i></td><td>'+
        	'<input type="text" placeholder="Required" aria-label="value of meta data" name="zAttrStr_'+value.attrName+'"' +
        	'style="width:70%;"></td></tr>');
		}      
		
	});	

	
}

function addNewMetaDataCollection(tableName) {
	var rowId =  $("#"+tableName + " tbody").length;
	rowId = rowId +1; 
	$("#"+tableName + " tbody").append('<tr id="addRow'+rowId+'"><td><input type="text" placeholder="Required" style="width:70%;" ' +
	'name="_addAttrName'+rowId+'" aria-label="add new row" id="_addAttrName'+rowId+'"></td><td><input type="text" placeholder="Required" style="width:70%;" id="_addAttrValue'+rowId+'" name="_addAttrValue'+rowId+'" >' +
	'&nbsp;&nbsp;<input class="btn btn-primary pull-right" type="button" value="X" onclick="removeCollectionRow(\'addRow' + rowId + '\')"></td></tr>');
	 
	
}

function addNewMetaDataRowsForDataFile($this) {
	var rowId =  $this.parent().find('div').length;
	rowId = rowId +1; 
	
	$this.parent().append('&nbsp;&nbsp;<div id="addDataRow'+rowId+'"><input type="text" style="width:40%;" ' +
	'name="_addAttrName'+rowId+'" aria-label="add new row" id="_addAttrName'+rowId+'">&nbsp;<input type="text" style="width:40%;" id="_addAttrValue'+rowId+'" name="_addAttrValue'+rowId+'" >' +
	'&nbsp;&nbsp;<input class="btn btn-primary pull-right" type="button" value="X" onclick="removeCollectionRow(\'addDataRow' + rowId + '\')"></div>');
	
}


function retrieveCollectionList(data,status) {	
	var assetType = $("#registerCollectionModal").find("#assetType").val();
	var collectionType;
	var parentAccessGrp;
	$.each(data, function (key, val) {
		if(val.key == "parentAccessGroup")
		   parentAccessGrp = val.value;
	});
	
	if(!collectionType) {
		collectionType = data[0].key;
    }
	
	 var parent = data[0].value;
	 $("#registerCollectionModal").find('#parentCollectionLabel').text(parent + " Collection Name");
	 $("#registerCollectionModal").find("#parentCollectionType").val(parent);
	 $("#registerCollectionModal").find("#parentAccessGroup").val(parentAccessGrp);
	 $("#registerCollectionModal").find("#collectionType").val(collectionType);
	 $("#registerCollectionModal").find("#registerCollectionBtn").val("Register " + collectionType);
	 $("#registerCollectionModal").find("#collectionMetaDataLabel").text(collectionType + " Metadata");
	 $("#registerCollectionModal").find("#registerModalTitle").html("Register " + collectionType + " Collection");
	 $("#registerCollectionModal").find("#addNewMetaData").html("<img src='images/Uploads.add.png' th:src='@{/images/Uploads.add.png}' class='uploadslogo' alt='add metadata'>&nbsp;Add " + collectionType + " Metadata");
	 $("#registerCollectionModal").modal('show');
	 var collectionPath = $("#registerCollectionModal").find("#collectionPath").val();
		
	 if(collectionType && collectionPath) {
		var params1= {selectedPath:collectionPath,collectionType:collectionType,assetType:assetType};
		invokeAjax('/addCollection','GET',params1,constructNewCollectionMetaDataSet,null,null,null);		
	} 	
} 




function openUploadModal(selectTarget) {
	
	var selectedIndexPathVal = $("#" + selectTarget).val();
	var parentName = $( "#" +selectTarget+ " option:selected" ).text();
	if(selectTarget == 'basePath') {
		$("#registerCollectionModal").find(".parentCollectionDiv").hide();
	} else {
		$("#registerCollectionModal").find(".parentCollectionDiv").show();
		$("#registerCollectionModal").find("#parentCollectionName").val(parentName);
	}
	$("#registerCollectionModal").find("#collectionPath").val(selectedIndexPathVal);
	$("#newMetaDataTable tbody").html("");
	$("#registerCollectionModal").find(".registerMsg").html("");
	$("#registerCollectionModal").find("#newMetaDataTable tbody").html("");
	$("#registerCollectionModal").find(".registerMsgBlock").hide();
	$("#registerCollectionModal").find(".registerMsgErrorBlock").hide();
	$("#registerCollectionModal").find(".registerErrorMsg").html("");
	var params= {parent:selectedIndexPathVal};
	invokeAjax('/addCollection/collectionTypes','GET',params,retrieveCollectionList,null,null,null);
	//loadJson for permissions list
	loadJsonData('/metaDataPermissionsList', $("#registerCollectionModal").find("#metaDataPermissionsList"), false, null, null, null, "key", "value"); 
}


function registerCollection() {
	
	$("#registerCollectionModal").find(".registerErrorMsg").html("");
	$("#registerCollectionModal").find(".registerMsgErrorBlock").hide();
	var collectionPath = $("#registerCollectionModal").find("#collectionPath").val();
	var collectionType = $("#registerCollectionModal").find("#collectionType").val();
	
	var newCollectionPath;
	var collectionName;
	var validate = true;
	var usermetaDataEntered = true;
	
	$('table#newMetaDataTable input[type="text"]').each(function(){
		var name = $(this).val();
	        if(!name){
	        	usermetaDataEntered = false;
	        } 
	        
	        
	        if(name && ($(this).attr('name') == ('zAttrStr_' + collectionType.toLowerCase() + '_' + 'identifier'))) {
	        	var modifiedName = name.replace(/ /g,"_");
	        	//collectionName = collectionType + "_" + modifiedName;
	        	collectionName = modifiedName;
	        }
		});
			$(".simple-select2").each(function(){
				var name = $(this).val();
				if(!name  || (name && name == 'Select')) {
	        	usermetaDataEntered = false;
	        }
			});
	
	if(!usermetaDataEntered) {
		validate = false;
		$("#registerCollectionModal").find(".registerErrorMsg").append("Enter values for all required metadata.");
		$("#registerCollectionModal").find(".registerMsgErrorBlock").show();
	}
	
	if(collectionPath && collectionName) {
		newCollectionPath = collectionPath + "/" + collectionName.trim();
		$("#registerCollectionModal").find("#newCollectionPath").val(newCollectionPath);
	}

	if(validate && newCollectionPath) {
		var data = $('#registerCollectionForm').serialize();
		$.ajax({
			type : "POST",
		     url : "/addCollection",
			 data : data,
			 beforeSend: function () {
		    	   $("#spinner").show();
		           $("#dimmer").show();
		       },
			 success : function(msg) {
				 $("#spinner").hide();
		         $("#dimmer").hide();
				 console.log('SUCCESS: ', msg);
				 postSuccessRegisterCollection(msg,collectionType);
				 
			 },
			error : function(e) {
				$("#spinner").hide();
		         $("#dimmer").hide();
				 console.log('ERROR: ', e);				 
			}
		});
	}
	
}

function postSuccessRegisterCollection(data,collectionType) {
	if(data.indexOf("Collection is created") != -1) {
		if(collectionType  == 'Program') {
			var params= {selectedPath:$("#basePath").val(),refreshNode:'true'};
			loadJsonData('/browse/collection', $("#instituteList"), true, params, displaySuccessMsg, null, "key", "value"); 
		} else if(collectionType == 'Study') {
			var params= {selectedPath:$("#instituteList").val(),refreshNode:'true'};
			loadJsonData('/browse/collection', $("#studyList"), true, params, displaySuccessMsg, null, "key", "value");
			
		} else if(collectionType == 'Asset') {
			var params= {selectedPath:$("#studyList").val(),refreshNode:'true'};
			 loadJsonData('/browse/collection', $("#dataList"), true, params, displaySuccessMsg, null, "key", "value");
		}
	} else {
		$("#registerCollectionModal").find(".registerErrorMsg").html("Error in create collection:" + data);
		$("#registerCollectionModal").find(".registerMsgErrorBlock").show();
	}

	
}


function displaySuccessMsg(data,status) {
	$("#registerCollectionModal").find(".registerMsg").html("Collection created successfully.");
	$("#registerCollectionModal").find(".registerMsgBlock").show();	
}

function openBulkDataRegistration() {
	var datafilePath = $("#dataList").val();
	$("#bulkDataFilePath").val(datafilePath);
	$("#bulkDataFilePathCollection").val(datafilePath);
	$(".registerBulkDataFileSuccess").hide();
	$(".registerBulkDataFile").html("");
	$(".uploadBulkDataError").hide();
	$(".uploadBulkDataErrorMsg").html("");
	clearRegisterDataDiv();
}

function cancelAndReturnToUploadTab() {
	var params= {selectedPath:$("#dataList").val(),refreshNode:'true'};
	 invokeAjax('/browse/collection','GET',params,contructDataListDiv,null,null,null);
}

function registerBulkDataFile() {
	var uploadType = $('input[name=datafileTypeUpload]:checked').val();
	var usermetaDataEntered = true;
	
	if(uploadType == 'singleData') {
		var file = $("#doeDataFile").val();
		var dataFilePath = $("#dataFilePath").val();
		
		$('table#newMetaDataTableForSingleFile input[type="text"]').each(function(){
			var name = $(this).val();
		        if(!name){
		        	usermetaDataEntered = false;
		        } 
			});
		
		if(!file || !dataFilePath) {
			$(".uploadBulkDataError").show();
			$(".uploadBulkDataErrorMsg").html("Upload data source file.")
		} else if(!usermetaDataEntered) {
			$(".uploadBulkDataError").show();
			$(".uploadBulkDataErrorMsg").html("Enter the values for all file metadata.");
		}
		else if(dataFilePath && file) {	
			$("#registerDataFileForm").attr('dataFilePath', dataFilePath);	 
				var form = $('#registerDataFileForm')[0];		 
		       var data = new FormData(form);
		      data.append('dataFilePath', dataFilePath);
				$.ajax({
					type : "POST",
					enctype: "multipart/form-data",
				     url : "/addDatafile",
					 data : data,
					 processData: false, 
		             contentType: false,
					 beforeSend: function () {
				    	   $("#spinner").show();
				           $("#dimmer").show();
				       },
					 success : function(msg) {
						 $("#spinner").hide();
				         $("#dimmer").hide();
				    	 if(msg && msg.indexOf("The system has registered your file") != -1) {
							 console.log('SUCCESS: ', msg);
							 $('body,html').animate({scrollTop: 0 }, 500);
							 $(".uploadBulkDataError").hide();
							 $(".uploadBulkDataErrorMsg").html("");
							 $(".registerBulkDataFile").html(msg);
							 $(".registerBulkDataFileSuccess").show();
							 $("#uploadDataFilesTab").hide();
							 clearRegisterDataDiv();
							 cancelAndReturnToUploadTab();
						 } else {
							 console.log('ERROR: ', msg);	
							 $(".uploadBulkDataError").show();
							 $(".uploadBulkDataErrorMsg").html(msg); 
						 }
						 
					 },
					error : function(e) {
						$("#spinner").hide();
				         $("#dimmer").hide();
						 console.log('ERROR: ', e);			
						 $(".uploadBulkDataError").show();
						 $(".uploadBulkDataErrorMsg").html("Error in Registration.");
					}
				});
			}
		
		
	} else {
		var dataFilePath = $("#bulkDataFilePathCollection").val();
		var bulkUploadType = $('input[name=datafileTypeUpload]:checked').val();
		var validate = true;
		$('form#registerBulkDataForm input[type="text"]').each(function(){
	        if(!$(this).val()){
	        	validate = false;
	        }          
	     });
		
		if(bulkUploadType == 's3' && !validate) {
			$(".uploadBulkDataError").show();
			$(".uploadBulkDataErrorMsg").html("Enter all the required fields.")
		} else if(bulkUploadType == 'globus' && !$("#globusEndPointInformation").length) {
			$(".uploadBulkDataError").show();
			$(".uploadBulkDataErrorMsg").html("Select globus end point information.")
		} else if(bulkUploadType == 'drive' && (!$("#fileNamesDiv").length || !$("#folderNamesDiv").length )) {
			$(".uploadBulkDataError").show();
			$(".uploadBulkDataErrorMsg").html("Select google drive information.")
		}
		else if(dataFilePath) {	
			$("#uploadType").val(bulkUploadType);
			$("#bulkDatafilePath").val(dataFilePath);
	        var data = $('#registerBulkDataForm').serialize();
			$.ajax({
				type : "POST",
			     url : "/addbulk",
				 data : data,
				 beforeSend: function () {
			    	   $("#spinner").show();
			           $("#dimmer").show();
			       },
				 success : function(msg) {
					 $("#spinner").hide();
			         $("#dimmer").hide();
			    	 if(msg && msg.indexOf("Your bulk data file registration request has the following task ID") != -1) {
			    		 console.log('SUCCESS: ', msg);
						 $('body,html').animate({scrollTop: 0 }, 500);
						 $(".uploadBulkDataError").hide();
						 $(".uploadBulkDataErrorMsg").html("");
						 $(".registerBulkDataFile").html(msg);
						 $(".registerBulkDataFileSuccess").show();
						 $("#uploadDataFilesTab").hide();
						 clearRegisterDataDiv();
						 cancelAndReturnToUploadTab();	
			    	 } else {
			    		 //console.log('ERROR: ', e);	
						 $(".uploadBulkDataError").show();
						 $(".uploadBulkDataErrorMsg").html(msg);
			    	 }
					 	 
				 },
				error : function(e) {
					$("#spinner").hide();
			         $("#dimmer").hide();
					 console.log('ERROR: ', e);	
					 $(".uploadBulkDataError").show();
					 $(".uploadBulkDataErrorMsg").html("Error in Registration.");
				}
			});
		}
	}

}

function appendFileName($this) {
    //Append the file name to the data file path
    var filename = $this.val().replace(/^C:\\fakepath\\/, "")
    var value = $("#bulkDataFilePathCollection").val() + "/" + filename;
    $("#dataFilePath").val(value);
    
}

function displayDataFileSection(value) {
	var datafilePath = $("#dataList").val();	
	$("#bulkDataFilePathCollection").val(datafilePath);
	$(".registerBulkDataFileSuccess").hide();
	$(".registerBulkDataFile").html("");
	$("#registerFileBtnsDiv").show();
	
	if(value == 'singleData') {
		$("#singleFileDataUploadSection").show();
		$("#bulkFileUploadSection").hide();					
	    $("#dataFilePath").val(datafilePath);	
		
	} else if(value == 'globus'){
		$("#singleFileDataUploadSection").hide();
		$("#bulkFileUploadSection").show();
		$("#registerBulkDataForm").show();		
			$("#displayGlobusUploadDiv").show();
			$("#displayS3UploadDiv").hide();	
			$("#fileNamesDiv").show();
			$("#folderNamesDiv").show();
			$("#displayDriveUploadDiv").hide();
	} else if(value == 's3'){
			$("#singleFileDataUploadSection").hide();
			$("#bulkFileUploadSection").show();	
			$("#registerBulkDataForm").show();
			$("#displayGlobusUploadDiv").hide();
			$("#displayS3UploadDiv").show();
			$("#fileNamesDiv").hide();
			$("#folderNamesDiv").hide();
			$("#displayDriveUploadDiv").hide();
		} else if(value == 'drive'){
			$("#singleFileDataUploadSection").hide();
			$("#bulkFileUploadSection").show();	
			$("#registerBulkDataForm").show();
			$("#displayGlobusUploadDiv").hide();
			$("#displayS3UploadDiv").hide();
			$("#fileNamesDiv").hide();
			$("#folderNamesDiv").hide();
			$("#displayDriveUploadDiv").show();
		}
	
}

function postUploadGlobusFunction(data,status) {
	location.replace(data);
}