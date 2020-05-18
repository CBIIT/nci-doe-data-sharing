
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
				$('input[name=bulkUploadType]:checked').val();
				$("#datafileTypeBulk").prop("checked", true);
				$("#uploadTypeGlobus").prop("checked", true);
				$("#singleFileDataUploadSection").hide();
				$("#bulkFileUploadSection").show();
				$("#registerFileBtnsDiv").show();		
				$(".registerBulkDataFileSuccess").hide();
				$(".registerBulkDataFile").html("");
				$("#registerBulkDataForm").show();
				$("#displayGlobusUploadDiv").show();
				$("#displayS3UploadDiv").hide();
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


function retrieveCollections($this, selectedIndex) {
	
	var selectTarget = $this.name;
	var params= {selectedPath:selectedIndex.value};
	
	if(selectTarget == 'instituteList') {
		$("#uploadDataFilesTab").hide();
		$("#addBulkDataFiles").hide();
		if(selectedIndex && selectedIndex.value != 'ANY') {
			loadJsonData('/browse/collection', $("#studyList"), true, params, null, null, "key", "value");
			$("#studyListDiv").show();
			$("#dataSetListDiv").hide();
			$("#dataListDiv").hide();
		} else {
			$("#studyListDiv").hide();
			$("#dataSetListDiv").hide();
			$("#dataListDiv").hide();
		}
		
		
	} else if(selectTarget == 'studyList') {
		$("#addBulkDataFiles").hide();
		$("#uploadDataFilesTab").hide();
		 $("#dataListDiv").hide();
		if(selectedIndex && selectedIndex.value != 'ANY') {
			var params1= {selectedPath:selectedIndex.value,refreshNode:'true'};			
		  loadJsonData('/browse/collection', $("#dataList"), true, params1, null, null, "key", "value");
		  $("#studyListDiv").show();
		  $("#dataSetListDiv").show();
		} else {
			$("#studyListDiv").show();
			  $("#dataSetListDiv").hide();
		}
		
	} else if(selectTarget == 'dataList') {	
		$("#studyListDiv").show();
		$("#uploadDataFilesTab").hide();
		if(selectedIndex && selectedIndex.value != 'ANY') {
			invokeAjax('/browse/collection','GET',params,contructDataListDiv,null,null,null);		
			$("#addBulkDataFiles").show();
		} else {
			$("#dataListDiv").hide();
			$("#addBulkDataFiles").hide()
			
		}
		
	}
	
}

function clearRegisterDataDiv() {
	$('input[name=datafileTypeUpload]').prop('checked', false);
	$('input[name=bulkUploadType]').prop('checked', false);
	$("#singleFileDataUploadSection").hide();
	$("#bulkFileUploadSection").hide();
	$("#registerFileBtnsDiv").hide();			
	$("#registerBulkDataForm").hide();
	$("#displayGlobusUploadDiv").hide();
	$("#displayS3UploadDiv").hide();
	$("#selectedFoldersDiv").html("");
	$("#selectedFilesDiv").html("");
	$("#globusEndPointInformation").html("");
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
	var parentAccessgrp = $("#parentAccessGroup").val();
	$.each(data, function(key, value) {	
		if(value.attrName  =='access_group') {
			if(parentAccessgrp && parentAccessgrp == "public") {	
		 	$("#newMetaDataTable tbody").append('<tr><td>' + value.attrName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
        			'data-placement="right" title="'+value.description+'"></i></td><td>'+
        			'<select class="simple-select2" multiple="multiple" id="accessGroupSelect" name="zAttrStr_'+value.attrName+'"' +
        			'style="width:70%;"></select> &nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
        			'data-placement="right" title="Leave this field empty for public access."></i></td></tr>');
		 	loadJsonData('/metaDataPermissionsList', $("#registerCollectionModal").find("#accessGroupSelect"), false, null, null, null, "key", "value"); 
		
			} else {
				$("#newMetaDataTable tbody").append('<tr><td>' + value.attrName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
	        			'data-placement="right" title="'+value.description+'"></i></td><td>'+
	        			'<input type="text" placeholder="Required" name="zAttrStr_'+value.attrName+'" value ="'+ parentAccessgrp+'"' +
	        			"disabled='disabled' style='width:70%;'> &nbsp;&nbsp;<i class='fas fa-question-circle' data-toggle='tooltip'"+
	        			'data-placement="right" title="Access group inherited from parent."></i></td></tr>'); 
			}
	   } else {
		 	$("#newMetaDataTable tbody").append('<tr><td>' + value.attrName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
        			'data-placement="right" title="'+value.description+'"></i></td><td>'+
        			'<input type="text" placeholder="Required" name="zAttrStr_'+value.attrName+'"' +
        			'style="width:70%;"></td></tr>');
		}       
	});	
	
	
}

function addNewMetaDataCollection(tableName) {
	var rowId =  $("#"+tableName + " tbody").length;
	rowId = rowId +1; 
	$("#"+tableName + " tbody").append('<tr id="addRow'+rowId+'"><td><input type="text" placeholder="Required" style="width:70%;" ' +
			 'name="_addAttrName'+rowId+'" id="_addAttrName'+rowId+'"></td><td><input type="text" placeholder="Required" style="width:70%;" id="_addAttrValue'+rowId+'" name="_addAttrValue'+rowId+'" >' +
	 		'&nbsp;&nbsp;<input class="btn btn-primary pull-right" type="button" value="X" onclick="removeCollectionRow(\'addRow' + rowId + '\')"></td></tr>');
	 
	
}

function addNewMetaDataRowsForDataFile($this) {
	var rowId =  $this.parent().find('div').length;
	rowId = rowId +1; 
	
	$this.parent().append('&nbsp;&nbsp;<div id="addDataRow'+rowId+'"><input type="text" style="width:40%;" ' +
			 'name="_addAttrName'+rowId+'" id="_addAttrName'+rowId+'">&nbsp;<input type="text" style="width:40%;" id="_addAttrValue'+rowId+'" name="_addAttrValue'+rowId+'" >' +
	 		'&nbsp;&nbsp;<input class="btn btn-primary pull-right" type="button" value="X" onclick="removeCollectionRow(\'addDataRow' + rowId + '\')"></div>');
	
}


function retrieveCollectionList(data,status) {	
	 var collectionType = data[0].key;
	 var parent = data[0].value;
	 $("#registerCollectionModal").find('label[for="parentCollectionName"]').text(parent + " Collection Name");
	 $("#registerCollectionModal").find("#parentCollectionType").val(parent);
	 $("#registerCollectionModal").find("#collectionType").val(collectionType);
	 $("#registerCollectionModal").find("#registerCollectionBtn").val("Register " + collectionType);
	 $("#registerCollectionModal").find("#collectionMetaDataLabel").text(collectionType + " MetaData");
	 $("#registerCollectionModal").find("#registerModalTitle").html("Register " + collectionType + " Collection");
	 $("#registerCollectionModal").find("#addNewMetaData").html("Add " + collectionType + " MetaData");
	 $("#registerCollectionModal").modal('show');
	 var collectionPath = $("#registerCollectionModal").find("#collectionPath").val();
		
	 if(collectionType && collectionPath) {
		var params1= {selectedPath:collectionPath,collectionType:collectionType};
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
	        if(name && ($(this).attr('name') == ('zAttrStr_' + collectionType.toLowerCase() + '_' + 'name'))) {
	        	var modifiedName = name.replace(/ /g,"_");
	        	collectionName = collectionType + "_" + modifiedName;
	        }
		});
	
	if(!usermetaDataEntered) {
		validate = false;
		$("#registerCollectionModal").find(".registerErrorMsg").append("Enter the values for all collection MetaData.");
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
				 console.log('ERROR: ', e);				 
			}
		});
	}
	
}

function postSuccessRegisterCollection(data,collectionType) {
	if(data.indexOf("Collection is created") != -1) {
		
		$("#registerCollectionModal").find(".registerMsg").html("Collection created successfully.");
		$("#registerCollectionModal").find(".registerMsgBlock").show();		
	} else {
		$("#registerCollectionModal").find(".registerErrorMsg").html("Error in create collection.");
		$("#registerCollectionModal").find(".registerMsgErrorBlock").show();
	}

	if(collectionType  == 'Program') {
		var params= {selectedPath:'/DOE_TEST_Archive',refreshNode:'true'};
		loadJsonData('/browse/collection', $("#instituteList"), true, params, null, null, "key", "value"); 
	} else if(collectionType == 'Study') {
		var params= {selectedPath:$("#instituteList").val(),refreshNode:'true'};
		loadJsonData('/browse/collection', $("#studyList"), true, params, null, null, "key", "value");
		
	} else if(collectionType == 'Data_Set') {
		var params= {selectedPath:$("#studyList").val(),refreshNode:'true'};
		 loadJsonData('/browse/collection', $("#dataList"), true, params, null, null, "key", "value");
	}
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
	
	if(uploadType == 'singleData') {
		var file = $("#doeDataFile").val();
		var dataFilePath = $("#dataFilePath").val();
		
		if(!file || !dataFilePath) {
			$(".uploadBulkDataError").show();
			$(".uploadBulkDataErrorMsg").html("upload data source file.")
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
						 console.log('SUCCESS: ', msg);
						 $(".registerBulkDataFile").html(msg);
						 $(".registerBulkDataFileSuccess").show();
						 $('body,html').animate({scrollTop: 0 }, 500); 
						 $("#uploadDataFilesTab").hide();
						 clearRegisterDataDiv();
						 cancelAndReturnToUploadTab();
						 
					 },
					error : function(e) {
						 console.log('ERROR: ', e);				 
					}
				});
			}
		
		
	} else {
		var dataFilePath = $("#bulkDataFilePathCollection").val();
		var bulkUploadType = $('input[name=bulkUploadType]:checked').val();
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
			$(".uploadBulkDataErrorMsg").html("Select Globus End point information.")
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
					 console.log('SUCCESS: ', msg);
					 $('body,html').animate({scrollTop: 0 }, 500);
					 $(".uploadBulkDataError").hide();
					 $(".uploadBulkDataErrorMsg").html("");
					 $(".registerBulkDataFile").html(msg);
					 $(".registerBulkDataFileSuccess").show();
					 $("#uploadDataFilesTab").hide();
					 clearRegisterDataDiv();
					 cancelAndReturnToUploadTab();		 
				 },
				error : function(e) {
					 console.log('ERROR: ', e);				 
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
	
	if(value == 'singleData') {
		$("#singleFileDataUploadSection").show();
		$("#bulkFileUploadSection").hide();					
	    $("#dataFilePath").val(datafilePath);	
	    $("#registerFileBtnsDiv").show();
		
	} else if(value == 'bulkData'){
		$("#singleFileDataUploadSection").hide();
		$("#bulkFileUploadSection").show();
		$("#registerFileBtnsDiv").hide();
		
		//hide the globus and s3 radio buttons and  dic
		$('input[name=bulkUploadType]').prop('checked', false);
		$("#registerFileBtnsDiv").hide();			
		$("#registerBulkDataForm").hide();
	}
}


function displayUploadTypeDiv(value){
	$("#registerFileBtnsDiv").show();
	$(".registerBulkDataFileSuccess").hide();
	$(".registerBulkDataFile").html("");
	$("#registerBulkDataForm").show();
	if(value == 'globus') {
		$("#displayGlobusUploadDiv").show();
		$("#displayS3UploadDiv").hide();								
		
	} else if(value == 's3'){
		$("#displayGlobusUploadDiv").hide();
		$("#displayS3UploadDiv").show();
	}
}


function postUploadGlobusFunction(data,status) {
	location.replace(data);
}