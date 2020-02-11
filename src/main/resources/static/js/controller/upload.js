function  loadUploadTab() {	 
	 
		 loadJsonData('/browse', $("#instituteList"), true, null, null, null, "key", "value"); 
		
	 	 

}

function retrieveCollections($this, selectedIndex) {
	
	var selectTarget = $this.name;
	var params= {selectedPath:selectedIndex.value};
	
	if(selectTarget == 'instituteList') {
		
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
		$("#addDataFiles").prop('disabled',true);
		if(selectedIndex && selectedIndex.value != 'ANY') {
		  loadJsonData('/browse/collection', $("#dataList"), true, params, null, null, "key", "value");
		  $("#studyListDiv").show();
		  $("#dataSetListDiv").show();
		  $("#dataListDiv").hide();
		} else {
			$("#studyListDiv").show();
			  $("#dataSetListDiv").hide();
		}
		
	} else if(selectTarget == 'dataList') {	
		$("#studyListDiv").show();
		if(selectedIndex && selectedIndex.value != 'ANY') {
			invokeAjax('/browse/collection','GET',params,contructDataListDiv,null,null,null);		
			$("#addDataFiles").prop('disabled',false);
		} else {
			$("#dataListDiv").hide();
			$("#addDataFiles").prop('disabled',true);
		}
		
	}
	
}


function contructDataListDiv(data,status) {
	$("#dataListing").html("");
	$("#dataListDiv").show();
	$.each(data, function(key, value) {	
		$("#dataListing").append('<li>'+value.value+'</li>');
	});
}

function constructNewCollectionMetaDataSet(data,status) {
	$("#newMetaDataTable tbody").html("");
	$.each(data, function(key, value) {	
        $("#newMetaDataTable tbody").append('<tr><td>' + value.attrName + '</td><td><input type="text"  name="zAttrStr_'+value.attrName+'" style="width:70%;"></td></tr>');
	});
	
	
}

function addNewMetaDataCollection() {
	var rowId =  $("#newMetaDataTable tbody").length;
	rowId = rowId +1; 
	 $("#newMetaDataTable tbody").append('<tr id="addRow'+rowId+'"><td><input type="text" style="width:70%;" ' +
			 'name="_addAttrName'+rowId+'" id="_addAttrName'+rowId+'"></td><td><input type="text" style="width:70%;" id="_addAttrValue'+rowId+'" name="_addAttrValue'+rowId+'" >' +
	 		'&nbsp;&nbsp;<input class="btn btn-primary pull-right" type="button" value="X" onclick="removeCollectionRow(\'addRow' + rowId + '\')"></td></tr>');
	 
	
}


function retrieveCollectionList($this, selectedIndex) {
	
	var selectTarget = $this.name;
	var collectionPath = $("#registerCollectionModal").find("#collectionPath").val();
	
		if(selectedIndex && selectedIndex.value != 'ANY') {
			var params= {selectedPath:collectionPath,collectionType:selectedIndex.value};
			invokeAjax('/addCollection','GET',params,constructNewCollectionMetaDataSet,null,null,null);		
		} 		
} 


function openUploadModal(selectTarget) {
	
	var selectedIndexPathVal = $("#" + selectTarget).val();
	$("#registerCollectionModal").find("#collectionPath").val(selectedIndexPathVal);
	$("#newMetaDataTable tbody").html("");
	$("#registerCollectionModal").find(".registerMsg").html("");
	$("#registerCollectionModal").find(".registerMsgBlock").hide();
	var params= {parent:selectedIndexPathVal};		
	loadJsonData('/addCollection/collectionTypes', $("#registerCollectionModal").find("#collectionType"), true, params, null, null, "key", "value");
	$("#registerCollectionModal").modal('show');
	
}


function registerCollection() {
	
	var collectionPath = $("#registerCollectionModal").find("#collectionPath").val();

	var collectionName = $("#registerCollectionModal").find("#collectionName").val();
	
	var newCollectionPath;
	if(collectionPath && collectionName) {
		newCollectionPath = collectionPath + "/" + collectionName.trim();
		$("#registerCollectionModal").find("#newCollectionPath").val(newCollectionPath);
	}

	if(newCollectionPath) {
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
				 postSuccessRegisterCollection(msg);
				 
			 },
			error : function(e) {
				 console.log('ERROR: ', e);				 
			}
		});
	}
	
}

function postSuccessRegisterCollection(data) {
	$("#registerCollectionModal").find(".registerMsg").html(data);
	$("#registerCollectionModal").find(".registerMsgBlock").show();
}


function openRegisterDataFileModal() {
	var datafilePath = $("#dataList").val();
	$("#registerDataFileModal").find("#dataFilePath").val(datafilePath);
	$("#registerDataFileModal").find("#dataFilePathCollection").val(datafilePath);
	$("#registerDataFileModal").find(".registerDataFile").html("");
	$("#registerDataFileModal").find(".registerDataFileSuccess").hide();
}


function openBulkDataRegistration() {
	var datafilePath = $("#dataList").val();
	$("#bulkDataFilePath").val(datafilePath);
	$("#bulkDataFilePathCollection").val(datafilePath);
	$(".registerBulkDataFileSuccess").hide();
	$(".registerBulkDataFile").html("");
}

function registerDataFile() {
	var dataFilePath = $("#registerDataFileModal").find("#dataFilePath").val();
	
	var file = $("#doeDataFile").val();
	
	if(dataFilePath && file) {	
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
				 $("#registerDataFileModal").find(".registerDataFile").html(msg);
				 $("#registerDataFileModal").find(".registerDataFileSuccess").show();
				
				 
			 },
			error : function(e) {
				 console.log('ERROR: ', e);				 
			}
		});
	}
}


function registerBulkDataFile() {
	var dataFilePath = $("#bulkDataFilePath").val();
	
	var file = $("#bulkDoeDataFile").val();
	
	if(dataFilePath && file) {	
	$("#registerBulkDataForm").attr('dataFilePath', dataFilePath);	 
		var form = $('#registerBulkDataForm')[0];		 
       var data = new FormData(form);
      data.append('dataFilePath', dataFilePath);
		$.ajax({
			type : "POST",
			enctype: "multipart/form-data",
		     url : "/addbulk",
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
				
				 
			 },
			error : function(e) {
				 console.log('ERROR: ', e);				 
			}
		});
	}
}

function appendFileName($this) {
    //Append the file name to the data file path
    var filename = $this.val().replace(/^C:\\fakepath\\/, "")
    var value = $("#registerDataFileModal").find("#dataFilePathCollection").val() + "/" + filename;
    $("#registerDataFileModal").find("#dataFilePath").val(value);
    
}

function appendBulkFileName($this) {
	//Append the file name to the data file path
    var filename = $this.val().replace(/^C:\\fakepath\\/, "")
    var value = $("#bulkDataFilePathCollection").val() + "/" + filename;
    $("#bulkDataFilePath").val(value);
}