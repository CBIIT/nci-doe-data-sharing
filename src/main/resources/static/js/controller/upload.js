$(document).ready(function () {
	$("#landing-tab").removeClass('active');
	$("#upload-tab").addClass('active');
	loadUploadTab();
	
	$('body').tooltip({
		selector : '[data-toggle="tooltip"]'
	});
	
	$("#registerBulkDataFileBtn").click(function(e) {
		registerBulkDataFile();
	});
	
	$("#addBulkDataFiles").click(function(e) {
		$("#uploadDataFilesTab").show();
		openBulkDataRegistration();
	});

	$("#primaryGlobusButton").click(function(e) {

		var d = {};
		d.institutionPath = $("#instituteList").val();
		d.studyPath = $("#studyList").val();
		d.dataSetPath = $("#dataList").val();
		d.uploadPath = $("#bulkDataFilePathCollection").val();

		invokeAjax('/upload', 'GET', d, postUploadGlobusFunction, postFailureFunction, null, 'text');
	});
	
	$("#driveUploadAuthlink").click(function(e) {
		var d = {};
		d.institutionPath = $("#instituteList").val();
		d.studyPath = $("#studyList").val();
		d.dataSetPath = $("#dataList").val();
		d.uploadPath = $("#bulkDataFilePathCollection").val();
		d.action = "Drive";
		invokeAjax('/upload', 'GET', d, postUploadGlobusFunction, postFailureFunction, null, 'text');
	});
	
	$("#assetSelectionGlobusButton").click(function(e) {
		var d = {};
		d.institutionPath = $("#instituteList").val();
		d.studyPath = $("#studyList").val();
		d.uploadType = "assetBulkUpload";

		invokeAjax('/upload', 'GET', d, postUploadGlobusFunction, postFailureFunction, null, 'text');
	});

	$(document).on('click', '.uploadDataSet', function() {
		var folderPath = $(this).parent().find('a').attr('data-name');
		$("#uploadSectionDiv").hide();
		$("#uploadHeader").hide();
		$("#uploadDataFilesTab").show();
		openBulkDataRegistration(folderPath);
	});

	
	$("#addBulkDataFiles").click(function(e) {
		$("#uploadSectionDiv").hide();
		$("#uploadHeader").hide();
		$("#uploadDataFilesTab").show();
		openBulkDataRegistration();
	});
	
	$(".backToUploadTab").click(function(e){	
		$("#createCollectionFragment").hide();
		$("#uploadSectionDiv").show();
		$("#uploadHeader").show();
		$("#uploadDataFilesTab").hide();
		resetSelectionsForBackToUploadTab();
	});
	
	$("#btnSelectAssetType").click(function(e) {
		var assetType = $("#createAssetModal").find("#createAssetCollectionType option:selected").val();
		$("#assetType").val(assetType);
		$("#createCollectionFragment").show();
		$("#uploadSectionDiv").hide();
		$("#uploadHeader").hide();
		createCollectionDiv('studyList');
	});
	
	$(document).on('change','#publicAccess',function() {
	    if ($(this).is(":checked")) {
			$("#accessGroupSelect").next(".select2-container").hide();
		} else {
			$("#accessGroupSelect").next(".select2-container").show();
		}

	});
});

function loadUploadTab() {	 
	 
	var ins = $("#institutePath").val();
	var stu= $("#studyPath").val();
	var data = $("#datafilePath").val();
	var bulkUploadCollection = $("#bulkUploadCollection").val();
	var uploadPath = $("#uploadPath").val();
	
	if(ins){
		$("input[name=selectProgram][value='Select Program']").prop("checked",true);
		showSelect('Program','true');
		$("#studyListDiv").show();
		$("#deleteProgram").show();
		if(stu) {
			$("input[name=selectStudy][value='Select Study']").prop("checked",true);
			showSelect('Study',ins);
			$("#dataSetListDiv").show();
			$("#deleteStudy").show();
			$("#addAsets").show();
			if (bulkUploadCollection) {
				$("#addAsets").click();
				$("input[name=selectAsset][value='Register Asset']").click();

			}
			if (data) {
				if (!bulkUploadCollection) {
					showSelect('Asset',stu);
				}
				$("input[name=selectAsset][value='Select Asset']").prop("checked",true);
				$("#deleteDataSet").show();
				constructAssetFileAndFoldersDiv(data);
			    $("#uploadAndRegisterFiles").show();
				$("#uploadDataFilesTab").show();
				$('input[name=datafileTypeUpload]:checked').val();
				$("#bulkFileUploadSection").show();
				$("#registerFileBtnsDiv").show();	
				$("#bulkDataFilePathCollection").val(uploadPath);
				$(".registerBulkDataFileSuccess").hide();
				$(".registerBulkDataFile").html("");
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
		
		if(!bulkUploadCollection && $("#showUploadDataFilesTab").val() == 'true') {
			$("#uploadSectionDiv").hide();
			$("#uploadHeader").hide();
			$("#uploadDataFilesTab").show();
			$("#uploadCollectionPath").val(uploadPath);
		}
	} 
  
}

function retrieveCollections($this, selectedIndex, action) {

	$("#assetUploadDiv").removeClass('show');
	var selectTarget = $this;
	var params;
	var selectedValue;
	if (action == 'onChange') {
		selectedValue = selectedIndex.value;
		params = {
			selectedPath : selectedIndex.value
		};
		if (selectedIndex && selectedIndex.value != 'ANY') {
			$("#" + selectTarget + " option[value='ANY']").remove();
		}
	} else {
		selectedValue = selectedIndex;
		params = {
			selectedPath : selectedIndex
		};
	}
	resetOnChangeofSelectCollection(selectTarget, selectedValue);
}

function clearRegisterDataDiv() {
	$('input[name=datafileTypeUpload]').prop('checked', false);
	$("#doeDataFile").val("");
	$("#newMetaDataTableForSingleFile tbody").html("");
	$("#singleFileDataUploadSection").hide();
	$("#bulkFileUploadSection").hide();
	$("#registerFileBtnsDiv").hide();
	$("#displayGlobusUploadDiv").hide();
	$("#displayS3UploadDiv").hide();
	$("#folderNamesDiv").html("");
	$("#fileNamesDiv").html("");
	$("#globusEndPointInformation").html("");
}

function postSuccessCanEdit(data) {
	$("#assetPermissions").val(data);
	if(data == false) {
		$("#registerFolder").css('pointer-events','none');
		$("#registerFolder").parent().prop("title","Insufficient permissions to register subcollection.");
		$("#addBulkDataFiles").css('pointer-events','none');
		$("#addBulkDataFiles").parent().prop("title","Insufficient permissions to add data.");
	} else {
		$("#registerFolder").css('pointer-events','all');
		$("#registerFolder").parent().prop("title","");
		$("#addBulkDataFiles").css('pointer-events','all');
		$("#addBulkDataFiles").parent().prop("title","");
    }
}

function constructAssetFileAndFoldersDiv(assetPath) {
	$.ajax({
		"url": "/addbulk/canEdit",
		"type": "GET",
		data: {selectedPath:assetPath},
		beforeSend: function() {
			$("#spinner").show();
			$("#dimmer").show();
		},
		success: function(msg) {
		   $("#spinner").hide();
		   $("#dimmer").hide();
		   postSuccessCanEdit(msg);
		},
		error: function(e) {
			console.log('ERROR: ', e);
			$("#spinner").hide();
			$("#dimmer").hide();								
		}
	}).done(function(e) {	
		invokeAjax('/getDataObjects','GET',{path:assetPath},contructDataListDiv,null,null,null);
		});
}

function contructDataListDiv(data,status) {
	$("#dataListing").html("");
	var assetPermissions = $("#assetPermissions").val();
	if(data.length == 0) {
		$("#dataListDiv").hide();
	}
	$.each(data, function(key, value) {	
		$("#dataListDiv").show();
		var html = "";
		if(value.isFolder == false){
			html += '<li>'+value.name+'</ol>';
		} else  {
			var datalist = "dataList";
			if(assetPermissions && assetPermissions == 'true') {
				html += '<li><a class="detail-control" data-name = '+value.name+'>'+
				'<i class="expand far fa-folder"></i></a>&nbsp;'+value.name+' &nbsp;&nbsp;'+
				'<a href="#" class="uploadDataSet" title="Upload Files to Asset Subcollection" style="font-size: 15px;color: #F39530;"><i class="fas fa-upload">'+
				'</i></a> &nbsp;&nbsp;<a href="#" title="Register Subcollection" onclick="createCollectionDiv(\'' + datalist + '\',\''+value.name+'\')" class="addDeleteUploadLabels">'+        
	            '<img src="/images/Uploads.add.png" class="uploadslogo" alt="register"></a></li>';
			} else {
				html += '<li><a class="detail-control" data-name = '+value.name+'>'+
				'<i class="expand far fa-folder"></i></a>&nbsp;'+value.name+' &nbsp;&nbsp;'+
				'</li>';
			}
			
		}
		$("#dataListing").append(html);
	});
}

$('#dataListing').on('click','a.detail-control',function() {
	    var $this = $(this);
	    var assetPermissions = $("#assetPermissions").val();
	    var $lithis = $(this).closest('li');
	         if ($this.hasClass('shown')) {
	        	 $lithis.next('ul').remove();
	        	 $this.removeClass('shown');
	        	 $this.find("i.expand.far").toggleClass('fa-folder fa-folder-open');
				} else {
						var name = $this.attr('data-name');
						var params ={path:$("#dataList").val()+"/"+name};						
						$.ajax({
							"url": "/getDataObjects",
							"type": "GET",
							data: params,
							beforeSend: function() {
								$("#spinner").show();
								$("#dimmer").show();
							},
							success: function(msg) {
							   $("#spinner").hide();
							   $("#dimmer").hide();
							   var html="<ul>";
							   $.each(msg, function(key, value) {
								   if(value.isFolder == false){
										html += '<li>'+value.name+'</ol>';
									} else  {
										if(assetPermissions && assetPermissions == 'true') {
											html += '<li><a class="detail-control" data-name = '+name+"/"+value.name+'>'+
											'<i class="expand far fa-folder"></i></a>&nbsp;'+value.name+' &nbsp;&nbsp;'+
											'<a href="#" class="uploadDataSet"  title="Upload Files to Asset Subcollection" style="font-size: 15px;color: #F39530;">'+
											'<i class="fas fa-upload"></i></a></li>';
										} else {
											html += '<li><a class="detail-control" data-name = '+name+"/"+value.name+'>'+
											'<i class="expand far fa-folder"></i></a>&nbsp;'+value.name+' &nbsp;&nbsp;'+
											'</li>';
										}
										
									}
							   });
							   html += "</ul>";
							   $lithis.after(html);
							   $this.find("i.expand.far").toggleClass('fa-folder fa-folder-open');
							   $this.addClass('shown');
							},
							error: function(e) {
								console.log('ERROR: ', e);
								$("#spinner").hide();
								$("#dimmer").hide();								
							}
						});												
					}
});

function constructNewCollectionMetaDataSet(data,status) {

	$("#newMetaDataTable tbody").html("");
	var parentAccessgrp = $("#parentAccessGroup").val();
	var assetType = $("#assetType").val();
	$.each(data, function(key, value) {	
		if(value.attrName  =='access_group') {
			
			if(!parentAccessgrp || (parentAccessgrp && parentAccessgrp == "public")) {	
		 	   $("#newMetaDataTable tbody").append('<tr><td>' +  value.displayName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
        	   'data-placement="right" title="'+value.description+'"></i></td><td>'+
        	   '<select class="simple-select2" multiple="multiple" id="accessGroupSelect" name="zAttrStr_'+value.attrName+'"' +
        	   'style="width:70%;"></select> &nbsp;&nbsp;<input type="checkbox" id="publicAccess" checked="false" aria-label="public access" value="public access"/>&nbsp;&nbsp;Public</td></tr>');
		 	   loadJsonData('/metaDataPermissionsList', $("#accessGroupSelect"), false, null, null, null, "key", "value"); 
		
			} else {
				
				$("#newMetaDataTable tbody").append('<tr><td>' +  value.displayName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
	        	'data-placement="right" title="'+value.description+'"></i></td><td>'+
	        	'<input type="text" placeholder="Required" aria-label="value of meta data" name="zAttrStr_'+value.attrName+'" value ="'+ parentAccessgrp+'"' +
	        	'disabled="disabled" style="width:70%;background-color: #dddddd;"><input type="hidden" name="zAttrStr_'+value.attrName+'" value ="'+ parentAccessgrp+'"/> &nbsp;&nbsp;<i class="fas fa-question-circle"><span>Access group inherited from parent.</span></i></td></tr>'); 
			}
	   } else if(value.attrName == 'asset_type') {
		   
		    $("#newMetaDataTable tbody").append('<tr><td>' +  value.displayName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
       		'data-placement="right" title="'+value.description+'"></i></td><td>'+
       		'<input type="text" disabled="disabled" aria-label="value of meta data" value ="'+ assetType +'" name="zAttrStr_'+value.attrName+'"' +
       		'style="width:70%;"><input type="hidden" name="zAttrStr_'+value.attrName+'" value ="'+ assetType +'"/> </td></tr>');
	   
	   } else if(value.validValues != null){
		   
	    	$("#newMetaDataTable tbody").append('<tr><td>' +  value.displayName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
        	'data-placement="right" title="'+value.description+'"></i></td><td>'+
        	'<select class="simple-select2" is_mandatory="'+value.mandatory+'" style="width:70%;" id="'+value.attrName+'" name="zAttrStr_'+value.attrName+'" value="'+value.attrValue+'"></select></td></tr>');
	    	
	    	  var $select = $("#"+value.attrName);
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
		 	$("#newMetaDataTable tbody").append('<tr><td>' +  value.displayName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
        	'data-placement="right" title="'+value.description+'"></i></td><td>'+
        	'<input type="text" is_mandatory="'+value.mandatory+'" placeholder="Required" aria-label="value of meta data" value="'+value.attrValue+'" name="zAttrStr_'+value.attrName+'"' +
        	'style="width:70%;"></td></tr>');
	   } else {
		   var placeholderValue ="";
		     if(value.mandatory && value.mandatory == true) {
		    	 placeholderValue = "Required";
		     }
		     if(value.attrName.indexOf("description") != -1) {
		    	 $("#newMetaDataTable tbody").append('<tr><td>' +  value.displayName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
		    	        	'data-placement="right" title="'+value.description+'"></i></td><td>'+
		    	        	'<textarea rows="5" is_mandatory="'+value.mandatory+'" placeholder="'+placeholderValue+'" aria-label="value of meta data" name="zAttrStr_'+value.attrName+'"' +
		    	        	'style="width:70%;"></textarea></td></tr>');
		     } else {
		    	 $("#newMetaDataTable tbody").append('<tr><td>' +  value.displayName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
		    	        	'data-placement="right" title="'+value.description+'"></i></td><td>'+
		    	        	'<input type="text" is_mandatory="'+value.mandatory+'" placeholder="'+placeholderValue+'" aria-label="value of meta data" name="zAttrStr_'+value.attrName+'"' +
		    	        	'style="width:70%;"></td></tr>'); 
		     }
			
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
	var assetType = $("#assetType").val();
	var collectionType;
	var parentAccessGrp;
	var displayCollectionType;
	$.each(data, function (key, val) {
		if(val.key == "parentAccessGroup")
		   parentAccessGrp = val.value;
	});
	
	if(!collectionType) {
		collectionType = data[0].key;
    }
	
	 var parent = data[0].value;
	 if(parent) {
		 $('#parentCollectionLabel').text(parent.toUpperCase() + " COLLECTION NAME"); 
	 }
	 
	 if(collectionType  ==  'Folder') {
		 displayCollectionType ='Asset Subcollection'; 
	 } else {
		 displayCollectionType = collectionType;
	 }
	 $("#parentCollectionType").val(parent);
	 $("#parentAccessGroup").val(parentAccessGrp);
	 $("#collectionType").val(collectionType);
	 $("#registerCollectionBtn").html("Register " + displayCollectionType);
	 $("#collectionMetaDataLabel").text(displayCollectionType + " Metadata");
	 $("#registerModalTitle").html("Register " + displayCollectionType);
	 $("#addNewMetaData").html("<img src='images/Uploads.add.png' th:src='@{/images/Uploads.add.png}' class='uploadslogo' alt='add metadata'>&nbsp;Add Metadata");
	 var collectionPath = $("#collectionPath").val();
		
	 if(collectionType && collectionPath) {
		var params1= {selectedPath:collectionPath,collectionType:collectionType,controllerValue:assetType,controllerAttribute:'asset_type'};
		invokeAjax('/addCollection','GET',params1,constructNewCollectionMetaDataSet,null,null,null);		
	} 	
} 

function createCollectionDiv(selectTarget, folderPath) {

	var selectedIndexPathVal = $("#" + selectTarget).val();	
	var parentName = $( "#" +selectTarget+ " option:selected" ).text();
	if(folderPath) {
		selectedIndexPathVal += "/" + folderPath;
		parentName = folderPath;
	}
	if(selectTarget == 'basePath') {
		$(".parentCollectionDiv").hide();
	} else {
		$(".parentCollectionDiv").show();
		$("#parentCollectionName").val(parentName);
	}
	if(selectTarget == 'dataList') {
		$(".permissionsLabel").hide();
		$("#parentCollectionNameDiv").css('margin-left','0px');
	} else {
		$(".permissionsLabel").show();
		$("#parentCollectionNameDiv").css('margin-left','-125px');
	}
	$("#collectionPath").val(selectedIndexPathVal);
	$("#newMetaDataTable tbody").html("");
	$(".registerMsg").html("");
	$("#newMetaDataTable tbody").html("");
	$(".registerMsgBlock").hide();
	$(".registerMsgErrorBlock").hide();
	$(".registerErrorMsg").html("");
	var params= {parent:selectedIndexPathVal};
	invokeAjax('/addCollection/collectionTypes','GET',params,retrieveCollectionList,null,null,null);
	// loadJson for permissions list
	loadJsonData('/metaDataPermissionsList', $("#metaDataPermissionsList"), false, null, null, null, "key", "value"); 
	
	// show create fragment
	$("#createCollectionFragment").show();
	$("#uploadSectionDiv").hide();
	$("#uploadHeader").hide();
	
}

function registerCollection() {
	
	$(".registerErrorMsg").html("");
	$(".registerMsgErrorBlock").hide();
	var collectionPath = $("#collectionPath").val();
	var collectionType = $("#collectionType").val();
	
	var newCollectionPath;
	var collectionName;
	var validate = true;
	var usermetaDataEntered = true;
	
	$('table#newMetaDataTable input[type="text"]').each(function(){
		var name = $(this).val();
		var ismandatory = $(this).attr('is_mandatory');
	    if(!name && ismandatory && ismandatory != "false" ){
	        	usermetaDataEntered = false;
         }  
	        
	        
	        if(name && ($(this).attr('name') == ('zAttrStr_' + collectionType.toLowerCase() + '_' + 'identifier'))) {
	        	var modifiedName = name.replace(/ /g,"_");
	        	collectionName = modifiedName;
	        }
	});
	
	$("textarea").each(function(){
		var ismandatory = $(this).attr('is_mandatory');
        if(!$(this).val() && ismandatory && ismandatory != "false" ){
        	usermetaDataEntered = false;
        }          
    });
	 
	$("table#newMetaDataTable .simple-select2").each(function(){
		var ismandatory = $(this).attr('is_mandatory');
		var name = $(this).val();
		if(ismandatory && ismandatory != "false" && name && name == 'Select') {
	       usermetaDataEntered = false;
	    }
	});
	
	if(!usermetaDataEntered) {
		validate = false;
		$(".registerErrorMsg").append("Enter values for all required metadata.");
		$(".registerMsgErrorBlock").show();
		$('body,html').animate({scrollTop: 0 }, 500);
	}
	
	if(collectionPath && collectionName) {
		newCollectionPath = collectionPath + "/" + collectionName.trim();
		$("#newCollectionPath").val(newCollectionPath);
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
				 $('body,html').animate({scrollTop: 0 }, 500);
				 
			 },
			error : function(e) {
				 $("#spinner").hide();
		         $("#dimmer").hide();
				 console.log('ERROR: ', e);	
				 $(".registerErrorMsg").html(e.responseText);
				 $(".registerMsgErrorBlock").show();				 
				 $('body,html').animate({scrollTop: 0 }, 500);
			}
		});
	}
}

function showSelect(collection,selection) {
	
	var isEmptyOption;
	var seclectedValue;
	
	if(selection) {
		isEmptyOption = false;
	} else {
		isEmptyOption =true;
	}
	
	if (collection == 'Program') {		
		loadJsonData('/browse', $("#instituteList"), isEmptyOption, null, null, null, "key", "value");		
		$("#showSelectProgramDiv").show();
		
	} else if (collection == 'Study') {
		
		if(selection) {
			seclectedValue= selection;
		} else {
			seclectedValue = $("#instituteList").val();
		}
		
		var params = {
				selectedPath : seclectedValue,
			};
		
		loadJsonData('/browse/collection', $("#studyList"), isEmptyOption, params, null, null, "key", "value");

		$("#showSelectStudyDiv").show();
	} else if (collection == 'Asset') {
				
		if(selection) {
			seclectedValue= selection;
		} else {
			seclectedValue = $("#studyList").val();
		}
		
		var params = {
			selectedPath : seclectedValue,
			refreshNode : 'true'
		};
		loadJsonData('/browse/collection', $("#dataList"), isEmptyOption, params, null, null, "key", "value");
		resetAssetsSelection(true);
		$("#showSelectAssetDiv").show();
		$("#showSubAssetSelectionDiv").hide();
		$("#assetUploadDiv").removeClass('show');
		$('input[name="assetSelection"]').prop('checked', false);
	} else if (collection == 'subAsset') {
		$("#showSubAssetSelectionDiv").show();
		$("#showSelectAssetDiv").hide();
		$("#uploadAndRegisterFiles").hide();
		$("#dataListDiv").hide();
		$("#assetUploadDiv").removeClass('show');
	}
}

function resetSelectionsForBackToUploadTab() {

	if ($("input[name=selectProgram]").is(":visible")) {
		if ($("#instituteList").is(":visible")) {
			$("input[name=selectProgram][value='Select Program']").prop("checked", true);
		} else {
			$("input[name=selectProgram][value='Select Program']").click();
		}
	}
	if ($("input[name=selectStudy]").is(":visible")) {
		if ($("#studyList").is(":visible")) {
			$("input[name=selectStudy][value='Select Study']").prop("checked", true);
		} else {
			$("input[name=selectStudy][value='Select Study']").click();
		}
	}
	if ($("input[name=selectAsset]").is(":visible")) {
		if ($("#dataList").is(":visible")) {
			$("input[name=selectAsset][value='Select Asset']").prop("checked", true);
		} else {
			$("input[name=selectAsset][value='Select Asset']").click();
		}
	}
}

function resetOnChangeofSelectCollection(selectTarget, selectedValue) {
	if (selectTarget == 'instituteList') {
		$("#uploadAndRegisterFiles").hide();
		$("#addAsets").hide();
		if (selectedValue && selectedValue != 'ANY') {
			$("#studyListDiv").show();
			$("#deleteStudy").hide();
			$("#deleteProgram").show();
			$("#dataSetListDiv").hide();
			$("#dataListDiv").hide();
			$("#showSelectStudyDiv").hide();
			$("input[name=selectStudy][value='Select Study']").prop("checked", false);
		} else {
			$("#studyListDiv").hide();
			$("#dataSetListDiv").hide();
			$("#dataListDiv").hide();
			$("#deleteProgram").hide();
		}
	} else if (selectTarget == 'studyList') {
		$("#uploadAndRegisterFiles").hide();
		$("#dataListDiv").hide();
		if (selectedValue && selectedValue != 'ANY') {
			$("#studyListDiv").show();
			$("#dataSetListDiv").show();
			$("#deleteStudy").show();
			$("#addAsets").show();
			$("#deleteDataSet").hide();
			$("#showSelectAssetDiv").hide();
			$("#showSubAssetSelectionDiv").hide();
			$("input[name=selectAsset]").prop("checked", false);
			$('input[name="assetSelection"]').prop('checked', false);
			
		} else {
			$("#studyListDiv").show();
			$("#dataSetListDiv").hide();
			$("#deleteStudy").hide();
			$("#addAsets").hide();
		}
	} else if (selectTarget == 'dataList') {
		$("#studyListDiv").show();
		if (selectedValue && selectedValue != 'ANY') {
			$("#uploadAndRegisterFiles").show();
			constructAssetFileAndFoldersDiv(selectedValue);
			$("#deleteDataSet").show();

		} else {
			$("#dataListDiv").hide();
			$("#uploadAndRegisterFiles").hide();
			$("#deleteDataSet").hide();
		}
	}
}
function postSuccessRegisterCollection(data,collectionType) {
	if(data.indexOf("Collection is created") != -1) {
		if(collectionType  == 'Program') {
			var params= {selectedPath:$("#basePath").val(),refreshNode:'true'};
			loadJsonData('/browse/collection', $("#instituteList"), true, params, displaySuccessMsg, null, "key", "value"); 	
			resetOnChangeofSelectCollection("instituteList",null);
		} else if(collectionType == 'Study') {
			var params= {selectedPath:$("#instituteList").val(),refreshNode:'true'};
			loadJsonData('/browse/collection', $("#studyList"), true, params, displaySuccessMsg, null, "key", "value");
			resetOnChangeofSelectCollection("studyList",null);
		} else if(collectionType == 'Asset') {
			var params= {selectedPath:$("#studyList").val(),refreshNode:'true'};
			loadJsonData('/browse/collection', $("#dataList"), true, params, displaySuccessMsg, null, "key", "value");
			resetAssetsSelection();
			$("#assetUploadDiv").removeClass('show');
		} else if(collectionType == 'Folder') {
			constructAssetFileAndFoldersDiv($("#dataList").val());
			displaySuccessMsg();
		}
	} else {
		$(".registerErrorMsg").html("Error creating collection:" + data);
		$(".registerMsgErrorBlock").show();
	}
}

function resetAssetsSelection(reset) {
	if($("#dataListDiv").is(":visible") || reset) {
		var data = {
			    id: 'ANY',
			    text: 'Select'
			};
		var newOption = new Option(data.text, data.id, true, true);
		if ($('#dataList').find("option[value='" + data.id + "']").length) {
		    $('#dataList').val(data.id).trigger('change');
		} else {
			$('#dataList').append(newOption).trigger('change');
		}
	}
}

function displaySuccessMsg(data,status) {
	$(".registerMsg").html("Collection created successfully.");
	$(".registerMsgBlock").show();	
}

function openBulkDataRegistration(folderPath) {
	var datafilePath = $("#dataList").val();
	if(folderPath) {
		datafilePath += "/" + folderPath;
	}
	$("#bulkDataFilePath").val(datafilePath);
	$("#bulkDataFilePathCollection").val(datafilePath);
	$(".registerBulkDataFileSuccess").hide();
	$(".registerBulkDataFile").html("");
	$(".uploadBulkDataError").hide();
	$(".uploadBulkDataErrorMsg").html("");
	$("#uploadCollectionPath").val(datafilePath);
	clearRegisterDataDiv();
}

function cancelAndReturnToUploadTab() {
	constructAssetFileAndFoldersDiv($("#dataList").val());
}

function retrieveAssetTypeDiv(data) {
	 if(data.value != 'Select') {
		    var params1= {collectionType:'Asset',controllerValue:data.value,refresh:false,controllerAttribute:'asset_type'};
			invokeAjax('/addCollection','GET',params1,constructAssetTypeBulkDiv,null,null,null);
	  } else {
		  $("#addMetadataDiv").hide();
		  $("#assetBulkMetadataTable tbody").html("");
	  }
}

function constructAssetTypeBulkDiv(data,status) {
	$("#addMetadataDiv").show();
	$("#assetBulkMetadataTable tbody").html("");
	
	$("#assetBulkMetadataTable tbody").append('<tr><td style="width: 24%;">Asset Group Identifier&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
        	'data-placement="right" title="The system uses the Asset Group Identifier to create a unique Asset Identifier for each asset, in the format <group name>_<integer>"></i></td><td>'+
        	'<input type="text" placeholder="Required" class="bulkAssetTextbox" is_mandatory="true" aria-label="value of meta data"  name="assetGroupIdentifier"' +
        	'></td></tr>');
	
	$.each(data, function(key, value) {	
		
		var placeholderValue = "";
	     if(value.mandatory && value.mandatory == true) {
	    	 placeholderValue = "Required";
	     }

	    if(value.validValues != null && value.attrName !='asset_type'){
		   
	    	$("#assetBulkMetadataTable tbody").append('<tr><td>' +  value.displayName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
        	'data-placement="right" title="'+value.description+'"></i></td><td>'+
        	'<select class="simple-select2" is_mandatory="'+value.mandatory+'" style="width:99%;" id="'+value.attrName+'" name="zAttrStr_'+value.attrName+'" value="'+value.attrValue+'"></select></td></tr>');
	    	
	    	  var $select = $("#"+value.attrName);
	    	  if(value.attrValue){
	    	 	$select.append($('<option></option>').attr('value', value.attrValue).text(value.attrValue));
	    	  } else {
	    	    $select.append($('<option></option>').attr('value', 'Select').text('Select'));
	    	  }
	    	  	    	  
	    	  for (var i = 0; i < value.validValues.length; i++) {
	    		   $select.append($('<option></option>').attr('value', value.validValues[i]).text(value.validValues[i]));
              }
               
	    	  $select.select2().trigger('change');
	    	
	   } else if(value.attrName && value.attrName != 'asset_name' && value.attrName !='asset_type' &&
			   value.attrName !='asset_identifier' && value.attrName !='access_group'){	
		   if(value.attrName == 'description') {
			   $("#assetBulkMetadataTable tbody").append('<tr><td>' +  value.displayName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
			        	'data-placement="right" title="Please note that this description will be applied to all Assets. You may edit the description of each Asset separately after it is uploaded."></i></td><td>'+
			        	'<textarea rows="4" placeholder ="Required" is_mandatory="'+value.mandatory+'" aria-label="value of meta data"  name="zAttrStr_'+value.attrName+'"' +
			        	'style="width:100%;padding-left: 10px;font-family:inter_regular;"></textarea></td></tr>'); 
		   } else {
		 	$("#assetBulkMetadataTable tbody").append('<tr><td>' +  value.displayName + '&nbsp;&nbsp;<i class="fas fa-question-circle" data-toggle="tooltip"'+
        	'data-placement="right" title="'+value.description+'"></i></td><td>'+
        	'<input type="text" placeholder="'+placeholderValue+'" class="bulkAssetTextbox" is_mandatory="'+value.mandatory+'" aria-label="value of meta data"  name="zAttrStr_'+value.attrName+'"' +
        	'></td></tr>');
		   }
	   }      
		
	});	
}

function registerBulkAssets() {
    var studyPath = $("#studyPath").val();
	$("#registerBulkAssetForm").attr('bulkDatafilePath', studyPath);	
	$("#registerBulkAssetForm").attr('uploadType', 'globus');
	var form = $('#registerBulkAssetForm')[0];		 
    var data = new FormData(form);
    data.append('bulkDatafilePath', studyPath);
    data.append('uploadType','globus');
    data.append('assetType',$("#assetTypeSelect").val())
    var isValidated  = true;
    var usermetaDataEntered = true;
    var isFormBulkUpload;
    if(!$("#assetGlobusEndpointId").length || !$("#assetGlobusEndpointPath").length) {
    	isValidated =false;
    	bootbox.dialog({ 
    	    message: 'Select Assets from Globus.'
    	});
    } else if(!$("#assetSelectedFolders").length) {
    	isValidated =false;
    	bootbox.dialog({ 
    	    message: 'Select Folders from Globus.'
    	});
    } else if($("#assetSelectedFiles").length) {
    	isValidated = false;
    	bootbox.dialog({ 
    	    message: 'Select folders only.'
    	});
    } else if($('input[name="assetUploadType"]:checked').length == 0) {
    	isValidated = false;
    	bootbox.dialog({ 
    	    message: 'Select your choice of upload.'
    	});
    } else if($("input[name='assetUploadType']:checked").val() == 'No' && $("#assetTypeSelect").val() == 'Select') {
    	isValidated = false;
    	bootbox.dialog({ 
    	    message: 'Select Asset Type.'
    	});
    } else if($("input[name='assetUploadType']:checked").val() == 'Yes' && !$("#doeMetadataFile").val()) {
    	isValidated = false;
    	bootbox.dialog({ 
    	    message: 'Upload CSV Metadata file.'
    	});
    }

	$('table#assetBulkMetadataTable input[type="text"]').each(function(){
		var name = $(this).val();
		var ismandatory = $(this).attr('is_mandatory');
	    if(!name && ismandatory && ismandatory != "false" ){
	    	isValidated = false;
	    	usermetaDataEntered = false;
         }
	});
	
	$("table#assetBulkMetadataTable").find("textarea").each(function(){
		var ismandatory = $(this).attr('is_mandatory');
        if(!$(this).val() && ismandatory && ismandatory != "false" ){
        	isValidated = false;
	    	usermetaDataEntered = false;
        }          
    });
	 
	$("table#assetBulkMetadataTable").find(".simple-select2").each(function(){
		var ismandatory = $(this).attr('is_mandatory');
		var name = $(this).val();
		if(ismandatory && ismandatory != "false" && name && name == 'Select') {
			isValidated = false;
	    	usermetaDataEntered = false;
	    }
	});
  
	if(!usermetaDataEntered) {
		bootbox.dialog({ 
    	    message: 'Enter values for all required metadata.'
    	});
	}
   if(isValidated)  {
	   if($("input[name='assetUploadType']:checked").val() == 'No') {
		   isFormBulkUpload = true;
	   } else if($("input[name='assetUploadType']:checked").val() == 'Yes') {
		   isFormBulkUpload = false;
	   }
	$.ajax({
		type : "POST",
		enctype: "multipart/form-data",
	     url : "/addbulk?isFormBulkUpload="+isFormBulkUpload,
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

	    	 if(!msg) {
	    		msg= "Error in bulk upload";
	    	 }
	    	 bootbox.dialog({ 
	     	    message: msg,
	     	    onEscape: function() {
	     		  location.replace("/addbulk");
	     	    }
	     	}); 
		 },
		error : function(e) {
			 $("#spinner").hide();
	         $("#dimmer").hide();
			 console.log('ERROR: ', e);
			 bootbox.dialog({ 
		     	   message: msg,
		     	   onEscape: function() {
		     		  location.replace("/addbulk");
		     	    }
		    });
			 
		}
	 });
   }
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
						 $(".uploadBulkDataErrorMsg").html(e);
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
		else if((bulkUploadType == 'drive' && $("input[name=folderIds]").length) || 
				(bulkUploadType == 'globus' && $("#folderNamesDiv ul li").length)) {
			$(".uploadBulkDataError").show();
			$(".uploadBulkDataErrorMsg").html("Select files only.")
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
						 cancelAndReturnToUploadTab();	
			    	 } else {
						 $(".uploadBulkDataError").show();
						 $(".uploadBulkDataErrorMsg").html(msg);
			    	 }
					 	 
				 },
				error : function(e) {
					 $("#spinner").hide();
			         $("#dimmer").hide();
					 console.log('ERROR: ', e);	
					 $(".uploadBulkDataError").show();
					 $(".uploadBulkDataErrorMsg").html(e);
				}
			});
		}
	}

}

function appendFileName($this) {
    // Append the file name to the data file path
    var filename = $this.val().replace(/^C:\\fakepath\\/, "")
    var value = $("#bulkDataFilePathCollection").val() + "/" + filename;
    $("#dataFilePath").val(value);
    
}

function displayDataFileSection(value) {
	$(".registerBulkDataFileSuccess").hide();
	$(".registerBulkDataFile").html("");
	$("#registerFileBtnsDiv").show();
	
	if(value == 'singleData') {
		    $("#singleFileDataUploadSection").show();
		    $("#bulkFileUploadSection").hide();						
		
	} else if(value == 'globus'){
		    $("#singleFileDataUploadSection").hide();
		    $("#bulkFileUploadSection").show();		
			$("#displayGlobusUploadDiv").show();
			$("#displayS3UploadDiv").hide();	
			$("#fileNamesDiv").show();
			$("#folderNamesDiv").show();
			$("#displayDriveUploadDiv").hide();
	} else if(value == 's3'){
			$("#singleFileDataUploadSection").hide();
			$("#bulkFileUploadSection").show();	
			$("#displayGlobusUploadDiv").hide();
			$("#displayS3UploadDiv").show();
			$("#fileNamesDiv").hide();
			$("#folderNamesDiv").hide();
			$("#displayDriveUploadDiv").hide();
		} else if(value == 'drive'){
			$("#singleFileDataUploadSection").hide();
			$("#bulkFileUploadSection").show();	
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

function displayAssetTpeSelection(data) {
	if(data == 'Yes') {
		$("#uploadCsvFile").show();
		$("#formAssetSelection").hide();
		$("#assetTypeSelect").val("Select").trigger('change');
		$("#addMetadataDiv").hide();
	} else if(data =='No') {
		$("#doeMetadataFile").val("");
		$("#uploadCsvFile").hide();
		$("#formAssetSelection").show();
	}
}

function functionDelete($this,collectionType) {
	var selectedValue = $( "#" +$this+ " option:selected" ).text();
	var textvalue = $( "#" +$this+ " option:selected" ).val();
	if(textvalue && textvalue != "ANY") {
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
				         returnErrorMessage(e);
					}
				});
		    	}   
		    }
		});
	} else {
		bootbox.alert("Select a collection to delete.");
	}
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