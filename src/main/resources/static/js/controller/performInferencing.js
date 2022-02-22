$(document).ready(function() {
	$('body').on('click',function(e) {
	$(".infoTooltip").popover({html:true});
	});
});

$(document).on('change', '#uploadTestInferFile', function() {
	var $this = $(this);
	var filename = $this.val().replace(/^C:\\fakepath\\/, "")
	var value = $("#testInputPath").val() + "/" + filename;
	$("#performInferencingModel").find("#testInputPath").val(value);
});

$(document).on('click', '#openInferModal', function() {
	$("#dataSetTable tr").each(function() {
		var len = $(this).find('td:first input[type=checkbox]').length;
		if (len > 0) {
			var value = $(this).find('td:first input[type=checkbox]').attr('id');
			if (value && value.indexOf('.h5') != -1) {
				$("#performInferencingModel").find("#modelPath").val(value);
			}
		}
	});

	var isExternalDataSetSupported = $("#isExternalDataSetSupported").val();
	if(isExternalDataSetSupported && isExternalDataSetSupported == "true") {
		$("#performInferencingModel").find("#displayDataTypeDiv").show();
		$("#performInferencingModel").find("#labelForInputType").html("Upload file to generate predictions.&nbsp; <span id='tooltipHtml'></span>");
	} else {
		$("#performInferencingModel").find("#displayDataTypeDiv").hide();
		$("#performInferencingModel").find("#labelForInputType").html("Upload GDC file to generate predictions.&nbsp; <span id='tooltipHtml'></span>");
	}
	
	
	var modelFile = $("#performInferencingModel").find("#modelPath").val();
	$("#performInferencingModel").find("#testInputPath").val($("#selectedAssetPath").text());
	if(modelFile.indexOf('mt_cnn') != -1 || modelFile.indexOf('mt-cnn') != -1) {
		var title="Upload GDC manifest or <br/> pathology report. <br/>For more details, refer to the<br/><a target='_blank' href='https://wiki.nci.nih.gov/x/cQh2H'> user guide</a>.";
		var outputTitle = "This is optional. Upload file to <br/>evaluate the model. <br/>For more details, refer to the<br/><a target='_blank' href='https://wiki.nci.nih.gov/x/cQh2H'> user guide</a>.";
	} else {
		var title="Upload GDC manifest or <br/> FPKM-UQ file. <br/>For more details, refer to the<br/><a target='_blank' href='https://wiki.nci.nih.gov/x/bwh2H'> user guide</a>.";
		var outputTitle = "This is optional. Upload file to <br/>evaluate the model. <br/>For more details, refer to the<br/><a target='_blank' href='https://wiki.nci.nih.gov/x/bwh2H'> user guide</a>.";
	}
	
	$("#performInferencingModel").find("#tooltipHtml").html('<i class="fas fa-question-circle infoTooltip"'+
			'data-toggle="popover" data-content="'+title+'"></i>');
	
	$("#performInferencingModel").find("#outputTooltipHtml").html('<i class="fas fa-question-circle infoTooltip"'+
			'data-toggle="popover" data-content="'+outputTitle+'"></i>');
	
	$(".performInferencingError").hide();
	$(".performInferMsgError").html("");
	$("input[name=uploadFrom]").prop("checked",false);
	$("#uploadTestInferFile").val("");
	$("#uploadTestOutputFile").val("")
	$("#performInferencingModel").modal({show: true, backdrop: 'static', keyboard: false});

});

$(document).on('click', '#performInferencing', function() {

	var form = $('#performInferForm')[0];
	var data = new FormData(form);
	var file = $("#uploadTestInferFile").val();
	var uploadType = $('input[name=uploadFrom]:checked').val();
	
	if(!file) {
		$(".performInferencingError").show();
		$(".performInferMsgError").html("Upload file to generate predictions.")
	}
	
	if(!uploadType) {
		$(".performInferencingError").show();
		$(".performInferMsgError").html("Choose upload data from type.")
	}
	
	if (file && uploadType) {
		$.ajax({
			type : "POST",
			enctype : "multipart/form-data",
			url : "/performInferencing",
			data : data,
			processData : false,
			contentType : false,
			beforeSend : function() {
				$("#spinner").show();
				$("#dimmer").show();
			},
			success : function(msg) {
				$("#spinner").hide();
				$("#dimmer").hide();
				$("#performInferencingModel").modal('hide');
				bootbox.dialog({
					message : msg
				});

			},
			error : function(e) {
				$("#spinner").hide();
				$("#dimmer").hide();
				console.log('ERROR: ', e);
				bootbox.dialog({
					message : e
				});
			}
		});
	}
});