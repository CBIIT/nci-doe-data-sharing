$(document).ready(function() {
	$('body').on('click', function(e) {
		$(".infoTooltip").popover({
			html : true
		});
	});
});

$(document).on('change', '#uploadTestInferFile', function() {
	var $this = $(this);
	var filename = $this.val().replace(/^C:\\fakepath\\/, "")
	var value = $("#testInputPath").val() + "/" + filename;
	$("#performInferencingModel").find("#testInputPath").val(value);
});

$(document)
		.on(
				'click',
				'#openInferModal',
				function() {
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
					if (isExternalDataSetSupported && isExternalDataSetSupported == "true") {
						$("#performInferencingModel").find("#displayDataTypeDiv").show();
						$("#performInferencingModel").find("#labelForInputType").html(
								"Upload input file to generate predictions.&nbsp;<span id='tooltipHtml'></span>");
					} else {
						$("#performInferencingModel").find("#displayDataTypeDiv").hide();
						$("#performInferencingModel").find("#labelForInputType").html(
								"Upload GDC file to generate predictions.&nbsp;<span id='tooltipHtml'></span>");
					}

					var modelFile = $("#performInferencingModel").find("#modelPath").val();
					$("#performInferencingModel").find("#testInputPath").val($("#selectedAssetPath").text());
					if (modelFile.indexOf('mt_cnn') != -1 || modelFile.indexOf('mt-cnn') != -1) {
						var title = "Upload a GDC manifest file<br/> (TXT) or a pathology report<br/> (TXT or PDF). For more<br/> details, refer to the <a target='_blank' href='https://wiki.nci.nih.gov/x/cQh2H'> user guide</a>.";
						var outputTitle = "This is optional. Upload CSV<br/> outcome file to evaluate<br/> the model. For more details,<br/> refer to the <a target='_blank' href='https://wiki.nci.nih.gov/x/cQh2H'> user guide</a>.";
					} else {
						var title = "Upload GDC manifest file<br/> (such as TXT) or an FPKM-UQ<br/> file (TXT or CSV). For more<br/> details, refer to the <a target='_blank' href='https://wiki.nci.nih.gov/x/bwh2H'> user guide</a>.";
						var outputTitle = "This is optional. Upload CSV<br/> outcome file to evaluate<br/> the model. For more details,<br/> refer to the <a target='_blank' href='https://wiki.nci.nih.gov/x/bwh2H'> user guide</a>.";
					}

					$("#performInferencingModel").find("#tooltipHtml").html(
							'<i class="fas fa-question-circle infoTooltip"' + 'data-toggle="popover" data-content="'
									+ title + '"></i>');

					$("#performInferencingModel").find("#outputTooltipHtml").html(
							'<i class="fas fa-question-circle infoTooltip"' + 'data-toggle="popover" data-content="'
									+ outputTitle + '"></i>');

					$(".performInferencingError").hide();
					$(".performInferMsgError").html("");
					$("input[name=uploadFrom]").prop("checked", false);
					$("#uploadTestInferFile").val("");
					$("#uploadTestOutputFile").val("")
					$("#performInferencingModel").modal({
						show : true,
						backdrop : 'static',
						keyboard : false
					});

				});

$(document).on('click', '#performInferencing', function() {

	var form = $('#performInferForm')[0];
	var data = new FormData(form);
	var file = $("#uploadTestInferFile").val();
	var uploadType = $('input[name=uploadFrom]:checked').val();
	var validate = true;

	if (!file) {
		$(".performInferencingError").show();
		$(".performInferMsgError").html("Upload input file to generate predictions.");
		validate = false;
	}

	if ($("input[name=uploadFrom]").is(":visible") && !uploadType) {
		$(".performInferencingError").show();
		$(".performInferMsgError").html("Choose upload data from type.");
		validate = false;
	}

	if (validate) {
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

$(document).on(
		'click',
		'#performModelAnalysis',
		function() {

			$("#performModelAnalysisModal").find("#selectApplicableModelName").html("");
			var resultName = $("#resultFileName").val();
			var applicableModelNames = $("#applicableModelName").val();
			var outputResultFilePath;
			var isResultFileNameFound = false;
			var testInputPath;
			$("#dataSetTable tr").each(function() {
				var len = $(this).find('td:first input[type=checkbox]').length;
				if (len > 0) {
					var resultVal = $(this).find('td').eq(1).text().trim().toLowerCase();
					var value = $(this).find('td:first input[type=checkbox]').attr('id');
					if (resultVal && resultName && resultVal.indexOf(resultName.toLowerCase()) != -1) {
						isResultFileNameFound = true;
						outputResultFilePath = value;
					} else {
						testInputPath = value;
					}
				}
			});

			if (!isResultFileNameFound) {
				bootbox.alert("Result file not found.");
			} else {

				$("#performModelAnalysisModal").find("#testInputPath").val(testInputPath);
				$("#performModelAnalysisModal").find("#outputResultFilePath").val(outputResultFilePath);
				var applicableModelNamesList = applicableModelNames.split(',');

				$("#performModelAnalysisModal").find("#selectApplicableModelName").append(
						$('<option></option>').attr('value', "Select").text("Select"));
				$.each(applicableModelNamesList, function(index, value) {

					var displayVal = value.substring(value.lastIndexOf('/') + 1, value.length);
					$("#performModelAnalysisModal").find("#selectApplicableModelName").append(
							$('<option></option>').attr('value', value).text(displayVal));
				});

				$(".performModelAnalysisError").hide();
				$(".performAnalysisMsgError").html("");
				$("#performModelAnalysisModal").modal({
					show : true,
					backdrop : 'static',
					keyboard : false
				});
			}

		});

$(document).on('click', '#performModelAnalysisBtn', function() {

	var resultName = $("#performModelAnalysisModal").find("#outputResultFilePath").val();
	var applicableModelName = $("#performModelAnalysisModal").find("#selectApplicableModelName").val();
	var testInputPath = $("#performModelAnalysisModal").find("#testInputPath").val();

	var inference = {};
	inference.applicableModelNames = applicableModelName;
	inference.outputResultName = resultName;
	inference.uploadFrom = "gdcData";
	inference.assetPath = $("#assetPath").val();
	inference.testInputPath = testInputPath;

	if (applicableModelName && applicableModelName == 'Select') {

		$(".performModelAnalysisError").show();
		$(".performAnalysisMsgError").html("Select applicable model name.");

	} else {

		$.ajax({
			type : "POST",
			url : "/performInferencing/performModelAnalysis",
			data : inference,
			beforeSend : function() {
				$("#spinner").show();
				$("#dimmer").show();
			},
			success : function(msg) {
				$("#spinner").hide();
				$("#dimmer").hide();
				$("#performModelAnalysisModal").modal('hide');
				bootbox.dialog({
					message : msg
				});

			},
			error : function(e) {
				$("#spinner").hide();
				$("#dimmer").hide();
				console.log('ERROR: ', e);
				returnErrorMessage(e);
			}
		});
	}
});