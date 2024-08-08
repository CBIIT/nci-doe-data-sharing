function constructAssetFileAndFoldersDiv(assetPath) {
	$.ajax({
		"url" : "/collection/canEdit",
		"type" : "GET",
		data : {
			selectedPath : assetPath,
			verifyAtAssetLevel : true
		},
		beforeSend : function() {
			$("#spinner").show();
			$("#dimmer").show();
		},
		success : function(msg) {
			$("#spinner").hide();
			$("#dimmer").hide();
			postSuccessCanEdit(msg);
		},
		error : function(e) {
			console.log('ERROR: ', e);
			$("#spinner").hide();
			$("#dimmer").hide();
		}
	}).done(function(e) {
		invokeAjax('/getDataObjects', 'GET', {
			path : assetPath
		}, contructDataListDiv, null, null, null);
	});
}

function contructDataListDiv(data, status) {
	$("#dataListing").html("");
	var assetPermissions = $("#assetPermissions").val();
	if (data.length == 0) {
		$("#dataListDiv").hide();
	}
	$
			.each(
					data,
					function(key, value) {
						$("#dataListDiv").show();
						var html = "";
						if (value.isFolder == false) {
							html += '<li>' + value.name + '</ol>';
						} else {
							var datalist = "dataList";
							if (assetPermissions && assetPermissions == 'true') {
								html += '<li><a class="detail-control" data-name = '
										+ value.name
										+ '>'
										+ '<i class="expand far fa-folder"></i></a>&nbsp;'
										+ value.name
										+ ' &nbsp;&nbsp;'
										+ '<a href="#" class="uploadDataSet" title="Upload to Asset Subcollection" style="font-size: 15px;color: #F39530;"><i class="fas fa-upload">'
										+ '</i></a> &nbsp;&nbsp;<a href="#" title="Register Subcollection" onclick="createCollectionDiv(\''
										+ datalist + '\',\'' + value.name + '\')" class="addDeleteUploadLabels">'
										+ '<img src="/images/Uploads.add.png" class="uploadslogo" alt="register"></a>';

								html += "&nbsp;&nbsp;<span style='border: transparent;' class='btn btn-link btn-sm editFolderMetadata'  folder_name = '" + value.name + "' metadata_path  = '"
										+ value.path
										+ "'"
										+ "collectionId = '"
										+ value.collectionId
										+ "'>"
										+ "<img src='images/Edit-FileMetadata.png' data-toggle='tooltip' title='Edit Folder Metadata' th:src='@{/images/Edit-FileMetadata.png}' "
										+ "style='width:16px;' alt='edit collection'></span>";

								html += "<span style='border: transparent;' coll_path = '"
										+ value.path
										+ "' coll_name='"
										+ value.name
										+ "' class='btn btn-link btn-sm deleteFolderCollectionBtn'>"
										+ "<img src='images/Delete.png' data-toggle='tooltip' title='Delete Collection' th:src='@{/images/Delete.png}' "
										+ "style='width:12px;' alt='Delete Collection'></span></li>";

							} else {
								html += '<li><a class="detail-control" data-name = ' + value.name + '>'
										+ '<i class="expand far fa-folder"></i></a>&nbsp;' + value.name
										+ ' &nbsp;&nbsp;' + '</li>';
							}

						}
						$("#dataListing").append(html);
					});
}

function postSuccessCanEdit(data) {
	$("#assetPermissions").val(data);
	if (data == false) {
		$("#registerFolder").css('pointer-events', 'none');
		$("#registerFolder").parent().prop("title", "Insufficient permissions to register subcollection.");
		$("#addBulkDataFiles").css('pointer-events', 'none');
		$("#addBulkDataFiles").parent().prop("title", "Insufficient permissions to add data.");
	} else {
		$("#registerFolder").css('pointer-events', 'all');
		$("#registerFolder").parent().prop("title", "");
		$("#addBulkDataFiles").css('pointer-events', 'all');
		$("#addBulkDataFiles").parent().prop("title", "");
	}
}

$('#dataListing')
		.on(
				'click',
				'a.detail-control',
				function() {
					var $this = $(this);
					var assetPermissions = $("#assetPermissions").val();
					var $lithis = $(this).closest('li');
					if ($this.hasClass('shown')) {
						$lithis.next('ul').remove();
						$this.removeClass('shown');
						$this.find("i.expand.far").toggleClass('fa-folder fa-folder-open');
					} else {
						var name = $this.attr('data-name');
						var params = {
							path : $("#dataList").val() + "/" + name
						};
						$
								.ajax({
									"url" : "/getDataObjects",
									"type" : "GET",
									data : params,
									beforeSend : function() {
										$("#spinner").show();
										$("#dimmer").show();
									},
									success : function(msg) {
										$("#spinner").hide();
										$("#dimmer").hide();
										var html = "<ul>";
										$
												.each(
														msg,
														function(key, value) {
															if (value.isFolder == false) {
																html += '<li>' + value.name + '</ol>';
															} else {
																if (assetPermissions && assetPermissions == 'true') {
																	html += '<li><a class="detail-control" data-name = '
																			+ name
																			+ "/"
																			+ value.name
																			+ '>'
																			+ '<i class="expand far fa-folder"></i></a>&nbsp;'
																			+ value.name
																			+ ' &nbsp;&nbsp;'
																			+ '<a href="#" class="uploadDataSet"  title="Upload to Asset Subcollection" style="font-size: 15px;color: #F39530;">'
																			+ '<i class="fas fa-upload"></i></a>';
																	html += "&nbsp;&nbsp;<span style='border: transparent;' class='btn btn-link btn-sm editFolderMetadata' folder_name = '" + value.name + "' metadata_path  = '"
																			+ value.path
																			+ "'"
																			+ "collectionId = '"
																			+ value.collectionId
																			+ "'>"
																			+ "<img src='images/Edit-FileMetadata.png' data-toggle='tooltip' title='Edit Folder Metadata' th:src='@{/images/Edit-FileMetadata.png}' "
																			+ "style='width:16px;' alt='edit collection'></span>";

																	html += "<span style='border: transparent;' coll_path = '"
																			+ value.path
																			+ "' coll_name='"
																			+ value.name
																			+ "' class='btn btn-link btn-sm deleteFolderCollectionBtn'>"
																			+ "<img src='images/Delete.png' data-toggle='tooltip' title='Delete Collection' th:src='@{/images/Delete.png}' "
																			+ "style='width:12px;' alt='Delete Collection'></span></li>";
																} else {
																	html += '<li><a class="detail-control" data-name = '
																			+ name
																			+ "/"
																			+ value.name
																			+ '>'
																			+ '<i class="expand far fa-folder"></i></a>&nbsp;'
																			+ value.name + ' &nbsp;&nbsp;' + '</li>';
																}

															}
														});
										html += "</ul>";
										$lithis.after(html);
										$this.find("i.expand.far").toggleClass('fa-folder fa-folder-open');
										$this.addClass('shown');
									},
									error : function(e) {
										console.log('ERROR: ', e);
										$("#spinner").hide();
										$("#dimmer").hide();
									}
								});
					}
});
