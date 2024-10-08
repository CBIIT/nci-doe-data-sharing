$(document).ready(function () {

   if (screen.width > 990) {
      $("#mobileKeyworkSearchDiv").hide();
   } else {
      $("#filterSectionDiv").removeClass('col-lg-3 col-md-8');
      $("#mobileKeyworkSearchDiv").show();
   }

   if ($(".backToStatusTabLink").is(':visible')) {
      $("#search").removeClass('active-nav');
      $("#status").addClass('active-nav');
   }
   else {
      $("#status").removeClass('active-nav');
      $("#search").addClass('active-nav');
   }
   
   //for mobile only
   $(".landing-tab").removeClass('active');
   $(".search-tab").addClass('active');   
   //

   $('.dt-responsive tbody').on('click', 'td', function () {
      initializeToolTips();

   });
   $('body').tooltip({
      selector: '[data-toggle="tooltip"]'
   });

   var dmeDataId = $("#dmeDataId").val();
   var doiId = $("#doi").val();
   var returnToSearch = $("#returnToSearch").val();
   if (dmeDataId || doiId) {
      populateSearchCriteria('datasetUrl');
   } else if (returnToSearch) {
      var search = $("#searchQuery").val();
      var list = JSON.parse(search);
      for (var i = 1; i < list.attrName.length; i++) {
         var attrVal = list.attrValuesString.split('@@');
         var iskeyWordSearch = list.iskeyWordSearch[i];
         if (iskeyWordSearch == true) {
         //when keyword is entered on search page
            var attrval = attrVal[i];
            var newAttrVal = attrval.replaceAll('%', '');

            if (screen.width > 990) {
               $("#attributeVal").val(newAttrVal);
               $("#resetBtn").show();
            } else {
               $("#mobileKeyworkSearchDiv").find("#attributeVal").val(newAttrVal);
               $("#resetBtnMobile").show();
            }

         } else {
            var attrName = list.attrName[i];
            var attrVal = attrVal[i];
            $(".attrName").each(function () {
               if ($(this).text() == attrName) {
                  $(this).parent().parent().find(".filteritem").each(function () {
                     if ($(this).val() == attrVal) {
                        $(this).prop("checked", true).trigger('change');
                     }
                  })
               }
            })
         }
      }
      populateSearchCriteria();
   } else if ($("#keyWord").val()) {
   
      // when keyword is entered on landing page
      if (screen.width > 990) {
         $("#attributeVal").val($("#keyWord").val());
         $("#resetBtn").show();
      } else {
         $("#mobileKeyworkSearchDiv").find("#attributeVal").val($("#keyWord").val());
         $("#resetBtnMobile").show();
      }

      populateSearchCriteria();
   } else if (screen.width > 990) {
      populateSearchCriteria();

   }
   showFirstFewFields();

   $(document).keypress(function (event) {
      var keycode = (event.keyCode ? event.keyCode : event.which);
      if (keycode == '13') {
         if ($("#searchBtn").is(':visible')) {
            event.preventDefault();
            populateSearchCriteria();
         } else if ($("#searchMobileBtn").is(':visible')) {
            $("#searchMobileBtn").click();
         }
      }
   });

   $(document).on('click', '.dataTargetCollapse', function () {
      if ($(this).parent().parent().find('div.dataDivCollapse').is(":visible")) {
         $(this).parent().css('margin-bottom', '-1px');
         $(this).parent().parent().find('div.dataDivCollapse').css('display', 'none');
         $(this).parent().parent().find('.search_filter_div').css('display', 'none');
         $(this).attr('src', '/images/AccordionUp.png');
         $(this).parent().parent().find(".css-17rpx5x").hide();
         $(this).parent().parent().find(".css-17rpx5xLess").hide();
      } else {
         $(this).parent().css('margin-bottom', '15px');
         $(this).parent().parent().find('div.dataDivCollapse').css('display', 'block');
         $(this).parent().parent().find('.search_filter_div').css('display', 'none');
         $(this).attr('src', '/images/AccordionDown.png');
         $(this).parent().parent().find('.css-17rpx5x').show();
         $(this).parent().parent().find('.showMore').show();
         showFirstFewFields($(this).parent().parent(), 'Less');
      }
   });

   $(document).on('click', '.searchCheckBoxlist', function () {
      if ($(this).parent().parent().find('.search_filter_div').is(":visible")) {
         $(this).parent().parent().find('.search_filter_div').css('display', 'none');
      } else {
         $(this).parent().parent().find('.search_filter_div').css('display', 'flex');
      }
   });

   $(document).on('click', '.cancel_fiter_icon', function () {
      $(this).parent().find('.filterSearchBox').val("").trigger('keyup');
   });

   $(document).on('keyup', '.filterSearchBox', function () {
      var query = $(this).val().toLowerCase();
      $(this).parent().parent().find('.filteritem').each(function (i, elem) {
         var x = $(this).val().toLowerCase();
         if (x.indexOf(query) != -1) {
            $(this).parent().parent().show();

         } else {
            $(this).parent().parent().hide();
         }
      });

      showFirstFewFields($(this).parent(), 'searchFilter');
   });

   $(document).on('click', '#clearFilters', function () {
      $(".filterGroupDiv").each(function (e) {
         $(this).show();
         $(this).find('.showMorefields').show();
         $(this).find('.showMorefields').removeClass('filter_checked');
         $(this).find('.filteritem').prop('checked', false);
      });
      resetPositionsForFilters();
      showFirstFewFields();
      populateSearchCriteria();
      resetSearchFilterCountsOnRefresh();

   });

   $(document).on('click', '#cancelFiltersMobile', function () {
      $(".filterGroupDiv").each(function (e) {
         $(this).show();
         $(this).find('.filteritem').prop('checked', false);

         $(this).find('.showMorefields').show();
         $(this).find('.showMorefields').removeClass('filter_checked');
         $(this).find('div').css('color', '#212529');
      });
      showFirstFewFields();
   });


   $(document).on('click', '.showMore', function () {

      $(this).parent().hide();
      $(this).parent().parent().find(".css-17rpx5xLess").show();
      showFirstFewFields($(this).parent().parent(), 'More');
   });

   $(document).on('click', '.showLess', function () {

      $(this).parent().hide();
      $(this).parent().parent().find(".css-17rpx5x").show();
      $(this).parent().parent().find(".showMore").show();
      showFirstFewFields($(this).parent().parent(), 'Less');
   });

   // function when clicking on expand and collpase button for search sidebar
   $("#expand_collapse_filters").click(function () {

      var title = $(this).attr('data-original-title');
      // Find all filters inside inside the filter section to collapse
      var buttons = $("#filterSectionDiv").find('.dataTargetCollapse');

      if (title == 'Collapse all filters') {

         // Click on each button
         buttons.each(function () {
            $(this).parent().css('margin-bottom', '-1px');
            $(this).parent().parent().find('div.dataDivCollapse').css('display', 'none');
            $(this).parent().parent().find('.search_filter_div').css('display', 'none');
            $(this).attr('src', '/images/AccordionUp.png');
            $(this).parent().parent().find(".css-17rpx5x").hide();
            $(this).parent().parent().find(".css-17rpx5xLess").hide();
         });
         $(this).attr('data-original-title', 'Expand all filters');
         $(this).find('img').attr('src', '/images/expand_filters.png');
      } else {

         // Click on each button
         buttons.each(function () {
            $(this).parent().css('margin-bottom', '15px');
            $(this).parent().parent().find('div.dataDivCollapse').css('display', 'block');
            $(this).parent().parent().find('.search_filter_div').css('display', 'none');
            $(this).attr('src', '/images/AccordionDown.png');
            $(this).parent().parent().find('.css-17rpx5x').show();
            $(this).parent().parent().find('.showMore').show();
            showFirstFewFields($(this).parent().parent(), 'Less');
         });
         $(this).attr('data-original-title', 'Collapse all filters');
         $(this).find('img').attr('src', '/images/collapse_filters.png');
      }


   });
   
 });

function populateSearchCriteria(searchType) {

   if (screen.width > 990) {
      $("#searchResultsDiv").show();
      $("#searchResultsMobileDiv").hide();
   } else {
      $("#searchResultsDiv").hide();
      $("#mobileKeyworkSearchDiv").hide();
      $("#filterSectionDiv").hide();
      $("#searchResultsMobileDiv").show();
   }

   search_criteria_json.detailed = true;
   var attrNames = [];
   var attrValues = [];
   var levelValues = [];
   var isExcludeParentMetadata = [];
   var rowIds = [];
   var operators = [];
   var iskeyWordSearch = [];

   attrNames.push("collection_type");
   attrValues.push("Asset");
   levelValues.push("ANY");
   isExcludeParentMetadata.push(true);
   rowIds.push(1);
   operators.push("EQUAL");
   iskeyWordSearch.push(false);
   var attributeVal;

   if (screen.width > 990) {
      attributeVal = $("#attributeVal").val();
   } else {
      attributeVal = $("#mobileKeyworkSearchDiv").find("#attributeVal").val();
   }

   if (attributeVal) {
      attrNames.push("ANY");
      attrValues.push('%' + attributeVal.trim() + '%');
      levelValues.push("ANY");
      isExcludeParentMetadata.push(false);
      rowIds.push(2);
      operators.push("LIKE");
      iskeyWordSearch.push(true);
   }

   var rowId = 3;
   $(".filteritem:checked").each(function () {
      var attrName = $(this).parent().parent().attr('id');
      var attrVal = $(this).val();
      attrNames.push(attrName);
      levelValues.push("ANY");
      attrValues.push(attrVal);
      rowIds.push(rowId);
      isExcludeParentMetadata.push(false);
      operators.push("EQUAL");
      iskeyWordSearch.push(false);
      rowId = rowId + 1;

   });

   if (searchType == 'datasetUrl') {
      var rowId = 3;
      var attrVal = $("#dmeDataId").val();
      var attrVal1 = $("#doi").val();
      if (attrVal) {
         attrNames.push('dme_data_id');
         attrValues.push('%' + attrVal + '%');
      } else if (attrVal1) {
         attrNames.push('doi');
         attrValues.push('%' + attrVal1 + '%');
      }
      iskeyWordSearch.push(false);
      levelValues.push("Asset");
      rowIds.push(rowId);
      isExcludeParentMetadata.push(true);
      operators.push("LIKE");
   }

   search_criteria_json.attrName = attrNames.join();
   search_criteria_json.attrValuesString = attrValues.join("@@");
   search_criteria_json.rowId = rowIds.join();
   search_criteria_json.level = levelValues.join();
   search_criteria_json.isExcludeParentMetadata = isExcludeParentMetadata.join();
   search_criteria_json.iskeyWordSearch = iskeyWordSearch.join();
   search_criteria_json.operator = operators.join();

   if (screen.width > 990) {
      refreshDataTable();
   } else {
      refreshDataTableMobile();
   }

}

function refreshDataTable() {
   var isVisible = (loggedOnUserInfo ? true : false);
   console.log("refresh datatable");
   if (!$.fn.DataTable.isDataTable('#searchResultTable')) {
      dataTableInit(isVisible);
      $("div.toolbar").prepend(
         '<div id="show_search_count"></div><div id="sortingDiv" >' +
         '<span id="ascSpan" data-toggle = "tooltip" title = "Asset Name A-Z" ><img src="images/search_ascending.svg"/></span>&nbsp;&nbsp;' +
         '<span id="descSpan" data-toggle = "tooltip" title = "Asset Name Z-A"><img src="images/search_descending.svg"/></span>' +
         '</div>');
      $("div.toolbar").after(
         '<div class="col-lg-12 col-md-12 col-sm-12 float-right" style="display:inline-flex;">' +
         '<div class="col-lg-9 col-md-9 col-sm-9"><div id="selected_size_div" style="display: block;">TOTAL SELECTED SIZE: ' +
         '<span id="selected_size">0 B</span> </div><div id="informational_text">' +
         'Download up to 1 TB of data per request. Select the desired assets and click Download Selected Assets >' +
         '</div></div>' +
         '<div class="col-lg-3 col-md-3 col-sm-3 downloadSelectedDiv" style="padding-right:0px;">' +
         '<button id="downloadSelected" type="button"' +
         'class="btn btn-primary float-right" disabled>' +
         'DOWNLOAD <br />SELECTED ASSETS <img class="arrow_right_download_selected"' +
         'src="/images/white_right_arrow.svg"/>' +
         '</button></div></div>');
   } else {
      var t = $('#searchResultTable').DataTable();
      console.log(t);
      t.ajax.reload(null, true);

   }

}

function dataTableInit(isVisible) {
   $('#searchResultTable').DataTable({
      "paging": true,
      "info": true,
      "pageLength": 25,
      "responsive": true,
      "order": [
         [0, 'asc']
      ],
      "ajax": {
         "url": "/search",
         "type": "GET",
         "data": function (d) {
            d.detailed = search_criteria_json.detailed;
            d.level = search_criteria_json.level;
            d.attrName = search_criteria_json.attrName;
            d.attrValuesString = search_criteria_json.attrValuesString;
            d.rowId = search_criteria_json.rowId;
            d.isExcludeParentMetadata = search_criteria_json.isExcludeParentMetadata;
            d.iskeyWordSearch = search_criteria_json.iskeyWordSearch;
            d.operator = search_criteria_json.operator;
         },
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

      "initComplete": function () {
         $('body').tooltip({
            selector: '[data-toggle="tooltip"]'
         });
      },

      "drawCallback": function () {

         $(".expand_card_btn").click(function () {
            if ($(this).hasClass("collapsed")) {
               $(this).find('img').attr('data-original-title', 'Collapse card');
            } else {
               $(this).find('img').attr('data-original-title', 'Expand card');
            }
         });

         var table = $('#searchResultTable').DataTable();
         $("#searchResultTable thead").remove();
         if (isVisible) {
            $(".downloadSelectedDiv").show();

         } else {
            $(".downloadSelectedDiv").hide();
            $("#informational_text").hide();
            $("#selected_size_div").hide();
         }

         // display the informational text on the search results only 
         // when user is logged in and there is an option to download asset

         if (isVisible && ($("input:checkbox.selectAssetCheckBox").is(":visible") ||
               $("input.selectRadioForDataSet").is(":visible"))) {
            $("#informational_text").show();
            $("#selected_size_div").show();
         } else {
            $("#informational_text").hide();
            $("#selected_size_div").hide();
         }

         $(".assetDetailsFragment").click(function () {
            var dmeDataId = $(this).attr('dme_data_id');
            location.replace('/assetDetails?returnToSearch=true&&dme_data_id=' + dmeDataId);
         });


         $("#descSpan").click(function () {
            table.order([
               [0, 'desc']
            ]).draw();
            table.ajax.reload(null, true);
            $(this).find('img').attr('src', '/images/search_desc_selected.png');
            $(this).parent().find('#ascSpan img').attr('src', '/images/search_asc_deselected.png');
         });

         $('#ascSpan').on('click', function () {
            table.order([
               [0, 'asc']
            ]).draw();
            table.ajax.reload(null, true);
            $(this).find('img').attr('src', '/images/search_ascending.svg');
            $(this).parent().find('#descSpan img').attr('src', '/images/search_descending.svg');
         });
         $("#show_search_count").html("Showing " + getTotalSearchResultsCount() + " Results");

         $(".editCollectionMetadata").click(function () {
            $("#searchFragmentDiv").hide();
            $("#assetDetailsFragment").hide();
            $("#editCollectionFragment").show();
            $(".backToAssetDetailsBtn").hide();
            $(".backToAssetDetails_editCollection").hide();
            $(".backToSearchBtn").show();
            $(".backToSearch_editCollection").show();
            var metaDataPath = $(this).attr('metadata_path');
            var permissionsRole = $(this).attr('permissions_role');
            var collectionId = $(this).attr('collectionId');
            var fileName = $(this).attr('data-fileName');
            var selectedCollection = $(this).attr('selectedCollection');
            var assetType = $(this).attr('assetType'); 

            $("#userMetaData tbody").html("");
            $("#path").val(metaDataPath);
            $(".editCollectionSuccess").hide();
            $(".editCollectionMsg").html("");
            $(".editCollectionError").hide();
            $(".editCollectionErrorMsg").html("");
            $("#collectionId").val(collectionId);
            $("#isDataObject").val(false);
            $("#assetType").val(assetType);

            if (permissionsRole && permissionsRole == 'Owner') {
               $("#updatePermissions").show();
            } else {
               $("#updatePermissions").hide();
            }

            var params1 = {
               selectedPath: metaDataPath,
               collectionType: selectedCollection,
               refresh: false
            };
            invokeAjax('/addCollection', 'GET', params1, constructEditCollectionMetadata, null, null, null);

         });

         $(".editAccessGroupPermissions").click(function () {
            var collectionId = $(this).attr('collectionId');
            var metaDataPath = $(this).attr('metadata_path');
            var selectedCollection = $(this).attr('selectedCollection');
            var collectionName = $(this).attr('collection_name');
            var params = {
               selectedPath: metaDataPath,
               levelName: selectedCollection
            };

            $.ajax({
               type: "GET",
               url: '/getAccessgroups',
               contentType: 'application/json',
               data: params,
               beforeSend: function () {
                  $("#spinner").show();
                  $("#dimmer").show();
               },
               success: function (msg) {
                  $("#spinner").hide();
                  $("#dimmer").hide();
                  editAccessPermissions(collectionId, metaDataPath, msg, selectedCollection, collectionName);
               },
               error: function (e) {
                  console.log('ERROR: ', e);
                  $("#spinner").hide();
                  $("#dimmer").hide();
               }
            });

         });
         
         $("#downloadSelected").click(function(e) {
              downloadSelectedFunction();
         });

         $(".selectAssetCheckBox").click(function (e) {

            var table = $(e.target).closest('table').attr('id');
            var selectedCheckboxes = $('#' + table).DataTable().rows({ selected: true }).nodes().
   			to$().find("input[type=checkbox]:checked");
            var len = selectedCheckboxes.length;
            if (len >= 1) {
               $("#downloadSelected").prop("disabled", false);
            } else {
               $("#downloadSelected").prop("disabled", true);
            }

            calculateTotalSize(table);

         });

         $(".selectRadioForDataSet").click(function (e) {

            var table = $(e.target).closest('table').attr('id');
            var selectedRadioboxes = $('#' + table).DataTable().rows({ selected: true }).nodes().
   			to$().find("input[type=radio]:checked");
            var len = selectedRadioboxes.length;
            if (len >= 1) {
               $("#downloadSelected").prop("disabled", false);
            } else {
               $("#downloadSelected").prop("disabled", true);
            }
            calculateTotalSize(table);

         });

         var clipboard = new ClipboardJS('.share-link-copy-button');

         clipboard.on('success', function (e) {
            console.log(e);
            $(e.trigger).tooltip('hide').attr('data-original-title', 'Link copied to clipboard').tooltip('show');
            setTimeout(function () {
               $(e.trigger).tooltip('hide');
               $(e.trigger).attr('data-original-title', 'Copy asset link');
            }, 2000);

         });

         clipboard.on('error', function (e) {
            console.log(e);
         });

         initializeToolTips();
         initializePopover();
         displayPopover();
      },

      "columns": [{
         "data": "dataSetName",
         "render": function (data, type, row) {
            return renderDataSetName(data, type, row);
         },
      }, ],

      "dom": '<"toolbar top"lip>rt<"bottom">',

      "pagingType": "full_numbers",

      "lengthMenu": [
         [10, 25, 50, 100],
         [10, 25, 50, 100]
      ],

      "language": {
         "sLoadingRecords": "Loading...",
         "lengthMenu": "Results per Page: _MENU_",
         "zeroRecords": "Nothing found to display",
         "paginate": {
            next: '<img src="/images/paginate_right.svg"/>',
            previous: '<img src="/images/paginate_left.svg"/>'
         }
      }
   });
}

function downloadSelectedFunction() {
	var selectedPaths = [];
	var selectedCheckboxes = $('#searchResultTable').DataTable().
	rows({ selected: true }).nodes().to$().find("input[type=checkbox]:checked, input[type=radio]:checked");
	
	selectedCheckboxes.each(function() {
		selectedPaths.push($(this).attr('id'));
	});
	
	if (selectedPaths.length == 1) {
		location.replace('/downloadTab?selectedPaths=' + selectedPaths
				+ '&&downloadAsyncType=collection&&returnToSearch=true');
	} else {
		$("#downloadType").val("collectionfiles");
		location.replace('/downloadTab?selectedPaths=' + selectedPaths
				+ '&&downloadAsyncType=collectionfiles&&returnToSearch=true');
	}

 }

function renderDataSetName(data, type, row) {

   var html = "";
   var isLoggedOnuserExists = (loggedOnUserInfo ? true : false);
   var checkboxHtml = "";
   var editDataSetHtml = "";
   var editStudySetHtml = "";
   var editProgramSetHtml = "";

   if (isLoggedOnuserExists) {

      if (row.isBulkAsset == false) {
         if (isUploader && isUploader == true) {
            checkboxHtml += "<input aria-label='checkbox' data-toggle= 'tooltip' type='checkbox' id=" + row.dataSetPath + " " +
               "data-size = " + row.collectionSize + " class='selectAssetCheckBox'/>";
         } else {
            checkboxHtml += "<input aria-label='radio' data-toggle= 'tooltip' type='radio' name = 'selectRadioForDataSet' id=" + row.dataSetPath + " " +
               "data-size = " + row.collectionSize + "  class='selectRadioForDataSet'/>";
         }
      } else {
         checkboxHtml += "<input aria-label='checkbox' style='display:none;' type='checkbox' id=" + row.dataSetPath +
            " data-size = " + row.collectionSize + " class='selectAssetCheckBox'/>";
      }

      if (row.dataSetPermissionRole && row.dataSetPermissionRole != 'No Permissions') {
         editDataSetHtml = "&nbsp;<span class='editCollectionMetadata' assetType = '" + row.assetType + "' selectedCollection = 'Asset' " +
            "data-fileName = '" +
            row.dataSetName +
            "' collectionId  = '" +
            row.dataSetCollectionId +
            "' " +
            "permissions_role = '" +
            row.dataSetPermissionRole +
            "'" +
            " metadata_path  = '" +
            row.dataSetPath +
            "'>" +
            "<img class='editCollectionImg' src='images/Search_EditMetaData.svg' data-toggle='tooltip' title='Edit asset metadata' th:src='@{/images/Search_EditMetaData.svg}' " +
            "alt='edit collection'></span>&nbsp;";

         if (row.dataSetPermissionRole == 'Owner') {
            editDataSetHtml += "&nbsp;<span class='editAccessGroupPermissions' collection_name = '" +
               row.dataSetName + "' " + "collectionId  = '" + row.dataSetCollectionId + "' " +
               " selectedCollection = 'Asset' " + "metadata_path  = '" + row.dataSetPath + "'>" +
               "<img class='collectionAccessGrpImg' src='images/Search_AccessGroups.png' data-toggle='tooltip' " +
               "title='Edit access group permissions' " + "th:src='@{/images/Search_AccessGroups.png}' " +
               "alt='Edit Asset Access Permissions'></span>&nbsp;";
         }
      }


      if (row.studyPermissionRole && row.studyPermissionRole != 'No Permissions') {
         editStudySetHtml = "&nbsp;&nbsp;<span class='editCollectionMetadata' selectedCollection = 'Study' " +
            "data-fileName = '" + row.studyName + "' collectionId  = '" + row.studyCollectionId + "' " +
            "permissions_role = '" + row.studyPermissionRole + "' metadata_path  = '" + row.studyPath + "'> " +
            "<img src='images/Search_EditMetaData.svg' data-toggle='tooltip' title='Edit study metadata'" +
            "th:src='@{/images/Search_EditMetaData.svg}' class='editCollectionImg' alt='edit collection'></span>";

         if (row.studyPermissionRole == 'Owner') {
            editStudySetHtml += "&nbsp;&nbsp;<span class='editAccessGroupPermissions' collection_name ='" +
               row.studyName +
               "'" +
               " collectionId  = '" +
               row.studyCollectionId +
               "' " +
               " selectedCollection = 'Study'  " +
               "metadata_path  = '" +
               row.studyPath +
               "'>" +
               "<img class='collectionAccessGrpImg' src='images/Search_AccessGroups.png' data-toggle='tooltip' title='Edit access group permissions' " +
               "th:src='@{/images/Search_AccessGroups.png}' " +
               "alt='Edit access group permissions'</span>";
         }
      }

      if (row.programPermissionRole && row.programPermissionRole != 'No Permissions') {
         editProgramSetHtml = "&nbsp;&nbsp;<span class='editCollectionMetadata' selectedCollection = 'Program' " +
            "data-fileName = '" + row.programName + "' collectionId  = '" + row.programCollectionId + "'" +
            " permissions_role = '" + row.programPermissionRole + "' " + "metadata_path  = '" +
            row.programPath + "' >" +
            "<img src='images/Search_EditMetaData.svg' data-toggle='tooltip' title='Edit program metadata' " +
            "th:src='@{/images/Search_EditMetaData.svg}' " +
            "class='editCollectionImg' alt='edit collection'></span>";

         if (row.programPermissionRole == 'Owner') {
            editProgramSetHtml += "&nbsp;&nbsp;<span class='editAccessGroupPermissions' " +
               "collection_name ='" +
               row.programName +
               "' collectionId  = '" +
               row.programCollectionId +
               "' " +
               "selectedCollection = 'Program'  " +
               "metadata_path  = '" +
               row.programPath +
               "'>" +
               "<img src='images/Search_AccessGroups.png' data-toggle='tooltip' title='Edit access group permissions' " +
               "th:src='@{/images/Search_AccessGroups.png}' " +
               "class='collectionAccessGrpImg' alt='Edit access group permissions'</span>";
         }
      }
   }
   html += "<div class='ms-card-expanded-1 ms-card-expanded-4'>" +
      "<div class='col-lg-12 col-md-12 col-sm-12 flex-row-9'>" +
      checkboxHtml +
      "<div class='overlap-group2-1'>" +
      " <div class='frame-14'>" +
      " <div class='datasetDiv poppins-medium-congress-blue-25px'>" +
      " <a class='poppins-medium-congress-blue-25px assetDetailsFragment' dme_data_id = '" + row.dmeDataId + "'>" + row.dataSetName + "</a>" +
      " &nbsp;&nbsp;" +
      editDataSetHtml +
      "<button type='button' class='share-link-copy-button' data-toggle='tooltip' data-placement='bottom' " +
      " title='Copy asset link' data-clipboard-text='" + row.dataSetdmeDataId + "'>" +
      "<img src='images/copy_to_clipboard.svg' width='20' alt='Copy to clipboard'/></button>" +
      " </div>" +
      " </div>" +
      " </div>" +
      "<div class='ml-auto buttons_container'>" +
      "<button type='button' class='expand_card_btn'  data-toggle='collapse' href='#collapse" + row.dmeDataId + "'" +
      "><img src='images/expand_card.png' data-toggle='tooltip' title= 'Collapse card'" +
      "alt='Collapse asset details'></button>" +
      "<button type='button' class='view_asset_details_btn assetDetailsFragment' data-toggle='tooltip' data-placement='bottom' title='View asset details'" +
      " dme_data_id = '" + row.dmeDataId + "'><img src='images/view_details_arrow.png'" +
      "alt='View Asset Details'></button>" +
      "</div>" +
      "</div>" +
      "<div id='collapse" + row.dmeDataId + "' class='col-lg-12 col-md-12 col-sm-12'>" +
      " <img src='images/search_line.png' class='line_divider' alt='asset name divider'/>" +
      "<div class='asset-description_div'><div class='asset-description opensans-bold-midnight-blue-13px'>" +
      "<span class='opensans-bold-midnight-blue-13px'>ASSET DESCRIPTION: &nbsp;&nbsp;</span>" +
      "<span class='inter-normal-congress-blue-16px'>" + row.dataSetDescription + "</span></div></div>" +
      "<div class='asset-size-div'><div class='asset-size opensans-bold-midnight-blue-13px'>" +
      "<span class='opensans-bold-midnight-blue-13px'>ASSET SIZE: &nbsp;&nbsp;</span>" +
      "<span class='inter-normal-congress-blue-16px'>" + row.displayAssetSize + "</span></div></div>" +
      "<div class='study-container'><div class='study opensans-bold-midnight-blue-13px'>" +
      "<span class='opensans-bold-midnight-blue-13px'>STUDY: &nbsp;&nbsp;</span>" +
      "<a class='button2a' style='text-decoration:underline;' collection_name = '" + row.studyName + "' selected_path = '" + row.studyPath + "' collection_type='Study' tabindex='0'" +
      " data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' " +
      "data-popover-content='#a01'>" +
      "<span class='study_prog_name_class'>" + row.studyName + " </span></a>" +
      editStudySetHtml +
      "</div></div>" +
      "<div class='program-container'>" +
      "<div class='program opensans-bold-midnight-blue-13px'>" +
      "<span class='opensans-bold-midnight-blue-13px'>PROGRAM: &nbsp;&nbsp;</span>" +
      "<a class='button2a' style='text-decoration:underline;' collection_name = '" + row.programName + "' selected_path = '" + row.programPath + "' collection_type='Program' tabindex='0'" +
      "data-container='body' data-toggle='popover' data-placement='right' data-trigger='click' " +
      "data-popover-content='#a01'>" +
      "<span class='study_prog_name_class'>" + row.programName + " </span></a>" +
      editProgramSetHtml +
      "</div></div>" +
      "</div>" +
      "</div>";

   return html;
}


function filterPrevAndNextElemets($this, attributeTypeName) {

   var attributeName = $this.find('label').text();

   var rowId = 1;
   var d = {};
   var attrNames = [];
   var attrValues = [];
   var isExcludeParentMetadata = [];
   var rowIds = [];
   var operators = [];
   var levelValues =[];

   // filter a list based on the parent level selection
   $(".filteritem:checked").each(function () {
      var attrName = $(this).parent().parent().attr('id');
      var attrVal = $(this).val();
      if (attrName != attributeName) {
         levelValues.push("Asset");
         attrNames.push(attrName);
         attrValues.push(attrVal);
         rowIds.push(rowId);
         isExcludeParentMetadata.push(false);
         operators.push("EQUAL");
         rowId = rowId + 1;
      }
   });

   attrNames.push("collection_type");
   attrValues.push("Asset");
   rowIds.push(rowId);
   isExcludeParentMetadata.push(false);
   levelValues.push("Asset");
   operators.push("EQUAL");
   d.attrName = attrNames.join();
   d.attrValuesString = attrValues.join('@@');
   d.isExcludeParentMetadata = isExcludeParentMetadata.join();
   d.rowId = rowIds.join();
   d.operator = operators.join();
   d.searchName = attributeName;
   d.level = levelValues.join();
   

   $.ajax({
      url: '/getFilterList',
      type: 'GET',
      contentType: 'application/json',
      dataType: 'text',
      data: d,
      success: function (data, status) {
         var filterlist = JSON.parse(data);
         var list = filterlist.attrValues;
         var len = list.length;

         $this.parent().find('.filterGroupDiv').each(function (e) {
            var val = $(this).find('.filteritem').val();
            if (list.indexOf(val) != -1) {
               $(this).show();
               $(this).find('.showMorefields').show();
               if ($(this).find('.filteritem').is(':checked')) {
                  $(this).find('.showMorefields').addClass('filter_checked');
               } else {
                  $(this).find('.showMorefields').removeClass('filter_checked');
               }
               resetSearchFilterCount(val, $(this), filterlist);
            } else {
               $(this).hide();
               $(this).find('.showMorefields').removeClass('filter_checked');
               $(this).find('.filteritem').prop("checked", false);
            }
         });

         if (len && len > 4) {
            $this.parent().find('.css-17rpx5x').show();
            $this.parent().find('.showMore').show();
            $this.parent().find('.css-17rpx5xLess').hide();
            showFirstFewFields($this.parent(), 'Less');
         } else {
            $this.parent().find('.css-17rpx5xLess').hide();
            $this.parent().find('.css-17rpx5x').hide();
         }

      },
      error: function (data, status, error) {
         console.log("===> status: ", status);
         console.log("===> error: ", error);
         console.log("===> data: ", data);
      }
   });
}

function display(value) {
   if (value == "async") {
      $("#AsyncDiv").show();
      $("#SyncDiv").hide();
      $("#s3Div").hide();
      $("#driveDiv").hide();
      $("#googleCloudDiv").hide();
      $("#download-btn").prop("disabled", false);
      $("#download-btn").css('cursor', 'pointer');
   } else if (value == "sync") {
      $("#SyncDiv").show();
      $("#AsyncDiv").hide();
      $("#s3Div").hide();
      $("#driveDiv").hide();
      $("#googleCloudDiv").hide();
      $("#download-btn").prop("disabled", false);
      $("#download-btn").css('cursor', 'pointer');
   } else if (value == "drive") {
      $("#SyncDiv").hide();
      $("#AsyncDiv").hide();
      $("#s3Div").hide();
      $("#driveDiv").show();
      $("#googleCloudDiv").hide();
      var googleDriveIsAuthorized = $("#googleDriveIsAuthorized").val();
      if (googleDriveIsAuthorized) {
         $("#download-btn").prop("disabled", false);
         $("#download-btn").css('cursor', 'pointer');
         $("#driveAuthlink").css('pointer-events', 'none');
         $("#driveAuthlink").css('opacity', '0.65');
      } else {
         $("#download-btn").prop("disabled", true);
         $("#download-btn").css('cursor', 'default');
         $("#driveAuthlink").css('pointer-events', 'auto');
         $("#driveAuthlink").css('opacity', '1');
      }

   } else if (value == 'cloud') {
      $("#SyncDiv").hide();
      $("#AsyncDiv").hide();
      $("#s3Div").hide();
      $("#driveDiv").hide();
      $("#googleCloudDiv").show();
      var googleCloudIsAuthorized = $("#googleCloudIsAuthorized").val();
      if (googleCloudIsAuthorized) {
         $("#download-btn").prop("disabled", false);
         $("#download-btn").css('cursor', 'pointer');
         $("#googleCloudAuthlink").css('pointer-events', 'none');
         $("#googleCloudAuthlink").css('opacity', '0.65');
      } else {
         $("#download-btn").prop("disabled", true);
         $("#download-btn").css('cursor', 'default');
         $("#googleCloudAuthlink").css('pointer-events', 'auto');
         $("#googleCloudAuthlink").css('opacity', '1');
      }
   } else {
      $("#SyncDiv").hide();
      $("#AsyncDiv").hide();
      $("#s3Div").show();
      $("#driveDiv").hide();
      $("#googleCloudDiv").hide();
      $("#download-btn").prop("disabled", false);
      $("#download-btn").css('cursor', 'pointer');
   }
}

function initializeToolTips() {
   $('[data-toggle="tooltip"]').tooltip();
   $('body').tooltip({
      selector: '[data-toggle="tooltip"]'
   });
}

function initializePopover() {
   $("[data-toggle=popover]").popover({
      html: true,
      trigger: 'manual',
      container: 'body',
      placement: 'left',
      delay: {
         show: 50,
         hide: 50
      },
      content: function () {
         var content = $(this).attr("data-popover-content");
         return $(content).children(".popover-body").html();
      },
      title: function () {
         var title = $(this).attr("data-popover-content");
         return $(title).children(".popover-heading").html();
      }
   });
}


function resetSearchFilterCount(val, $this, filterlist) {

   // evaluating and displaying the new counts on click of any checkbox filters

   var attrName = $this.attr('id');
   if (val == 'Dataset') {
      $this.find(".asset_type_count").text(filterlist.datasetCount);
   } else if (val == 'Model') {
      $this.find(".asset_type_count").text(filterlist.modelCount);
   } else if (attrName == 'Is Reference Dataset' && val == 'Yes') {
      $this.find(".asset_type_count").text(filterlist.referenceDatasetCount);
   } else if (attrName == 'Is Reference Dataset' && val == 'No') {
      $this.find(".asset_type_count").text(filterlist.nonReferenceDatasetCount);
   } else if (attrName == 'Is Model Deployed' && val == 'Yes') {
      $this.find(".asset_type_count").text(filterlist.modelDeployedCount);
   } else if (attrName == 'Is Model Deployed' && val == 'No') {
      $this.find(".asset_type_count").text(filterlist.modelNotDeployedCount);
   }
}


function resetSearchFilterCountsOnRefresh() {

   //on click of clear filters icon, replacing the counts with the 
   //initial counts stored in hidden variable

   $("#datasetCount").text($("#intialDatasetCount").val());
   $("#modelCount").text($("#initialModelCount").val());
   $("#refDatasetCount").text($("#initialRefDatasetCount").val());
   $("#nonrefDatasetCount").text($("#initialNonrefDatasetCount").val());
   $("#modelDeployedCount").text($("#initialmodelDeployedCount").val());
   $("#modelNotDeployedCount").text($("#initialModelNotDeployedCount").val());

}

function resetPositionsForFilters() {

   $(".dataDivCollapse").each(function (e) {

      $(this).find(".filteritem").each(function (e) {

         // Get the parent div of the clicked checkbox
         var parentDiv = $(this).closest(".filterGroupDiv");
         var className = $(this).attr('class');
         var substring = "checkbox_";
         var numberIndex = className.indexOf(substring) + substring.length;
         var index = className.substring(numberIndex);
         var beforeDiv = $(this).closest('.dataDivCollapse').find('.checkbox_' + (index - 1)).closest('.filterGroupDiv');
         // Move the parent div back to its original position
         parentDiv.insertAfter(beforeDiv);
      });

   });

}

function showFirstFewFields($this, oper) {
   if ($this) {
      if (oper == 'More') {
         $this.find('.filterGroupDiv:visible').each(function (index) {
            $(this).find('.showMorefields').show();

         });
      } else if (oper == 'Less') {
         var len = $this.find('.filterGroupDiv:visible').length;
         if (len > 4) {
            var modifiedSize = len - 4;
            $this.find('.filterGroupDiv:visible').each(function (index) {
               if (index > 3) {
                  $(this).find('.showMorefields').hide();
               } else {
                  $(this).find('.showMorefields').show();
               }
            });
            $this.find(".showMore").html(modifiedSize + " More Items " + '<i class="fas fa-angle-right"></i>');
         } else {
            $this.find('.css-17rpx5x').hide();
         }
      } else if (oper == 'searchFilter') {
         var len = $this.find('.filterGroupDiv:visible').length;
         if (len > 4) {
            var modifiedSize = len - 4;
            $this.find('.css-17rpx5x').show();
            $this.find(".showMore").html(modifiedSize + " More Items " + '<i class="fas fa-angle-right"></i>');
         } else {
            $this.find('.css-17rpx5x').hide();
         }
      }

   } else {
      $(".dataDivCollapse").each(function (e) {
         var len = $(this).find('.filterGroupDiv:visible').length;
         var modifiedSize = len - 4;
         if (len > 4) {
            $(this).parent().find('.showMore').html(modifiedSize + " More Items " + '<i class="fas fa-angle-right"></i>');
            $(this).parent().find('.showMore').show();
            $(this).parent().find('.css-17rpx5x').show();
            $(this).parent().find('.css-17rpx5xLess').hide();
            $(this).find('.filterGroupDiv:visible').each(function (index) {
               if (index > 3) {
                  $(this).find(".showMorefields").hide();
               } else {
                  $(this).find(".showMorefields").show();
               }

            });
         } else {
            $(this).parent().find('.css-17rpx5x').hide();
            $(this).parent().find('.css-17rpx5xLess').hide();
         }

      });
   }
}

function displayPopover() {
   $('.button2a').on('click', function () {
      openPopOver($(this));

   });
   $('.button2a').on('keypress', function (e) {
      if (e.which == 13 || e.keyCode == 13) {
         openPopOver($(this));
      }
   });
}

function openPopOver($this) {
   var pop = $this;
   $('.button2a').not($this).popover('hide');
   var selectedPath = $this.attr('selected_path');
   var collection_type = $this.attr('collection_type');
   var collection_name = $this.attr('collection_name');

   var headerName = "<div class='popoverHeader'><p class='popoverInfo'>Metadata for " + collection_name + "</p>" +
      "<p class='popoverSubInfo'>" + collection_type + " Metadata</p></div>";

   if (collection_type != 'DataObject') {
      var params = {
         selectedPath: selectedPath,
         collectionType: collection_type,
         refresh: false
      };
      $
         .ajax({
            url: '/addCollection',
            type: 'GET',
            contentType: 'application/json',
            dataType: 'json',
            data: params,
            beforeSend: function () {
               $("#spinner").show();
               $("#dimmer").show();
            },
            success: function (data, status) {
               $("#spinner").hide();
               $("#dimmer").hide();
               var table = "";

               if (data.length > 0) {

                  var ind = "<div id=\"a01\" class=\"col-md-12 hidden\"> <div class=\"popover-heading\"><a class=\"button closeBtn float-right\" href=\"javascript:void(0);\"><img src='images/close_metadata_popover.svg' width='16' style='margin-top: -8px;'/></a>" +
                     headerName +
                     "</div>" +
                     "<div class='popover-body'> <div class='divTable' style='width: 100%;border: 1px solid #000;'>" +
                     "<div class='divTableBody'><div class='divTableRow'>" +
                     "<div class='divTableHead'>ATTRIBUTE</div>" +
                     "<div class='divTableHead'>VALUE</div></div>";

                  var content = "";

                  $
                     .each(
                        data,
                        function (key, value) {
                           var attrVal = value.attrValue;
                           if (attrVal &&
                              (attrVal.startsWith('https') || attrVal.startsWith('http'))) {
                              content += "<div class='divTableRow divTableContent'><div class='divTableCell divAttrName'>" +
                                 value.displayName.toUpperCase() +
                                 "</div>" +
                                 "<div class='divTableCell divAttrVal'><a target='_blank' href=" +
                                 attrVal + ">" + attrVal + "</a></div></div>";
                           } else if (value.attrName.indexOf("access_group") == -1) {
                              content += "<div class='divTableRow divTableContent'><div class='divTableCell divAttrName'>" +
                                 value.displayName.toUpperCase() +
                                 "</div>" +
                                 "<div class='divTableCell divAttrVal'>" +
                                 attrVal +
                                 "</div></div>";
                           }

                        });
                  table = ind + content + "</div> </div></div> </div>";
               } else {
                  table = "<div id=\"a01\" class=\"col-md-12 hidden\">" +
                     "<div class=\"popover-heading\"> NO USER METADATA &nbsp;&nbsp;" +
                     "<a class=\"button closeBtn float-right\" href=\"javascript:void(0);\"><img src='images/close_metadata_popover.svg' width='16' style='margin-top: -8px;'/></a> </div>" +
                     "<div class='popover-body'></div></div>";
               }

               $("#a01").remove();
               pop.after(table);
               initializePopover();
               pop.data('bs.popover').setContent();
               pop.popover('show');
            },
            error: function (data, status, error) {
               $("#spinner").hide();
               $("#dimmer").hide();
               console.log("===> data: ", data);
               console.log("===> status: ", status);
               console.log("===> error: ", error);
            }

         });
   } else {
      openDataObjectPopOver($this);
   }
}

function openDataObjectPopOver($this) {
   var pop = $this;
   $('.button2a').not($this).popover('hide');
   var fileName = $this.attr('file_name');
   var selectedPath = $this.attr('selected_path');

   var headerName = "<div class='popoverHeader'><p class='popoverInfoDatObj'>Metadata for " + fileName + "</p></div>";

   var ind = "<div id=\"a01\" class=\"col-md-12 hidden\"><div class=\"popover-heading\"><a class=\"button closeBtn float-right\" href=\"javascript:void(0);\">" +
      "<img src='images/close_metadata_popover.svg' width='16' style='margin-top: -8px;'/></a>" +
      headerName +
      " </div><div class='popover-body'>";

   var table = "";
   var content = "";

   var params = {
      selectedPath: selectedPath
   };

   $
      .ajax({
         url: '/getDataObjects/getMetadata',
         type: 'GET',
         contentType: 'application/json',
         dataType: 'json',
         data: params,
         beforeSend: function () {
            $("#spinner").show();
            $("#dimmer").show();
         },
         success: function (data, status) {
            $("#spinner").hide();
            $("#dimmer").hide();
            console.log("success status: ", status);

            if (data.selfMetadata.length > 0) {

               ind += "<p class='divDataObjUserMetadata'>User Metadata</p><div class='divTable' style='width: 100%;border: 1px solid #000;'>" +
                  "<div class='divTableBody'><div class='divTableRow'>" +
                  "<div class='divTableHead rowAttribute'>ATTRIBUTE</div>" +
                  "<div class='divTableHead'>VALUE</div></div>";

               $
                  .each(
                     data.selfMetadata,
                     function (key, value) {
                        if (value.value.startsWith('https') || value.value.startsWith('http')) {
                           content += "<div class='divTableRow divTableContent'><div class='divTableCell divAttrName'>" +
                              value.displayName.toUpperCase() +
                              "</div>" +
                              "<div class='divTableCell divAttrVal'><a target='_blank' href=" +
                              value.value + ">" + value.value + "</a></div></div>";
                        } else {
                           content += "<div class='divTableRow divTableContent'><div class='divTableCell divAttrName'>" +
                              value.displayName.toUpperCase() +
                              "</div>" +
                              "<div class='divTableCell divAttrVal'>" +
                              value.value +
                              "</div></div>";
                        }

                     });
               content += "</div> </div><br/>";

            }

            if (data.systemMetadata.length > 0) {
               content += "<p class='divDataObjSysMetadata'>Key System Metadata</p><div class='divTable' style='width: 100%;border: 1px solid #000;'>" +
                  "<div class='divTableBody'><div class='divTableRow'>" +
                  "<div class='divTableHead rowAttribute'>ATTRIBUTE</div>" +
                  "<div class='divTableHead'>VALUE</div></div>";

               $
                  .each(
                     data.systemMetadata,
                     function (key, value) {
                        content += "<div class='divTableRow divTableContent'><div class='divTableCell divAttrName'>" +
                           value.displayName.toUpperCase() +
                           "</div>" +
                           "<div class='divTableCell divAttrVal'>" +
                           value.value +
                           "</div></div>";
                     });

               content += "</div> </div>";

            }

            table += ind + content + "</div> </div></div> </div>";
            $("#a01").remove();
            pop.after(table);
            initializePopover();
            pop.data('bs.popover').setContent();
            pop.popover('show');

         },
         error: function (data, status, error) {
            $("#spinner").hide();
            $("#dimmer").hide();
            console.log("===> data: ", data);
            console.log("===> status: ", status);
            console.log("===> error: ", error);
         }

      });
}

function calculateTotalSize(table) {

   var totalSize = 0;
   var rows = $('#' + table).DataTable().rows({ selected: true }).nodes().to$();
  
   rows.find("input[type=checkbox]:checked, input[type=radio]:checked").each(function () {
      var size = parseInt($(this).attr('data-size'));
      totalSize += size;
   });

   $("#selected_size").html(addHumanReadableSize(totalSize.toString()));

   if (totalSize > 1099511627776) {
      // Get all the unchecked checkboxes in the table.
      rows.find("input[type=checkbox]:not(:checked), input[type=radio]:not(:checked)").attr('disabled', true).
      attr('data-original-title', 'You have selected 1 TB of data for download. Clear another check box to make this one available.');
   } else {
      rows.find("input[type=checkbox], input[type=radio]").attr('disabled', false).removeAttr('data-original-title');
   }
}

function addHumanReadableSize(value) {
   var base = 1000;
   var bytes = parseFloat(value);

   // When using the smallest unit no decimal point is needed, because it's
   // the exact number.
   if (bytes < base) {
      return bytes + " " + ["B", "KB", "MB", "GB", "TB"][0];
   }

   var exponent = Math.floor(Math.log(bytes) / Math.log(base));
   var unit = ["B", "KB", "MB", "GB", "TB"][exponent];

   var humanReadableSize = (bytes / Math.pow(base, exponent)).toFixed(1) + " " + unit;
   return humanReadableSize;
}

// Function to get the total search results count
function getTotalSearchResultsCount() {
   var dataTable = $('#searchResultTable').DataTable();
   return dataTable.rows({
      search: 'applied'
   }).data().length;
}

//function to place the unchecked filter item back to its original position
function resetFilterItemToOriginalPosition($this, parentDiv) {

   $this.closest('label.showMorefields').removeClass('filter_checked');

   // Filter the div elements which are checked
   var divsWithClass = $this.closest('.dataDivCollapse').find('.filter_checked');
   // Find the last div element which is checked
   var lastDivWithCheckedClass = divsWithClass.last().closest('.filterGroupDiv');
   if (lastDivWithCheckedClass.length == 0) {
      lastDivWithCheckedClass = parentDiv;
   }


   //insert after the last checked element and also in the order of the remaining elements
   var currEle = parentDiv.find('.filter_text').text();
   var smallerClassNameSibling;
   lastDivWithCheckedClass.nextAll().each(function () {
      var ele = $(this).find('.filter_text').text();
      if (currEle.localeCompare(ele) < 0) {
         smallerClassNameSibling = $(this);
         return false; // Exit the loop
      }
   });
   
   if(!smallerClassNameSibling) {
   	 // insert at the end because this element is alphabetically greater than others
   	 var lastDiv  = $this.closest('.dataDivCollapse').find('.filterGroupDiv').last();
  	 parentDiv.insertAfter(lastDiv);
   } else  {
   	 parentDiv.insertBefore(smallerClassNameSibling);
   }
}