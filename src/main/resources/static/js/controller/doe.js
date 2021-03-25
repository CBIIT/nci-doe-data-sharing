/**
*
* @param url
* @param type
* @param params
* @param successCallback
* @param failureCallback
* @param contentType
* @param dataType
*
*/
function invokeAjax(url, type, params, successCallback, failureCallback, contentType, dataType) {
   if (!contentType) {
       contentType = 'application/json';
   }
   if (!dataType) {
       dataType = 'json';
   }

   return $.ajax({
       url: url,
       type: type,
       contentType: contentType,
       dataType: dataType,
       data: params,
       beforeSend: function () {
    	   $("#spinner").show();
           $("#dimmer").show();
           logAjaxCall('invokeAjax', url, params);
       },
       success: function (data, status) {
    	   $("#spinner").hide();
           $("#dimmer").hide();
           if (successCallback) {
               successCallback(data, status);
           }
       },
       error: function (data, status, error) {
    	   $("#spinner").hide();
	         $("#dimmer").hide();
           handleAjaxError(url, params, status, error, data);
           if (failureCallback) {
               failureCallback(url, params, status, error, data);
           }
       }

   });
}

/**
*
* @param context
* @param url
* @param params
*/
function logAjaxCall(context, url, params) {
   console.log("===> ::" + context + ":: ajax call to URL " + url);
   if (params) {
       console.log("====> additional params ", params);
   }
}

/**
*
* @param url
* @param params
* @param status
* @param error
* @param data
*/
function handleAjaxError(url, params, status, error, data) {
   // TODO: we need better handling of ajax errors - consider SentryIO or similar logging package.
   console.log("Ajax error in URL '" + url + "'");
   if (params != null) {
       console.log("===> params: ", params);
   }
   console.log("===> status: ", status);
   console.log("===> error: ", error);
   console.log("===> data: ", data);
}


function loadJsonData(url, selectTarget, emptyOption, params, successCallback, failureCallback, valueField, textField) {

    return $.ajax({
        url: url,
        type: 'GET',
        contentType: 'application/json',
        dataType: 'json',
        data: params,
        beforeSend: function () {
        	 $("#spinner").show();
             $("#dimmer").show();
            logAjaxCall('loadJsonData', url, params);
        },
        success: function (data, status) {
        	$("#spinner").hide();
            $("#dimmer").hide();
            
            var $select = (selectTarget instanceof jQuery) ? selectTarget : $('#' + selectTarget);

           
            $select.empty();
            if (emptyOption) {
                $select.append($('<option></option>').attr('value', "ANY").text("Select"));
            }

            var prog = $("#institutePath").val();
        	var study= $("#studyPath").val();
        	var asset = $("#datafilePath").val();
        	
            for (var i = 0; i < data.length; i++) {
            	  var value = data[i][valueField];
                 if(value == prog || value == study || value == asset) {
                	 $select.append($('<option selected></option>').attr('value', data[i][valueField]).text(data[i][textField]));
                 } else {
                	 $select.append($('<option></option>').attr('value', data[i][valueField]).text(data[i][textField]));
                 }
                 
                
            }
            $select.select2();
            $("#registerCollectionModal").find("#accessGroupSelect").next(".select2-container").hide();
            if (successCallback) {
                successCallback(data, status);
            }
        },
        error: function (data, status, error) {
        	$("#spinner").hide();
	         $("#dimmer").hide();
            handleAjaxError(url, params, status, error, data);
            if (failureCallback) {
                failureCallback();
            }
        }

    });
}

function postFailureFunction(url, params, status, error, data) {
	returnErrorMessage(data);
}

function returnErrorMessage(data) {
	if(data) {
	    var errorJson = JSON.parse(data.responseText);
	    return bootbox.alert(errorJson.message);
	} else {
		bootbox.alert("Unknown Error. Contact Technical Support!");
	}
}