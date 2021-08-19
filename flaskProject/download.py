import json
import urllib
import requests
import os

modac_user = "ncidoesvct2"
modac_pass = "Cancer$1wApr24t"


def get_file_from_modac(fname, origin):
    print('Downloading data for ' + origin)

    data = {}
    headers = {'Content-Type': 'application/json'}
    auth = (modac_user, modac_pass)
    post_url = origin + '/download'
    print("Downloading: " + post_url + " ...")
    response = requests.post(post_url, data=json.dumps(data), headers=headers, auth=auth, stream=True, verify=False)
    if response.status_code != 200:
        print("Error in download")
        raise Exception("Response code: {0}, Response message: {1}".format(response.status_code, response.text))
    print(response.status_code)
    block_size = 1024
    with open(fname, 'wb') as file:
        for data in response.iter_content(block_size):
            file.write(data)
    print('File downloaded successfully' + fname)
    return fname


def uploadPredictionsFile(origin, y_pred, file_path=None):
    print('Registering the file' +y_pred)
    register_url = origin
    params = {'generateUploadRequestURL': 'true'}
    files = {}
    files['dataObjectRegistration'] = ('attributes', json.dumps(params), 'application/json')
    files['dataObject'] = (file_path, open(y_pred, 'rb'), 'application/octet-stream')

    auth = (modac_user, modac_pass)
    headers = {}

    response = requests.put(register_url, headers=headers, auth=auth, files=files, verify=False)
    if response.status_code != 200:
        print(response.headers)
        print(response.text)
        print("Error registering file")
        raise Exception("Response code: {0}, Response message: {1}".format(response.status_code, response.text))

    print(response.text, response.status_code)
    return response.status_code
