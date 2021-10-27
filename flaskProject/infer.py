import json
import os

import paramiko
from keras.models import load_model
import numpy as np
import pandas as pd


def main(fname, model_name, testdataset):
    print('Perform Inferencing')
    test_x = np.load(testdataset)
    # Predict on Test data
    model = load_model(model_name)
    pred_probs = model.predict(np.array(test_x))
    hist_site_pred = []

    with open('histology_class_mapper.json') as json_file:
        histologyLabel = json.load(json_file)
        histologyIdtoLabel = {}
        for k, v in histologyLabel.items():
            histologyIdtoLabel[v] = k

    with open('site_class_mapper.json') as json_file:
        siteLabel = json.load(json_file)
        siteIdtoLabel = {}
        for k, v in siteLabel.items():
            siteIdtoLabel[v] = k

    for site, hist in zip(pred_probs[0], pred_probs[1]):
        hist_site_pred.append([siteIdtoLabel[np.argmax(site)], histologyIdtoLabel[np.argmax(hist)]])

    hist_site_dataframe = pd.DataFrame(hist_site_pred)
    hist_site_dataframe.to_csv(fname);
    if os.path.isfile(fname):
        host_name = 'fsdmel-modac01d.ncifcrf.gov'
        user_name = 'ncidoesvct2'
        password = '#'
        # Connect to remote server
        t = paramiko.Transport(host_name)
        t.connect(username=user_name, password=password)
        sftp = paramiko.SFTPClient.from_transport(t)
        sftp.put(fname, '/mnt/IRODsTest/'+fname)
    return fname
