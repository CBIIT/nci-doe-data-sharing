import json
import os
from keras.models import load_model
import numpy as np
import pandas as pd


def main(fname, model_name, testdataset):
    print('Perform Inferencing')
    test_x = np.load('/mnt/IRODsTest/'+testdataset)
    # Predict on Test data
    model = load_model('/mnt/IRODsTest/'+model_name)
    pred_probs = model.predict(np.array(test_x))
    hist_site_pred = []

    with open('/mnt/IRODsTest/histology_class_mapper.json') as json_file:
        histologyLabel = json.load(json_file)
        histologyIdtoLabel = {}
        for k, v in histologyLabel.items():
            histologyIdtoLabel[v] = k

    with open('/mnt/IRODsTest/site_class_mapper.json') as json_file:
        siteLabel = json.load(json_file)
        siteIdtoLabel = {}
        for k, v in siteLabel.items():
            siteIdtoLabel[v] = k

    for site, hist in zip(pred_probs[0], pred_probs[1]):
        hist_site_pred.append([siteIdtoLabel[np.argmax(site)], histologyIdtoLabel[np.argmax(hist)]])

    hist_site_dataframe = pd.DataFrame(hist_site_pred)
    hist_site_dataframe.to_csv(fname);
    if os.path.isfile(fname):
        print('file exists: ' + fname)
        with open(os.path.join('/mnt/IRODsTest/', fname), "w") as file1:
            file1.write(fname)
    return fname