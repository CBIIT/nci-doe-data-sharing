from __future__ import print_function

import os
import shutil
import sys
import time

import numpy as np
import pandas as pd

from gdc_rnaseq_tool import run_pre_process

try:
    import configparser
except ImportError:
    import ConfigParser as configparser

from keras.models import Sequential, Model, model_from_json, model_from_yaml


def data_preprocess():
    final_preprocessed_file = datafilename + '_final_preprocess.tsv'
    Merged_File_Name_val = datafilename + '_merged_file.tsv'
    run_pre_process(datafilename, Merged_File_Name_val)
    while not os.path.exists(Merged_File_Name_val):
        time.sleep(1)
    df_FPKM_UQ = pd.read_csv(Merged_File_Name_val, low_memory=False, sep="\t")

    # Transpose the data
    dft_FPKM_UQ = df_FPKM_UQ.T
    # remove the two two rows and save the output
    dftm_FPKM_UQ = dft_FPKM_UQ.drop(dft_FPKM_UQ.index[0:2], axis=0)
    dftm_FPKM_UQ['submitter_id'] = dftm_FPKM_UQ.index
    # reset the index
    dftm_FPKM_UQ = dftm_FPKM_UQ.reset_index(drop=True)

    dftm_FPKM_UQ.drop(['submitter_id'], axis=1, inplace=True)
    sfeatures = dftm_FPKM_UQ.div(dftm_FPKM_UQ.sum(axis=1), axis=0)
    sfeatures = sfeatures * 1000000
    sfeatures1 = sfeatures.astype(np.float64).apply(np.log10)
    sfeatures1[sfeatures1 < 0] = 0

    sfeatures1.to_csv(final_preprocessed_file, sep='\t', index=False)
    test_X = pd.read_csv(final_preprocessed_file, sep="\t")
    test_X_np = test_X.values
    print(test_X_np.shape)
    os.remove(Merged_File_Name_val)
    return final_preprocessed_file


def run(data):
    # load json and create model
    trained_model_json = "tc1.model.json"
    json_file = open(trained_model_json, 'r')
    loaded_model_json = json_file.read()
    json_file.close()
    loaded_model_json = model_from_json(loaded_model_json)

    # load weights into new model
    trained_model_h5 = 'tc1.model.h5'
    loaded_model_json.load_weights(trained_model_h5)
    loaded_model_json.compile(loss='categorical_crossentropy',
                              optimizer='sgd',
                              metrics=['accuracy'])

    # # evaluate json loaded model on test data
    # X_train, Y_train, X_test, Y_test = bmk.load_data()
    # this reshaping is critical for the Conv1D to work
    # X_test = np.expand_dims(X_test, axis=2)
    test_X = pd.read_csv(data, sep="\t")
    test_X_np = test_X.values
    X_test = np.expand_dims(test_X_np, axis=2)
    print(X_test.shape)
    pred = loaded_model_json.predict(np.array(X_test))
    print('Prediction on test set')
    print(pred.shape)
    hm = {}
    final_pred = []
    with open('type_18_class_labels') as f:
        lines = f.readlines()
        for line in lines:
            k, v = line.split()
            hm[int(k)] = v
    for p in pred:
        val = np.argmax(p)
        if val in hm:
            final_pred.append(hm[val])
        else:
            final_pred.append("No Predictions found")

    dataframe = pd.DataFrame(final_pred)
    dataframe.to_csv(pred_name, header=None, index=False)
    shutil.rmtree("RNA-Seq")
    os.remove(data)


if __name__ == '__main__':

    try:
        #datafilename = 'gdc_manifest_20220106_005415.txt'
        #modelfilename = sys.argv[2]
        #pred_name = 'final_predictions.csv'

        datafilename = sys.argv[1]
        modelfilename = sys.argv[2]
        pred_name = sys.argv[3]

        print("data filename: " + datafilename)
        print("fname:" + pred_name)

        preprocess_final_file = data_preprocess()
        run(preprocess_final_file)

    except Exception as e:
        pred_file_name = os.path.basename(pred_name)
        error_file_name = pred_file_name + "_error.txt"
        print("error file name" + error_file_name)
        print("An exception occurred: " + str(e))
        text_file = open(error_file_name, "wt")
        text_file.write(str(e))
        text_file.close()
        shutil.move(error_file_name, '/mnt/IRODsTest/' + error_file_name)
        print("completed error file copy to mount location")
