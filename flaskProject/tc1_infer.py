from __future__ import print_function

import os
import shutil
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd

from gdc_rnaseq_tool import run_pre_process

try:
    import configparser
except ImportError:
    import ConfigParser as configparser

from keras.models import Sequential, Model, model_from_json, model_from_yaml


def data_preprocess(isManifestFile):
    print("preprocess manifest file")
    Merged_File_Name_val = datafilename + '_merged_file.tsv'
    gene_map = pd.read_csv("gencode.txt", sep='\t')
    gene_map = gene_map[['gene_id', 'gene_name']]
    gene_map = gene_map.set_index('gene_id')

    if isManifestFile == "false":
        print("not a manifest file")
        Matrix = {}
        p = Path(datafilename)
        FileNames.append(datafilename)
        Name = str(p.name).replace('.tsv', '')
        print("Name is: " + Name)
        Counts_DataFrame = pd.read_csv(input_file, sep='\t', header=None, names=['GeneId', Name])
        Matrix[Name] = tuple(Counts_DataFrame[Name])
    else:
        print("a manifest file")
        Files = run_pre_process(datafilename, manifest_dir_name, output_results)
        Matrix = {}
        for file in Files:
            p = Path(file)
            FileNames.append(p.name.replace('_unzippedFile', ''))
            Name = str(p.name).replace('.tsv', '')
            print("Name is: " + Name)
            Counts_DataFrame = pd.read_csv(file, sep='\t', header=None, names=['GeneId', Name])
            Matrix[Name] = tuple(Counts_DataFrame[Name])

    if len(Matrix) > 0:
        print('Creating merged file')
        Counts_Final_Df = pd.DataFrame(Matrix, index=tuple((Counts_DataFrame['GeneId'])))
        Counts_Final_Df = gene_map.merge(Counts_Final_Df, how='outer', left_index=True, right_index=True)
        Counts_Final_Df.to_csv(Merged_File_Name_val, sep='\t', index=True)

    while not os.path.exists(Merged_File_Name_val):
        time.sleep(1)
    preprocess(Merged_File_Name_val)


def preprocess(Merged_File_Name_val):
    shutil.rmtree(manifest_dir_name, ignore_errors=True)
    final_preprocessed_file = datafilename + '_final_preprocess.tsv'
    df_FPKM_UQ = pd.read_csv(Merged_File_Name_val, low_memory=False, sep="\t")
    # Transpose the data
    dft_FPKM_UQ = df_FPKM_UQ.T
    dftm_FPKM_UQ = dft_FPKM_UQ.drop(dft_FPKM_UQ.index[0:2], axis=0)
    # # reset the index
    dftm_FPKM_UQ = dftm_FPKM_UQ.reset_index(drop=True)
    sfeatures = dftm_FPKM_UQ.div(dftm_FPKM_UQ.sum(axis=1), axis=0)
    sfeatures = sfeatures * 1000000
    sfeatures1 = sfeatures.astype(np.float64).apply(np.log10)
    sfeatures1[sfeatures1 < 0] = 0
    sfeatures1.to_csv(final_preprocessed_file, sep='\t', index=False)
    test_X = pd.read_csv(final_preprocessed_file, sep="\t")
    test_X_np = test_X.values
    print(test_X_np.shape)
    os.remove(Merged_File_Name_val)
    run(final_preprocessed_file)


def run(data):
    # load json and create model
    trained_model_json = "tc1.model.json"
    json_file = open('/mnt/IRODsTest/' +trained_model_json, 'r')
    loaded_model_json = json_file.read()
    json_file.close()
    loaded_model_json = model_from_json(loaded_model_json)

    # load weights into new model
    trained_model_h5 = 'tc1.model.h5'
    loaded_model_json.load_weights('/mnt/IRODsTest/' +trained_model_h5)
    loaded_model_json.compile(loss='categorical_crossentropy',
                              optimizer='sgd',
                              metrics=['accuracy'])

    # evaluate json loaded model on test data
    test_X = pd.read_csv(data, sep="\t")
    test_X_np = test_X.values
    # this reshaping is critical for the Conv1D to work
    X_test = np.expand_dims(test_X_np, axis=2)
    print(X_test.shape)
    pred = loaded_model_json.predict(np.array(X_test))
    print(pred.shape)

    final_pred = []
    for idx, p in enumerate(pred):
        val = np.argmax(p)
        if val in hm:
            final_pred.append([FileNames[idx], hm[val]])
        else:
            final_pred.append([FileNames[idx], "No Predictions found"])

    if output_results:
        dataframe = pd.DataFrame(output_results)
        y = np.array(dataframe, dtype='int')
        print("y")
        print(y.shape)
        input_shape = y.shape
        if input_shape and input_shape[-1] == 1 and len(input_shape) > 1:
            input_shape = tuple(input_shape[:-1])
        y = y.ravel()
        num_classes = pred.shape[1]
        if not num_classes:
            num_classes = np.max(y) + 1
        n = y.shape[0]
        categorical = np.zeros((n, num_classes), dtype=np.float32)
        categorical[np.arange(n), y] = 1
        output_shape = input_shape + (num_classes,)
        categorical = np.reshape(categorical, output_shape)
        score_json = loaded_model_json.evaluate(np.array(X_test), np.array(categorical), verbose=0)
        print("accuracy is: %.2f%%" % (score_json[1] * 100))
        final_pred.append(["The score of the model is: ", str(score_json[0])])
        final_pred.append(["The accuracy of the model is: ", str(score_json[1] * 100) + "%"])

    headerList = ['Filename', 'Tumor Type']
    dataframe = pd.DataFrame(final_pred)
    dataframe.to_csv(pred_name, header=headerList, index=False)
    os.remove(data)

    if os.path.isfile(pred_name):
        print('file exists ' + pred_name)
    shutil.move(pred_name, '/mnt/IRODsTest/' +pred_name)

    print("inference completed")


if __name__ == '__main__':
    try:
        datafilename = sys.argv[1]
        modelfilename = sys.argv[2]
        pred_name = sys.argv[3]
        input_type = sys.argv[4]
        output_file = sys.argv[5]
        # output_file = 'test_Y.csv'
        print("data filename: " + datafilename)
        print("fname:" + pred_name)
        print("input_type: " + input_type)
        input_file = datafilename
        file_name, file_extension = os.path.splitext(datafilename)
        manifest_dir_name = 'GDC-DATA_' + file_name

        FileNames = []
        output_results = []
        hm_rev = {}
        hm = {}

        if not os.path.isfile(input_file):
            raise Exception("Error in uploading input file.")

        with open('type_18_class_labels') as f:
            lines = f.readlines()
            for line in lines:
                k, v = line.split()
                hm[int(k)] = v
                hm_rev[v] = int(k)

        if output_file is not None and output_file != 'None':
            print("output file is not empty: " + output_file)
            header_list = ["Name"]
            df1 = pd.read_csv(output_file, names=header_list)
            for ind in df1.index:
                if df1['Name'][ind] in hm_rev:
                    output_results.append(hm_rev[df1['Name'][ind]])

        if input_type is not None and input_type == "gdcData":
            if "manifest" in datafilename.lower() and file_extension == '.txt':
                data_preprocess("true")
            elif file_extension == '.tsv' or file_extension == '.csv' or file_extension == '.txt':
                data_preprocess("false")
            else:
                raise Exception("Invalid file format")

        else:
            if file_extension == '.tsv' or file_extension == '.csv' or file_extension == '.txt':
                data_preprocess("false")
            else:
                raise Exception("Invalid file format")

    except Exception as e:
        shutil.rmtree(manifest_dir_name, ignore_errors=True)
        pred_file_name = os.path.basename(pred_name)
        error_file_name = pred_file_name + "_error.txt"
        print("error file name" + error_file_name)
        print("An exception occurred: " + str(e))
        error_msg = str(e)
        if "possible malformed input file" in error_msg:
            error_msg = "Invalid input file."
        text_file = open(error_file_name, "wt")
        text_file.write(error_msg)
        text_file.close()
        shutil.move(error_file_name, '/mnt/IRODsTest/' + error_file_name)
        print("completed error file copy to mount location")
