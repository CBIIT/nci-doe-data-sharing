import json
import os
import pickle
import re
import shutil
import string
import sys
import tarfile

from keras.models import load_model
import numpy as np
import pandas as pd

from pdfconverter import convert_PDF_to_Txt
from gdc_rnaseq_tool import run_pre_process


def clearup(document):
    document = document.translate(string.punctuation)
    document = re.sub('\(\d+.\d+\)|\d-\d|\d', '', document) \
        .replace('.', '').replace(',', '').replace(',', '').replace(':', '').replace('~', '') \
        .replace('!', '').replace('@', '').replace('#', '').replace('$', '').replace('/', '') \
        .replace('%', '').replace('(', '').replace(')', '').replace('?', '') \
        .replace('-', '').replace(';', '').replace('&quot', '').replace('&lt', '') \
        .replace('^', '').replace('"', '').replace('{', '').replace('}', '').replace('\\', '').replace('+', '') \
        .replace('&gt', '').replace('&apos', '').replace('*', '').strip().lower().split()
    return document


def vectorSingle(filename, word2_idx, vocabs):
    if os.path.isfile(filename):
        doc = open(filename, 'r', encoding="utf8").read().strip()
        doc = clearup(doc)
        max_len = 1500
        unk = len(vocabs) - 1
        # convert words to indices
        text_idx = np.zeros((1, max_len))
        # for i, sent in enumerate(doc):
        singleDocVec = [word2_idx[word] if word in word2_idx else unk for word in doc][:max_len]
        length = len(singleDocVec)
        text_idx[0, :length] = singleDocVec
        print("complete converting to indices")
        return text_idx


def getAccuracy(pred_probs, output_result_final, hist_site_pred):
    site_true_count = 0
    hist_true_count = 0
    for kk in range(len(pred_probs[0])):
        if np.argmax(pred_probs[0][kk]) == output_result_final[kk][0]:
            site_true_count += 1
    for j in range(len(pred_probs[1])):
        if np.argmax(pred_probs[1][j]) == output_result_final[j][1]:
            hist_true_count += 1
    print(site_true_count * 100 / pred_probs[0].shape[0])
    print(hist_true_count * 100 / pred_probs[1].shape[0])
    hist_site_pred.append(["The accuracy for site is :", str(site_true_count * 100 / pred_probs[0].shape[0]) + "%"])
    hist_site_pred.append(["The accuracy for histology is :", str(hist_true_count * 100 / pred_probs[1].shape[0]) + "%"])


def modelPredict(vector_results, filenames, output_result):
    hist_site_pred = []
    pred_probs = model.predict(vector_results)

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

    for filename, site, hist in zip(filenames, pred_probs[0], pred_probs[1]):
        if isinstance(filenames, list):
            f_name = os.path.basename(filename)
        else:
            f_name = datafilename
        hist_site_pred.append([f_name, siteIdtoLabel[np.argmax(site)], histologyIdtoLabel[np.argmax(hist)]])

    if output_results:
        getAccuracy(pred_probs, output_result, hist_site_pred)
    return hist_site_pred


try:
    datafilename = sys.argv[1]
    modelfilename = sys.argv[2]
    pred_name = sys.argv[3]
    upload_from = sys.argv[4]
    input_file_name = '/mnt/IRODsTest/' + datafilename
    output_file = sys.argv[5]
    output_results = []

    file_name, file_extension = os.path.splitext(datafilename)
    manifest_dir_name = 'GDC-DATA_' + file_name

    print("data filename: " + datafilename)
    print("model file Name:" + modelfilename)
    print("fname:" + pred_name)
    print("upload_from:" + upload_from)
    print("output_file:" + output_file)

    if not os.path.isfile(input_file_name):
        raise Exception("Error in uploading input file.")

    # to calculate the accuracy, read the output file from user if given, else skip this test:
    if output_file is not None and output_file != 'None':
        with open('site_class_mapper.json') as json_file:
            siteLabelRev = json.load(json_file)
            siteIdtoLabelRev = {}
            for k, v in siteLabelRev.items():
                siteIdtoLabelRev[k] = v

        with open('histology_class_mapper.json') as json_file:
            histologyLabelRev = json.load(json_file)
            histologyIdtoLabelRev = {}
            for k, v in histologyLabelRev.items():
                histologyIdtoLabelRev[k] = v

        header_list = ["Site", "Histology"]
        df1 = pd.read_csv('/mnt/IRODsTest/' + output_file, names=header_list)
        for ind in df1.index:
            site = siteIdtoLabelRev[df1['Site'][ind]]
            ex = str(float(df1['Histology'][ind])).strip()
            hist = histologyIdtoLabelRev[ex]
            output_results.append([site, hist])

    model = load_model('/mnt/IRODsTest/' + modelfilename)

    with open('word2idx.pkl', 'rb') as f:
        word2idx = pickle.load(f)
    vocab = np.load('vocab.npy')

    if tarfile.is_tarfile(input_file_name):
        print("is tar file")
        my_tar = tarfile.open(input_file_name)
        dir_name = file_name + "_input_data"
        if os.path.exists(dir_name):
            shutil.rmtree(dir_name, ignore_errors=True)
        os.mkdir(dir_name)
        my_tar.extractall(dir_name)
        my_tar.close()
        print("tar file extracted")
        vec_final = []
        Files = []
        for root, dirs, files in os.walk(dir_name):
            print("loop through files")
            for file in files:
                file = file.strip("._")
                Files.append(file)
                file_n, file_ext = os.path.splitext(file)
                if file_ext == '.pdf':
                    print("is pdf")
                    file_txt = convert_PDF_to_Txt(os.path.join(root, file))
                    print("call vector single method")
                    vec = vectorSingle(file_txt, word2idx, vocab)
                    os.remove(file_txt)
                else:
                    print("call vector single method")
                    vec = vectorSingle(os.path.join(root, file), word2idx, vocab)
                vec_final.append(list(vec.T))
        print("perform predictions")
        hist_site_pred_results = modelPredict(np.array(vec_final), Files, output_results)
        # remove the directory after inferencing is complete
        if os.path.exists(dir_name):
            shutil.rmtree(dir_name, ignore_errors=True)
    elif file_extension == '.pdf':
        print("file is a pdf")
        input_txt_file = convert_PDF_to_Txt(input_file_name)
        vec = vectorSingle(input_txt_file, word2idx, vocab)
        os.remove(input_txt_file)
        hist_site_pred_results = modelPredict(vec, datafilename, output_results)
    elif upload_from is not None and upload_from == "gdcData" and "manifest" in datafilename.lower() and file_extension == '.txt':
        print("manifest file")
        Files = run_pre_process(datafilename, manifest_dir_name, output_results)
        vec_final = []
        for i in range(len(Files)):
            f = Files[i]
            input_txt_file = convert_PDF_to_Txt(f)
            vec = vectorSingle(input_txt_file, word2idx, vocab)
            print("vec shape", vec.shape)
            vec_final.append(list(vec.T))
            os.remove(input_txt_file)
        hist_site_pred_results = modelPredict(np.array(vec_final), Files, output_results)
        shutil.rmtree(manifest_dir_name, ignore_errors=True)

    else:
        print("not a tar file, pdf file or manifest file")
        vec = vectorSingle(input_file_name, word2idx, vocab)
        hist_site_pred_results = modelPredict(vec, datafilename, output_results)

    headerList = ['Filename', 'Site', 'Histology']
    hist_site_dataframe = pd.DataFrame(hist_site_pred_results)
    hist_site_dataframe.to_csv(pred_name, header=headerList, index=False)

    if os.path.isfile(pred_name):
        print('file exists ' + pred_name)
        shutil.move(pred_name, '/mnt/IRODsTest/' + pred_name)

    print("inference completed")

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
