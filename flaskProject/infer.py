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


def modelPredict(pred_probs, filename):
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
        hist_site_pred.append([filename, siteIdtoLabel[np.argmax(site)], histologyIdtoLabel[np.argmax(hist)]])
    return hist_site_pred


try:
    datafilename = sys.argv[1]
    modelfilename = sys.argv[2]
    pred_name = sys.argv[3]
    upload_from = sys.argv[4]
    input_file_name = '/mnt/IRODsTest/' + datafilename

    # datafilename = "MANIFEST_1.txt"
    # modelfilename = "mt_cnn_model.h5"
    # pred_name = "final.csv"
    # upload_from = "gdcData"
    print("data filename: " + datafilename)
    print("model file Name:" + modelfilename)
    print("fname:" + pred_name)

    if not os.path.isfile(input_file_name):
        raise Exception("Error in uploading input file.")

    model = load_model('/mnt/IRODsTest/' + modelfilename)
    with open('word2idx.pkl', 'rb') as f:
        word2idx = pickle.load(f)
    vocab = np.load('vocab.npy')
    vec = []
    hist_site_pred_results = []
    file_name, file_extension = os.path.splitext(datafilename)
    if tarfile.is_tarfile(input_file_name):
        print("is tar file")
        my_tar = tarfile.open(input_file_name)
        dir_name = os.path.splitext(input_file_name)[0] + "_input_data"
        if os.path.exists(dir_name):
            shutil.rmtree(dir_name, ignore_errors=True)
        os.mkdir(dir_name)
        my_tar.extractall(dir_name)
        my_tar.close()
        print("tar file extracted")
        for root, dirs, files in os.walk(dir_name):
            print("loop through files")
            for file in files:
                file = file.strip("._")
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
                print("perform predictions")
                pred_probs_list = model.predict(np.array(vec))
                hist_site_pred_results += modelPredict(pred_probs_list, file)
        # remove the directory after inferencing is complete
        if os.path.exists(dir_name):
            shutil.rmtree(dir_name, ignore_errors=True)
    elif file_extension == '.pdf':
        print("file is a pdf")
        input_txt_file = convert_PDF_to_Txt(input_file_name)
        vec = vectorSingle(input_txt_file, word2idx, vocab)
        os.remove(input_txt_file)
        pred_probs_list = model.predict(np.array(vec))
        hist_site_pred_results = modelPredict(pred_probs_list, datafilename)
    elif upload_from is not None and upload_from == "gdcData" and "manifest" in datafilename.lower() and file_extension == '.txt':
        print("manifest file")
        Files = run_pre_process(input_file_name)
        for f in Files:
            input_txt_file = convert_PDF_to_Txt(f)
            vec = vectorSingle(input_txt_file, word2idx, vocab)
            os.remove(input_txt_file)
            pred_probs_list = model.predict(np.array(vec))
            hist_site_pred_results += modelPredict(pred_probs_list, datafilename)
        shutil.rmtree("GDC-DATA", ignore_errors=True)

    else:
        print("not a tar file, pdf file or manifest file")
        vec = vectorSingle(input_file_name, word2idx, vocab)
        pred_probs_list = model.predict(np.array(vec))
        hist_site_pred_results = modelPredict(pred_probs_list, datafilename)

    headerList = ['filename', 'site', 'histology']
    hist_site_dataframe = pd.DataFrame(hist_site_pred_results)
    hist_site_dataframe.to_csv(pred_name, header=headerList, index=False)

    if os.path.isfile(pred_name):
        print('file exists ' + pred_name)
        shutil.move(pred_name, '/mnt/IRODsTest/' + pred_name)

    print("inference completed")

except Exception as e:
    shutil.rmtree("GDC-DATA", ignore_errors=True)
    pred_file_name = os.path.basename(pred_name)
    error_file_name = pred_file_name + "_error.txt"
    print("error file name" + error_file_name)
    print("An exception occurred: " + str(e))
    text_file = open(error_file_name, "wt")
    text_file.write(str(e))
    text_file.close()
    shutil.move(error_file_name, '/mnt/IRODsTest/' + error_file_name)
    print("completed error file copy to mount location")
