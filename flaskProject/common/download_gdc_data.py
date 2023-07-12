"""
Code to validate the manifest file provided by the user and download the files in manifest using gdc API
Written by Mounica Ganta
Date: 07/02/2022
"""

import gzip
import os
import shutil
import urllib
import magic
import pandas as pd


# Function for downloading files :
def download(uuid, fileName, save_path, retry=0):
    PARAM = {
        'url-data': "https://api.gdc.cancer.gov/data/{uuid}",
        'max retry': 2,
    }
    try:
        fileName = fileName.replace("/", '')
        print("Downloading (attempt {}): {}".format(retry, uuid))
        url = PARAM['url-data'].format(uuid=uuid)

        with urllib.request.urlopen(url) as response:
            data = response.read()

        completeName = os.path.join(save_path, fileName)
        with open(completeName, 'wb+') as file:
            file.write(data)

        print("downloaded file complete")
        return completeName

    except Exception as e:
        print("Error (attempt {}): {}".format(retry, e))
        if retry <= PARAM['max retry']:
            return download(uuid, fileName, save_path, retry + 1)
        else:
            print("Failed to download file: " + fileName)
            raise Exception("Failed to download file from manifest: " + fileName)


# Function to verify if the file format is of type manifest:
def isValidManifestFile(manifestFile):
    with open(manifestFile, 'r') as myfile:
        file_content = myfile.read()
        if 'id' in file_content and 'filename' in file_content and 'md5' in file_content \
                and 'size' in file_content and 'state' in file_content:
            return "manifestFile"
        else:
            return "not a manifest file"


# Function for reading manifest file :
def read_manifest(manifest_loc):
    uuid_list = []
    with open('/mnt/IRODsTest/MoDaC_Inferencing/' + manifest_loc, 'r') as myfile:
        if myfile.readline()[0:2] != 'id':
            raise ValueError('Bad Manifest File')
        else:
            for x in myfile:
                uuid = x.split('\t')[0]
                uuid_list.append(uuid)
    return uuid_list


# Function to unzip files to the output path :
def gunzip(file_path, output_path):
    with gzip.open(file_path, "rb") as f_in, open(output_path, "wb") as f_out:
        shutil.copyfileobj(f_in, f_out)


# Function to call read manifest and download from GDC :
def run_pre_process(File, manifest_dir_save_path, output_results):
    Manifest_Loc = str(File.replace('\\', '').strip())
    print('Reading Manifest File from: ' + Manifest_Loc)
    UUIDs = read_manifest(Manifest_Loc)
    gdc_manifest = pd.read_csv('/mnt/IRODsTest/MoDaC_Inferencing/' + Manifest_Loc, sep="\t")
    gdc_manifest.set_index("id", inplace=True)

    print("UUIDs are extracted from manifest")
    if os.path.exists(manifest_dir_save_path):
        print("dir path exists, this happens only when a duplicate slurm job is running")
        raise Exception("duplicate slurm job is running.")
    os.mkdir(manifest_dir_save_path)

    Files = []

    for i in range(len(UUIDs)):
        # get the row from manifest using UUID
        u = UUIDs[i]
        row = gdc_manifest.loc[u]
        completeName = download(u, row['filename'], manifest_dir_save_path)
        file_type = magic.from_file(completeName)
        filename = os.path.basename(completeName)
        # if the file type is of type gzip compressed, unzip the files
        # to the unique directory created
        if "gzip compressed data" in file_type:
            print(file_type)
            print("unzip files")
            NewFilePath = manifest_dir_save_path + "/" + filename + "_unzippedFile"
            gunzip(completeName, NewFilePath)  # unzip to New file path
            Files.append(NewFilePath)
        else:
            NewFilePath = manifest_dir_save_path + "/" + filename
            Files.append(NewFilePath)

    print("completed download")
    # if the output file is provided, validate the number of rows in manifest
    # match the number of rows in output file
    if output_results and (len(UUIDs) != len(output_results)):
        print("length of output file is not matching the number of files in manifest")
        raise Exception("Invalid output file.")

    return Files