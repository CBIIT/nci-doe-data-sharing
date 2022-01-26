import gzip
import os
import shutil
import urllib
import magic
import pandas as pd


# Function for downloading files :
def download(uuid, fileName, save_path, retry=0):
    PARAM = {
        # URL
        'url-data': "https://api.gdc.cancer.gov/data/{uuid}",
        # Persistence upon error
        'max retry': 2,
    }
    try:
        fileName = fileName.replace("/", '')
        print("Downloading (attempt {}): {}".format(retry, uuid))
        url = PARAM['url-data'].format(uuid=uuid)

        with urllib.request.urlopen(url) as response:
            data = response.read()

        completeName = os.path.join(save_path, fileName)
        # if not os.path.exists(save_path):
        #     os.mkdir(save_path)

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


# Function for reading manifest file :
def read_manifest(manifest_loc):
    uuid_list = []
    with open('/mnt/IRODsTest/' + manifest_loc, 'r') as myfile:
        if myfile.readline()[0:2] != 'id':
            raise ValueError('Bad Manifest File')
        else:
            for x in myfile:
                uuid = x.split('\t')[0]
                uuid_list.append(uuid)
    return uuid_list


# Function that unpacks gz files into another directory :
def gunzip(file_path, output_path):
    with gzip.open(file_path, "rb") as f_in, open(output_path, "wb") as f_out:
        shutil.copyfileobj(f_in, f_out)


def run_pre_process(File, manifest_dir_save_path, output_results):
    Manifest_Loc = str(File.replace('\\', '').strip())
    print('Reading Manifest File from: ' + Manifest_Loc)
    UUIDs = read_manifest(Manifest_Loc)
    gdc_manifest = pd.read_csv('/mnt/IRODsTest/' + Manifest_Loc, sep="\t")
    gdc_manifest.set_index("id", inplace=True)

    print("UUIDs are extracted from manifest")
    if os.path.exists(manifest_dir_save_path):
        print("dir path exists, this happens only when a duplicate slurm job is running")
        raise Exception("duplicate slurm job is running.")
    os.mkdir(manifest_dir_save_path)

    Files = []

    for i in range(len(UUIDs)):
        u = UUIDs[i]
        row = gdc_manifest.loc[u]
        completeName = download(u, row['filename'], manifest_dir_save_path)
        file_type = magic.from_file(completeName)
        filename = os.path.basename(completeName)
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
    if output_results and (len(UUIDs) != len(output_results)):
        print("length of output file is not matching the number of files in manifest")
        raise Exception("Invalid output file.")

    return Files
