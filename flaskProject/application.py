from flask import Flask, request
from infer import *
import paramiko

app = Flask(__name__)


@app.route('/', methods=['GET'])
def hello():
    return "hello"


@app.route('/modac-routing', methods=['GET'])
def modac_routing():
    datafilename = request.args.get("dataFileName", None)
    modelfilename = request.args.get("modelName", None)
    predictions_filename = request.args.get("resultFileName", None)
    if datafilename is None:
        datafilename = 'test_X.npy'
    if modelfilename is None or not modelfilename:
        modelfilename = 'mt_cnn_model.h5'
    if datafilename is not None:
        downloadFile(datafilename)
    if modelfilename is not None and modelfilename:
        print("model file here:" + modelfilename)
        downloadFile(modelfilename)

    print(predictions_filename)
    print(modelfilename)
    print(datafilename)
    if predictions_filename is not None and modelfilename is not None and datafilename is not None:
        main(predictions_filename, modelfilename, datafilename)
    # subprocess.call(['python', 'infer.py', predictions_filename, modelfilename, datafilename])
    # os.system("sbatch %s" % main)
    # os.system('ssh ncidoesvct2@batch.ncifcrf.gov python < infer.py >> mylog.txt 2>&1')
    return "OK"


def downloadFile(fileName):
    # Server connection information
    # this is used when running on local
    host_name = 'fsdmel-modac01d.ncifcrf.gov'
    user_name = 'ncidoesvct2'
    password = '#'

    # Connect to remote server
    t = paramiko.Transport(host_name)
    t.connect(username=user_name, password=password)
    sftp = paramiko.SFTPClient.from_transport(t)

    # Remote file path (absolute path required)
    remote_dir = '/mnt/IRODsTest/' + fileName
    print(remote_dir)
    # Local file storage path (either absolute or relative)
    local_dir = fileName
    # Files, downloading directly
    print('Start downloading file: ' + remote_dir)
    try:
        sftp.get(remote_dir, local_dir)
        # Close connection
        t.close()

    except Exception:
        print(Exception)
        pass


if __name__ == '__main__':
    app.run()
