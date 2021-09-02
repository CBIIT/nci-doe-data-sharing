from flask import Flask, request


import wget
from infer import *

app = Flask(__name__)


@app.route('/', methods=['GET'])
def hello():
    return "hello"


@app.route('/modac-routing', methods=['GET'])
def modac_routing():
    datafilepath = request.args.get("dataFileName", None)
    modelfilepath = request.args.get("modelName", None)
    predictions_filename = request.args.get("resultFileName", None)
    if modelfilepath is None:
        modelname = 'mt_cnn_model.h5'
    testdataset = None

    if datafilepath is not None:
        wget.download('/mnt/iRODsScratch/ModaC' + datafilepath)
    if modelfilepath is not None:
        wget.download('/mnt/iRODsScratch/ModaC' + modelfilepath)
    # if resultpath is not None:
    # indexofpredictions = resultpath.rindex("/", 0, len(resultpath))
    # predictions_filename = resultpath[indexofpredictions + 1:len(resultpath)]
    # origin_1 = api_server + '/v2/dataObject' + resultpath

    print(predictions_filename)
    main(predictions_filename, modelname, datafilepath)
    os.system("sbatch %s" % main)
    return "OK"


if __name__ == '__main__':
    app.run()
