from flask import Flask, request
from download import *
from infer import *

app = Flask(__name__)


@app.route('/modac-routing', methods=['GET'])
def modac_routing():
    datafilepath = request.args.get("dataFilePath", None)
    modelfilepath = request.args.get("modelPath", None)
    resultPath = request.args.get("resultPath", None)
    if datafilepath is not None:
        indexofdatasetname = datafilepath.rindex("/", 0, len(datafilepath))
        testdataset = datafilepath[indexofdatasetname + 1:len(datafilepath)]
        origin = 'https://fsdmel-dsapi01t.ncifcrf.gov:7738/hpc-server/v2/dataObject' + datafilepath
        get_file_from_modac(testdataset, origin)
    if modelfilepath is not None:
        indexofmodel = modelfilepath.rindex("/", 0, len(modelfilepath))
        modelname = modelfilepath[indexofmodel + 1:len(modelfilepath)]
        modelpath = 'https://fsdmel-dsapi01t.ncifcrf.gov:7738/hpc-server/v2/dataObject' + modelfilepath
        get_file_from_modac(modelname, modelpath)

    #os.system("sbatch %s" %infer.py)
    # start the scheduler with resultPath,job id and infer file name
    main()
    origin_1 = origin = 'https://fsdmel-dsapi01t.ncifcrf.gov:7738/hpc-server/v2/dataObject' + resultPath
    return uploadPredictionsFile(origin_1, 'y_pred.txt')


if __name__ == '__main__':
    app.run()
