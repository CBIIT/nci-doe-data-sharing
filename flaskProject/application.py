import os

from flask import Flask, request

app = Flask(__name__)


@app.route('/modac-routing', methods=['GET'])
def modac_routing():
    print('flask API')
    datafilename = request.args.get("dataFileName", None)
    modelfilename = request.args.get("modelName", None)
    predictions_filename = request.args.get("resultFileName", None)
    if modelfilename is None or not modelfilename:
        modelfilename = 'mt_cnn_model.h5'
    print(predictions_filename)
    print(modelfilename)
    print(datafilename)
    print('submit batch job')
    #main(predictions_filename, modelfilename, datafilename)
    os.system('ssh ncidoesvct2@batch.ncifcrf.gov /home/ncidoesvct2/mt-cnn/bin/python < infer.py >> infer_log.txt 2>&1')
    return "OK"

if __name__ == '__main__':
    app.run()