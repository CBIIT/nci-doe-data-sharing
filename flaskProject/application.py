import os
from flask import Flask, request


app = Flask(__name__)


@app.route('/modac-routing', methods=['GET'])
def modac_routing():
    print('flask API')
    datafilename = request.args.get("dataFileName", None)
    modelfilename = request.args.get("modelName", None)
    predictions_filename = request.args.get("resultFileName", None)
    print(predictions_filename)
    print(modelfilename)
    print(datafilename)
    print('submit batch job')
    #main(predictions_filename, modelfilename, datafilename)
    os.system('sbatch infer_1.sh datafilename modelfilename predictions_filename')
    return "OK"

if __name__ == '__main__':
    app.run()
