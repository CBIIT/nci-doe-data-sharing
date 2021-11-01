import os
import subprocess

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
    sbatch_command = "sbatch infer.sh " + datafilename + " " + modelfilename + " " + predictions_filename
    sbatch_response = subprocess.getoutput(sbatch_command)
    print(sbatch_response)
    job_id = sbatch_response.split(' ')[-1].strip()
    print(job_id)
    return job_id

if __name__ == '__main__':
    app.run()
