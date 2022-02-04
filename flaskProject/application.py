"""
This file is used to call the Flask API from ModaC scheduler
"""

import subprocess

from flask import Flask, request

app = Flask(__name__)


@app.route('/modac-routing', methods=['GET'])
def modac_routing():
    print('flask API')
    datafilename = request.args.get("dataFileName", None)
    modelfilename = request.args.get("modelName", None)
    predictions_filename = request.args.get("resultFileName", None)
    upload_from = request.args.get("uploadFrom", None)
    output_results_name = request.args.get("outputResultsName", None)
    print(predictions_filename)
    print(modelfilename)
    print(datafilename)
    print(upload_from)
    print(output_results_name)
    print('submit batch job')
    if modelfilename == 'mt_cnn_model.h5':
        sbatch_command = "sbatch mt_cnn_infer.sh " + datafilename + " " + modelfilename + " " + predictions_filename + " " + upload_from + " " + output_results_name
    else:
        sbatch_command = "sbatch tc1_infer.sh " + datafilename + " " + modelfilename + " " + predictions_filename + " " + upload_from + " " + output_results_name
    sbatch_response = subprocess.getoutput(sbatch_command)
    print(sbatch_response)
    job_id = sbatch_response.split(' ')[-1].strip()
    print(job_id)
    return job_id
