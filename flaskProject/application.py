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
    print(predictions_filename)
    print(modelfilename)
    print(datafilename)
    print(upload_from)
    print('submit batch job')
    if modelfilename == 'mt_cnn_model.h5':
        sbatch_command = "sbatch infer.sh " + datafilename + " " + modelfilename + " " + predictions_filename + " " + upload_from
    else:
        sbatch_command = "sbatch tc1_infer.sh " + datafilename + " " + modelfilename + " " + predictions_filename + " " + upload_from
    sbatch_response = subprocess.getoutput(sbatch_command)
    print(sbatch_response)
    job_id = sbatch_response.split(' ')[-1].strip()
    print(job_id)
    return job_id

if __name__ == '__main__':
    app.run()