#!/bin/bash -l
#SBATCH --job-name=infer
#SBATCH --account=ncidoesvct2
#SBATCH --chdir=/home/ncidoesvct2
echo "Starting infer script"
echo datafilename is ${1}
echo modelfilename is ${2}
echo predfilename is ${3}
echo uploadFrom is ${4}
source /home/ncidoesvct2/mt-cnn/bin/activate
python /home/ncidoesvct2/mt-cnn_infer.py ${1} ${2} ${3} ${4}
echo "Completed infer script"