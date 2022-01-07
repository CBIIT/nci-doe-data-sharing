#!/bin/bash -l
#SBATCH --job-name=infer
#SBATCH --account=ncidoesvct2
#SBATCH --chdir=/home/ncidoesvct2
echo "Starting infer script"
echo datafilename is ${1}
echo modelfilename is ${2}
echo predfilename is ${3}
source /home/ncidoesvct2/tc-1/bin/activate
python /home/ncidoesvct2/tc1_infer.py ${1} ${2} ${3}
echo "Completed infer script"