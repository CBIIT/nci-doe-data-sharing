#!/bin/bash -l
#SBATCH --job-name=infer
#SBATCH --account=ncidoesvct2
#SBATCH --chdir=/home/ncidoesvct2/tc_1
echo "Starting infer script"
echo datafilename is ${1}
echo modelfilename is ${2}
echo predfilename is ${3}
echo uploadFrom is ${4}
echo outputResultsName is ${5}
source /home/ncidoesvct2/tc_1/bin/activate
python3 /home/ncidoesvct2/tc_1/tc1_infer.py ${1} ${2} ${3} ${4} ${5}
echo "Completed infer script"