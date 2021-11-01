#!/bin/sh
# This file is called ~/infer.sh
#SBATCH --output=slurm.out     # file to collect standard output
#SBATCH --error=slurm.err   # file to collect standard errors
#SBATCH -o myfile.out

source mt-cnn/bin/activate
python /home/ncidoesvct2/infer.py %1 %2 %3