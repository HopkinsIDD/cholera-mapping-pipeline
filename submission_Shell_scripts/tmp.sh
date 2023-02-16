#!/bin/bash
mkdir /data/aazman1/$USER/sbatch_logs/
#SBATCH --output=/data/aazman1/$USER/sbatch_logs/marcc_run_cod.log
#SBATCH --job-name=COD
#SBATCH --time=72:00:00
#SBATCH --mem=64G
#SBATCH --account=aazman1
#SBATCH --ntasks=4
#SBATCH --partition=defq

echo "DONE"
