#!/bin/bash
#SBATCH --output=sbatch_logs/prd_run_%A_%a.log
#SBATCH --job-name=idm_run
#SBATCH --time=30-00:00
#SBATCH --mem=16G
#SBATCH --array=0%1
#SBATCH -c 2
#SBATCH --nodelist=idmodeling2

echo "***** Beginning of script *****"
date

TAXDIR=cholera-mapping-pipeline 
CONFIGDIR=Analysis/configs/
RSCRIPT=R

cd $TAXDIR
~/miniconda3/bin/conda init
source ~/.bashrc
conda activate r4.2.2-env
CONFIGNAMES=($(ls $TAXDIR/$CONFIGDIR | tr ' ' '\n'))
#############################STAN MODEL OPTION && OFFICIAL PRODUCTION RUN
export PRODUCTION_RUN=FALSE
export CHOLERA_SKIP_STAN=FALSE
export COVARIATE_DATABASE_PASSWORD=<YOUR PASSWORD>
#############################STAN MODEL OPTION && OFFICIAL PRODUCTION RUN
Rscript $TAXDIR/Analysis/R/set_parameters.R -c $TAXDIR/$CONFIGDIR/${CONFIGNAMES[$SLURM_ARRAY_TASK_ID]}

echo "***** End of script *****"
date
