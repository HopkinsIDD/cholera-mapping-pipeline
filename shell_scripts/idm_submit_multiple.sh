#!/bin/bash
#SBATCH --output=sbatch_logs/pipeline_run_%A_%a.log
#SBATCH --job-name=ID-XX
#SBATCH --time=30-00:00
#SBATCH --mem=30G
#SBATCH --array=0-1%2
#SBATCH -c 4
#SBATCH --nodelist=idmodeling2

echo "***** Beginning of script *****"
date

TAXDIR=/home/.../cholera-mapping-pipeline
CONFIGDIR=Analysis/.../ 
RSCRIPT=/opt/R/4.0.3/bin/Rscript

cd $TAXDIR
CONFIGNAMES=($(ls $TAXDIR/$CONFIGDIR | tr ' ' '\n'))
#############################STAN MODEL OPTION && OFFICIAL PRODUCTION RUN
export CHOLERA_SKIP_STAN=TRUE
export PRODUCTION_RUN=TRUE
#############################STAN MODEL OPTION && OFFICIAL PRODUCTION RUN
echo $RSCRIPT $TAXDIR/Analysis/R/set_parameters.R -c $TAXDIR/$CONFIGDIR/${CONFIGNAMES[$SLURM_ARRAY_TASK_ID]}
$RSCRIPT $TAXDIR/Analysis/R/set_parameters.R -c $TAXDIR/$CONFIGDIR/${CONFIGNAMES[$SLURM_ARRAY_TASK_ID]} || exit 1

echo "***** End of script *****"
date
