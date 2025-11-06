#!/bin/bash
#SBATCH --output=sbatch_logs/postprocess_results_%A.log
#SBATCH --job-name=postprocess_results
#SBATCH --time=30-00:00
#SBATCH --mem=8G
#SBATCH -c 2
#SBATCH --nodelist=idmodeling2

echo "***** Beginning of script *****"
date

TAXDIR=/home/$USER/cholera-mapping-pipeline 
CONFIGDIR=Analysis/configs
DATADIR=/home/cholerapipelinerestmp/cholera-mapping-output-1/
INTERMDIR=/home/cholerapipelinerestmp/postprocess/interm/
OUTPUTDIR=/home/cholerapipelinerestmp/postprocess/processed_outputs/
RSCRIPT=R

~/miniconda3/bin/conda init
source ~/.bashrc
conda activate r4.2.2-env

Rscript $TAXDIR/Analysis/R/postprocess_results.R -c $TAXDIR/ -d $TAXDIR/$CONFIGDIR/ -o $OUTPUTDIR/ -x $DATADIR/ -y $INTERMDIR/ -i FALSE -r TRUE

echo "***** End of script *****"
date
