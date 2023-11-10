#!/bin/bash
#SBATCH --output=sbatch_logs/postprocess_results/postprocess_results_%A.log
#SBATCH --job-name=postprocess_results
#SBATCH --time=30-00:00
#SBATCH --mem=8G
#SBATCH -c 2

echo "***** Beginning of script *****"
date

TAXDIR=/home/$USER/cholera-mapping-pipeline 
CONFIGDIR=Analysis/configs/
DATADIR=/home/cholerapipelinerestmp/cholera-mapping-output-1/
INTERMDIR=/home/cholerapipelinerestmp/postprocess/interm/
OUTPUTDIR=/home/cholerapipelinerestmp/postprocess/processed_outputs/
RSCRIPT=/opt/R/4.0.3/bin/Rscript

echo $RSCRIPT $TAXDIR/Analysis/R/postprocess_results.R -c $TAXDIR/ -d $TAXDIR/$CONFIGDIR/ -o $OUTPUTDIR/ -x $DATADIR/ -y $INTERMDIR/ -i FALSE -r TRUE
$RSCRIPT $TAXDIR/Analysis/R/postprocess_results.R -c $TAXDIR/ -d $TAXDIR/$CONFIGDIR/ -o $OUTPUTDIR/ -x $DATADIR/ -y $INTERMDIR/ -i FALSE -r TRUE || exit 1

echo "***** End of script *****"
date
