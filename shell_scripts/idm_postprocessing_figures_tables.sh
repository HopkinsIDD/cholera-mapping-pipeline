#!/bin/bash
#SBATCH --output=sbatch_logs/postprocess_results/postprocess_tables_figures_%A.log
#SBATCH --job-name=postprocess_tables_figures
#SBATCH --time=30-00:00
#SBATCH --mem=8G
#SBATCH -c 2
#SBATCH --nodelist=idmodeling2

echo "***** Beginning of script *****"
date

TAXDIR=/home/cholera_mapping_pipeline 
CONFIGDIR=Analysis/configs/
DATADIR=/home/cholerapipelinerestmp/cholera-mapping-output-1/
OUTPUTDIR=/home/cholerapipelinerestmp/postprocess/processed_outputs/
RSCRIPT=/opt/R/4.0.3/bin/Rscript

echo $RSCRIPT $TAXDIR/Analysis/R/postprocess_results_figures_and_tables.R -c $TAXDIR/ -d $TAXDIR$CONFIGDIR/ -o $OUTPUTDIR/
$RSCRIPT $TAXDIR/Analysis/R/postprocess_results_figures_and_tables.R -c $TAXDIR/ -d $TAXDIR$CONFIGDIR/ -o $OUTPUTDIR/ || exit 1


echo "***** End of script *****"
date
