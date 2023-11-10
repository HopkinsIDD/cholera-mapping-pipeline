#!/bin/bash
#SBATCH --output=sbatch_logs/postprocess_results/postprocess_tables_figures_period_comparison_%A.log
#SBATCH --job-name=postprocess_period_comparisons
#SBATCH --time=30-00:00
#SBATCH --mem=8G
#SBATCH -c 2

echo "***** Beginning of script *****"
date

TAXDIR=/home/$USER/cholera-mapping-pipeline
OUTPUTDIR=/home/cholerapipelinerestmp/postprocess/processed_outputs/
FIGUREOUTPUT=/home/cholerapipelinerestmp/postprocess/figures/
RSCRIPT=/opt/R/4.0.3/bin/Rscript

export PERIOD1='2011_2015'
export PERIOD2='2016_2020'

echo $RSCRIPT $TAXDIR/Analysis/R/make_final_figures_and_tables.R -d $FIGUREOUTPUT/ -o $OUTPUTDIR/ -x $PERIOD1 -y $PERIOD2
$RSCRIPT $TAXDIR/Analysis/R/make_final_figures_and_tables.R -d $FIGUREOUTPUT/ -o $OUTPUTDIR/ -x $PERIOD1 -y $PERIOD2 || exit 1

echo "***** End of script *****"
date
