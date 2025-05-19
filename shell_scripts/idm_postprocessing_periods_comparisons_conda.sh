#!/bin/bash
#SBATCH --output=sbatch_logs/postprocess_tables_figures_period_comparison_%A.log
#SBATCH --job-name=postprocess_period_comparisons
#SBATCH --time=30-00:00
#SBATCH --mem=32G
#SBATCH -c 2
#SBATCH --nodelist=idmodeling2

echo "***** Beginning of script *****"
date

TAXDIR=/home/$USER/cholera-mapping-pipeline
OUTPUTDIR=/home/cholerapipelinerestmp/postprocess/processed_outputs/
FIGUREOUTPUT=/home/cholerapipelinerestmp/postprocess/figures/
RSCRIPT=R

export PERIOD1='2011_2015'
export PERIOD2='2016_2020'

~/miniconda3/bin/conda init
source ~/.bashrc
conda activate r4.2.2-env

Rscript $TAXDIR/Analysis/R/make_final_figures_and_tables.R -d $FIGUREOUTPUT/ -o $OUTPUTDIR/ -x $PERIOD1 -y $PERIOD2

echo "***** End of script *****"
date
