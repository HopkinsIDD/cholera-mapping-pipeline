#!/bin/bash
#SBATCH --job-name=data_compare
#SBATCH --output=data_comparison_report.txt
#SBATCH --time=1000:00:00
#SBATCH --mem=10G
#SBATCH --nodelist=idmodeling2
#SBATCH -c 4

echo "Beginning of script"
date
TAXDIR=/home/kaiyuezou/mapping_pipeline/data_comparison_report/cholera-mapping-pipeline
RSCRIPT=/opt/R/4.0.3/bin/Rscript

cd $TAXDIR

$RSCRIPT -e "rmarkdown::render('/home/kaiyuezou/mapping_pipeline/data_comparison_report/cholera-mapping-pipeline/Analysis/output/plot_data_kz_dev.Rmd', params=list(args = 'run-comparison'))" || exit 1

echo "End of script"
date