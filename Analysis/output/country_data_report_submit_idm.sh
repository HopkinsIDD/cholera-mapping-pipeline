#!/bin/bash
#SBATCH --job-name=UGA
#SBATCH --output=UGA_p.txt
#SBATCH --time=1000:00:00
#SBATCH --mem=30G
#SBATCH --nodelist=idmodeling2
#SBATCH -c 4

echo "Beginning of script"
date
TAXDIR=/home/kaiyuezou/mapping_pipeline/new_stan_testing/cholera-mapping-pipeline
RSCRIPT=/opt/R/4.0.3/bin/Rscript

cd $TAXDIR

$RSCRIPT -e "rmarkdown::render('Analysis/output/country_data_report.Rmd', params=list(args = 'run-comparison'))" || exit 1

echo "End of script"
date