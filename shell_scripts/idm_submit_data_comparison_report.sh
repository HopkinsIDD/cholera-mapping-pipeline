#!/bin/bash
#SBATCH --job-name=data_compare
#SBATCH --output=logs/data_comparison_report.log
#SBATCH --time=1000:00:00
#SBATCH --mem=10G
#SBATCH --nodelist=idmodeling2
#SBATCH -c 4

echo "Beginning of script"
date
TAXDIR=/home/.../cholera-mapping_pipeline #specify here
RSCRIPT=/opt/R/4.0.3/bin/Rscript

cd $TAXDIR

$RSCRIPT -e "rmarkdown::render('Analysis/output/data_comparison_report.Rmd', 
                                params=list(args = 'run-comparison')
                              )" || exit 1

echo "End of script"
date
