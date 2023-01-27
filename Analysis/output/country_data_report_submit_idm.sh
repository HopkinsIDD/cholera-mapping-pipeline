#!/bin/bash
#SBATCH --job-name=ctry_rpt
#SBATCH --output=ctry_rpt.txt
#SBATCH --time=1000:00:00
#SBATCH --mem=30G
#SBATCH --nodelist=idmodeling2
#SBATCH -c 4

echo "Beginning of script"
date
TAXDIR=/home/.../cholera-mapping-pipeline #specify here
CHOLERA_CONFIG=Analysis/configs/.../*.yml #specify here
RSCRIPT=/opt/R/4.0.3/bin/Rscript

cd $TAXDIR

$RSCRIPT -e "rmarkdown::render('Analysis/output/country_data_report.Rmd', 
                                params = list(cholera_directory = '$TAXDIR', config = '$CHOLERA_CONFIG', args = 'myarg'), 
                                output_file = ifelse(!is.null(yaml::read_yaml(paste0('$TAXDIR', '/', '$CHOLERA_CONFIG'))[['country_data_report_filename']]), 
                                                    yaml::read_yaml(paste0('$TAXDIR', '/', '$CHOLERA_CONFIG'))[['country_data_report_filename']], 
                                                    dplyr::last(stringr::str_replace(unlist(stringr::str_split('$CHOLERA_CONFIG', '/')), '.yml', ''))
                                                    ) 
                                )" || exit 1

echo "End of script"
date
