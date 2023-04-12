#!/bin/bash
#SBATCH --job-name=continent
#SBATCH --output=continent-log.txt
#SBATCH --time=1000:00:00
#SBATCH --mem=30G
#SBATCH --nodelist=idmodeling2
#SBATCH -c 4

echo "Beginning of script"
date
TAXDIR=/home/kaiyuezou/mapping_pipeline/tmp_config/cholera-mapping-pipeline
RSCRIPT=/opt/R/4.0.3/bin/Rscript
CONFIG_DIR=Analysis/configs/production_tests/ptc_2023_13-16/continent_tmp
DATA_DIR=Analysis/data

cd $TAXDIR

$RSCRIPT -e "rmarkdown::render('Analysis/output/continent_map_report.Rmd', 
                                params=list(
                                  cholera_directory = '$TAXDIR', 
                                  config_directory = '$CONFIG_DIR', 
                                  output_dir = '$DATA_DIR', 
                                  map_output_dir = 'Analysis/output/continent_map', 
                                  dropped_years = NULL,  
                                  separate_TZA = NULL, 
                                  tmp_dropped_countries = NULL, 
                                  args = 'continent'), 
                                output_file = paste0('$TAXDIR', '/Analysis/output/continent_map/continent_map_table.html'))" || exit 1

echo "End of script"
date
