#!/bin/bash
#SBATCH --job-name=ctry_rpt
#SBATCH --output=sbatch_logs/pipeline_run_%A_%a.log
#SBATCH --job-name=prd_nocvr
#SBATCH --time=30-00:00
#SBATCH --mem=30G
#SBATCH --array=0-43%3
#SBATCH --nodelist=idmodeling2
#SBATCH -c 4

echo "Beginning of script"
date
TAXDIR=/home/.../cholera-mapping-pipeline #specify here
CONFIGDIR=Analysis/...                    #specify a folder that has all the configs 
RSCRIPT=/opt/R/4.0.3/bin/Rscript

cd $TAXDIR
CONFIGNAMES=($(ls $TAXDIR/$CONFIGDIR | tr ' ' '\n'))

$RSCRIPT -e "rmarkdown::render('Analysis/output/country_data_report.Rmd', 
                                params = list(cholera_directory = '$TAXDIR', config = '$CONFIGDIR/${CONFIGNAMES[$SLURM_ARRAY_TASK_ID]}', args = 'myarg'), 
                                output_file = ifelse(!is.null(yaml::read_yaml(paste0('$TAXDIR', '/', '$CONFIGDIR/${CONFIGNAMES[$SLURM_ARRAY_TASK_ID]}'))[['country_data_report_filename']]), 
                                                    yaml::read_yaml(paste0('$TAXDIR', '/', '$CONFIGDIR/${CONFIGNAMES[$SLURM_ARRAY_TASK_ID]}'))[['country_data_report_filename']], 
                                                    dplyr::last(stringr::str_replace(unlist(stringr::str_split('$CONFIGDIR/${CONFIGNAMES[$SLURM_ARRAY_TASK_ID]}', '/')), '.yml', ''))
                                                    ) 
                                )" || exit 1

echo "End of script"
date
