#!/bin/bash
#SBATCH --job-name=ctry_rpt
#SBATCH --output=sbatch_logs/cdr_%A_%a.log
#SBATCH --time=30-00:00
#SBATCH --mem=32G
#SBATCH --array=0%1
#SBATCH --nodelist=idmodeling2
#SBATCH -c 4

echo "Beginning of script"
date
TAXDIR=cholera-mapping-pipeline #specify here
CONFIGDIR=Analysis/configs #specify a folder that has all the configs 
RSCRIPT=R

cd $TAXDIR
~/miniconda3/bin/conda init
source ~/.bashrc
conda activate r4.2.2-env
CONFIGNAMES=($(ls $TAXDIR/$CONFIGDIR | tr ' ' '\n'))

Rscript  -e "rmarkdown::render('Analysis/output/country_data_report.Rmd',params = list(cholera_directory = '$TAXDIR', config = '$CONFIGDIR/${CONFIGNAMES[$SLURM_ARRAY_TASK_ID]}', args = 'myarg'),output_file =yaml::read_yaml(paste0('$TAXDIR', '/', '$CONFIGDIR/${CONFIGNAMES[$SLURM_ARRAY_TASK_ID]}'))[['file_names']][['country_data_report_filename']])"
echo "End of script"
date
