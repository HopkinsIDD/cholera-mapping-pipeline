#!/bin/bash
#SBATCH --job-name cholera_mapping_single_year_configs_1cov
#SBATCH --mem=10G
#SBATCH --time=10000:00:00
#SBATCH --ntasks=1
#SBATCH --array=0-34%20
#SBATCH --nodelist=idmodeling2

echo "Beginning of script"
date
TAXDIR=/home/jkaminsky/git/cholera-mapping-pipeline/
CONFIGDIR=$TAXDIR/Analysis/configs/
RSCRIPT=/opt/R/4.0.3/bin/Rscript
CONFIG_SUBDIR=single_year_configs/mcov
export CHOLERA_SKIP_STAN=TRUE
export CONFIGS=($(find $CONFIGDIR/$CONFIG_SUBDIR -type f))
if [ $SLURM_ARRAY_TASK_COUNT == ${#CONFIGS[@]} ]
then
  echo $RSCRIPT $TAXDIR/Analysis/R/set_parameters.R -c ${CONFIGS[$SLURM_ARRAY_TASK_ID]}
  $RSCRIPT $TAXDIR/Analysis/R/set_parameters.R -c ${CONFIGS[$SLURM_ARRAY_TASK_ID]}
else
  echo "Expected $SLURM_ARRAY_TASK_COUNT configs, found ${#CONFIGS[@]}"
fi

cd $TAXDIR
