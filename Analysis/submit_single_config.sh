#!/bin/bash
#SBATCH --output=logs/%x_%A_%a.log
#SBATCH --time=3-00:00
#SBATCH --mem=30G
#SBATCH -c 4
#SBATCH -p shared

ml stack/0.3
ml gcc/7.4.0
ml openmpi
ml r-sf
ml r-rgdal
ml r-curl
ml r-mgcv
ml r-purrr
ml r-magrittr
ml r-stringr
ml r-tibble

echo "Beginning of script"
date

CHOLDIR='/home-2/fperez5@jhu.edu/scratch/Javier/cholera-mapping-pipline/'
CONFIG=$1

Rscript $CHOLDIR/Analysis/R/set_parameters.R -c $CHOLDIR/$CONFIG  -d $CHOLDIR -l $CHOLDIR/Layers || exit 1
echo "End of script"
date
