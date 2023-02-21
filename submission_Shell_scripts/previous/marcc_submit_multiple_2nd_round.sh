#!/bin/bash
#SBATCH --job-name 2nd_round
#SBATCH --time=3-00:00:00
#SBATCH --mem=80G
#SBATCH -c 4
#SBATCH --array=8,26,28
#SBATCH --partition=defq
#SBATCH --account=aazman1

export GCC_VERSION=9.3.0
export R_VERSION=4.0.2
module purge

ml gcc/$GCC_VERSION
ml openmpi
ml gdal
ml r/$R_VERSION
ml r-magrittr
ml r-optparse
ml r-yaml
ml r-rprojroot
ml r-purrr
ml r-jsonlite
ml r-dplyr
ml r-tidyverse
ml r-stringi
ml r-rstan

export CHOLERA_ON_MARCC=TRUE
export CHOLERA_PIPELINE_DIRECTORY=/data/aazman1/$USER/cholera-mapping-pipeline/
export CHOLERA_CONFIG_DIRECTORY=/data/aazman1/$USER/cholera-configs/no_covariate_production_2016_2020/2016_2020_country
export R_LIBRARY_DIRECTORY=$HOME/rlibs/cmp/$R_VERSION/gcc/$GCC_VERSION/
export CMDSTAN_LOCATION=/data/aazman1/$USER/cmdstan
export CHOLERA_SKIP_STAN=FALSE
export CONFIGNAMES=($(ls $CHOLERA_CONFIG_DIRECTORY | tr ' ' '\n'))

echo "START"
date 

cd $CHOLERA_PIPELINE_DIRECTORY
Rscript -e "library(ISOcodes, lib.loc='$R_LIBRARY_DIRECTORY'); library(igraph, lib.loc='$R_LIBRARY_DIRECTORY'); 
            library(taxdat, lib.loc='$R_LIBRARY_DIRECTORY'); library(cmdstanr, lib.loc='$R_LIBRARY_DIRECTORY'); library(withr, lib.loc='$R_LIBRARY_DIRECTORY'); library(processx, lib.loc='$R_LIBRARY_DIRECTORY'); cmdstanr::set_cmdstan_path('$CMDSTAN_LOCATION'); 
            Sys.setenv(CHOLERA_CONFIG='$CHOLERA_CONFIG_DIRECTORY/${CONFIGNAMES[$SLURM_ARRAY_TASK_ID]}'); source('Analysis/R/set_parameters.R')" || exit 1

echo "DONE"
date
