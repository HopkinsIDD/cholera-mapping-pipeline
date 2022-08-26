#!/bin/bash
#SBATCH --job-name submit_multiple_years
#SBATCH --time=01:00:00
#SBATCH --mem=10G
#SBATCH --ntasks=4
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
export CHOLERA_CONFIG=/data/aazman1/$USER/cholera-configs/
export CHOLERA_CONFIG_DIRECTORY=/data/aazman1/$USER/cholera-configs/no_covariate_production_2016_2020/2016_2020_country
export CHOLERA_CONFIG=$CHOLERA_CONFIG_DIRECTORY/config_CIV_2016_2020.yml
export R_LIBRARY_DIRECTORY=$HOME/rlibs/cmp/$R_VERSION/gcc/$GCC_VERSION/
export CMDSTAN_LOCATION=/data/aazman1/$USER/cmdstan
echo "R library directory is $R_LIBRARY_DIRECTORY"
ls $R_LIBRARY_DIRECTORY

Rscript -e "library(withr, lib.loc='$R_LIBRARY_DIRECTORY'); library(processx, lib.loc='$R_LIBRARY_DIRECTORY'); cmdstanr::set_cmdstan_path('$CMDSTAN_LOCATION'); source('Analysis/R/set_parameters.R')"

echo "DONE"
