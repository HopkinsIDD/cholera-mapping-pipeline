#!/bin/bash
#SBATCH --job-name KEN_pre
#SBATCH --time=72:00:00
#SBATCH --mem=30G
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
ml udunits
ml proj
ml libjpeg
ml sqlite
ml geos
ml libpng
ml curl

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
export CHOLERA_CONFIG_DIRECTORY=/data/aazman1/$USER/cholera-configs/production_tests/ptc_pretest_KZ
export CHOLERA_CONFIG=$CHOLERA_CONFIG_DIRECTORY/2016_2020_country/config_ID-01_KEN_2016_2020.yml
export R_LIBRARY_DIRECTORY=$HOME/rlibs/cmp/$R_VERSION/gcc/$GCC_VERSION/
export CMDSTAN_LOCATION=/data/aazman1/$USER/cmdstan

cd $CHOLERA_PIPELINE_DIRECTORY

Rscript -e "invisible(lapply(c('ISOcodes','igraph', 'roxygen2', 'ggplot2', 'terra', 'geodata', 'taxdat', 'cmdstanr', 'withr', 'processx'), 
                require, character.only = TRUE, lib.loc='$R_LIBRARY_DIRECTORY')); 
            cmdstanr::set_cmdstan_path('$CMDSTAN_LOCATION'); 
            source('Analysis/R/set_parameters.R')"

echo "DONE"
