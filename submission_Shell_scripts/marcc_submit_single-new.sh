#!/bin/bash
mkdir /data/aazman1/$USER/sbatch_logs/
#SBATCH --output=/data/aazman1/$USER/sbatch_logs/marcc_run_<...>.log
#SBATCH --job-name=<...>
#SBATCH --time=72:00:00
#SBATCH --mem=64G
#SBATCH --account=aazman1
#SBATCH --ntasks=4
#SBATCH --partition=defq

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
ml proj
ml geos
ml libjpeg

export CHOLERA_ON_MARCC=TRUE
export CHOLERA_PIPELINE_DIRECTORY=/data/aazman1/$USER/cholera-mapping-pipeline/
export CHOLERA_CONFIG_DIRECTORY=/data/aazman1/$USER/cholera-configs/...
export CHOLERA_CONFIG=$CHOLERA_CONFIG_DIRECTORY/....yml
export R_LIBS_USER=$HOME/rlibs/cmp/$R_VERSION/gcc/$GCC_VERSION/
export CMDSTAN_LOCATION=/data/aazman1/$USER/cmdstan

cd $CHOLERA_PIPELINE_DIRECTORY

Rscript -e "if (!require(package = 'ISOcodes', character.only = T, lib='$R_LIBS_USER')) {
                    install.packages('ISOcodes', lib='$R_LIBS_USER') }; 
                if (!require(package = 'igraph', character.only = T, lib='$R_LIBS_USER')) {
                    install.packages('igraph', lib='$R_LIBS_USER') };
                if (!require(package = 'roxygen2', character.only = T, lib='$R_LIBS_USER')) {
                    install.packages('roxygen2', lib='$R_LIBS_USER') };
            library(terra, lib.loc='$R_LIBS_USER');
            install.packages('$CHOLERA_PIPELINE_DIRECTORY/packages/taxdat', type='source', repos = NULL, lib='$R_LIBS_USER');
            library(taxdat, lib.loc='$R_LIBS_USER'); 
            library(cmdstanr, lib.loc='$R_LIBS_USER'); 
            library(withr, lib.loc='$R_LIBS_USER'); 
            library(processx, lib.loc='$R_LIBS_USER'); 
            cmdstanr::set_cmdstan_path('$CMDSTAN_LOCATION'); 
            source('Analysis/R/set_parameters.R')"

echo "DONE"
