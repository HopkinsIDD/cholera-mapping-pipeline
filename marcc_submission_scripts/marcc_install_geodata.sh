#!/bin/bash
#SBATCH --job-name KEN
#SBATCH --time=3-00:00:00
#SBATCH --mem=50G
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
export R_LIBRARY_DIRECTORY=$HOME/rlibs/cmp/$R_VERSION/gcc/$GCC_VERSION/

cd $CHOLERA_PIPELINE_DIRECTORY
mkdir -p $R_LIBRARY_DIRECTORY \
 && Rscript -e "options(error=quit, status = 1); 
                if (!require(package = 'terra', character.only = T, lib='$R_LIBRARY_DIRECTORY')) {
                    install.packages('terra', lib='$R_LIBRARY_DIRECTORY') }; 
                if (!require(package = 'geodata', character.only = T, lib='$R_LIBRARY_DIRECTORY')) {
                    install.packages('geodata', lib='$R_LIBRARY_DIRECTORY') }"

echo "DONE"
