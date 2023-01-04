#!/bin/bash
#SBATCH --job-name diag
#SBATCH --time=2-23:59:00
#SBATCH --mem=20G
#SBATCH --ntasks=1
#SBATCH --partition=defq
#SBATCH --account=aazman1

# Setup on MARCC (gcm should be changed to cmp)
export GCC_VERSION=9.3.0
export R_VERSION=4.0.2
export CHOLERA_DIRECTORY=/data/aazman1/$USER/cholera-mapping-pipeline
export R_LIBRARY_DIRECTORY=$HOME/rlibs/gcm/$R_VERSION/gcc/$GCC_VERSION/
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
ml pandoc
ml freetype
ml freetype2
ml fontconfig
ml cairo

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
ml r-cairo


echo "Beginning of script"
date

cd $CHOLERA_DIRECTORY

Rscript -e "Sys.setenv(R_LIBRARY_DIRECTORY='$R_LIBRARY_DIRECTORY'); 
            Sys.setenv(RUN_ON_MARCC=TRUE); 
            library(rmarkdown, lib = '$R_LIBRARY_DIRECTORY'); 
            rmarkdown::render('Analysis/output/country_data_report.Rmd', params=list(args = 'myarg'))" || exit 1

echo "End of script"
date
