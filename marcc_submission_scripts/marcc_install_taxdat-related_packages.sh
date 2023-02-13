#!/bin/bash
#SBATCH --job-name get_pkg
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
export R_LIBS_USER=$HOME/rlibs/cmp/$R_VERSION/gcc/$GCC_VERSION/

cd $CHOLERA_PIPELINE_DIRECTORY
mkdir -p $R_LIBS_USER \
 && Rscript -e "options(error=quit, status = 1); 
                package_list <- c('sf', 'terra', 'ISOcodes', 'igraph', 'roxygen2', 'withr', 'processx', 'geodata'); 
                for (pkg in package_list){
                    if (!require(package = pkg, lib='$R_LIBS_USER')) {
                        install.packages(pkg, lib='$R_LIBS_USER') 
                    }; 
                }
                "
                
echo "DONE"
