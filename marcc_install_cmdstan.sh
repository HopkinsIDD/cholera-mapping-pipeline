#!/bin/bash
#SBATCH --job-name submit_multiple_years
#SBATCH --time=00:10:00
#SBATCH --mem=2G
#SBATCH --ntasks=1
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

export R_LIBRARY_DIRECTORY=/home/jkaminsky/rlibs/cmp/$R_VERSION/gcc/$GCC_VERSION/
export CMDSTAN_LOCATION=/home/jkaminsky/data_aazman1/jkaminsky/cmdstan



mkdir -p $R_LIBRARY_DIRECTORY \
 && cd $CMDSTAN_LOCATION \
 && make build \
 && Rscript -e "options(error=quit, status = 1); install.packages('cmdstanr', repos = c('https://mc-stan.org/r-packages/', getOption('repos')), lib='$R_LIBRARY_DIRECTORY')" \
 && Rscript -e "options(error=quit, status = 1); install.packages(c('processx','withr'), repos = c('http://cran.r-project.org/'), lib='$R_LIBRARY_DIRECTORY')"

echo "DONE"
