#!/bin/bash
#SBATCH --job-name submit_multiple_years
#SBATCH --time=00:10:00
#SBATCH --mem=2G
#SBATCH --ntasks=1
#SBATCH --partition=defq
#SBATCH --account=aazman1

# Specify toolchain
export GCC_VERSION=9.3.0
export R_VERSION=4.0.2

# Set filepaths for:
## Local R libraries
export R_LIBRARY_DIRECTORY=$HOME/rlibs/cmp/$R_VERSION/gcc/$GCC_VERSION/
## cmdstan repo
export CMDSTAN_LOCATION=/data/aazman1/$USER/cmdstan

## clone cmdstan (will not fail if already cloned)
git clone https://github.com/stan-dev/cmdstan.git $CMDSTAN_LOCATION --recurse-submodules
cd $CMDSTAN_LOCATION

## Update cmdstan
# git reset --hard $CMDSTAN_LOCATION
# git clean -fxd $CMDSTAN_LOCATION
# git pull


## Set up modules
## (we need to do git things since reset removes git)
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


## Actually install cmdstanr including dependent r packages and cmdstan
mkdir -p $R_LIBRARY_DIRECTORY \
 && cd $CMDSTAN_LOCATION \
 && make build \
 && Rscript -e "options(error=quit, status = 1); install.packages('cmdstanr', repos = c('https://mc-stan.org/r-packages/', getOption('repos')), lib='$R_LIBRARY_DIRECTORY')" \
 && Rscript -e "options(error=quit, status = 1); install.packages(c('processx','withr'), repos = c('http://cran.r-project.org/'), lib='$R_LIBRARY_DIRECTORY')"
 ## We need to install these packages again, since they need newer versions than rockfish has available

echo "DONE"
