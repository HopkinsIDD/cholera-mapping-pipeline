#!/bin/bash
#SBATCH --job-name=install_cmdstan
#SBATCH --time=00:30:00
#SBATCH --mem=16G
#SBATCH --ntasks=1
#SBATCH --partition=defq
#SBATCH --account=aazman1
# set SBATCH --output based on your directory structure

CMDSTAN_LOCATION=/data/aazman1/pfang3/cmdstan_42
R_LIBRARY_DIRECTORY=$HOME/R/x86_64-pc-linux-gnu-library/4.2
CHOLERA_PIPELINE_DIRECTORY=/data/aazman1/$USER/cholera-mapping-pipeline/

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
module load GCC/11.3.0
module load R/4.2.1-foss-2022a
module load openmpi
module load gdal


## Actually install cmdstanr including dependent r packages and cmdstan
mkdir -p $R_LIBRARY_DIRECTORY \
 && cd $CMDSTAN_LOCATION \
 && make build \
 && Rscript -e "options(error=quit, status = 1); source('${CHOLERA_PIPELINE_DIRECTORY}Rockfish.Rprofile'); install.packages('cmdstanr', lib='$R_LIBRARY_DIRECTORY')"
 #

echo "DONE"
