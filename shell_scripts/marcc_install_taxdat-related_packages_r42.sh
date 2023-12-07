#!/bin/bash
#SBATCH --job-name get_pkg
#SBATCH --time=3-00:00:00
#SBATCH --mem=16G
#SBATCH --ntasks=4
#SBATCH --partition=defq
#SBATCH --account=aazman1
# set SBATCH --output based on your directory structure

module purge

ml GCC/11.3.0
ml openmpi
ml gdal
ml R/4.2.1-foss-2022a

export CHOLERA_ON_MARCC=TRUE
export CHOLERA_PIPELINE_DIRECTORY=/data/aazman1/$USER/cholera-mapping-pipeline/
export R_LIBS_USER=$HOME/R/x86_64-pc-linux-gnu-library/4.2/

# sf, terra, ISOcodes,igraph, roxygen2, withr, processx included in module R/4.2.1 can be removed from install
mkdir -p $R_LIBS_USER \
 && Rscript -e "source('${CHOLERA_PIPELINE_DIRECTORY}Rockfish.Rprofile');
                options(error=quit, status = 1); 
                package_list <- c('geodata'); 
                for (pkg in package_list){
                    if (!require(package = pkg, lib='$R_LIBS_USER')) {
                        install.packages(pkg, lib='$R_LIBS_USER') 
                    }; 
                };
                install.packages('${CHOLERA_PIPELINE_DIRECTORY}packages/taxdat', type='source', repos = NULL, lib='$R_LIBS_USER')"                
echo "DONE"

