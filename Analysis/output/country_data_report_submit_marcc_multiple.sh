#!/bin/bash
#SBATCH --job-name diag
#SBATCH --time=2-23:59:00
#SBATCH --mem=30G
#SBATCH --array=0-43%10
#SBATCH --partition=defq
#SBATCH --account=aazman1

# Specify manually which config directory to use
export CHOLERA_CONFIG_DIRECTORY=/data/aazman1/$USER/cholera-configs/...
export CONFIGNAMES=($(ls $CHOLERA_CONFIG_DIRECTORY | tr ' ' '\n'))

# Setup on MARCC
export GCC_VERSION=9.3.0
export R_VERSION=4.0.2
export CHOLERA_DIRECTORY=/data/aazman1/$USER/cholera-mapping-pipeline
export R_LIBRARY_DIRECTORY=$HOME/rlibs/cmp/$R_VERSION/gcc/$GCC_VERSION/
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
ml fontconfig
ml freetype
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

# Install necessary packages
mkdir -p $R_LIBRARY_DIRECTORY \
 && cd $CHOLERA_DIRECTORY \
 && Rscript -e "options(error=quit, status = 1); 
                if (!require(package = 'remotes', character.only = T, lib='$R_LIBRARY_DIRECTORY')) {
                    install.packages('remotes', lib='$R_LIBRARY_DIRECTORY') }; 
                require(remotes, lib='$R_LIBRARY_DIRECTORY'); 
                if (!require(package = 'sf', character.only = T, lib='$R_LIBRARY_DIRECTORY')) {
                    install_version('sf', version = '1.0.8', repos = 'http://cran.us.r-project.org', lib='$R_LIBRARY_DIRECTORY', dependencies = TRUE) }; 
                if (!require(package = 'GADMTools', character.only = T, lib='$R_LIBRARY_DIRECTORY')) {
                    install_version('GADMTools', version = '3.9.1', repos = 'http://cran.us.r-project.org', lib='$R_LIBRARY_DIRECTORY') }; 
                if (!require(package = 'exactextractr', character.only = T, lib='$R_LIBRARY_DIRECTORY')) {
                    install_version('exactextractr', version = '0.9.0', repos = 'http://cran.us.r-project.org', lib='$R_LIBRARY_DIRECTORY') }; 
                if (!require(package = 'raster', character.only = T, lib='$R_LIBRARY_DIRECTORY')) {
                    install_version('raster', version = '3.4.13', repos = 'http://cran.us.r-project.org', lib='$R_LIBRARY_DIRECTORY') }; 
                if (!require(package = 'Rcpp', character.only = T, lib='$R_LIBRARY_DIRECTORY')) {
                    install_version('Rcpp', version = '1.0.9', repos = 'http://cran.us.r-project.org', lib='$R_LIBRARY_DIRECTORY') }; 
                if (!require(package = 'terra', character.only = T, lib='$R_LIBRARY_DIRECTORY')) {
                    install_version('terra', version = '1.4.22', repos = 'http://cran.us.r-project.org', lib='$R_LIBRARY_DIRECTORY') }; 

                if (!require(package = 'drat', character.only = T, lib='$R_LIBRARY_DIRECTORY')) {
                    install.packages('drat', lib='$R_LIBRARY_DIRECTORY') }; 
                if (!require(package = 'roxygen2', character.only = T, lib='$R_LIBRARY_DIRECTORY')) {
                    install.packages('roxygen2', lib='$R_LIBRARY_DIRECTORY') }; 
                if (!require(package = 'data.table', character.only = T, lib='$R_LIBRARY_DIRECTORY')) {
                    install.packages('data.table', lib='$R_LIBRARY_DIRECTORY') }; 
                if (!require(package = 'fasterize', character.only = T, lib='$R_LIBRARY_DIRECTORY')) {
                    install.packages('fasterize', lib='$R_LIBRARY_DIRECTORY') }; 
                if (!require(package = 'truncnorm', character.only = T, lib='$R_LIBRARY_DIRECTORY')) {
                    install.packages('truncnorm', lib='$R_LIBRARY_DIRECTORY') }; 
                if (!require(package = 'MCMCglmm', character.only = T, lib='$R_LIBRARY_DIRECTORY')) {
                    install.packages('MCMCglmm', lib='$R_LIBRARY_DIRECTORY') }; 
                if (!require(package = 'codetools', character.only = T, lib='$R_LIBRARY_DIRECTORY')) {
                    install.packages('codetools', lib='$R_LIBRARY_DIRECTORY') }; 
                if (!require(package = 'gert', character.only = T, lib='$R_LIBRARY_DIRECTORY')) {
                    install.packages('gert', lib='$R_LIBRARY_DIRECTORY') }" 


# Generate the report 
cd $CHOLERA_DIRECTORY
Rscript -e "Sys.setenv(R_LIBRARY_DIRECTORY='$R_LIBRARY_DIRECTORY'); 
            Sys.setenv(RUN_ON_MARCC=TRUE); 
            library(rmarkdown, lib = '$R_LIBRARY_DIRECTORY'); 
            rmarkdown::render(  'Analysis/output/country_data_report.Rmd', 
                                params = list(cholera_directory = '$CHOLERA_DIRECTORY', config = '$CONFIGDIR/${CONFIGNAMES[$SLURM_ARRAY_TASK_ID]}', args = 'myarg'), 
                                output_file = ifelse(!is.null(yaml::read_yaml(paste0('$CHOLERA_DIRECTORY', '/', '$CONFIGDIR/${CONFIGNAMES[$SLURM_ARRAY_TASK_ID]}'))[['country_data_report_filename']]), 
                                                    yaml::read_yaml(paste0('$CHOLERA_DIRECTORY', '/', '$CONFIGDIR/${CONFIGNAMES[$SLURM_ARRAY_TASK_ID]}'))[['country_data_report_filename']], 
                                                    dplyr::last(stringr::str_replace(unlist(stringr::str_split('$CONFIGDIR/${CONFIGNAMES[$SLURM_ARRAY_TASK_ID]}', '/')), '.yml', ''))
                                                    ) 
                                )" || exit 1

echo "End of script"
date
