#!/bin/bash
#SBATCH --job-name=install_r_pkgs
#SBATCH --output=logs/%x_%j.log
#SBATCH --partition=shared-cpu
#SBATCH --time=04:00:00
#SBATCH --mem=8G
#SBATCH -c 4
# Install taxdat and the R packages the covariates pipeline needs into
# R_LIBS_USER (see env.sh). Re-run after pulling taxdat changes.
set -euo pipefail
source "$(dirname "$(readlink -f "$0")")/../env.sh"
cd "$CMP_REPO"
Rscript -e '
lib <- Sys.getenv("R_LIBS_USER"); options(repos = c(CRAN = "https://stat.ethz.ch/CRAN/"), Ncpus = 4)
need <- c("DBI", "RPostgres", "sf", "terra", "ncdf4", "jsonlite", "lubridate", "glue", "digest",
          "purrr", "stringr", "yaml", "dplyr", "tidyr", "magrittr", "ISOcodes", "igraph", "geodata",
          "optparse", "rprojroot", "rstudioapi", "withr", "testthat", "remotes", "hashids", "tibble")
missing <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, lib = lib)
install.packages("packages/taxdat", repos = NULL, type = "source", lib = lib)
library(taxdat); cat("taxdat", as.character(packageVersion("taxdat")), "installed in", lib, "\n")'
