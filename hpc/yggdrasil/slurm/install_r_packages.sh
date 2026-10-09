#!/bin/bash
#SBATCH --job-name=install_r_pkgs
#SBATCH --output=logs/%x_%j.log
#SBATCH --partition=shared-cpu
#SBATCH --time=04:00:00
#SBATCH --mem=8G
#SBATCH -c 4
# Install taxdat and the R packages the covariates pipeline needs into
# R_LIBS_USER (see env.sh). Re-run after pulling taxdat changes.
#
# Packages already in any library on R_LIBS_USER (e.g. compiled for
# OutbreakExtractR in R's default user library) are reused; missing ones come
# from a dated CRAN snapshot that matches the R 4.3.2 module (CMP_CRAN_REPO):
# current CRAN needs R >= 4.4 for Matrix and Rcpp >= 1.1 for terra and units,
# which the module's Rcpp 1.0.11 does not meet. New installs and taxdat go to
# the first R_LIBS_USER entry.
set -euo pipefail
# sbatch runs a copy of this script from /var/spool/slurmd, so find the
# repository from the submission directory (submit from the repository root);
# with plain `bash`, from this file's location.
CMP_REPO="${CMP_REPO:-${SLURM_SUBMIT_DIR:-$(cd "$(dirname "$(readlink -f "$0")")/../../.." && pwd)}}"
if [[ ! -f "$CMP_REPO/hpc/yggdrasil/env.sh" ]]; then
  echo "Cannot find hpc/yggdrasil/env.sh under $CMP_REPO: submit from the repository root or export CMP_REPO" >&2
  exit 1
fi
source "$CMP_REPO/hpc/yggdrasil/env.sh"
cd "$CMP_REPO"

# s2 builds its bundled Abseil with CMake
if command -v module >/dev/null 2>&1; then
  module load CMake/3.26.3 2>/dev/null \
    || echo "CMake/3.26.3 module not found; s2 may fail to build. Check: module spider CMake" >&2
fi
export CMP_CRAN_REPO="${CMP_CRAN_REPO:-https://packagemanager.posit.co/cran/2024-04-15}"
echo "R library: $R_LIBS_USER"
echo "CRAN snapshot: $CMP_CRAN_REPO"

Rscript -e '
lib <- strsplit(Sys.getenv("R_LIBS_USER"), ":", fixed = TRUE)[[1]][1]
options(repos = c(CRAN = Sys.getenv("CMP_CRAN_REPO")), Ncpus = 4)
cat("Library search path:\n"); print(.libPaths())
inst <- function(p) install.packages(p, lib = lib)
need <- c("Matrix", "DBI", "RPostgres", "blob", "units", "s2", "sf", "terra", "ncdf4", "jsonlite",
          "lubridate", "glue", "digest", "purrr", "stringr", "yaml", "dplyr", "tidyr",
          "magrittr", "ISOcodes", "igraph", "geodata", "optparse", "rprojroot", "rstudioapi",
          "withr", "testthat", "remotes", "hashids", "tibble")
missing <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
cat("Missing, to install:", if (length(missing)) missing else "none", "\n")
if (length(missing)) inst(missing)
install.packages("packages/taxdat", repos = NULL, type = "source", lib = lib)
key <- c("Rcpp", "Matrix", "terra", "sf", "s2", "units", "igraph", "RPostgres", "taxdat")
ok <- vapply(key, function(p) requireNamespace(p, quietly = TRUE), logical(1))
for (p in key) cat(sprintf("%-10s %s\n", p, if (ok[[p]]) as.character(packageVersion(p)) else "MISSING"))
if (!all(ok)) quit(status = 1)
cat("terra GDAL:", terra::gdal(), " sf:", sf::sf_extSoftVersion()[c("GDAL", "PROJ", "GEOS")], "\n")'
