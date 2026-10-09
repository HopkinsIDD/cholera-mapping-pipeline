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
Rscript -e '
lib <- Sys.getenv("R_LIBS_USER"); options(repos = c(CRAN = "https://stat.ethz.ch/CRAN/"), Ncpus = 4)
need <- c("DBI", "RPostgres", "sf", "terra", "ncdf4", "jsonlite", "lubridate", "glue", "digest",
          "purrr", "stringr", "yaml", "dplyr", "tidyr", "magrittr", "ISOcodes", "igraph", "geodata",
          "optparse", "rprojroot", "rstudioapi", "withr", "testthat", "remotes", "hashids", "tibble")
missing <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, lib = lib)
install.packages("packages/taxdat", repos = NULL, type = "source", lib = lib)
library(taxdat); cat("taxdat", as.character(packageVersion("taxdat")), "installed in", lib, "\n")'
