#!/bin/bash
#SBATCH --job-name=covariates_extract
#SBATCH --output=logs/%x_%j.log
#SBATCH --partition=shared-cpu
#SBATCH --time=02:00:00
#SBATCH --mem=16G
#SBATCH -c 2
# Extract every covariate in the database over one country and check it
# against the source rasters (Analysis/R/extract_covariates_country.R).
# Writes CSVs and a PDF to $CMP_DIAGNOSTICS/<database>/extract_<ISO>/.
#
# Compute nodes have no internet: cache the country's boundary first, on the
# login node (source env.sh, then):
#   Rscript -e 'taxdat::cache_admin_units("BDI", 0)'
# Usage: sbatch hpc/unige/slurm/covariates_extract_country.sh BDI
set -euo pipefail
# sbatch runs a copy of this script from /var/spool/slurmd, so find the
# repository from the submission directory (submit from the repository root);
# with plain `bash`, from this file's location.
CMP_REPO="${CMP_REPO:-${SLURM_SUBMIT_DIR:-$(cd "$(dirname "$(readlink -f "$0")")/../../.." && pwd)}}"
if [[ ! -f "$CMP_REPO/hpc/unige/env.sh" ]]; then
  echo "Cannot find hpc/unige/env.sh under $CMP_REPO: submit from the repository root or export CMP_REPO" >&2
  exit 1
fi
source "$CMP_REPO/hpc/unige/env.sh"
ISO=$(echo "${1:-BDI}" | tr '[:lower:]' '[:upper:]')
cmp_wait_db
cd "$CMP_REPO"
OUT="$CMP_DIAGNOSTICS/$PGDATABASE/extract_$ISO"
Rscript Analysis/R/extract_covariates_country.R -i "$ISO" -l "$CMP_LAYERS" -o "$OUT"
chmod -R g+rX "$OUT"
