#!/bin/bash
#SBATCH --job-name=covariates_ingest
#SBATCH --output=logs/%x_%j.log
#SBATCH --partition=shared-cpu
#SBATCH --time=12:00:00
#SBATCH --mem=8G
#SBATCH -c 2
# Load the pre-computed covariates into the database (serial; re-uses the
# processed-file cache, so it only loads).
# Usage: sbatch slurm/covariates_ingest.sh "p:20,dw:20,aw:20,p:1" [MODE]
set -euo pipefail
source "$(dirname "$(readlink -f "$0")")/../env.sh"
IFS=',' read -r -a PAIRS <<< "${1:?covariate:resolution list}"
MODE=${2:-ingest_missing}
cmp_wait_db
cd "$CMP_REPO"
for RES in $(printf '%s\n' "${PAIRS[@]}" | sed 's/.*://' | sort -un); do
  COVS=$(printf '%s\n' "${PAIRS[@]}" | awk -F: -v r="$RES" '$2 == r {print $1}' | paste -sd, -)
  echo "Loading [$COVS] at $RES km"
  Rscript Analysis/R/prepare_covariates_cmd.R -l "$CMP_LAYERS" -r "$RES" -t "${CMP_RES_TIME:-1 years}" \
    -a "$CMP_AOI" -b "$CMP_AOI_BUFFER_KM" -c "$COVS" -m "$MODE"
done
