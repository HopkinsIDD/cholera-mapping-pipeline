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
# sbatch runs a copy of this script from /var/spool/slurmd, so find the
# repository from the submission directory (submit from the repository root);
# with plain `bash`, from this file's location.
CMP_REPO="${CMP_REPO:-${SLURM_SUBMIT_DIR:-$(cd "$(dirname "$(readlink -f "$0")")/../../.." && pwd)}}"
if [[ ! -f "$CMP_REPO/hpc/yggdrasil/env.sh" ]]; then
  echo "Cannot find hpc/yggdrasil/env.sh under $CMP_REPO: submit from the repository root or export CMP_REPO" >&2
  exit 1
fi
source "$CMP_REPO/hpc/yggdrasil/env.sh"
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
