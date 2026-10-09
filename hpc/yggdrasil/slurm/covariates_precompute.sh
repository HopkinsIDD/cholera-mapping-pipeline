#!/bin/bash
#SBATCH --job-name=covariates_precompute
#SBATCH --output=logs/%x_%A_%a.log
#SBATCH --partition=shared-cpu
#SBATCH --time=12:00:00
#SBATCH --mem=32G
#SBATCH -c 8
# One array task per covariate: crop, aggregate in time and warp onto the
# grid file, filling $CMP_LAYERS/processed_covariates. No database access.
# Usage: sbatch --array=0-N slurm/covariates_precompute.sh "p:20,dw:20,aw:20,p:1"
#   (abbreviation:resolution_km pairs; the task index picks one pair)
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
PAIR=${PAIRS[${SLURM_ARRAY_TASK_ID:-0}]:-}
[[ -n "$PAIR" ]] || { echo "No covariate for task ${SLURM_ARRAY_TASK_ID:-0}"; exit 0; }
COV=${PAIR%%:*}; RES=${PAIR##*:}
echo "Task ${SLURM_ARRAY_TASK_ID:-0}: covariate $COV at $RES km"
cd "$CMP_REPO"
Rscript Analysis/R/prepare_covariates_cmd.R -l "$CMP_LAYERS" -r "$RES" -t "${CMP_RES_TIME:-1 years}" \
  -a "$CMP_AOI" -b "$CMP_AOI_BUFFER_KM" -c "$COV" -n "$SLURM_CPUS_PER_TASK" --precompute_only TRUE
