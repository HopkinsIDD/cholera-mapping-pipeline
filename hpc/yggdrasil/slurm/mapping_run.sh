#!/bin/bash
#SBATCH --job-name=mapping_run
#SBATCH --output=logs/%x_%j.log
#SBATCH --partition=shared-cpu
#SBATCH --time=12:00:00
#SBATCH --mem=16G
#SBATCH -c 4
# Run the mapping pipeline for one config against the cluster database.
# Usage: sbatch slurm/mapping_run.sh CONFIG.yml [OBSERVATIONS.rds]
# Stan is skipped unless CHOLERA_SKIP_STAN=FALSE is exported.
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
CONFIG=$(readlink -f "${1:?config file}")
if [[ -n "${2:-}" ]]; then
  export CHOLERA_OBSERVATIONS_RDS=$(readlink -f "$2")
fi
export CHOLERA_SKIP_STAN=${CHOLERA_SKIP_STAN:-TRUE}
export PRODUCTION_RUN=${PRODUCTION_RUN:-FALSE}
cmp_wait_db
cd "$CMP_REPO"
Rscript Analysis/R/set_parameters.R -c "$CONFIG" -d "$CMP_REPO" -l "$CMP_LAYERS"
