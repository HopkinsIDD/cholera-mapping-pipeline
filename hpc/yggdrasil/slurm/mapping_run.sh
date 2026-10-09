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
source "$(dirname "$(readlink -f "$0")")/../env.sh"
CONFIG=$(readlink -f "${1:?config file}")
if [[ -n "${2:-}" ]]; then
  export CHOLERA_OBSERVATIONS_RDS=$(readlink -f "$2")
fi
export CHOLERA_SKIP_STAN=${CHOLERA_SKIP_STAN:-TRUE}
export PRODUCTION_RUN=${PRODUCTION_RUN:-FALSE}
cmp_wait_db
cd "$CMP_REPO"
Rscript Analysis/R/set_parameters.R -c "$CONFIG" -d "$CMP_REPO" -l "$CMP_LAYERS"
