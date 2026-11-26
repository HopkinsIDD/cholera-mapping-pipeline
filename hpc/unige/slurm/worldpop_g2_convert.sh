#!/bin/bash
#SBATCH --job-name=worldpop_g2_convert
#SBATCH --output=logs/%x_%j.log
#SBATCH --partition=shared-cpu
#SBATCH --time=06:00:00
#SBATCH --mem=32G
#SBATCH -c 2
# Rewrite the downloaded WorldPop Global 2 mosaics (tools/download_worldpop_g2.sh)
# as $CMP_LAYERS/pop/population_<year>_yearly.nc: 0 on unsettled land, NoData
# off land (land from CHOLERA_MASTER_GRID_FILE). No database access.
# Usage: sbatch hpc/unige/slurm/worldpop_g2_convert.sh [YEARS]   (default 2015:2030)
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
cd "$CMP_REPO"
Rscript Analysis/R/prepare_worldpop_g2_cmd.R -l "$CMP_LAYERS" -m "$CHOLERA_MASTER_GRID_FILE" -y "${1:-2015:2030}"
chmod -R g+rX "$CMP_LAYERS/pop"
