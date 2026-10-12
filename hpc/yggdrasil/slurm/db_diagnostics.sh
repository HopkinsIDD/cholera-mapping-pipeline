#!/bin/bash
#SBATCH --job-name=db_diagnostics
#SBATCH --output=logs/%x_%j.log
#SBATCH --partition=shared-cpu
#SBATCH --time=01:00:00
#SBATCH --mem=8G
#SBATCH -c 2
# Diagnostic figures for the database build and the test run's extraction
# (Analysis/R/db_diagnostics.R). Writes a PDF, PNGs and CSVs to
# $SHARE/diagnostics/<database>/.
# Usage: sbatch hpc/yggdrasil/slurm/db_diagnostics.sh CONFIG.yml
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
cmp_wait_db
cd "$CMP_REPO"
OUT="$SHARE/diagnostics/$PGDATABASE"
Rscript Analysis/R/db_diagnostics.R -c "$CONFIG" -l "$CMP_LAYERS" -d Analysis/data -o "$OUT"
chmod -R g+rX "$OUT"
