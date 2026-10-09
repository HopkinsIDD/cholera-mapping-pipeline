#!/bin/bash
#SBATCH --job-name=prepare_grid
#SBATCH --output=logs/%x_%j.log
#SBATCH --partition=shared-cpu
#SBATCH --time=02:00:00
#SBATCH --mem=16G
#SBATCH -c 2
# Build the master grid, the modelling grid and the 1 km grid (used for
# population weights) for the area of interest.
# Usage: sbatch slurm/prepare_grid.sh [RES_SPACE_KM ...]   (default: 20 1)
set -euo pipefail
source "$(dirname "$(readlink -f "$0")")/../env.sh"
cmp_wait_db
cd "$CMP_REPO"
for res in "${@:-20 1}"; do
  for r in $res; do
    Rscript Analysis/R/prepare_grid_cmd.R -l "$CMP_LAYERS" -r "$r" -a "$CMP_AOI" -b "$CMP_AOI_BUFFER_KM"
  done
done
