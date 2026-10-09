#!/bin/bash
#SBATCH --job-name=pull_sif
#SBATCH --output=logs/%x_%j.log
#SBATCH --partition=shared-cpu
#SBATCH --time=00:30:00
#SBATCH --mem=8G
#SBATCH -c 4
# Pull the PostGIS image (the HPC docs ask for pulls on a compute node).
# If compute nodes cannot reach Docker Hub, run `apptainer pull` on a laptop
# and copy the .sif with tools/rsync_inputs_yggdrasil.sh --what sif.
set -euo pipefail
source "$(dirname "$(readlink -f "$0")")/../env.sh"

if [[ -f "$CHOLERA_SIF" ]]; then
  echo "Image already present: $CHOLERA_SIF"; exit 0
fi
export APPTAINER_CACHEDIR="$SCRATCH_SHARE/apptainer_cache/$USER"
export APPTAINER_TMPDIR="${TMPDIR:-$SCRATCH_SHARE/tmp/$USER}"
mkdir -p "$(dirname "$CHOLERA_SIF")" "$APPTAINER_CACHEDIR" "$APPTAINER_TMPDIR"
apptainer pull "$CHOLERA_SIF" "$CMP_SIF_SOURCE"
sha256sum "$CHOLERA_SIF" > "$CHOLERA_SIF.sha256"
chmod g+r "$CHOLERA_SIF" "$CHOLERA_SIF.sha256"
apptainer exec "$CHOLERA_SIF" postgres --version
apptainer exec "$CHOLERA_SIF" raster2pgsql -G | head -3
