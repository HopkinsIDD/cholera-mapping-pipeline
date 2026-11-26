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
# sbatch runs a copy of this script from /var/spool/slurmd, so find the
# repository from the submission directory (submit from the repository root);
# with plain `bash`, from this file's location.
CMP_REPO="${CMP_REPO:-${SLURM_SUBMIT_DIR:-$(cd "$(dirname "$(readlink -f "$0")")/../../.." && pwd)}}"
if [[ ! -f "$CMP_REPO/hpc/yggdrasil/env.sh" ]]; then
  echo "Cannot find hpc/yggdrasil/env.sh under $CMP_REPO: submit from the repository root or export CMP_REPO" >&2
  exit 1
fi
source "$CMP_REPO/hpc/yggdrasil/env.sh"

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
# The image has the PostGIS extensions but not the raster2pgsql client;
# rasters are loaded from R (taxdat::load_raster_to_db, dbi loader).
apptainer exec "$CHOLERA_SIF" ls /usr/share/postgresql/17/extension/postgis_raster.control
