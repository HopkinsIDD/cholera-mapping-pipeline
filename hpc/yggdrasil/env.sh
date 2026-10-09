# hpc/yggdrasil/env.sh — environment for the covariates database on Yggdrasil.
# Source it (never execute it): every slurm/, stages/ and tools/ script does
#   source "$(dirname "$(readlink -f "$0")")/../env.sh"
# Override any variable by exporting it before sourcing.

[[ -n "${CMP_ENV_LOADED:-}" ]] && return 0
export CMP_ENV_LOADED=1

# --- Locations -------------------------------------------------------------
# Repository checkout on the cluster (code arrives with git pull)
export CMP_REPO="${CMP_REPO:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"
# Shared project directories (group GL_M_gdd_cholera_mapping). Files count
# against their owner's 1 TB home quota; scratch has no size quota, but files
# untouched for 3 months are deleted and nothing is backed up.
export SHARE="${SHARE:-/home/shares/azman/cholera_mapping}"
export SCRATCH_SHARE="${SCRATCH_SHARE:-/srv/beegfs/scratch/shares/azman/cholera_mapping}"

export CMP_AOI="${CMP_AOI:-BDI}"                    # area of interest (ISO3 or raw)
export CMP_AOI_BUFFER_KM="${CMP_AOI_BUFFER_KM:-50}"
CMP_AOI_TAG=$(echo "$CMP_AOI" | tr '[:upper:]' '[:lower:]')

export CMP_LAYERS="${CMP_LAYERS:-$SHARE/Layers}"     # raw covariates, grids, processed cache
export CHOLERA_AOI_CACHE_DIR="${CHOLERA_AOI_CACHE_DIR:-$CMP_LAYERS/admin_units}"
export CHOLERA_MASTER_GRID_FILE="${CHOLERA_MASTER_GRID_FILE:-$CMP_LAYERS/pop_old/ppp_2020_1km_Aggregated.tif}"
export CMP_DATA="${CMP_DATA:-$SHARE/data}"           # pre-pulled observations (.rds)
export CMP_BACKUPS="${CMP_BACKUPS:-$SHARE/backups}"
export CMP_SECRETS="${CMP_SECRETS:-$SHARE/secrets}"  # db.env, superuser.pw (mode 640/600)

# --- PostgreSQL server (runs in the Apptainer image) ------------------------
# Set CHOLERA_SIF="" to use native binaries (testing on a workstation)
export CHOLERA_SIF="${CHOLERA_SIF-$SHARE/sif/postgis_17-3.5.sif}"
export CMP_SIF_SOURCE="${CMP_SIF_SOURCE:-docker://postgis/postgis:17-3.5}"
# One data directory per server; pilot on the backed-up home share
export PGDATA="${PGDATA:-$SHARE/pgdata/cholera_covariates}"
export PG_LOG="${PG_LOG:-$SHARE/logs/postgres}"
export DB_ENDPOINT_FILE="${DB_ENDPOINT_FILE:-$SHARE/db_endpoint}"
export DB_STOP_FILE="${DB_STOP_FILE:-$SHARE/db_stop_requested}"
# Directories the container must see
export CHOLERA_SIF_BINDS="${CHOLERA_SIF_BINDS:-$SHARE,$SCRATCH_SHARE}"

# --- Database client settings (libpq; read by R, psql, raster2pgsql, GDAL) ---
export PGPORT="${PGPORT:-5433}"
export PGDATABASE="${PGDATABASE:-cholera_covariates_${CMP_AOI_TAG}}"
# PGUSER / PGPASSWORD come from the secrets file, never from git
if [[ -r "$CMP_SECRETS/db.env" ]]; then
  # shellcheck disable=SC1091
  source "$CMP_SECRETS/db.env"
fi

# --- R and GDAL from modules (same stack as the climate project's GDAL jobs) -
if command -v module >/dev/null 2>&1; then
  module load GCCcore/12.3.0 GCC/12.3.0 libdeflate/1.18 Abseil/20230125.3 \
              OpenMPI/4.1.5 R/4.3.2 GDAL/3.7.1 PostgreSQL/16.1 >/dev/null 2>&1 \
    || echo "env.sh: module load failed; check 'module spider R/4.3.2'" >&2
fi
export R_LIBS_USER="${R_LIBS_USER:-$HOME/R_libs/cmp-4.3.2}"
mkdir -p "$R_LIBS_USER"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
# Temporary rasters on the node-local SSD (purged at job end) when in a job.
# The container must see TMPDIR: raster2pgsql reads VRTs written there.
if [[ -n "${SLURM_JOB_ID:-}" && -d /scratch ]] && mkdir -p "/scratch/${USER}_${SLURM_JOB_ID}" 2>/dev/null; then
  export TMPDIR="/scratch/${USER}_${SLURM_JOB_ID}"
  export CHOLERA_SIF_BINDS="$CHOLERA_SIF_BINDS,$TMPDIR"
fi

# --- Helpers ---------------------------------------------------------------
# Run a PostgreSQL/PostGIS binary from the image (natively when CHOLERA_SIF
# is empty, e.g. to test these scripts on a workstation)
cmp_pg() {
  if [[ -n "$CHOLERA_SIF" ]]; then
    apptainer exec --bind "$CHOLERA_SIF_BINDS" "$CHOLERA_SIF" "$@"
  else
    "$@"
  fi
}

# Start postgres in the foreground of a background job (the pattern the HPC
# team tested), then wait until it accepts connections. Sets CMP_PG_PID.
#   cmp_pg_start READY_HOST LOGFILE [postgres options...]
# READY_HOST is where pg_isready probes: "localhost" when serving over TCP,
# the socket directory when TCP is off. The server's own log goes to $PG_LOG.
cmp_pg_start() {
  local ready_host=$1 log=$2; shift 2
  cmp_pg postgres -D "$PGDATA" "$@" >> "$log" 2>&1 &
  CMP_PG_PID=$!
  for ((i = 0; i < 90; i++)); do
    sleep 2
    if ! kill -0 "$CMP_PG_PID" 2>/dev/null; then
      echo "postgres exited during startup; see $log and $PG_LOG" >&2; return 1
    fi
    if cmp_pg pg_isready -q -h "$ready_host" -p "$PGPORT"; then
      return 0
    fi
  done
  echo "postgres not ready after 180 s; see $log and $PG_LOG" >&2
  return 1
}

# Fast shutdown (rolls back open transactions, no recovery needed on restart)
cmp_pg_stop() {
  cmp_pg pg_ctl -D "$PGDATA" -m fast -w -t 100 stop || true
  [[ -n "${CMP_PG_PID:-}" ]] && wait "$CMP_PG_PID" 2>/dev/null
  return 0
}

# Read the server's endpoint ("host port jobid") into PGHOST / PGPORT
cmp_read_endpoint() {
  if [[ ! -s "$DB_ENDPOINT_FILE" ]]; then
    echo "No database endpoint in $DB_ENDPOINT_FILE (is db_serve running?)" >&2
    return 1
  fi
  read -r PGHOST PGPORT DB_SERVE_JOBID < "$DB_ENDPOINT_FILE"
  export PGHOST PGPORT DB_SERVE_JOBID
}

# Wait until the server accepts connections (default 10 minutes)
cmp_wait_db() {
  local tries=${1:-60}
  for ((i = 0; i < tries; i++)); do
    if cmp_read_endpoint 2>/dev/null && cmp_pg pg_isready -q -h "$PGHOST" -p "$PGPORT"; then
      echo "Database ready at $PGHOST:$PGPORT (server job $DB_SERVE_JOBID)"
      return 0
    fi
    sleep 10
  done
  echo "Database not ready after $((tries * 10)) s" >&2
  return 1
}
