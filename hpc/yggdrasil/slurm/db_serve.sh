#!/bin/bash
#SBATCH --job-name=db_serve
#SBATCH --output=logs/%x_%j.log
#SBATCH --partition=public-longrun-cpu
#SBATCH --time=14-00:00:00
#SBATCH --mem=20G
#SBATCH -c 2
#SBATCH --dependency=singleton
#SBATCH --signal=B:TERM@120
# Serve the covariates database for up to 14 days (public-longrun-cpu: 2
# cores max). Writes "host port jobid" to $DB_ENDPOINT_FILE once ready and
# removes it on shutdown. Stop with stages/db_service.sh stop.
# --dependency=singleton: a second db_serve queues until this one ends, so a
# data directory is never served twice.
set -uo pipefail
# sbatch runs a copy of this script from /var/spool/slurmd, so find the
# repository from the submission directory (submit from the repository root);
# with plain `bash`, from this file's location.
CMP_REPO="${CMP_REPO:-${SLURM_SUBMIT_DIR:-$(cd "$(dirname "$(readlink -f "$0")")/../../.." && pwd)}}"
if [[ ! -f "$CMP_REPO/hpc/yggdrasil/env.sh" ]]; then
  echo "Cannot find hpc/yggdrasil/env.sh under $CMP_REPO: submit from the repository root or export CMP_REPO" >&2
  exit 1
fi
source "$CMP_REPO/hpc/yggdrasil/env.sh"

[[ -e "$PGDATA/PG_VERSION" ]] || { echo "PGDATA not initialised; run slurm/db_init.sh" >&2; exit 1; }
if [[ -s "$DB_ENDPOINT_FILE" ]]; then
  read -r old_host old_port old_job < "$DB_ENDPOINT_FILE"
  if squeue -h -j "$old_job" >/dev/null 2>&1 && [[ -n "$(squeue -h -j "$old_job" 2>/dev/null)" ]]; then
    echo "Another db_serve ($old_job on $old_host) is running" >&2; exit 1
  fi
  echo "Removing stale endpoint from job $old_job"; rm -f "$DB_ENDPOINT_FILE"
fi
# db_init.sh runs its own server on PGDATA; never start next to it
INIT_MARKER="${PGDATA%/}.init_job"
if [[ -s "$INIT_MARKER" ]]; then
  init_job=$(cat "$INIT_MARKER")
  if [[ -n "$(squeue -h -j "$init_job" 2>/dev/null)" ]]; then
    echo "db_init job $init_job is using $PGDATA; resubmit once it finishes" >&2; exit 1
  fi
  echo "Removing stale db_init marker from job $init_job"; rm -f "$INIT_MARKER"
fi
# A node crash can leave a pid file behind; postgres refuses to start then
if [[ -f "$PGDATA/postmaster.pid" ]]; then
  echo "Stale postmaster.pid found (previous server did not stop cleanly); removing"
  rm -f "$PGDATA/postmaster.pid"
fi
mkdir -p "$PG_LOG"

stop_server() {
  echo "$(date '+%F %T') stopping postgres"
  rm -f "$DB_ENDPOINT_FILE"
  cmp_pg_stop
  exit 0
}
trap stop_server TERM INT

SERVER_LOG="$PG_LOG/server_${SLURM_JOB_ID}.log"
cmp_pg_start localhost "$SERVER_LOG" -p "$PGPORT" || { cmp_pg_stop; exit 1; }
HOST=$(hostname -s)
echo "$HOST $PGPORT $SLURM_JOB_ID" > "$DB_ENDPOINT_FILE.tmp" && mv "$DB_ENDPOINT_FILE.tmp" "$DB_ENDPOINT_FILE"
chmod g+r "$DB_ENDPOINT_FILE"
echo "$(date '+%F %T') serving $PGDATABASE at $HOST:$PGPORT (job $SLURM_JOB_ID)"

# Background sleep + wait so the TERM trap runs promptly
while true; do
  sleep 60 & wait $!
  if ! kill -0 "$CMP_PG_PID" 2>/dev/null || ! cmp_pg pg_isready -q -h localhost -p "$PGPORT"; then
    echo "$(date '+%F %T') postgres is not responding; see $SERVER_LOG" >&2
    rm -f "$DB_ENDPOINT_FILE"; cmp_pg_stop; exit 1
  fi
  if [[ -f "$DB_STOP_FILE" ]]; then
    rm -f "$DB_STOP_FILE"; stop_server
  fi
done
