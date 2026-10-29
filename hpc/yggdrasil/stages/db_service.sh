#!/bin/bash
# Start, stop or inspect the database server job (login node).
# Usage: bash hpc/yggdrasil/stages/db_service.sh start|stop|status
#   start  submit slurm/db_serve.sh (queues behind a running one: singleton)
#   stop   ask the running server to shut down cleanly
#   status endpoint, job state and remaining time
set -euo pipefail
HERE="$(dirname "$(readlink -f "$0")")"
source "$HERE/../env.sh"
mkdir -p "$CMP_REPO/logs"
cd "$CMP_REPO"

running_jobs() { squeue -h -u "$USER" -n db_serve -o "%i %T %L %N" 2>/dev/null || true; }

case "${1:-status}" in
  start)
    JOB=$(sbatch --parsable "$HERE/../slurm/db_serve.sh")
    echo "Submitted db_serve job $JOB"
    echo "DB_SERVE_JOB_ID=$JOB"
    ;;
  stop)
    if [[ ! -s "$DB_ENDPOINT_FILE" ]]; then
      echo "No endpoint file; nothing to stop."; running_jobs; exit 0
    fi
    touch "$DB_STOP_FILE"
    echo "Stop requested; the server stops within a minute. Jobs:"; running_jobs
    ;;
  status)
    if cmp_read_endpoint 2>/dev/null; then
      echo "Endpoint: $PGHOST:$PGPORT (job $DB_SERVE_JOBID), database $PGDATABASE"
      cmp_pg pg_isready -h "$PGHOST" -p "$PGPORT" || true
    else
      echo "No endpoint: the server is not running."
    fi
    echo "db_serve jobs (id state time_left node):"; running_jobs
    ;;
  *)
    echo "usage: $0 start|stop|status" >&2; exit 1
    ;;
esac
