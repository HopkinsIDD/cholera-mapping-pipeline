#!/bin/bash
# Open an SSH tunnel from a laptop to the running database server.
# SSH to a compute node is allowed while you have a job there; the server job
# is yours if you started it. Otherwise ask the person who started it, or run
# psql from a job on the cluster.
#
# Usage: bash hpc/yggdrasil/tools/pg_tunnel.sh USER [LOCAL_PORT]
# Then:  psql -h localhost -p LOCAL_PORT -U cholera_app cholera_covariates_bdi
set -euo pipefail
U=${1:?cluster user}; LPORT=${2:-5433}
LOGIN=login1.yggdrasil.hpc.unige.ch
SHARE=${SHARE:-/home/shares/azman/cholera_mapping}
read -r HOST PORT JOB < <(ssh "$U@$LOGIN" "cat $SHARE/db_endpoint")
echo "Server on $HOST:$PORT (job $JOB); forwarding localhost:$LPORT. Ctrl-C to close."
ssh -N -J "$U@$LOGIN" -L "$LPORT:localhost:$PORT" "$U@$HOST"
