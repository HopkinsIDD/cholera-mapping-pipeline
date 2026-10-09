#!/bin/bash
# Create a read-write login role for USERNAME on the local Docker server.
# Usage: NEW_USER_PASSWORD=... bash grant_cholera_database.sh USERNAME
set -euo pipefail
REPO_DIR=$(dirname "$(readlink -f "$0")")
PSQL_SUPER="${PSQL_SUPER:-sudo -u postgres psql}" \
  bash "$REPO_DIR/hpc/yggdrasil/tools/create_db_user.sh" "$1"
