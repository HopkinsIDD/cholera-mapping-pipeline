#!/bin/bash
# Initialise a PostgreSQL + PostGIS server for the covariates database inside
# the Docker image (runs as root; the server runs as the postgres OS user).
# On Yggdrasil use hpc/yggdrasil/slurm/db_init.sh instead.
#
# Environment:
#   CHOLERA_APP_PASSWORD  password for the cholera_app login role (required)
#   PGDATABASE            database to create (default cholera_covariates)
#   PGDATA                data directory (default /var/lib/postgresql/data)
set -euo pipefail

: "${CHOLERA_APP_PASSWORD:?set CHOLERA_APP_PASSWORD}"
PGBIN=${PGBIN:-/usr/lib/postgresql/17/bin}
PGDATA=${PGDATA:-/var/lib/postgresql/data}
DBNAME=${PGDATABASE:-cholera_covariates}
REPO_DIR=$(dirname "$(readlink -f "$0")")
SQL_DIR=${SQL_DIR:-$REPO_DIR/hpc/yggdrasil/sql}

if [[ ! -f "$PGDATA/PG_VERSION" ]]; then
  mkdir -p "$PGDATA" && chown postgres:postgres "$PGDATA"
  su postgres -c "$PGBIN/initdb -D $PGDATA --auth-local=peer --auth-host=scram-sha-256"
fi
su postgres -c "$PGBIN/pg_ctl -D $PGDATA -l $PGDATA/../logfile -w start"

run_sql() {  # run_sql DB FILE [psql -v args...]
  local db=$1 file=$2; shift 2
  sudo -u postgres psql -v ON_ERROR_STOP=1 -d "$db" "$@" -f "$file"
}
run_sql postgres "$SQL_DIR/00_roles.sql" -v app_password="'$CHOLERA_APP_PASSWORD'"
run_sql postgres "$SQL_DIR/01_database.sql" -v dbname="$DBNAME"
run_sql "$DBNAME" "$SQL_DIR/02_schemas_grants.sql"
echo "Database $DBNAME ready."
