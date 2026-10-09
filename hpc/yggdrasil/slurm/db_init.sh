#!/bin/bash
#SBATCH --job-name=db_init
#SBATCH --output=logs/%x_%j.log
#SBATCH --partition=shared-cpu
#SBATCH --time=00:30:00
#SBATCH --mem=4G
#SBATCH -c 2
# Create the PostgreSQL data directory, roles, the area-of-interest database,
# extensions and schemas. Run once per data directory; refuses to touch an
# existing one. The submitting user becomes the database superuser.
#
# Needs $CMP_SECRETS/superuser.pw (one line) and $CMP_SECRETS/db.env with
#   export PGUSER=cholera_app
#   export PGPASSWORD='...'
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
SQL_DIR="$CMP_REPO/hpc/yggdrasil/sql"

if [[ -e "$PGDATA/PG_VERSION" ]]; then
  echo "PGDATA already initialised: $PGDATA (adding database $PGDATABASE only)"
  NEW_CLUSTER=0
else
  NEW_CLUSTER=1
fi
for f in "$CMP_SECRETS/superuser.pw" "$CMP_SECRETS/db.env"; do
  [[ -r "$f" ]] || { echo "Missing $f" >&2; exit 1; }
done
: "${PGPASSWORD:?PGPASSWORD for cholera_app must be set in $CMP_SECRETS/db.env}"

if [[ $NEW_CLUSTER == 1 ]]; then
  mkdir -p "$PGDATA" "$PG_LOG"
  chmod 750 "$PGDATA"
  cmp_pg initdb -D "$PGDATA" -U "$USER" --pwfile="$CMP_SECRETS/superuser.pw" \
    --auth-local=peer --auth-host=scram-sha-256 --data-checksums \
    --allow-group-access --encoding=UTF8 --locale=C.UTF-8
  export PG_SHARED_BUFFERS=${PG_SHARED_BUFFERS:-4GB} PG_WORK_MEM=${PG_WORK_MEM:-256MB} \
         PG_MAINT_MEM=${PG_MAINT_MEM:-2GB} PG_EFFECTIVE_CACHE=${PG_EFFECTIVE_CACHE:-12GB} \
         PG_SUPERUSER="$USER" CMP_CLUSTER_CIDR=${CMP_CLUSTER_CIDR:-0.0.0.0/0}
  envsubst < "$SQL_DIR/postgresql.conf.tmpl" > "$PGDATA/postgresql.conf"
  envsubst < "$SQL_DIR/pg_hba.conf.tmpl" > "$PGDATA/pg_hba.conf"
fi

# Socket only while initialising: nobody else can connect
SOCK=$(mktemp -d /tmp/cmpinit.XXXX)
trap 'cmp_pg_stop; rm -rf "$SOCK"' EXIT
cmp_pg_start "$SOCK" "$PG_LOG/init_${SLURM_JOB_ID:-local}.log" \
  -c listen_addresses='' -k "$SOCK" -p "$PGPORT"

PSQL=(cmp_pg psql -v ON_ERROR_STOP=1 -h "$SOCK" -p "$PGPORT" -U "$USER")
"${PSQL[@]}" -d postgres -v app_password="'$PGPASSWORD'" -f "$SQL_DIR/00_roles.sql"
"${PSQL[@]}" -d postgres -v dbname="$PGDATABASE" -f "$SQL_DIR/01_database.sql"
"${PSQL[@]}" -d "$PGDATABASE" -f "$SQL_DIR/02_schemas_grants.sql"
"${PSQL[@]}" -d "$PGDATABASE" -c "SELECT postgis_full_version();"
echo "db_init OK: $PGDATA, database $PGDATABASE"
