#!/bin/bash
# Create (or update) a login role for a team member on the covariates server.
#
# Usage:
#   NEW_USER_PASSWORD=... bash create_db_user.sh USERNAME [--readonly]
#
# The password is read from NEW_USER_PASSWORD so it never appears in argv.
# PSQL_SUPER is the command that opens psql as the superuser:
#   - on Yggdrasil (inside db_serve's node):  PSQL_SUPER="cmp_pg psql -h $PG_RUN"
#   - in the Docker image:                    PSQL_SUPER="sudo -u postgres psql"
# Read-write users join cholera_owner and act as it by default, so the tables
# they create are shared with the team. --readonly users join cholera_ro.
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "usage: NEW_USER_PASSWORD=... $0 USERNAME [--readonly]" >&2
  exit 1
fi
user="$1"
group="cholera_owner"
[[ "${2:-}" == "--readonly" ]] && group="cholera_ro"
: "${NEW_USER_PASSWORD:?set NEW_USER_PASSWORD in the environment}"
PSQL_SUPER="${PSQL_SUPER:-psql}"

# shellcheck disable=SC2086
$PSQL_SUPER -v ON_ERROR_STOP=1 -d postgres \
  -v u="$user" -v p="$NEW_USER_PASSWORD" -v g="$group" <<'SQL'
SELECT format('CREATE ROLE %I LOGIN', :'u')
WHERE NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = :'u') \gexec
SELECT format('ALTER ROLE %I PASSWORD %L', :'u', :'p') \gexec
SELECT format('GRANT %I TO %I', :'g', :'u') \gexec
SELECT format('ALTER ROLE %I SET role = %L', :'u', :'g')
WHERE :'g' = 'cholera_owner' \gexec
SQL
echo "Role '$user' ready (member of $group)."
