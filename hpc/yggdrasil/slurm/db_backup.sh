#!/bin/bash
#SBATCH --job-name=db_backup
#SBATCH --output=logs/%x_%j.log
#SBATCH --partition=shared-cpu
#SBATCH --time=04:00:00
#SBATCH --mem=8G
#SBATCH -c 2
# Logical backup of the area-of-interest database to $CMP_BACKUPS (keeps the
# last 3). Uses the image's pg_dump, which matches the server version.
# Restore: cmp_pg pg_restore -j 4 -d $PGDATABASE <dump directory>
set -euo pipefail
source "$(dirname "$(readlink -f "$0")")/../env.sh"
cmp_wait_db
mkdir -p "$CMP_BACKUPS"
OUT="$CMP_BACKUPS/${PGDATABASE}_$(date +%Y%m%d_%H%M)"
cmp_pg pg_dump -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -Fd -j 2 -f "$OUT" "$PGDATABASE"
chmod -R g+rX "$OUT"
du -sh "$OUT"
ls -dt "$CMP_BACKUPS/${PGDATABASE}"_* | tail -n +4 | xargs -r rm -rf
