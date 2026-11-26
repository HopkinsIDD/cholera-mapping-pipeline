#!/bin/bash
# Push a staging directory (tools/stage_inputs_local.sh) and/or the PostGIS
# image to the shared project directory on Yggdrasil.
#
# Usage:
#   bash hpc/yggdrasil/tools/rsync_inputs_yggdrasil.sh --remote USER@login1.yggdrasil.hpc.unige.ch \
#        [--stage stage_dir] [--sif postgis_17-3.5.sif] [--share /home/shares/azman/cholera_mapping] [--dry-run]
set -euo pipefail
REMOTE="${REMOTE:-}"; STAGE=""; SIF=""; SHARE="/home/shares/azman/cholera_mapping"; DRY=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    --remote) REMOTE="$2"; shift 2 ;;
    --stage) STAGE="$2"; shift 2 ;;
    --sif) SIF="$2"; shift 2 ;;
    --share) SHARE="$2"; shift 2 ;;
    --dry-run) DRY=(--dry-run); shift ;;
    *) echo "Unknown argument $1" >&2; sed -n '2,8p' "$0"; exit 1 ;;
  esac
done
[[ -n "$REMOTE" ]] || { echo "--remote USER@host is required" >&2; exit 1; }
OPTS=(-avh --progress --chmod=Dg+rwxs,Fg+rw "${DRY[@]}")
if [[ -n "$STAGE" ]]; then
  rsync "${OPTS[@]}" "$STAGE/Layers/" "$REMOTE:$SHARE/Layers/"
  rsync "${OPTS[@]}" "$STAGE/data/" "$REMOTE:$SHARE/data/"
fi
if [[ -n "$SIF" ]]; then
  rsync "${OPTS[@]}" "$SIF" "$REMOTE:$SHARE/sif/"
fi
