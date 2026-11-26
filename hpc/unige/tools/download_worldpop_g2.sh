#!/bin/bash
# Download WorldPop Global 2 1 km population to the Layers directory. Run on
# the LOGIN node (compute nodes have no internet); about 5.5 GB, resumable.
#   - constrained global mosaics of release R2025A, one per year (population)
#   - the unconstrained 2020 mosaic of R2024B (land mask and master grid)
#
# Usage: bash hpc/unige/tools/download_worldpop_g2.sh [--years "2015 2016 ..."] [--dest DIR] [--dry-run]
# Default destination: $CMP_LAYERS/worldpop_g2_raw
set -euo pipefail
HERE="$(dirname "$(readlink -f "$0")")"
source "$HERE/../env.sh"

YEARS=$(seq 2015 2030); DEST="$CMP_LAYERS/worldpop_g2_raw"; DRY=0
while [[ $# -gt 0 ]]; do
  case "$1" in
    --years) YEARS="$2"; shift 2 ;;
    --dest) DEST="$2"; shift 2 ;;
    --dry-run) DRY=1; shift ;;
    *) echo "Unknown argument $1" >&2; sed -n '2,9p' "$0"; exit 1 ;;
  esac
done

BASE=https://data.worldpop.org/GIS/Population/Global_2015_2030
URLS=("$BASE/R2024B/2020/0_Mosaicked/v1/1km_ua/unconstrained/global_pop_2020_UC_1km_R2024B_UA_v1.tif")
for y in $YEARS; do
  URLS+=("$BASE/R2025A/$y/0_Mosaicked/v1/1km_ua/constrained/global_pop_${y}_CN_1km_R2025A_UA_v1.tif")
done

mkdir -p "$DEST"
cd "$DEST"
fail=0
for url in "${URLS[@]}"; do
  f=$(basename "$url")
  if [[ $DRY == 1 ]]; then echo "$url"; continue; fi
  size=$(curl -sIL "$url" | awk 'tolower($1) == "content-length:" {s = $2} END {print s + 0}')
  if [[ -f "$f" && "$size" -gt 0 && $(stat -c %s "$f") == "$size" ]]; then
    echo "ok       $f"
    continue
  fi
  echo "download $f ($((size / 1048576)) MB)"
  curl -fL --retry 5 --retry-delay 10 -C - -o "$f.part" "$url" && mv "$f.part" "$f"
  if [[ "$size" -gt 0 && $(stat -c %s "$f") != "$size" ]]; then
    echo "FAIL     $f: size differs from the server's ($size bytes)" >&2; fail=1
  fi
done
[[ $DRY == 1 ]] && exit 0
[[ $fail == 0 ]] || exit 1

# Every file must open and sit on the same lattice
for f in global_pop_*.tif; do
  gdalinfo "$f" | awk -v f="$f" '/^Size is|^Origin|^Pixel Size/ {printf "%s ", $0} END {print "<- " f}'
done | tee lattice.txt
if [[ $(sed 's/ <- .*//' lattice.txt | sort -u | wc -l) != 1 ]]; then
  echo "FAIL: the files are not all on the same lattice (see $DEST/lattice.txt)" >&2; exit 1
fi
sha256sum global_pop_*.tif > SHA256SUMS
echo "Downloaded on $(date +%F) from $BASE (CC BY 4.0, doi:10.5258/SOTON/WP00845)" > DOWNLOADED.txt
chmod -R g+rX "$DEST"
echo "Done: $(ls global_pop_*.tif | wc -l) files in $DEST"
echo "Master grid source: export CHOLERA_MASTER_GRID_FILE=$DEST/global_pop_2020_UC_1km_R2024B_UA_v1.tif"
