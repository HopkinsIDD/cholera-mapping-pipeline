#!/bin/bash
# Assemble everything the cluster needs for one area of interest in a local
# staging directory (run on a laptop with taxonomy-database access).
#
# Usage:
#   bash hpc/yggdrasil/tools/stage_inputs_local.sh \
#     --config CONFIG.yml \
#     --covariates-repo ~/projects/cholera-covariates   # git-lfs checkout, files pulled
#     --worldpop /path/to/ppp_2020_1km_Aggregated.tif     # WorldPop 2020 1 km mosaic
#     [--stage stage_dir] [--aoi BDI] [--margin-km 150]
#
# Produces <stage>/Layers/{covariate_dictionary.yml, pop_old/, admin_units/,
# <covariate dirs>} and <stage>/data/observations_<AOI>.rds, then lists sizes.
# Push with tools/rsync_inputs_yggdrasil.sh --stage <stage>.
set -euo pipefail
CONFIG=""; COVREPO=""; WORLDPOP=""; STAGE="stage_yggdrasil"; AOI=""; MARGIN=150
while [[ $# -gt 0 ]]; do
  case "$1" in
    --config) CONFIG=$(readlink -f "$2"); shift 2 ;;
    --covariates-repo) COVREPO=$(readlink -f "$2"); shift 2 ;;
    --worldpop) WORLDPOP=$(readlink -f "$2"); shift 2 ;;
    --stage) STAGE="$2"; shift 2 ;;
    --aoi) AOI="$2"; shift 2 ;;
    --margin-km) MARGIN="$2"; shift 2 ;;
    *) echo "Unknown argument $1" >&2; sed -n '2,15p' "$0"; exit 1 ;;
  esac
done
for v in CONFIG COVREPO WORLDPOP; do
  [[ -n "${!v}" ]] || { echo "--$(echo $v | tr '[:upper:]' '[:lower:]') is required" >&2; exit 1; }
done
REPO=$(cd "$(dirname "$(readlink -f "$0")")/../../.." && pwd)
mkdir -p "$STAGE/Layers/pop_old" "$STAGE/data"
STAGE=$(readlink -f "$STAGE")
AOI=${AOI:-$(Rscript -e 'cat(yaml::read_yaml(commandArgs(TRUE)[1])$aoi)' "$CONFIG")}
export CHOLERA_AOI_CACHE_DIR="$STAGE/Layers/admin_units"

echo "== 1. Observations and admin boundaries ($AOI)"
(cd "$REPO" && Rscript Analysis/R/pull_observations_local.R -c "$CONFIG" \
   -o "$STAGE/data/observations_${AOI}.rds" -l "$STAGE/Layers")
Rscript -e 'taxdat::cache_admin_units(commandArgs(TRUE)[1], 0:2)' "$AOI"

echo "== 2. WorldPop 1 km grid cropped to $AOI + ${MARGIN} km"
TE=$(Rscript -e 'sf::sf_use_s2(FALSE); a <- suppressMessages(taxdat::get_aoi(commandArgs(TRUE)[1], as.numeric(commandArgs(TRUE)[2]), snap_to = commandArgs(TRUE)[3])); cat("TE", a$bbox[c("xmin","ymin","xmax","ymax")], "\n")' \
     "$AOI" "$MARGIN" "$WORLDPOP" | sed -n 's/^TE //p')
[[ $(wc -w <<< "$TE") == 4 ]] || { echo "Could not compute the crop extent for $AOI (see the R error above)" >&2; exit 1; }
gdalwarp -overwrite -q -te $TE -co COMPRESS=DEFLATE -co TILED=YES "$WORLDPOP" \
  "$STAGE/Layers/pop_old/ppp_2020_1km_Aggregated.tif"

echo "== 3. Covariate dictionary and raw covariates"
cp "$COVREPO/covariate_dictionary.yml" "$STAGE/Layers/"
DIRS=$(Rscript -e '
cfg <- yaml::read_yaml(commandArgs(TRUE)[1]); d <- yaml::read_yaml(commandArgs(TRUE)[2])
keep <- unique(c(names(d)[vapply(d, `[[`, "", "abbr") == "p"], unlist(cfg$covariate_choices)))
cat(vapply(d[keep], function(x) sub("/.*$", "", x$dir), ""), sep = "\n")' "$CONFIG" "$COVREPO/covariate_dictionary.yml")
for d in $DIRS; do
  if find "$COVREPO/$d" -type f -size -1k -name '*.nc' -o -type f -size -1k -name '*.tif' | grep -q .; then
    echo "  $d: some files are git-lfs pointers; run 'git lfs pull --include \"$d/*\"' in $COVREPO" >&2
    exit 1
  fi
  rsync -a "$COVREPO/$d" "$STAGE/Layers/"
  echo "  $d"
done
du -sh "$STAGE"/Layers/* "$STAGE"/data/* 2>/dev/null
echo "Staged in $STAGE. Next: bash hpc/yggdrasil/tools/rsync_inputs_yggdrasil.sh --stage $STAGE --remote USER@login1.yggdrasil.hpc.unige.ch"
