#!/bin/bash
# Assemble everything the cluster needs for one area of interest in a local
# staging directory (run on a laptop with taxonomy-database access).
#
# Usage:
#   bash hpc/yggdrasil/tools/stage_inputs_local.sh \
#     --config CONFIG.yml \
#     --covariates-repo ~/projects/cholera-covariates   # git-lfs checkout, files pulled
#     --worldpop /path/to/ppp_2020_1km_Aggregated.tif     # WorldPop 2020 1 km mosaic
#     [--stage stage_dir] [--aoi BDI] [--margin-km 150] [--skip-pull] [--no-crop]
#
# By default the raw covariates are cropped to the same window as the grid
# (area of interest + margin): the pipeline crops to the area of interest
# anyway, and population drops from ~14 GB to a few MB. --no-crop copies the
# full files (needed for a global or multi-country database). --skip-pull
# keeps an existing observations file and boundary cache.
#
# Produces <stage>/Layers/{covariate_dictionary.yml, pop_old/, admin_units/,
# <covariate dirs>} and <stage>/data/observations_<AOI>.rds, then lists sizes.
# Push with tools/rsync_inputs_yggdrasil.sh --stage <stage>.
set -euo pipefail
CONFIG=""; COVREPO=""; WORLDPOP=""; STAGE="stage_yggdrasil"; AOI=""; MARGIN=150; PULL=1; CROP=1
while [[ $# -gt 0 ]]; do
  case "$1" in
    --config) CONFIG=$(readlink -f "$2"); shift 2 ;;
    --covariates-repo) COVREPO=$(readlink -f "$2"); shift 2 ;;
    --worldpop) WORLDPOP=$(readlink -f "$2"); shift 2 ;;
    --stage) STAGE="$2"; shift 2 ;;
    --aoi) AOI="$2"; shift 2 ;;
    --margin-km) MARGIN="$2"; shift 2 ;;
    --skip-pull) PULL=0; shift ;;
    --no-crop) CROP=0; shift ;;
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

if [[ $PULL == 1 ]]; then
  echo "== 1. Observations and admin boundaries ($AOI)"
  (cd "$REPO" && Rscript Analysis/R/pull_observations_local.R -c "$CONFIG" \
     -o "$STAGE/data/observations_${AOI}.rds" -l "$STAGE/Layers")
  Rscript -e 'taxdat::cache_admin_units(commandArgs(TRUE)[1], 0:2)' "$AOI"
else
  echo "== 1. Skipped (--skip-pull): keeping $STAGE/data and $STAGE/Layers/admin_units"
  [[ -f "$STAGE/Layers/admin_units/${AOI}_adm0.gpkg" ]] || { echo "No cached ${AOI} boundaries in $STAGE" >&2; exit 1; }
fi

echo "== 2. WorldPop 1 km grid cropped to $AOI + ${MARGIN} km"
TE=$(Rscript -e 'sf::sf_use_s2(FALSE); a <- suppressMessages(taxdat::get_aoi(commandArgs(TRUE)[1], as.numeric(commandArgs(TRUE)[2]), snap_to = commandArgs(TRUE)[3])); cat("TE", a$bbox[c("xmin","ymin","xmax","ymax")], "\n")' \
     "$AOI" "$MARGIN" "$WORLDPOP" | sed -n 's/^TE //p')
[[ $(wc -w <<< "$TE") == 4 ]] || { echo "Could not compute the crop extent for $AOI (see the R error above)" >&2; exit 1; }
gdalwarp -overwrite -q -te $TE -co COMPRESS=DEFLATE -co TILED=YES "$WORLDPOP" \
  "$STAGE/Layers/pop_old/ppp_2020_1km_Aggregated.tif"
echo "Cropped to $AOI + ${MARGIN} km (extent $TE) from $WORLDPOP on $(date +%F). Not valid for other areas." \
  > "$STAGE/Layers/pop_old/CROPPED_TO_${AOI}_${MARGIN}KM.txt"

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
  if [[ $CROP == 0 ]]; then
    rsync -a "$COVREPO/$d" "$STAGE/Layers/"
    echo "  $d (full copy)"
    continue
  fi
  # Crop every raster to the grid window; copy anything else as is
  [[ -n "$d" && "$d" != "." && "$d" != ".." ]] || { echo "Bad covariate directory '$d'" >&2; exit 1; }
  rm -rf "$STAGE/Layers/$d.part" && mkdir -p "$STAGE/Layers/$d.part"
  find "$COVREPO/$d" -maxdepth 1 -type f ! -name '*.nc' ! -name '*.tif' -exec cp {} "$STAGE/Layers/$d.part/" \;
  Rscript -e '
args <- commandArgs(TRUE); te <- as.numeric(args[1:4]); src <- args[5]; out <- args[6]
for (f in list.files(src, "\\.(nc|tif)$", full.names = TRUE)) {
  dst <- file.path(out, basename(f))
  if (grepl("\\.nc$", f)) {
    m <- taxdat::get_ncdf_metadata(f)
    r <- suppressWarnings(terra::crop(terra::rast(f), terra::ext(te[1], te[3], te[2], te[4]), snap = "out"))
    taxdat::write_covariate_ncdf(r, dst, var_name = m$var_name, long_name = m$var_att$long_name,
                                 unit = if (is.null(m$var_att$units)) "" else m$var_att$units, dates = m$dates)
  } else {
    system2("gdalwarp", c("-q", "-overwrite", "-te", te, "-co", "COMPRESS=DEFLATE", f, dst))
  }
}' $TE "$COVREPO/$d" "$STAGE/Layers/$d.part"
  echo "Cropped to $AOI + ${MARGIN} km (extent $TE) from $COVREPO/$d on $(date +%F). Not valid for other areas." \
    > "$STAGE/Layers/$d.part/CROPPED_TO_${AOI}_${MARGIN}KM.txt"
  rm -rf "$STAGE/Layers/$d" && mv "$STAGE/Layers/$d.part" "$STAGE/Layers/$d"
  echo "  $d (cropped)"
done
du -sh "$STAGE"/Layers/* "$STAGE"/data/* 2>/dev/null
echo "Staged in $STAGE. Next: bash hpc/yggdrasil/tools/rsync_inputs_yggdrasil.sh --stage $STAGE --remote USER@login1.yggdrasil.hpc.unige.ch"
