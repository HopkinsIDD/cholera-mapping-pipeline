#!/bin/bash
# Build the covariates database for one area of interest and test it with a
# mapping run (login node). Submits a dependency chain:
#   db_serve -> prepare_grid -> covariates_precompute[array] -> covariates_ingest
#            -> mapping_run (smoke, Stan skipped) -> db_diagnostics + db_backup
#
# Usage:
#   bash hpc/yggdrasil/stages/db_build.sh --config CONFIG.yml --observations OBS.rds \
#        [--covariates "dw,aw"] [--res 20] [--mode ingest_missing] [--dry-run]
# --covariates defaults to the config's covariate_choices; population is always
# added, at the model resolution and at 1 km (population weights).
set -euo pipefail
HERE="$(dirname "$(readlink -f "$0")")"
source "$HERE/../env.sh"

CONFIG=""; OBS=""; COVS=""; RES=""; MODE="ingest_missing"; DRY=0
while [[ $# -gt 0 ]]; do
  case "$1" in
    --config) CONFIG=$(readlink -f "$2"); shift 2 ;;
    --observations) OBS=$(readlink -f "$2"); shift 2 ;;
    --covariates) COVS="$2"; shift 2 ;;
    --res) RES="$2"; shift 2 ;;
    --mode) MODE="$2"; shift 2 ;;
    --dry-run) DRY=1; shift ;;
    *) echo "Unknown argument $1" >&2; sed -n '2,13p' "$0"; exit 1 ;;
  esac
done
[[ -f "$CONFIG" ]] || { echo "--config is required" >&2; exit 1; }

# Pre-flight checks -----------------------------------------------------------
fail=0
check() { if eval "$2"; then echo "  ok   $1"; else echo "  FAIL $1" >&2; fail=1; fi; }
echo "Pre-flight checks:"
check "image $CHOLERA_SIF"                "[[ -f '$CHOLERA_SIF' ]]"
check "data directory $PGDATA"            "[[ -e '$PGDATA/PG_VERSION' ]]"
check "secrets $CMP_SECRETS/db.env"       "[[ -r '$CMP_SECRETS/db.env' ]]"
check "covariate dictionary"              "[[ -f '$CMP_LAYERS/covariate_dictionary.yml' ]]"
check "WorldPop master grid source"       "[[ -f '$CHOLERA_MASTER_GRID_FILE' ]]"
check "admin units for $CMP_AOI"          "[[ -f '$CHOLERA_AOI_CACHE_DIR/${CMP_AOI}_adm0.gpkg' ]]"
check "observations file"                 "[[ -z '$OBS' || -f '$OBS' ]]"
check "taxdat installed"                  "Rscript -e 'library(taxdat)' >/dev/null 2>&1"
check "config dictionary (Analysis/configs)" "[[ -f '$CMP_REPO/Analysis/configs/config_dictionary.yml' ]]"
[[ $fail == 0 ]] || { echo "Fix the failed checks first (see hpc/yggdrasil/README.md)." >&2; exit 1; }

# Covariates and resolution from the config ------------------------------------
read -r CFG_RES CFG_COVS < <(Rscript -e '
cfg <- yaml::read_yaml(commandArgs(TRUE)[1]); dict <- yaml::read_yaml(commandArgs(TRUE)[2])
abbr <- vapply(dict[unlist(cfg$covariate_choices)], `[[`, "", "abbr")
cat(cfg$res_space, paste(c("p", abbr), collapse = ","), "\n")' "$CONFIG" "$CMP_LAYERS/covariate_dictionary.yml")
RES=${RES:-$CFG_RES}
COVS=${COVS:-$CFG_COVS}
PAIRS=$(echo "p,$COVS" | tr ',' '\n' | awk 'NF' | sort -u | awk -v r="$RES" '{print $1 ":" r}' | paste -sd, -)
PAIRS="$PAIRS,p:1"
N=$(echo "$PAIRS" | tr ',' '\n' | wc -l)
echo "Area of interest $CMP_AOI (+${CMP_AOI_BUFFER_KM} km), database $PGDATABASE"
echo "Covariates: $PAIRS"

S="$HERE/../slurm"
if [[ $DRY == 1 ]]; then
  echo "Dry run; would submit: db_serve (if not running), prepare_grid $RES 1,"
  echo "  covariates_precompute --array=0-$((N - 1)), covariates_ingest, mapping_run, db_backup"
  exit 0
fi

mkdir -p "$CMP_REPO/logs"
cd "$CMP_REPO"
SERVE=$(squeue -h -u "$USER" -n db_serve -o %i 2>/dev/null | head -1)
if [[ -z "$SERVE" ]]; then
  SERVE=$(sbatch --parsable "$S/db_serve.sh")
  echo "db_serve:   $SERVE (new)"
else
  echo "db_serve:   $SERVE (already running or queued)"
fi
GRID=$(sbatch --parsable --dependency=after:"$SERVE" "$S/prepare_grid.sh" "$RES" 1)
PRE=$(sbatch --parsable --dependency=afterok:"$GRID" --array=0-$((N - 1)) "$S/covariates_precompute.sh" "$PAIRS")
ING=$(sbatch --parsable --dependency=afterok:"$PRE" "$S/covariates_ingest.sh" "$PAIRS" "$MODE")
RUN=$(sbatch --parsable --dependency=afterok:"$ING" "$S/mapping_run.sh" "$CONFIG" ${OBS:+"$OBS"})
DIAG=$(sbatch --parsable --dependency=afterok:"$RUN" "$S/db_diagnostics.sh" "$CONFIG")
BKP=$(sbatch --parsable --dependency=afterok:"$RUN" "$S/db_backup.sh")
cat <<MSG
prepare_grid:          $GRID
covariates_precompute: $PRE (array 0-$((N - 1)))
covariates_ingest:     $ING
mapping_run (smoke):   $RUN
db_diagnostics:        $DIAG  (figures in $SHARE/diagnostics/$PGDATABASE/)
db_backup:             $BKP
Logs in $CMP_REPO/logs/. Stop the server when done:
  bash hpc/yggdrasil/stages/db_service.sh stop
MSG
