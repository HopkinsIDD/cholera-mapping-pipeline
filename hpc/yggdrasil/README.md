# Covariates database on Yggdrasil

PostgreSQL 17 + PostGIS run inside an Apptainer image in a Slurm job; the
mapping pipeline (R from cluster modules) connects to it over TCP from other
jobs. One database per area of interest (`cholera_covariates_<iso3>`); the
Burundi pilot uses `cholera_covariates_bdi`.

```
hpc/yggdrasil/
  env.sh                 sourced by every script: paths, modules, PG* settings, helpers
  sql/                   roles, database, schemas; postgresql.conf / pg_hba.conf templates
  slurm/                 one job each (submit with sbatch)
    pull_sif.sh            pull docker://postgis/postgis:17-3.5 into $SHARE/sif
    install_r_packages.sh  taxdat and its dependencies into R_LIBS_USER
    db_init.sh             initdb, roles, database, extensions, schemas (once)
    db_serve.sh            serve the database (public-longrun-cpu, 2 cores, 14 days)
    prepare_grid.sh        master grid, modelling grid, 1 km grid
    covariates_precompute.sh  array: one covariate per task, no database access
    covariates_ingest.sh   load pre-computed covariates (serial)
    mapping_run.sh         set_parameters.R for one config (Stan skipped by default)
    db_backup.sh           pg_dump of the database to $SHARE/backups (keeps 3)
    db_diagnostics.sh      figures: build, covariates by year, test-run extraction
  stages/                login-node drivers
    db_service.sh          start | stop | status of the server job
    db_build.sh            submits the whole build chain for one config
  tools/
    stage_inputs_local.sh  laptop: observations, boundaries, cropped WorldPop, raw covariates
    rsync_inputs_yggdrasil.sh  laptop -> $SHARE
    create_db_user.sh      add a team member's database role
    pg_tunnel.sh           laptop -> database through the login node
```

## How it fits together

- **Server.** `db_serve.sh` runs `postgres` from the image on one compute node
  and writes `host port jobid` to `$SHARE/db_endpoint`. Every client job calls
  `cmp_wait_db` (env.sh), which reads that file and waits for `pg_isready`.
  `--dependency=singleton` means a second server job queues behind the first,
  so the data directory is never served twice. On TERM (Slurm sends it 120 s
  before walltime) or `db_service.sh stop`, the server shuts down cleanly and
  removes the endpoint file.
- **Clients.** R and GDAL come from the modules (the same combination as
  OutbreakExtractR). Rasters are loaded from R (`taxdat::load_raster_to_db`):
  `gdal_translate` cuts them into chunks that the server decodes with
  `ST_FromGDALRaster`, because the postgis/postgis image has no
  `raster2pgsql`. R packages: `R_LIBS_USER` puts `~/R_libs/cmp-4.3.2` first
  (taxdat from this branch, anything newly installed) and R's default user
  library second, so packages already compiled there are reused; missing ones
  come from a CRAN snapshot matching R 4.3 (April 2024).
  Connection settings are the libpq variables `PGHOST PGPORT PGDATABASE PGUSER
  PGPASSWORD`; `PGUSER`/`PGPASSWORD` come from `$SHARE/secrets/db.env`.
- **Area of interest.** Rasters are cropped and masked to the country's
  boundary plus `CMP_AOI_BUFFER_KM` (default 50 km) before entering the
  database. Boundaries come from the GeoPackage cache in
  `$SHARE/Layers/admin_units` (compute nodes have no internet).
- **Observations.** Compute nodes cannot reach the taxonomy database, so
  observations are pulled on a laptop (`Analysis/R/pull_observations_local.R`)
  and given to the run with `CHOLERA_OBSERVATIONS_RDS` (`mapping_run.sh` takes
  the file as its second argument).
- **Storage.** The data directory is `$SHARE/pgdata/cholera_covariates` on the
  backed-up home share (fine for the pilot: a few GB). Files in the share
  count against their owner's 1 TB quota. For a full-size database (> 300 GB)
  move `PGDATA` to scratch and keep `db_backup.sh` dumps on home: scratch
  files untouched for 3 months are deleted.

## First-time setup (pilot: Burundi)

On a laptop with taxonomy-database access and a git-lfs checkout of
HopkinsIDD/cholera-covariates (with the needed directories pulled):

```bash
bash hpc/yggdrasil/tools/stage_inputs_local.sh --config Analysis/configs/BDI_pilot.yml \
  --covariates-repo ~/projects/cholera-covariates --worldpop ~/data/ppp_2020_1km_Aggregated.tif
bash hpc/yggdrasil/tools/rsync_inputs_yggdrasil.sh --remote USER@login1.yggdrasil.hpc.unige.ch \
  --stage stage_yggdrasil
```

The pilot config needs `aoi: BDI` (and optionally `aoi_buffer_km`).

On the cluster, from the repository checkout:

```bash
mkdir -p logs /home/shares/azman/cholera_mapping/secrets
chmod 2770 /home/shares/azman/cholera_mapping/secrets
# one line, the database superuser's password (only the initialising user reads it)
nano /home/shares/azman/cholera_mapping/secrets/superuser.pw && chmod 600 $_
# export PGUSER=cholera_app / export PGPASSWORD='...'   (readable by the group)
nano /home/shares/azman/cholera_mapping/secrets/db.env && chmod 640 $_
git clone git@github.com:HopkinsIDD/cholera-configs.git Analysis/configs   # config_dictionary.yml

sbatch hpc/yggdrasil/slurm/pull_sif.sh
sbatch hpc/yggdrasil/slurm/install_r_packages.sh
sbatch hpc/yggdrasil/slurm/db_init.sh          # after pull_sif finishes
bash hpc/yggdrasil/stages/db_build.sh --config Analysis/configs/BDI_pilot.yml \
  --observations /home/shares/azman/cholera_mapping/data/observations_BDI.rds
bash hpc/yggdrasil/stages/db_service.sh status
```

`db_build.sh --dry-run` runs the pre-flight checks only. Stop the server when
you are done: `bash hpc/yggdrasil/stages/db_service.sh stop`.

## Daily use

```bash
bash hpc/yggdrasil/stages/db_service.sh start            # serve (queues if one runs)
sbatch hpc/yggdrasil/slurm/mapping_run.sh CONFIG.yml OBS.rds
bash hpc/yggdrasil/stages/db_service.sh stop
```

Add a teammate (from a job on the server's node, or any node with the
superuser password in `PGPASSWORD`):
`NEW_USER_PASSWORD=... PSQL_SUPER="cmp_pg psql -h $PGHOST -U <superuser>" bash hpc/yggdrasil/tools/create_db_user.sh alamc`

## Global population (full extent, all years)

This builds yearly population for 2000 to 2020 at the full WorldPop extent,
uncropped, in its own database `cholera_covariates_raw`, at 1 km (for
population weights) and 20 km (the model grid). It runs on the same server and
data directory as the pilot. Its inputs live in a separate Layers directory,
because the pilot's `$SHARE/Layers/pop` is cropped to Burundi. The build
refuses inputs that carry a `CROPPED_TO_<ISO>_*.txt` marker.

What ends up in `cholera_covariates_raw`:

| Table | Content |
|---|---|
| `grids.master_grid` | 1 km land mask from WorldPop 2020 |
| `grids.grid_20_20` + `_centroids`, `_polys` | model grid |
| `grids.grid_1_1` | 1 km grid, raster only (nothing reads its geometries) |
| `covariates.pop_1_years_20_20`, `covariates.pop_1_years_1_1` | 21 bands, one per year |
| `covariates.metadata`, `covariates.bands`, `grids.metadata` | aoi_name `raw` |

Tiles that are NoData in every band (oceans) are not stored, which keeps the
1 km table at roughly a third of its full size.

### 1. Laptop: push the full population files (about 14 GB)

```bash
COVREPO=~/projects/archive/cholera-covariates   # git-lfs checkout of HopkinsIDD/cholera-covariates
find "$COVREPO/pop" -name '*.nc' -size -1k       # must print nothing (else: git lfs pull --include "pop/*")
ls "$COVREPO/pop"/*.nc | wc -l                   # 21 (2000-2020)
ssh USER@login1.yggdrasil.hpc.unige.ch mkdir -p /home/shares/azman/cholera_mapping/Layers_global
rsync -avh --progress --chmod=Dg+rwxs,Fg+rw "$COVREPO/covariate_dictionary.yml" "$COVREPO/pop" \
  USER@login1.yggdrasil.hpc.unige.ch:/home/shares/azman/cholera_mapping/Layers_global/
```

No WorldPop mosaic is needed: the master grid is built from
`pop/population_2020_yearly.nc`.

### 2. Cluster: update the code and taxdat

```bash
cd ~/projects/cholera-mapping-pipeline
git pull
sbatch hpc/yggdrasil/slurm/install_r_packages.sh   # reinstalls taxdat from this checkout
```

Wait for the install job to finish: its log ends with the package versions
and `terra GDAL: ...`.

### 3. Cluster: point the shell at the global build

Run this in a fresh login shell, before anything else sources `env.sh` there:
a second `source` returns early and keeps the first settings. Jobs inherit the
variables. Open another fresh shell for pilot work afterwards.

```bash
cd ~/projects/cholera-mapping-pipeline
export CMP_AOI=raw
export CMP_LAYERS=/home/shares/azman/cholera_mapping/Layers_global
export CHOLERA_MASTER_GRID_FILE=$CMP_LAYERS/pop/population_2020_yearly.nc
source hpc/yggdrasil/env.sh
echo "$PGDATABASE $CMP_LAYERS"   # cholera_covariates_raw /home/shares/azman/cholera_mapping/Layers_global
```

### 4. Add the database (the server must be stopped)

`db_init.sh` starts its own server on the data directory, so it refuses to
run while `db_serve` is up.

```bash
bash hpc/yggdrasil/stages/db_service.sh stop
bash hpc/yggdrasil/stages/db_service.sh status   # repeat until no db_serve job is listed
                                                 # (scancel any queued one)
sbatch hpc/yggdrasil/slurm/db_init.sh            # log ends with "db_init OK: ..., database cholera_covariates_raw"
bash hpc/yggdrasil/stages/db_service.sh start    # once db_init has finished
bash hpc/yggdrasil/stages/db_service.sh status   # wait for "Endpoint: <node>:5433"
```

The pilot database is untouched and served again by the same job.

### 5. Grids

Submit once the server shows an endpoint: client jobs wait at most 10 minutes
for the database.

```bash
JOB_GRID=$(sbatch --parsable --time=06:00:00 --mem=48G hpc/yggdrasil/slurm/prepare_grid.sh)
```

The log ends with `DONE GRID: grids.grid_20_20 (N cells)` and the same line for
`grids.grid_1_1`. The grid files are written to `$CMP_LAYERS/grids/`.

### 6. Pre-compute population at 1 km and 20 km (no database)

```bash
JOB_PRE=$(sbatch --parsable --dependency=afterok:$JOB_GRID --array=0-1%1 -c 4 --mem=64G \
  hpc/yggdrasil/slurm/covariates_precompute.sh "p:1,p:20")
```

`%1` runs the two tasks one after the other, so the 20 km task reuses the
time-aggregated files written by the 1 km task instead of rebuilding them. Each
task processes the 21 years with 4 workers. Each task log ends with
`Done population`. The results are in
`$CMP_LAYERS/processed_covariates/population/raw/`.

If a task fails with "No space left on device" (the node-local `/scratch` is
too small), resubmit with temporary files on scratch:

```bash
CMP_TMPDIR=/srv/beegfs/scratch/shares/azman/cholera_mapping/tmp/$USER sbatch --array=0-1%1 -c 4 --mem=64G \
  hpc/yggdrasil/slurm/covariates_precompute.sh "p:1,p:20"
```

### 7. Load into the database

```bash
JOB_ING=$(sbatch --parsable --dependency=afterok:$JOB_PRE --partition=public-cpu --time=1-00:00:00 --mem=16G \
  hpc/yggdrasil/slurm/covariates_ingest.sh "p:1,p:20")
```

The 1 km load is the long part: about 700 chunks of 1024 × 1024 pixels × 21
bands. Each table goes into a staging table first and is swapped in only
after the band count is checked, so an interrupted load leaves nothing half
written. Resubmitting the same command resumes the work: finished tables are
skipped and the processed files are reused.

### 8. Check the result

```bash
srun -p shared-cpu -t 01:00:00 -c 1 --mem=4G bash -c 'source hpc/yggdrasil/env.sh && cmp_wait_db 1 && cmp_pg psql -h "$PGHOST" -p "$PGPORT" -f -' <<'SQL'
SELECT covariate, aoi_name, ingested_at FROM covariates.metadata ORDER BY 1;
SELECT covariate, count(*) AS bands, min(tl), max(tl) FROM covariates.bands GROUP BY 1 ORDER BY 1;
SELECT relname, pg_size_pretty(pg_total_relation_size(oid)) FROM pg_class
 WHERE relnamespace IN ('covariates'::regnamespace, 'grids'::regnamespace) AND relkind = 'r' ORDER BY 1;
-- world total per year (billions), 20 km vs 1 km: should agree within 1 %
SELECT b AS band, round(sum((ST_SummaryStats(ST_Band(rast, b), 1, true)).sum)::numeric / 1e9, 3) AS pop_20km
  FROM covariates.pop_1_years_20_20, generate_series(1, 21) b GROUP BY b ORDER BY b;
SELECT b AS band, round(sum((ST_SummaryStats(ST_Band(rast, b), 1, true)).sum)::numeric / 1e9, 3) AS pop_1km
  FROM covariates.pop_1_years_1_1, generate_series(1, 21) b WHERE b IN (1, 21) GROUP BY b ORDER BY b;
SQL
```

Expect 21 bands from 2000-01-01 to 2020-01-01 for both tables, and world totals
in the billions that rise every year. The diagnostics job (`db_diagnostics.sh`)
draws maps for one country's test run, so it is not meant for this database.

### 9. Back up

```bash
sbatch --dependency=afterok:$JOB_ING --time=12:00:00 hpc/yggdrasil/slurm/db_backup.sh
```

The dump goes to `$SHARE/backups/cholera_covariates_raw_<date>`. The job keeps
the three most recent dumps.

### Resources (estimates, not yet measured)

| Step | Request | Expected | BU |
|---|---|---|---|
| prepare_grid | 2 cores, 48 GB, 6 h | 1 to 3 h | 15 to 45 |
| covariates_precompute | 2 tasks × 4 cores, 64 GB, 12 h | 1 to 2 h each | 40 to 80 |
| covariates_ingest | 2 cores, 16 GB, 1 day | 3 to 8 h | 20 to 50 |
| db_backup | 2 cores, 8 GB, 12 h | 1 to 3 h | 5 to 12 |

Storage on the owner's home quota:

| Item | Size |
|---|---|
| `Layers_global/pop` (inputs) | 14 GB |
| `Layers_global/processed_covariates` | about 30 GB |
| database (both population tables, grids) | about 30 GB, about 60 GB if empty tiles were stored |
| each dump in `backups/` | 10 to 20 GB, three kept |

A model run against this database uses `aoi: raw` in its config.

## Cost (billing units: 1 BU = 1 core-hour; memory counts 25 %)

| Job | Resources | Pilot (BDI) |
|---|---|---|
| db_serve | 2 cores, 20 GB, about 7 BU per hour | about 2,350 BU per 14-day stint |
| prepare_grid | 2 cores, 16 GB, under 2 h | about 15 |
| covariates_precompute | per task 8 cores, 32 GB, under 6 h | about 380 for 8 tasks |
| covariates_ingest | 2 cores, 8 GB, under 8 h | about 30 |
| mapping_run, db_backup | | about 30 each |

The group allocation is 100,000 BU per year. Stop the server in idle months;
the data directory persists.

## Tested

The scripts were first run locally (native PostgreSQL 17 + PostGIS 3.6
standing in for the image, `CHOLERA_SIF=""`) on synthetic Burundi data. On
Yggdrasil, the Burundi pilot ran image pull, R install, init, serve, grids,
pre-compute, population ingest at 1 km and 20 km, and the mapping run through
the covariate cube. The global population steps above were tested locally on
a synthetic raster with the same code paths, but not yet on Yggdrasil.

## Open questions for the HPC team

1. Is a 2-core job on `public-longrun-cpu` holding the database for up to 14
   days, resubmitted with `--dependency=singleton`, an acceptable use?
2. Can jobs on `public-longrun-cpu` or `shared-cpu` be preempted, and is
   `--signal=B:TERM@120` delivered to the batch script?
3. Which IP range covers compute and login nodes (to narrow
   `CMP_CLUSTER_CIDR` in `pg_hba.conf`; it defaults to any address with
   password authentication)? Can the login node open TCP to a compute-node port?
4. How large is the node-local `/scratch` SSD on Yggdrasil CPU nodes?
5. Do compute nodes reach Docker Hub (for `apptainer pull`) or HTTPS through a
   proxy? If so, what are the proxy settings and its egress IP (for the
   taxonomy-database whitelist, versus 129.194.1.230)?
6. For a ~350 GB database, can one account's home quota be raised, or should
   the data directory live on scratch with dumps on home?
7. Is a PostgreSQL data directory created with `initdb --allow-group-access`
   on the BeeGFS home share fine from your side (locking, snapshots)?
8. Do the module combination `GCC/12.3.0 R/4.3.2 GDAL/3.7.1 PostgreSQL/16.1`
   and GDAL's netCDF driver work together on the CPU nodes?
