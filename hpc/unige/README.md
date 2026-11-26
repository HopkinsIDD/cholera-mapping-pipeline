# Covariates database on the UNIGE clusters (Yggdrasil, Baobab)

PostgreSQL 17 + PostGIS run inside an Apptainer image in a Slurm job; the
mapping pipeline (R from cluster modules) connects to it over TCP from other
jobs. One database per area of interest (`cholera_covariates_<iso3>`); the
Burundi pilot uses `cholera_covariates_bdi`.

```
hpc/unige/
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
    db_backup.sh           pg_dump of the database to $CMP_BACKUPS (keeps 3)
    db_diagnostics.sh      figures: build, covariates by year, test-run extraction
    worldpop_g2_convert.sh WorldPop Global 2 GeoTIFFs -> yearly population NetCDF
    covariates_extract_country.sh  all covariates over one country, checked against sources
  profiles/              named builds (CMP_PROFILE=<name>): paths of one database
    baobab_raw_g2.env      global database on Baobab, WorldPop Global 2
  stages/                login-node drivers
    db_service.sh          start | stop | status of the server job
    db_build.sh            submits the whole build chain for one config
  tools/
    stage_inputs_local.sh  laptop: observations, boundaries, cropped WorldPop, raw covariates
    rsync_inputs.sh        laptop -> $SHARE
    create_db_user.sh      add a team member's database role
    pg_tunnel.sh           laptop -> database through the login node
    download_worldpop_g2.sh  login node: WorldPop Global 2 1 km mosaics
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

## Where the computation and memory go

The server starts once per `db_serve` job and keeps running. Nothing is rebuilt
when a job connects: opening a connection takes milliseconds and a few MB on
the server, and queries read only the tiles they need through the spatial
index. The heavy spatial steps of a run (location-period intersections,
population sums) run inside the server, on its 2 cores, so several heavy runs
at once wait on each other there.

| Job | What uses its RAM | Set by |
|---|---|---|
| Database server (`db_serve`) | A fixed 4 GB cache of recently read tables. Up to 256 MB per sort or join step in each running query. Up to 2 GB for index builds during loading. A few MB per connected job. | `--mem=20G` in the server job; cache and per-query limits written by `db_init` (`PG_SHARED_BUFFERS`, `PG_WORK_MEM`, `PG_MAINT_MEM`) |
| Pre-compute (`covariates_precompute`) | Terra and GDAL cropping, aggregating and warping rasters. No database involved. | the job's `--mem` (32 GB; 64 GB in the global runbook) |
| Loading (`covariates_ingest`) | R handles one chunk at a time (about 88 MB for 21 bands); the server decodes and tiles it, so most of the memory is on the server side. | the job's `--mem` (8 GB) and the server's 20 GB |
| Mapping run (`mapping_run`) | Observations, grid polygons, location periods, covariate cube, population weights and Stan input, held in R. Parallel workers each hold a copy. | the job's `--mem` (16 GB) |

The server's memory barely grows with the size of the database: the cache is
fixed and data stays on disk. What adds up is concurrent queries, each sort or
join taking its own 256 MB; 20 GB leaves room for a few runs at a time. If the
server exceeded its 20 GB, Slurm would kill it and every connected job would
fail; raise the server's `--mem` if many runs share it.

## First-time setup (pilot: Burundi)

On a laptop with taxonomy-database access and a git-lfs checkout of
HopkinsIDD/cholera-covariates (with the needed directories pulled):

```bash
bash hpc/unige/tools/stage_inputs_local.sh --config Analysis/configs/BDI_pilot.yml \
  --covariates-repo ~/projects/cholera-covariates --worldpop ~/data/ppp_2020_1km_Aggregated.tif
bash hpc/unige/tools/rsync_inputs.sh --remote USER@login1.yggdrasil.hpc.unige.ch \
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

sbatch hpc/unige/slurm/pull_sif.sh
sbatch hpc/unige/slurm/install_r_packages.sh
sbatch hpc/unige/slurm/db_init.sh          # after pull_sif finishes
bash hpc/unige/stages/db_build.sh --config Analysis/configs/BDI_pilot.yml \
  --observations /home/shares/azman/cholera_mapping/data/observations_BDI.rds
bash hpc/unige/stages/db_service.sh status
```

`db_build.sh --dry-run` runs the pre-flight checks only. Stop the server when
you are done: `bash hpc/unige/stages/db_service.sh stop`.

## Daily use

```bash
bash hpc/unige/stages/db_service.sh start            # serve (queues if one runs)
sbatch hpc/unige/slurm/mapping_run.sh CONFIG.yml OBS.rds
bash hpc/unige/stages/db_service.sh stop
```

Add a teammate (from a job on the server's node, or any node with the
superuser password in `PGPASSWORD`):
`NEW_USER_PASSWORD=... PSQL_SUPER="cmp_pg psql -h $PGHOST -U <superuser>" bash hpc/unige/tools/create_db_user.sh alamc`

## Test run by a second team member (Burundi)

A teammate runs the Burundi data preparation (steps 1 to 3, Stan skipped)
against the running server, from their own checkout and R library. The test
config sets `ingest_covariates: no`, so the run only reads the database; it
never ingests or overwrites covariates.

### Owner of the server, once

1. **Access.** The teammate needs a Yggdrasil account and membership of
   `GL_M_gdd_cholera_mapping`, the group in the share's access list
   (`getfacl /home/shares/azman/cholera_mapping`; `ls -ld` only shows root).
   Ask the share owner or hpc@unige.ch to add them; `getent group
   GL_M_gdd_cholera_mapping` lists the members. Files inside the share carry the
   group `hpc_users` (everyone's primary group), which works because only
   project members can enter the share.
2. **Test config and dictionary on the share** (adjust the source path to
   wherever the pilot config is on the cluster):

   ```bash
   source hpc/unige/env.sh
   mkdir -p $SHARE/configs
   sed 's/^ingest_covariates: yes/ingest_covariates: no/' Analysis/configs/BDI_pilot_api.yml \
     > $SHARE/configs/BDI_pilot_test.yml
   cp Analysis/configs/config_dictionary.yml $SHARE/configs/
   grep -n '^ingest_covariates' $SHARE/configs/BDI_pilot_test.yml   # ingest_covariates: no
   chmod -R g+rX $SHARE/configs
   ```

3. **Group read access** on what the teammate's jobs use. Every line should
   show `r` in the group column; fix with `chmod g+r` (files) or `chmod g+rx`
   (directories):

   ```bash
   ls -l $SHARE/db_endpoint $SHARE/secrets/db.env $CHOLERA_SIF $CMP_DATA/observations_BDI.rds
   ls -ld $SHARE/secrets $CMP_LAYERS $CMP_LAYERS/grids
   ```

4. **Server up** for longer than the test: `bash hpc/unige/stages/db_service.sh status`
   shows the endpoint and the time left. Start it if needed.
5. **Don't run the same config yourself at the same time.** Per-run tables are
   named after the config, so two simultaneous runs of one config overwrite
   each other's tables.

### Teammate

1. **Get the code** (on the login node):

   ```bash
   ssh USER@login1.yggdrasil.hpc.unige.ch
   mkdir -p ~/projects && cd ~/projects
   git clone https://github.com/HopkinsIDD/cholera-mapping-pipeline.git
   cd cholera-mapping-pipeline
   git checkout dev_hpc_db_rebuild
   mkdir -p logs Analysis/configs
   cp /home/shares/azman/cholera_mapping/configs/config_dictionary.yml Analysis/configs/
   ```

   Every later command runs from `~/projects/cholera-mapping-pipeline`.

2. **Check access:**

   ```bash
   source hpc/unige/env.sh
   echo "$PGDATABASE $PGUSER"          # cholera_covariates_bdi cholera_app
   bash hpc/unige/stages/db_service.sh status   # Endpoint: <node>:5433
   ls /home/shares/azman/cholera_mapping/configs/BDI_pilot_test.yml \
      /home/shares/azman/cholera_mapping/data/observations_BDI.rds
   ```

   An empty `PGUSER` or "Permission denied" means the group access is missing.

3. **Install R packages** into `~/R_libs/cmp-4.3.2` (one job, about 1 to 3 h:
   most packages compile from source):

   ```bash
   sbatch hpc/unige/slurm/install_r_packages.sh
   squeue -u $USER                            # wait until the job is gone
   tail -n 15 logs/install_r_pkgs_*.log       # versions, then "terra GDAL: ..."
   ```

   A line with `MISSING` means the install failed; send the log.

4. **Test the database connection** (debug partition, a few seconds):

   ```bash
   srun -p debug-cpu -t 00:10:00 --mem=2G bash -c 'source hpc/unige/env.sh && cmp_wait_db 1 &&
     cmp_pg psql -h "$PGHOST" -p "$PGPORT" -c "SELECT covariate, aoi_name FROM covariates.metadata ORDER BY 1"'
   ```

   Expect `pop_1_years_1_1` and `pop_1_years_20_20`, both for `bdi_b50km`.

5. **Run the data preparation:**

   ```bash
   sbatch hpc/unige/slurm/mapping_run.sh \
     /home/shares/azman/cholera_mapping/configs/BDI_pilot_test.yml \
     /home/shares/azman/cholera_mapping/data/observations_BDI.rds
   squeue -u $USER
   tail -f logs/mapping_run_<JOBID>.log      # Ctrl-C stops following, not the job
   ```

   What to expect in the log: step 1 keeps about 24,360 of 40,068 observations;
   step 2 builds a covariate cube of 87 cells × 5 years × 1 covariate; step 3
   writes the Stan input. The outputs go to `Analysis/data/` in the teammate's
   checkout (`ls -lt Analysis/data | head`).

6. **Report back:** the job ID, the log file, the outputs listed in
   `Analysis/data/`, and `git rev-parse --short HEAD`.

| Message | Meaning |
|---|---|
| `No database endpoint` / `Database not ready` | the server is not running: ask its owner |
| `Permission denied` under `/home/shares/...` | group membership or permissions (owner, step 3) |
| `there is no package called ...` | the R install has not finished or failed (step 3) |
| `Couldn't find ... It needs to be ingested` | a database table is missing: send the log to the owner |


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
sbatch hpc/unige/slurm/install_r_packages.sh   # reinstalls taxdat from this checkout
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
source hpc/unige/env.sh
echo "$PGDATABASE $CMP_LAYERS"   # cholera_covariates_raw /home/shares/azman/cholera_mapping/Layers_global
```

### 4. Add the database (the server must be stopped)

`db_init.sh` starts its own server on the data directory, so it refuses to
run while `db_serve` is up.

```bash
bash hpc/unige/stages/db_service.sh stop
bash hpc/unige/stages/db_service.sh status   # repeat until no db_serve job is listed
                                                 # (scancel any queued one)
sbatch hpc/unige/slurm/db_init.sh            # log ends with "db_init OK: ..., database cholera_covariates_raw"
bash hpc/unige/stages/db_service.sh start    # once db_init has finished
bash hpc/unige/stages/db_service.sh status   # wait for "Endpoint: <node>:5433"
```

The pilot database is untouched and served again by the same job.

### 5. Grids

Submit once the server shows an endpoint: client jobs wait at most 10 minutes
for the database.

```bash
JOB_GRID=$(sbatch --parsable --time=06:00:00 --mem=48G hpc/unige/slurm/prepare_grid.sh)
```

The log ends with `DONE GRID: grids.grid_20_20 (N cells)` and the same line for
`grids.grid_1_1`. The grid files are written to `$CMP_LAYERS/grids/`.

### 6. Pre-compute population at 1 km and 20 km (no database)

```bash
JOB_PRE=$(sbatch --parsable --dependency=afterok:$JOB_GRID --array=0-1%1 -c 4 --mem=64G \
  hpc/unige/slurm/covariates_precompute.sh "p:1,p:20")
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
  hpc/unige/slurm/covariates_precompute.sh "p:1,p:20"
```

### 7. Load into the database

```bash
JOB_ING=$(sbatch --parsable --dependency=afterok:$JOB_PRE --partition=public-cpu --time=1-00:00:00 --mem=16G \
  hpc/unige/slurm/covariates_ingest.sh "p:1,p:20")
```

The 1 km load is the long part: about 700 chunks of 1024 × 1024 pixels × 21
bands. Each table goes into a staging table first and is swapped in only
after the band count is checked, so an interrupted load leaves nothing half
written. Resubmitting the same command resumes the work: finished tables are
skipped and the processed files are reused.

### 8. Check the result

```bash
srun -p shared-cpu -t 01:00:00 -c 1 --mem=4G bash -c 'source hpc/unige/env.sh && cmp_wait_db 1 && cmp_pg psql -h "$PGHOST" -p "$PGPORT" -f -' <<'SQL'
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
sbatch --dependency=afterok:$JOB_ING --time=12:00:00 hpc/unige/slurm/db_backup.sh
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

## Full database on Baobab with WorldPop Global 2 (2015 to 2030)

One database, `cholera_covariates_raw`, uncropped: population from WorldPop
Global 2 at 1 km and 20 km, the other covariates at 20 km. `env.sh` detects
the cluster; Baobab has its own storage, so nothing built on Yggdrasil is
visible there.

What changes with Global 2 (release R2025A, constrained, doi:10.5258/SOTON/WP00845):

- Years 2015 to 2030 only, dated 1 January. A run with a time slice before
  2015 stops with "Covariate not found for modelling time slice".
- Unsettled land is NoData in the source. `worldpop_g2_convert.sh` writes 0
  there, so the land mask comes from another file: the unconstrained 2020
  mosaic of release R2024B, on the same lattice. That file is the master grid
  source (`CHOLERA_MASTER_GRID_FILE`).
- The lattice (origin -180, 84; down to 60 S) differs from Global 1 by a
  fraction of a pixel. Do not mix Global 1 and Global 2 files in one Layers
  directory; the conversion refuses a `pop/` directory it did not write.
- National totals already follow UN WPP 2024, so `adjust_pop_UN` becomes a
  small correction (1 January versus mid-year).

### 0. Check the cluster (login1.baobab.hpc.unige.ch)

```bash
sinfo -s                                        # public-longrun-cpu, shared-cpu, public-cpu, debug-cpu
module spider R/4.3.2 GDAL/3.7.1 PostgreSQL/16.1
ls -d /home/share*/azman/cholera_mapping /srv/beegfs/scratch/shares/azman/cholera_mapping
getent group GL_M_gdd_cholera_mapping
```

If the share does not exist, ask hpc@unige.ch for it (home and scratch). If
its path differs from `/home/shares/azman/cholera_mapping`, export `SHARE` and
`SCRATCH_SHARE` before step 2. Jobs whose walltime crosses a maintenance
window do not start before it: check https://status.hpc.unige.ch/ before
starting the 14-day server.

### 1. Code, image, R packages

```bash
mkdir -p ~/projects && cd ~/projects
git clone git@github.com:HopkinsIDD/cholera-mapping-pipeline.git && cd cholera-mapping-pipeline
git checkout dev_baobab_worldpop_g2
export CMP_PROFILE=baobab_raw_g2       # see step 2
S=/home/shares/azman/cholera_mapping
mkdir -p logs $S/secrets_g2 && chmod 2770 $S/secrets_g2
nano $S/secrets_g2/superuser.pw && chmod 600 $_   # one line
nano $S/secrets_g2/db.env && chmod 640 $_         # export PGUSER=cholera_app / export PGPASSWORD='...'
sbatch hpc/unige/slurm/pull_sif.sh
sbatch hpc/unige/slurm/install_r_packages.sh
```

### 2. Point the shell at the build

The build's paths are in the profile
[`profiles/baobab_raw_g2.env`](profiles/baobab_raw_g2.env). They carry a
`_g2` suffix (`Layers_g2`, `sif_g2`, `secrets_g2`, `pgdata_g2`, `logs_g2`,
`backups_g2`, `diagnostics_g2`, `db_endpoint_g2`) because a Burundi test
cluster already uses the default paths in the share. In a fresh login shell,
before anything else sources `env.sh`:

```bash
cd ~/projects/cholera-mapping-pipeline
export CMP_PROFILE=baobab_raw_g2
source hpc/unige/env.sh
echo "$CMP_CLUSTER $PGDATABASE $CMP_LAYERS $DB_ENDPOINT_FILE"
# baobab cholera_covariates_raw .../Layers_g2 .../db_endpoint_g2
```

Jobs inherit `CMP_PROFILE` from the submitting shell (Slurm exports the
environment by default). A variable exported before sourcing overrides the
profile's value.

### 3. Inputs

Population, on the login node (17 files, 5.4 GB, resumable):

```bash
bash hpc/unige/tools/download_worldpop_g2.sh
```

It checks every file's size against the server, that all files share one
lattice (`worldpop_g2_raw/lattice.txt`), and writes `SHA256SUMS`.

Dictionary and the other covariates, from a laptop with a git-lfs checkout of
HopkinsIDD/cholera-covariates (`git lfs pull` first; do not copy its `pop/`):

```bash
COVREPO=~/projects/archive/cholera-covariates
find "$COVREPO" -path "$COVREPO/.git" -prune -o \( -name '*.nc' -o -name '*.tif' \) -size -1k -print   # must print nothing
rsync -avh --progress --chmod=Dg+rwxs,Fg+rw --exclude pop --exclude .git "$COVREPO"/ \
  USER@login1.baobab.hpc.unige.ch:/home/shares/azman/cholera_mapping/Layers_g2/
```

Two dictionary entries cannot be built: `chirps_cum` (its directory is on a
machine that no longer exists) and `logpop` (its 2000 file is not in the
repository).

### 4. Convert population (no database)

```bash
JOB_POP=$(sbatch --parsable hpc/unige/slurm/worldpop_g2_convert.sh)
```

The log ends with `Done WorldPop Global 2: 16 year(s)`. Per-year totals are in
`$CMP_LAYERS/pop/worldpop_g2_summary.csv`; `people_off_land` (people on pixels
the land file does not cover, kept in the population but outside the master
grid) should be negligible.

### 5. Database and grids

```bash
sbatch hpc/unige/slurm/db_init.sh                # after pull_sif; ends with "db_init OK"
bash hpc/unige/stages/db_service.sh start
bash hpc/unige/stages/db_service.sh status       # wait for "Endpoint: <node>:5433"
JOB_GRID=$(sbatch --parsable --time=06:00:00 --mem=48G hpc/unige/slurm/prepare_grid.sh)
```

### 6. Pre-compute and load

Population first (1 km, then 20 km, as in the Global 1 runbook above), then
the other covariates at 20 km:

```bash
JOB_PRE=$(sbatch --parsable --dependency=afterok:$JOB_POP:$JOB_GRID --array=0-1%1 -c 4 --mem=64G \
  hpc/unige/slurm/covariates_precompute.sh "p:1,p:20")
JOB_ING=$(sbatch --parsable --dependency=afterok:$JOB_PRE --partition=public-cpu --time=1-00:00:00 --mem=16G \
  hpc/unige/slurm/covariates_ingest.sh "p:1,p:20")

OTHER="dw:20,dl:20,dr:20,dc:20,as:20,aw:20,ao:20,cs:20,cw:20,ac:20,ahm:20,ahw:20"
JOB_PRE2=$(sbatch --parsable --dependency=afterok:$JOB_GRID --array=0-11 --mem=64G \
  hpc/unige/slurm/covariates_precompute.sh "$OTHER")
JOB_ING2=$(sbatch --parsable --dependency=afterok:$JOB_PRE2:$JOB_ING hpc/unige/slurm/covariates_ingest.sh "$OTHER")
```

The WASH, stunting and wasting series end in 2020: a run that uses them is
limited to 2015 to 2020.

### 7. Check and back up

```bash
srun -p shared-cpu -t 01:00:00 -c 1 --mem=4G bash -c 'source hpc/unige/env.sh && cmp_wait_db 1 && cmp_pg psql -h "$PGHOST" -p "$PGPORT" -f -' <<'SQL'
SELECT covariate, aoi_name, ingested_at FROM covariates.metadata ORDER BY 1;
SELECT covariate, count(*) AS bands, min(tl), max(tl) FROM covariates.bands GROUP BY 1 ORDER BY 1;
SELECT b AS band, round(sum((ST_SummaryStats(ST_Band(rast, b), 1, true)).sum)::numeric / 1e9, 3) AS pop_20km
  FROM covariates.pop_1_years_20_20, generate_series(1, 16) b GROUP BY b ORDER BY b;
SELECT b AS band, round(sum((ST_SummaryStats(ST_Band(rast, b), 1, true)).sum)::numeric / 1e9, 3) AS pop_1km
  FROM covariates.pop_1_years_1_1, generate_series(1, 16) b WHERE b IN (1, 6, 16) GROUP BY b ORDER BY b;
SQL
sbatch --dependency=afterok:$JOB_ING2 --time=12:00:00 hpc/unige/slurm/db_backup.sh
```

Expect 16 population bands from 2015-01-01 to 2030-01-01, the same world
total at both resolutions and in `worldpop_g2_summary.csv` (band 6, 2020:
7.851 billion). Then run one country with `aoi: raw` in its config
(`mapping_run.sh`) as the end-to-end test.

### 8. Mapping runs against this database

Every run uses the profile and a config with `aoi: raw` (the database name
comes from it), time slices within 2015 to 2030, and `ingest_covariates: no`
so the run only reads the shared database:

```bash
cd ~/projects/cholera-mapping-pipeline
export CMP_PROFILE=baobab_raw_g2
sbatch hpc/unige/slurm/mapping_run.sh CONFIG.yml OBSERVATIONS.rds
```

`covariate_choices` takes dictionary names (`dist_to_water`, `water_access`,
`san_access`, `open_defe`, `access_cities`, ...). Stunting and wasting stop
in 2017; water, sanitation and open defecation in 2020.

To check the covariates over one country against their source rasters
(CSVs and a PDF in `$CMP_DIAGNOSTICS/<database>/extract_<ISO>/`), cache its
boundary on the login node first; compute nodes have no internet:

```bash
source hpc/unige/env.sh && Rscript -e 'taxdat::cache_admin_units("BDI", 0)'
sbatch hpc/unige/slurm/covariates_extract_country.sh BDI
```

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

The Global 2 path (conversion, master grid from the unconstrained mosaic,
grids, pre-compute, load with `aoi: raw`) was tested locally on WorldPop's
Burundi files for 2015, 2020 and 2030: the database totals equal the source
(12,441,306 for 2020) at 1 km and 20 km. Nothing has run on Baobab yet.

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
