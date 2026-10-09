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

The scripts were run locally (native PostgreSQL 17 + PostGIS 3.6 standing in
for the image, `CHOLERA_SIF=""`) on synthetic Burundi data: init, serve,
grid, three pre-compute tasks, ingest, mapping run through the covariate
cube, backup, and stop. Not yet run on Yggdrasil: Apptainer, modules, Slurm.

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
