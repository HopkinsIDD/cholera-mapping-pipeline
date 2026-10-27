# cholera-mapping-pipeline
Formerly part of cholera-taxonomy. The map creation scripts, packages, and file structure live here


# Baobab (UNIGE) Environment Setup

## 1. Clone repositories (one-time, at project root)

```bash
git clone https://github.com/HopkinsIDD/cholera-mapping-pipeline.git
cd cholera-mapping-pipeline
git clone git@github.com:HopkinsIDD/cholera-configs.git Analysis/configs
git clone git@github.com:HopkinsIDD/cholera-mapping-output-1.git Analysis/data
git clone git@github.com:HopkinsIDD/cholera-mapping-reports.git Analysis/output
```

Run these from the root directory `cholera-mapping-pipeline`.
The last three repos are private: SSH access to GitHub from Baobab must be configured first (`ssh -T git@github.com` should return `Hi <username>!`).

```bash
mkdir Layers
```
Download the covariate_dictionary.yml in the repo (https://github.com/HopkinsIDD/cholera-covariates.git) and move it to `cholera-mapping-pipeline/Layers`

## 2. Load the environment

Every new Baobab session, from the `cholera-mapping-pipeline` root:

```bash
source load_cholera_env.sh
```

Must be `source` and not `sbatch`, or the loaded modules will not persist in the shell.

### What the script does
 
The script runs the following steps in order. Steps 3 and 4 are idempotent: they only act when something is missing, so re-sourcing the script in every session is safe and fast.
 
1. **Loads the module chain** validated for `taxdat` on Baobab:
   - `GCC/11.3.0`, `OpenMPI/4.1.4`, `rgdal/1.6-6`, `R/4.2.1` (these cascade in `GDAL/3.5.0`, `PROJ/9.0.0`, `GEOS/3.10.3`, `UDUNITS/2.2.28`)
   - `HarfBuzz/4.2.1`, `FriBidi/1.0.12` (text rendering, required by `ragg`/`devtools`)
   - `CMake/3.24.3` (required by `fs`)
   - `libwebp/1.2.4` (required by `ragg`)
   - `PostgreSQL/14.4` (PostgreSQL client side, used for the cholera database connection via `pgcli` and `RPostgres`)
2. **Sets up paths.**
   - Prepends `$HOME/quarto-1.5.57/bin` to `PATH` (user-installed Quarto; this path must exist for `quarto` to be found).
   - Sets `R_LIBS_USER=$HOME/R_libs/4.2.1-foss-2022a` and creates the directory if absent, so R packages are installed in a user library tied to this R/toolchain combination.
   - Prepends `$HOME/.local/bin` to `PATH`, because `pip install --user` places executables there and it is not on `PATH` by default in a fresh shell.
3. **Installs `pgcli` if missing** (one-time, via `python3 -m pip install --user pgcli "psycopg[binary]" configobj`). `pgcli` is a command-line PostgreSQL client with autocompletion. The install is skipped when `pgcli` is already on `PATH`.
4. **Installs the R packages `sf` and `RPostgres` if missing** (one-time, from the ETH Zurich CRAN mirror). The check uses `requireNamespace`, so nothing is reinstalled when both are already present. The first run can take a while because `sf` compiles against the loaded GDAL/PROJ/GEOS modules.
5. **Verifies the environment** and prints a summary:
   - the list of loaded modules and the value of `R_LIBS_USER`
   - the R version and whether `taxdat` loads (`library(taxdat)`; this fails if `taxdat` has not been installed yet, see section 3 below)
   - whether `pgcli` is available (and its version)
   - whether `sf` and `RPostgres` are available
6. **Prompts for database credentials.** The script asks for the cholera database username and password with hidden input (`read -s`) and exports them as the environment variables `CHOLERA_USER` and `CHOLERA_PASSWORD`, so that R code and `pgcli` can read them without credentials being written in any file.

### Handling of credentials
 
- Credentials are held only in the environment of the current shell session.
- Every process started from this shell inherits them, including R sessions and jobs submitted with `sbatch` (Slurm exports the environment by default). Do not print them in logs, do not `echo`/`Sys.getenv()` them in outputs, and never hardcode them in scripts, configs or notebooks.
- They disappear when the session ends. Re-source the script to set them again.

## 3. Install taxdat (if not already done)

```bash
cd packages/taxdat
Rscript -e 'install.packages("roxygen2", repos="https://stat.ethz.ch/CRAN/")'
Rscript -e 'roxygen2::roxygenize(".")'
R CMD INSTALL .
```

## Why these specific versions

R 4.2.1 (close to the pinned `renv.lock` version, 4.2.2) and this module combination came from the [Baobab HPC community forum](https://hpc-community.unige.ch) — `module spider` alone suggests combinations that look coherent but fail in practice (e.g. `R/4.0.4` + `rgdal/1.5-23` fails on a missing `libssl.so.10`). Do not deviate from this combination without re-testing.
