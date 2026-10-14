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

Loads:
- `GCC/11.3.0`, `OpenMPI/4.1.4`, `rgdal/1.6-6`, `R/4.2.1` (cascades in `GDAL/3.5.0`, `PROJ/9.0.0`, `GEOS/3.10.3`, `UDUNITS/2.2.28`)
- `HarfBuzz/4.2.1`, `FriBidi/1.0.12` (text rendering, required by `ragg`/`devtools`)
- `CMake/3.24.3` (required by `fs`)
- `libwebp/1.2.4` (required by `ragg`)
- `R_LIBS_USER=$HOME/R_libs/4.2.1-foss-2022a`

Ends by verifying `taxdat` loads correctly.

## 3. Install taxdat (if not already done)

```bash
cd packages/taxdat
Rscript -e 'install.packages("roxygen2", repos="https://stat.ethz.ch/CRAN/")'
Rscript -e 'roxygen2::roxygenize(".")'
R CMD INSTALL .
```

## Why these specific versions

R 4.2.1 (close to the pinned `renv.lock` version, 4.2.2) and this module combination came from the [Baobab HPC community forum](https://hpc-community.unige.ch) — `module spider` alone suggests combinations that look coherent but fail in practice (e.g. `R/4.0.4` + `rgdal/1.5-23` fails on a missing `libssl.so.10`). Do not deviate from this combination without re-testing.
