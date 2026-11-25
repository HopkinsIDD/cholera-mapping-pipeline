# load_cholera_env.sh
# Loads the full module chain + R library path validated for running
# taxdat (cholera-mapping-pipeline) on Baobab (UNIGE).
# Usage: source load_cholera_env.sh   <-- MUST be sourced, not executed,
#        otherwise module loads are lost when the subshell exits.

module load GCC/11.3.0
module load OpenMPI/4.1.4
module load rgdal/1.6-6 # Issues with terra, rgdal, GDAL, geodata
module load R/4.2.1
module load HarfBuzz/4.2.1
module load FriBidi/1.0.12
module load CMake/3.24.3
module load libwebp/1.2.4
module load PostgreSQL/14.4

export PATH="$HOME/quarto-1.5.57/bin:$PATH"

export R_LIBS_USER="$HOME/R_libs/4.2.1-foss-2022a"
mkdir -p "$R_LIBS_USER"



# --- pgcli setup ---
# pgcli installs to ~/.local/bin via `pip install --user`, which is not
# on PATH by default on every fresh shell — same class of problem as
# R_LIBS_USER above, so it's re-exported here every time this script
# is sourced, not installed here (installation is a one-time step, done
# once outside this script — see the block below, left commented as a
# reminder rather than run automatically on every session).
export PATH="$HOME/.local/bin:$PATH"

# One-time install, if pgcli isn't already present. Guarded so sourcing
# this script repeatedly doesn't reinstall on every new session.
if ! command -v pgcli &> /dev/null; then
    echo "pgcli not found — installing (one-time setup)..."
    python3 -m pip install --user pgcli "psycopg[binary]" configobj
fi

# --- R packages: sf + RPostgres, one-time install ---
# Checked via requireNamespace (fast, harmless every session) — actual
# install only triggers if genuinely missing, so this doesn't slow down
# or reinstall anything on sessions where both are already present.
echo "Checking sf / RPostgres..."
Rscript -e '
missing_pkgs <- Filter(function(p) !requireNamespace(p, quietly = TRUE), c("sf", "RPostgres"))
if (length(missing_pkgs) > 0) {
  cat("Installing missing package(s):", paste(missing_pkgs, collapse = ", "), "\n")
  install.packages(missing_pkgs, repos = "https://stat.ethz.ch/CRAN/")
} else {
  cat("sf and RPostgres already present.\n")
}
'

echo "=== Environnement cholera-mapping-pipeline charge ==="
module list
echo "R_LIBS_USER = $R_LIBS_USER"
Rscript -e 'cat("R version:", R.version.string, "\n"); library(taxdat); cat("taxdat: OK\n")'

if command -v pgcli &> /dev/null; then
    echo "pgcli: OK ($(pgcli --version 2>&1 | head -n1))"
else
    echo "pgcli: NOT FOUND — check pip install output above"
fi

Rscript -e '
ok <- sapply(c("sf", "RPostgres"), requireNamespace, quietly = TRUE)
if (all(ok)) cat("sf/RPostgres: OK\n") else {
  cat("sf/RPostgres: MISSING ->", paste(names(ok)[!ok], collapse=", "), "\n")
}
'
echo "=== SQL credentials ==="
read -s -p "Cholera DB username: " CHOLERA_USER; export CHOLERA_USER; echo
read -s -p "Cholera DB password: " CHOLERA_PASSWORD; export CHOLERA_PASSWORD; echo
