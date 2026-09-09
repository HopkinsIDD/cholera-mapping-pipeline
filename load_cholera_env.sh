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

export R_LIBS_USER="$HOME/R_libs/4.2.1-foss-2022a"
mkdir -p "$R_LIBS_USER"

echo "=== Environnement cholera-mapping-pipeline charge ==="
module list
echo "R_LIBS_USER = $R_LIBS_USER"
Rscript -e 'cat("R version:", R.version.string, "\n"); library(taxdat); cat("taxdat: OK\n")'

### --- 3. API/DB credentials (session-only, never written to disk) ---
read -p "Cholera Taxonomy API username: " CHOLERA_API_USERNAME
export CHOLERA_API_USERNAME

read -s -p "Cholera Taxonomy API key: " CHOLERA_API_KEY
echo ""
export CHOLERA_API_KEY

### --- 4. SQL/Postgres credentials (session-only, never written to disk) ---

read -p "Cholera SQL database username: " CHOLERA_SQL_USERNAME
export CHOLERA_SQL_USERNAME

read -s -p "Cholera SQL database password: " CHOLERA_SQL_PASSWORD
echo ""
export CHOLERA_SQL_PASSWORD