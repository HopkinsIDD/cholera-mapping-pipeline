#!/bin/bash
#SBATCH --job-name=pull_taxonomy_data
#SBATCH --output=%x_%j.out
#SBATCH --error=%x_%j.err
#SBATCH --time=00:30:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem=4G
#SBATCH --partition=shared-cpu

set -e  # exit immediately if a command fails

# --- Load modules (non-interactive equivalent of load_cholera_env.sh, minus the prompts) ---
module load GCC/11.3.0
module load OpenMPI/4.1.4
module load rgdal/1.6-6
module load R/4.2.1
module load HarfBuzz/4.2.1
module load FriBidi/1.0.12
module load CMake/3.24.3
module load libwebp/1.2.4
module load PostgreSQL/14.4

export R_LIBS_USER="$HOME/R_libs/4.2.1-foss-2022a"

cd "$HOME/cholera_mapping/cholera-mapping-pipeline"

# --- Sanity check: credentials file must exist ---
if [ ! -f "Analysis/R/database_api_key.R" ]; then
    echo "ERROR: Analysis/R/database_api_key.R not found."
    echo "Create it with database_username, database_api_key, taxonomy_username, taxonomy_password."
    exit 1
fi

Rscript pull_taxonomy_data.R

echo "Data pull complete."
