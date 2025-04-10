#!/bin/bash
#SBATCH --job-name=population_agg
#SBATCH --output=population_agg2_%A.out
#SBATCH --error=population_agg2_%A.err
#SBATCH --nodelist=idmodeling2
#SBATCH --time=8:00:00
#SBATCH --mem=16G
#SBATCH --cpus-per-task=4

# Load required modules
source /home/$USER/.bashrc
conda activate r42-env

# Define the band number from the array task ID
TAXDIR=/home/$USER/Code/cholera-mapping-pipeline

echo "Job start"
date
# Run the R script with the appropriate band parameter
for BAND in {1..15}; do
    echo Rscript $TAXDIR/Analysis/R/population_aggregation.R -b $BAND -e FALSE -s TRUE
    Rscript $TAXDIR/Analysis/R/population_aggregation.R -b $BAND -e FALSE -s TRUE
done
echo "Job Finish"
date

