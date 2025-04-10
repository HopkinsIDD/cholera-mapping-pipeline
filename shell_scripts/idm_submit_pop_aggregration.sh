#!/bin/bash
#SBATCH --job-name=population_agg
#SBATCH --output=population_agg_%A_%a.out
#SBATCH --error=population_agg_%A_%a.err
#SBATCH --nodelist=idmodeling2
#SBATCH --time=8:00:00
#SBATCH --mem=16G
#SBATCH --cpus-per-task=4
#SBATCH --array=1-15

# Load required modules
source /home/$USER/.bashrc
conda activate r42-env

# Define the band number from the array task ID
BAND=$SLURM_ARRAY_TASK_ID
TAXDIR=/home/$USER/Code/cholera-mapping-pipeline

echo "Job start"
date
# Run the R script with the appropriate band parameter
echo Rscript $TAXDIR/Analysis/R/population_aggregation.R -b $BAND -e TRUE -s FALSE
Rscript $TAXDIR/Analysis/R/population_aggregation.R -b $BAND -e TRUE -s FALSE
echo "Job Finish"
date
