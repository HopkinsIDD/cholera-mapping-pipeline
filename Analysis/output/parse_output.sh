#!/bin/bash
##SBATCH --job-name=render_reports_%j.job
#SBATCH --output=logs/render_reports_%A_%a.log
#SBATCH --mem=50G
#SBATCH --time=02:00:00
#SBATCH --ntasks=1
#SBATCH --array=0-44%3
#SBATCH --nodelist=idmodeling2

echo "Beginning of script"
date
TAXDIR=/home/perez/projects/cholera-mapping-pipeline
CONFIGDIR=$TAXDIR/Analysis/configs/2015_2019_country
OUTDIR=Analysis/output
RSCRIPT=/opt/R/4.0.3/bin/Rscript
export LTO=-pg
CONFIGNAMES=($(ls $CONFIGDIR | tr ' ' '\n'))
THISCONFIG=${CONFIGNAMES[$SLURM_ARRAY_TASK_ID]}

$RSCRIPT $TAXDIR/Analysis/output/parse_output.R -c $CONFIGDIR/${THISCONFIG} -d $TAXDIR || exit 1
echo $RSCRIPT $TAXDIR/Analysis/output/parse_output.R -c $CONFIGDIR/${THISCONFIG} -d $TAXDIR || exit 1

