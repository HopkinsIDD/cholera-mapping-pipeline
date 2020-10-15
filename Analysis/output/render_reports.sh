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
TAXDIR=/home/jkaminsky/git/cholera-taxonomy/branches/postgis-covar/
CONFIGDIR=$TAXDIR/Analysis/configs/2015_2019
OUTDIR=Analysis/output
RBIN=/opt/R/4.0.3/bin/R
export LTO=-pg
# export GMON_OUT_PREFIX='/home/jkaminsky/gprof/'
CONFIGNAMES=($(ls $CONFIGDIR | tr ' ' '\n'))
THISCONFIG=${CONFIGNAMES[$SLURM_ARRAY_TASK_ID]}
THISHTML="${THISCONFIG/yml/html}"
THISHTML="${THISHTML/config/report}"
OUTFILE="$TAXDIR/${OUTDIR}/reports/${THISHTML}"
CALL="rmarkdown::render('$TAXDIR/${OUTDIR}/model_run_report.Rmd', output_file = '$OUTFILE', params = list(config = '$CONFIGDIR/${THISCONFIG}'))"
# CONFIGNAMES=$(ls $TAXDIR/$CONFIGDIR)

if [ -f "$OUTFILE" ]; then
    echo "$OUTFILE exists."
else 
   $RBIN -e "$CALL" || exit 1
fi

echo $RBIN $CALL || exit 1

