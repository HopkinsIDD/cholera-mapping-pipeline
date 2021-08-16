#!/bin/bash
##SBATCH --job-name=render_reports_%j.job
#SBATCH --output=logs/render_reports_%j.log
#SBATCH --mem=30G
#SBATCH --time=02:00:00
#SBATCH --ntasks=1
#SBATCH --nodelist=idmodeling2

echo "Beginning of script"
date
TAXDIR=/home/perez/projects/cholera-mapping-pipeline
CONFIGDIR=$1
OUTDIR=Analysis/output
RBIN=/opt/R/4.0.3/bin/R
export LTO=-pg
CONFIGNAMES=($(ls $CONFIGDIR | tr ' ' '\n'))
THISCONFIG=$2
THISHTML="${THISCONFIG/yml/html}"
THISHTML="${THISHTML/config/report}"
OUTFILE="$TAXDIR/${OUTDIR}/reports/${THISHTML}"
CALL="rmarkdown::render('$TAXDIR/${OUTDIR}/plot_data.Rmd', output_file = '$OUTFILE', params = list(cholera_directory='$TAXDIR', config = '$CONFIGDIR/${THISCONFIG}'))"
# CONFIGNAMES=$(ls $TAXDIR/$CONFIGDIR)

if [ -f "$OUTFILE" ]; then
    echo "$OUTFILE exists."
else 
   $RBIN -e "$CALL" || exit 1
fi

echo $RBIN $CALL || exit 1

