#!/bin/bash
mkdir sbatch_logs/
#SBATCH --output=sbatch_logs/<run name>.log
#SBATCH --job-name=<run name>
#SBATCH --time=10000:00:00
#SBATCH --mem=50G
#SBATCH --nodelist=idmodeling2
#SBATCH -c 4

echo "***** Beginning of script *****"
date

TAXDIR=/.../cholera-mapping-pipeline
RSCRIPT=/opt/R/4.0.3/bin/Rscript
CONFIG=$1 #specify it here or in the command line following the Shell script submission 

cd $TAXDIR

export CHOLERA_SKIP_STAN=TRUE
export PRODUCTION_RUN=TRUE
$RSCRIPT $TAXDIR/Analysis/R/set_parameters.R -c $TAXDIR/$CONFIG  || exit 1
echo "End of script"
date
