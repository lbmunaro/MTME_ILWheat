#!/bin/bash
#
#SBATCH --time=01-04:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=16
#SBATCH --mem=20G
#SBATCH --job-name=STSE_mod
#SBATCH --account=aces
#SBATCH --partition=aces
#SBATCH --output=../HPC_out/HPC_STSE_mod.o%j
#SBATCH --error=../HPC_out/HPC_STSE_mod.e%j
##SBATCH --mail-user=lucasb4@illinois.edu
##SBATCH --mail-type=BEGIN,END
#
# End of embedded SBATCH options
#

# Define directories
BASE_DIR=~/MTME_ILWheat
SCRIPT_DIR=$BASE_DIR
JOBS_DIR=$BASE_DIR/HPC_jobs
OUTPUT_DIR=$BASE_DIR/HPC_out

# Job name
JOB_NAME="03_STSE_mod"

{
  echo "$JOB_NAME.sh started on $(hostname) at $(date)"
  echo ""
  echo "========== $JOB_NAME.R =========="
  cat $SCRIPT_DIR/$JOB_NAME.R
} | mail -s "$JOB_NAME Started" lucasb4@illinois.edu

# Run R script
module purge
module load R/4.4.2
R CMD BATCH $SCRIPT_DIR/$JOB_NAME.R $OUTPUT_DIR/$JOB_NAME.out

{
  echo "$JOB_NAME.sh finished on $(hostname) at $(date)"
  echo ""
  echo "========== $JOB_NAME.out =========="
  cat $OUTPUT_DIR/$JOB_NAME.out
} | mail -s "$JOB_NAME Finished" lucasb4@illinois.edu