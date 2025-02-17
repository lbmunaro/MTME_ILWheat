#!/bin/bash
#
#SBATCH --time=06-00:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=20
#SBATCH --mem=152G
#SBATCH --job-name=MTzrr5a
#SBATCH --account=aces
#SBATCH --partition=aces
#
# End of embedded SBATCH options
#

# Define directories
BASE_DIR=~/MTME_ILWheat
SCRIPT_DIR=$BASE_DIR
JOBS_DIR=$BASE_DIR/HPC_jobs
OUTPUT_DIR=$BASE_DIR/HPC_out

# Job name
JOB_NAME="05_MTME.z_rr5a"

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