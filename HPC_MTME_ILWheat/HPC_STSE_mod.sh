#!/bin/bash
#
#SBATCH --time=00-04:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=16
#SBATCH --mem=20G
#SBATCH --job-name=STSE_mod
#SBATCH --account=aces
#SBATCH --partition=aces
##SBATCH --output=myjob.o%j
##SBATCH --error=myjob.e%j
##SBATCH --mail-user=lucasb4@illinois.edu
##SBATCH --mail-type=BEGIN,END
#
# End of embedded SBATCH options
#
{
  echo "HPC_STSE_mod.sh started on $(hostname) at $(date)"
  echo ""
  echo "========== HPC_STSE_mod.R =========="
  cat HPC_STSE_mod.R
} | mail -s "HPC_STSE_mod Started" lucasb4@illinois.edu

# Run 
module purge
module load R/4.4.2
R CMD BATCH HPC_STSE_mod.R HPC_STSE_mod.out

{
  echo "HPC_STSE_mod.sh finished on $(hostname) at $(date)"
  echo ""
  echo "========== HPC_STSE_mod.out =========="
  cat HPC_STSE_mod.out
} | mail -s "HPC_STSE_mod Finished" lucasb4@illinois.edu