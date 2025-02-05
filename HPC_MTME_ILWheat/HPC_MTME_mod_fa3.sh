#!/bin/bash
#
#SBATCH --time=10-00:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=8
#SBATCH --mem=192G
#SBATCH --job-name=MTME_fa3
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
  echo "HPC_MTME_mod_fa3.sh started on $(hostname) at $(date)"
  echo ""
  echo "========== HPC_MTME_mod_fa3.R =========="
  cat HPC_MTME_mod_fa3.R
} | mail -s "HPC_MTME_mod_fa3 Started" lucasb4@illinois.edu

# Run 
module purge
module load R/4.4.2
R CMD BATCH HPC_MTME_mod_fa3.R HPC_MTME_mod_fa3.out

{
  echo "HPC_MTME_mod_fa3.sh finished on $(hostname) at $(date)"
  echo ""
  echo "========== HPC_MTME_mod_fa3.out =========="
  cat HPC_MTME_mod_fa3.out
} | mail -s "HPC_MTME_mod_fa3 Finished" lucasb4@illinois.edu