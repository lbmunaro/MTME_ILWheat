#!/bin/bash
#
#SBATCH --time=10-00:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=40
#SBATCH --mem=152G
#SBATCH --job-name=MTME_rr4
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
  echo "HPC_MTME_mod_rr4.sh started on $(hostname) at $(date)"
  echo ""
  echo "========== HPC_MTME_mod_rr4.R =========="
  cat HPC_MTME_mod_rr4.R
} | mail -s "HPC_MTME_mod_rr4 Started" lucasb4@illinois.edu

# Run 
module purge
module load R/4.4.2
R CMD BATCH HPC_MTME_mod_rr4.R HPC_MTME_mod_rr4.out

{
  echo "HPC_MTME_mod_rr4.sh finished on $(hostname) at $(date)"
  echo ""
  echo "========== HPC_MTME_mod_rr4.out =========="
  cat HPC_MTME_mod_rr4.out
} | mail -s "HPC_MTME_mod_rr4 Finished" lucasb4@illinois.edu