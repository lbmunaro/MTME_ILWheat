#!/bin/bash
#
#SBATCH --time=07-00:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=8
#SBATCH --mem=192G
#SBATCH --job-name=MTME_rr2
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
  echo "HPC_MTME_mod_rr2.sh started on $(hostname) at $(date)"
  echo ""
  echo "========== HPC_MTME_mod_rr2.R =========="
  cat HPC_MTME_mod_rr2.R
} | mail -s "HPC_MTME_mod_rr2 Started" lucasb4@illinois.edu

# Run 
module purge
module load R/4.4.2
R CMD BATCH HPC_MTME_mod_rr2.R HPC_MTME_mod_rr2.out

{
  echo "HPC_MTME_mod_rr2.sh finished on $(hostname) at $(date)"
  echo ""
  echo "========== HPC_MTME_mod_rr2.out =========="
  cat HPC_MTME_mod_rr2.out
} | mail -s "HPC_MTME_mod_rr2 Finished" lucasb4@illinois.edu