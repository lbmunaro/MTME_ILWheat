#!/bin/bash
#
#SBATCH --time=10-00:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=8
#SBATCH --mem=256G
#SBATCH --job-name=MTME_rr3
#SBATCH --account=aces
#SBATCH --partition=aces
##SBATCH --output=myjob.o%j
##SBATCH --error=myjob.e%j
##SBATCH --mail-user=lucasb4@illinois.edu
##SBATCH --mail-type=BEGIN,END
#
# End of embedded SBATCH options
#

echo "HPC_MTME_mod_rr3.sh started on $(hostname) at $(date)" | mail -s "HPC_MTME_mod_rr3 Started" lucasb4@illinois.edu

# Run 
module purge
module load R/4.4.0
R CMD BATCH HPC_MTME_mod_rr3.R HPC_MTME_mod_rr3.out

echo "HPC_MTME_mod_rr3.sh finished on $(hostname) at $(date)" | mail -s "HPC_MTME_mod_rr3 Finished" lucasb4@illinois.edu