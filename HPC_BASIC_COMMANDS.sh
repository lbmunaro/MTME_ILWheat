#  OS: RedHat 9.4   HW: QEMU   CPU: 32x 2.75 GHz   RAM: 63 GB
#  ▝▜▛▘    ╔═╗┌─┐┌┬┐┌─┐┬ ┬┌─┐   ╔═╗┬  ┬ ┬┌─┐┌┬┐┌─┐┬─┐
#   ▐▌     ║  ├─┤│││├─┘│ │└─┐   ║  │  │ │└─┐ │ ├┤ ├┬┘
#  ▗▟▙▖    ╚═╝┴ ┴┴ ┴┴  └─┘└─┘   ╚═╝┴─┘└─┘└─┘ ┴ └─┘┴└─
#  Welcome to Campus Cluster @ University of Illinois

#   User Guide: https://campuscluster.illinois.edu/user_info/doc/
#      Support: help@campuscluster.illinois.edu

# Basic commands

## Login to HPC
ssh lucasb4@cc-login.campuscluster.illinois.edu 

## Transfer folder from home directory
### From home repository to HPC
scp -r ~/Documents/MTME_ILWheat lucasb4@cc-login.campuscluster.illinois.edu:MTME_ILWheat

### From HPC
scp -r lucasb4@cc-login.campuscluster.illinois.edu:MTME_ILWheat/HPC_out ~/Documents/MTME_ILWheat

## Transfer file
### To HPC
scp -r ~/Documents/MTME_ILWheat/<filename> lucasb4@cc-login.campuscluster.illinois.edu:MTME_ILWheat/<filename>
### From HPC
scp -r lucasb4@cc-login.campuscluster.illinois.edu:MTME_ILWheat/<filename> ~/Documents/MTME_ILWheat/<filename>

scp -r lucasb4@cc-login.campuscluster.illinois.edu:MTME_ILWheat/Data/MTME* ~/Documents/MTME_ILWheat/Data/

#BASIC COMMANDS

## Change directories 
cd 

## Move to the parent directory of the current directory
cd ..                                     

## Creating a directory called ‘projects’
mkdir projects          ## Creates a directory called 'projects'

## Moving and copying files from one directory to another
mv /dir/my_job_name.sh /dir/projects # Move
cp /dir/my_job_name.sh /dir/projects # Copy

## Lists all files within a given directory
ls –la            

## Print the first and/or last lines of ‘yourjob’
head my_job_name.sh       
tail my_job_name.sh       

## Output the contents of 'yourfile' as it runs
tail -f yourjob     

## Submitting jobs 
sbatch your_job_name.sh

## List all current jobs for a user
squeue -u lucasb4

##List all running jobs for a user
squeue -u lucasb4 -t RUNNING

##List all pending jobs for a user
squeue -u lucasb4 -t PENDING

## List all jobs in the queue in the 'aces' partition
squeue -p aces

## List all current jobs in the 'partition' partition for a user
squeue -u lucasb4 -p <partition>

## List detailed information for a job (useful for troubleshooting)
scontrol show jobid -dd <jobid>

## List status info for a currently running job:
sstat --format=AveCPU,AvePages,AveRSS,AveVMSize,JobID -j <jobid> --allsteps

## Get statistics on completed jobs by jobID:
sacct -j <jobid> --format=JobID,JobName,MaxRSS,Elapsed

## View the same information for all jobs of a user:
sacct -u lucasb4 --format=JobID,JobName,MaxRSS,Elapsed
 
## Cancel one job
scancel <jobid>

## To cancel all the jobs for a user
scancel -u lucasb4

## To cancel all the pending jobs for a user
scancel -t PENDING -u lucasb4

## To cancel one or more jobs by name
scancel --name myJobName

## To pause a particular job
scontrol hold <jobid>

## To resume a particular job
scontrol resume <jobid>

## To requeue (cancel and rerun) a particular job
scontrol requeue <jobid>

## To remove a file
rm <filename>

## To remove a file that starts with <string>
rm <string>*

## To remove files interactively (confirm before deleting)
rm -i <string>

##########################################################
# Script to commit and push large files (>100MB) to GitHub using Git LFS

# Step 1: Navigate to the GitHub repository
cd ~/Documents/MTME_ILWheat  # Change this path if needed

# Step 2: Install Git LFS (if not installed)
sudo apt update && sudo apt install git-lfs -y

# Step 3: Initialize Git LFS in the repository
git lfs install

# Step 4: Track large files using Git LFS (update file type if needed)
git lfs track "Data/*.RData"

# Step 5: Verify .gitattributes file contains the LFS tracking info
cat .gitattributes

# Step 6: Remove the large file from Git history if it was previously committed
git rm --cached Data/ILYT_Pheno-Gmatrix.RData

# Step 7: Re-add the large file and .gitattributes
git add .gitattributes
git add Data/ILYT_Pheno-Gmatrix.RData

# Step 8: Commit the changes
git commit -m "Data"

# Step 9: Push to GitHub
git push origin main

# Step 10: Verify the file is tracked by Git LFS
git lfs ls-files