#!/bin/bash
#SBATCH --qos=regular
#SBATCH --time=10:00:00
#SBATCH --ntasks=2
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=1
#SBATCH --array=1-2:1
#SBATCH --job-name=GCAM_run
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=10000Mc
#SBATCH --output=%x_%a.out
#SBATCH --error=%x_%a.err

# print the job ID and the current date and time to the output file
echo START JOB ${SLURM_JOB_ID} ${SLURM_ARRAY_TASK_ID}
echo $(date)
echo "================================================================================"
echo "================================================================================"

# run GCAM
./run-gcam-spec-db.sh ${SLURM_ARRAY_TASK_ID}