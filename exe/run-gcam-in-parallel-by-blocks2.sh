#!/bin/bash
#SBATCH --qos=regular
#SBATCH --time=15:00:00
#SBATCH --ntasks=1
#SBATCH --nodes=4
#SBATCH --ntasks-per-node=1
#SBATCH --array=1-4:1
#SBATCH --job-name=GCAM_run2
#SBATCH --cpus-per-task=16
#SBATCH --mem-per-cpu=10000Mc
#SBATCH --output=%x_%a.out
#SBATCH --error=%x_%a.err

# print the job ID and the current date and time to the output file
echo START JOB ${SLURM_JOB_ID} ${SLURM_ARRAY_TASK_ID}
echo $(date)
echo "================================================================================"
echo "================================================================================"

# run GCAM
./run-gcam-spec-db2.sh ${SLURM_ARRAY_TASK_ID}
