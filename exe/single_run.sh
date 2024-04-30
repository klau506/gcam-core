#!/bin/bash
#SBATCH --qos=regular
#SBATCH --time=10:00:00
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --job-name=RUN
#SBATCH --cpus-per-task=16
#SBATCH --mem-per-cpu=10000Mc
#SBATCH --output=%x_%a.out
#SBATCH --error=%x_%a.err

### SOME INFO
# print the job ID and the current date and time to the output file
echo START JOB ${SLURM_JOB_ID} ${SLURM_ARRAY_TASK_ID}
echo $(date)
echo "================================================================================"
echo "================================================================================"

### PATHS AND MODULES
# load modules
module load git
module load R/4.2.1-foss-2022a
module load Java/1.8.0_241
module load GCC/11.3.0
module load Boost/1.79.0-GCC-11.3.0
module load tbb/2021.5.0-GCCcore-11.3.0   

# define paths
export CXX=g++
export BOOST_INCLUDE=${BOOST_ROOT}/include
export BOOSTROOT=${BOOST_ROOT}
export BOOST_LIB=${BOOST_ROOT}/lib
export JARS_LIB=/dipc/bc3lc/libs/jars/*
export JAVA_INCLUDE=${JAVA_HOME}/include
export JAVA_LIB=${JAVA_HOME}/jre/lib/amd64/server
export EIGEN_INCLUDE=/dipc/bc3lc/libs/eigen
export TBB_INCLUDE=${EBROOTTBB}/include
export TBB_LIB=${EBROOTTBB}/lib  

### RUN
Rscript /scratch/bc3lc/gcam-core-iamcompact-xin/diets_analysis/prj_gathering.R
