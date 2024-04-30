#!/bin/bash
# This script launches all GCAM runs related to a single database
# INPUTS: 
#   a) database - scenarios mapping - PROVIDED BY THE USER
#   b) basic configuration file - PROVIDED BY THE USER
#   c) row number of that file - AUTOMATIC
# OUTPUTS: GCAM runs

### INPUTS
rownumber=$1
rownumber=$((rownumber - 1)) # Fix counting from 0
dbmapping=db.mapping.csv # USER MODIFICABLE
configuration_basic=configuration_base_IAMCOMPACT+XIN.xml # USER MODIFICABLE

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


### RUN ALL GCAM SCENARIOS TO BE SAVED IN THAT DATABASE
# 1. Get db name and scenario type
db_unique_names=($(tail -n +2 $dbmapping | cut -d ',' -f 1 | sort -u))
db_name="${db_unique_names[$rownumber]}"
if grep -q "sppnr" <<< "$db_name"; then
    scen_type="sppnr"
else
    scen_type="${db_name:0:3}"
fi

# 2. Get scenarios related to that db
mapfile -t scen_names < <(awk -v pat="$db_name" -F',' '$1 ~ pat {print $2}' $dbmapping)

# 3. Loop
for element in "${scen_names[@]}"; do
    echo "Consider $element database"
    
    # 3.1. arrange the configuration file
    configuration_loop=configuration_$db_name.xml

    sed -e "s/XXXDBXXX/$db_name/g" -e "s/XXXTYPEXXX/$scen_type/g" -e "s/XXXSCENXXX/$element/g" $configuration_basic > $configuration_loop

    # 3.2. run GCAM
    ./gcam.exe -C $configuration_loop
    wait
done


# 4. Create the GCAM prj
# rownumber=$((rownumber + 1)) # Fix counting from 1
Rscript /scratch/bc3lc/gcam-core-iamcompact-xin/diets_analysis/prj_creation.R ${db_name}
