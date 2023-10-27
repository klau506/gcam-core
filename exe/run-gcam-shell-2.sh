#!/bin/bash

# Read the CSV file and database name
csvfile=C:/GCAM/GCAM_7.0_Claudia/gcam-core/input/gcamdata/study7_analysis/outputs/snr/L201.snr_scenarios_database_mapping.csv
database=$1

# Read the CSV file (assuming col1 = scenario and col2 = database are the first and second columns)
while IFS=, read -r scenario col2; do
    echo "database = $col2"
    # Trim col2 to remove leading/trailing spaces
    trimmed_col2=$(echo "$col2" | tr -d '[:space:]' | tr -d '"')
    trimmed_database=$(echo "$database" | tr -d '[:space:]' | tr -d '"')
    # Check if col2 matches the database
    if [ "$trimmed_col2" = "$trimmed_database" ]; then
        trimmed_scenario=$(echo "$scenario" | tr -d '[:space:]' | tr -d '"')
        trimmed_scenario_xml=$(echo "$scenario" | tr -d '[:space:]' | tr -d '"' | sed 's/d/\./g')
        echo "Consider database = $trimmed_database and scenario = $trimmed_scenario"

        # Set the configuration file
        config_file_base=C:\\GCAM\\GCAM_7.0_Claudia\\gcam-core\\exe\\config_RvsM\\configuration_SNR.xml
        config_file_loop=C:\\GCAM\\GCAM_7.0_Claudia\\gcam-core\\exe\\configuration_$trimmed_database.xml

        sed -e "s/XXXDBXXX/$trimmed_database/g" -e "s/XXXSCENXXX/$trimmed_scenario/g"  -e "s/XXXSCENXMLXXX/$trimmed_scenario_xml/g" $config_file_base > $config_file_loop

        # Run gcam and redirect the output to the logFile
        ./run-gcam-specifyConfig.bat $config_file_loop
    fi
done < $csvfile

# Launch the R script to generate the project
trimmed_database=$(echo "$database" | tr -d '[:space:]' | tr -d '"')
./run-r-scripts.bat $trimmed_database

# # cd "/cygdrive/c/GCAM/GCAM_7.0_Claudia/gcam-core/input/gcamdata/study7_analysis"
# cd "/cygdrive/c/Program Files/R/R-4.3.1/bin/x64"
# # ./create_project_st7.R $trimmed_database
# LC_CTYPE=en_US.UTF-8  ./R.exe --vanilla /cygdrive/c/GCAM/GCAM_7.0_Claudia/gcam-core/input/gcamdata/study7_analysis/create_project_st7.R
# # LC_CTYPE=en_US.UTF-8  ./R.exe --vanilla /cygdrive/c/GCAM/GCAM_7.0_Claudia/gcam-core/input/gcamdata/study7_analysis/create_project_st7.R
# # ./R.exe --vanilla /cygdrive/c/GCAM/GCAM_7.0_Claudia/gcam-core/input/gcamdata/study7_analysis/create_project_st7.R $trimmed_database
# # ./R.exe --vanilla create_project_st7.R $trimmed_database
# echo 'PATH=$PATH:/cygdrive/c/Program\ Files/R/R-4.3.1/bin' >> .bashrc
