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
        echo "Consider database = $trimmed_database and scenario = $trimmed_scenario"

        # Set the configuration file
        config_file_base=C:\\GCAM\\GCAM_7.0_Claudia\\gcam-core\\exe\\config_RvsM\\configuration_SNR.xml
        config_file_loop=C:\\GCAM\\GCAM_7.0_Claudia\\gcam-core\\exe\\configuration_$trimmed_database.xml

        sed -e "s/YYY/$trimmed_database/g" -e "s/XXX/$trimmed_scenario/g" $config_file_base > $config_file_loop

        # Run gcam and redirect the output to the logFile
        ./run-gcam-specifyConfig.bat $config_file_loop
    fi
done < $csvfile
