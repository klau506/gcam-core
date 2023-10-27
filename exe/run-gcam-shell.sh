#!/bin/bash

# Read the CSV file and extract unique elements from col2 (database)
csvfile=C:/GCAM/GCAM_7.0_Claudia/gcam-core/input/gcamdata/study7_analysis/outputs/snr/L201.snr_scenarios_database_mapping.csv
unique_elements=($(tail -n +2 $csvfile | cut -d ',' -f 2 | sort -u))

# Function to process each unique element in parallel
process_element() {
    element=$(echo "$1" | tr -d '[:space:]')
    echo "Processing element: $element"
    
    # Set the log file
    logFile=$(echo "log_${element}.log" | tr -d '"')

    # Run-gcam script 2
    ./run-gcam-shell-2.sh $element > $logFile
}

# Counter for running processes
count=0

# Loop through unique elements and launch the processing function in parallel
for element in "${unique_elements[@]}"; do
    # Run process_element function in the background
    process_element "$element" &

    # Increment the count of running processes
    ((count++))

    # Wait if the number of running processes exceeds 5
    if [ $count -ge 5 ]; then
        wait # Wait for all background processes to finish
        count=0 # Reset the counter
    fi
done

# Wait for all background processes to finish
wait

echo "Processing completed for all unique elements."
