#!/bin/bash

# Loop from 30 to 60
for ((i=30; i<=60; i++)); do
    # Create the output filename with current loop value
    output_file="fuelprefelast_RvsM_Rreduced_${i}.xml"

    # Substitute ·xxx with current loop value and save it to output file
    sed "s/xxx/0.${i}/g" fuelprefelast_RvsM_Rreduced_base.xml > "$output_file"

    # Optional: Print a message indicating the completion of the operation
    echo "Processed xml and saved as $output_file"
done

# Optional: Print a completion message after the loop finishes
echo "XML modification and saving complete."
