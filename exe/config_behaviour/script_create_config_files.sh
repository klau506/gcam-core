#!/bin/bash

for i in {1..25}; do
    output_file="config_$i.xml"

    if ((i <= 5)); then
        sed -e "s/XXX/$i/g" -e "s/behaviour_basexdb/behaviour_basexdb_1_5/g" configuration_protein.ds.beh.base.xml > "$output_file"
    elif ((i > 5 && i <= 10)); then
        sed -e "s/XXX/$i/g" -e "s/behaviour_basexdb/behaviour_basexdb_6_10/g" configuration_protein.ds.beh.base.xml > "$output_file"
    elif ((i > 10 && i <= 15)); then
        sed -e "s/XXX/$i/g" -e "s/behaviour_basexdb/behaviour_basexdb_11_15/g" configuration_protein.ds.beh.base.xml > "$output_file"
    elif ((i > 15 && i <= 20)); then
        sed -e "s/XXX/$i/g" -e "s/behaviour_basexdb/behaviour_basexdb_16_20/g" configuration_protein.ds.beh.base.xml > "$output_file"
    else
        sed -e "s/XXX/$i/g" -e "s/behaviour_basexdb/behaviour_basexdb_21_25/g" configuration_protein.ds.beh.base.xml > "$output_file"
    fi

done