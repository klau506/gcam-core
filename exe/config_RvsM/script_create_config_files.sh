#!/bin/bash

for i in {30..60}; do
    output_file="config_$i.xml"

    if ((i <= 34)); then
        sed -e "s/XXX/$i/g" -e "s/behaviour_basexdb/behaviour_basexdb_RvsM_30_34/g" configuration_rvsm.ds.beh.base.xml > "$output_file"
    elif ((i >= 35 && i <= 39)); then
        sed -e "s/XXX/$i/g" -e "s/behaviour_basexdb/behaviour_basexdb_RvsM_35_39/g" configuration_rvsm.ds.beh.base.xml > "$output_file"
    elif ((i >= 40 && i <= 44)); then
        sed -e "s/XXX/$i/g" -e "s/behaviour_basexdb/behaviour_basexdb_RvsM_40_44/g" configuration_rvsm.ds.beh.base.xml > "$output_file"
    elif ((i >= 45 && i <= 49)); then
        sed -e "s/XXX/$i/g" -e "s/behaviour_basexdb/behaviour_basexdb_RvsM_45_49/g" configuration_rvsm.ds.beh.base.xml > "$output_file"
    elif ((i >= 50 && i <= 54)); then
        sed -e "s/XXX/$i/g" -e "s/behaviour_basexdb/behaviour_basexdb_RvsM_50_54/g" configuration_rvsm.ds.beh.base.xml > "$output_file"
    else
        sed -e "s/XXX/$i/g" -e "s/behaviour_basexdb/behaviour_basexdb_RvsM_55_59/g" configuration_rvsm.ds.beh.base.xml > "$output_file"
    fi

done
