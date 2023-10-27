#!/bin/bash

awk '{ sub("\r$", ""); print }' run-gcam-shell.sh > tmp.sh
mv tmp.sh run-gcam-shell.sh
chmod +x run-gcam-shell.sh

awk '{ sub("\r$", ""); print }' run-gcam-shell-2.sh > tmp.sh
mv tmp.sh run-gcam-shell-2.sh
chmod +x run-gcam-shell-2.sh
