#### prj_creation.R
####
#### Main script to create a GCAM project given the database name

#### INPUTS
# queries file

## Read the db number
args <- commandArgs(trailingOnly=TRUE)
print(args)

## Set the working directory and load libraries
setwd('/scratch/bc3lc/gcam-core-iamcompact-xin')
libP <- .libPaths()
.libPaths(c(libP,"/scratch/bc3lc/R-libs/4.1"))

library(dplyr)
library(tidyr)
library(rgcam)
library(xml2)

base_path <<- getwd()
source(file.path('diets_analysis','module_prj_creation.R'))

## Extract the db name
db_name <- args[1]
db_name <- paste0('database_basexdb_',db_name)
print(paste0('Start prj creation for db ', db_name))

## Create the prj
create_prj(db_name)
