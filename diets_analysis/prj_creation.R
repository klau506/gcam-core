#### prj_creation.R
####
#### Main script to create a GCAM project given the database number

#### INPUTS
# db-scenario mapping file
# queries file

## Read the db number
args <- commandArgs(trailingOnly=TRUE)
print(args)

## Set the working directory and load libraries
setwd('/scratch/bc3LC/gcam-core-iamcompact-xin')
libP <- .libPaths()
.libPaths(c(libP,"/scratch/bc3LC/R-libs/4.1"))

library(dplyr)
library(tidyr)
library(rgcam)

base_path <<- getwd()
source(file.path('diets_analysis','module_prj_creation.R'))

## Extract the db name
db_num <- as.numeric(args[1])
db_scen_mapping <- read.csv(file = file.path(base_path, 'exe', 'db.mapping.csv'))
unique_scen <- which(!duplicated(db_scen_mapping$database))
db_scen_mapping <- db_scen_mapping %>%
  dplyr::filter(database == db_scen_mapping$database[unique_scen[db_num]]
)
db_name = unique(db_scen_mapping$database)
print(paste0('Start prj creation for db ', db_name))

## Create the prj
create_prj(db_name)
