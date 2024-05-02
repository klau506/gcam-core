#### prj_gathering.R
####
#### Main script to gather all the GCAM projects and extract the queries

#### INPUTS
# -
args <- as.numeric(commandArgs(trailingOnly=TRUE))
print(args)


## Set the working directory and load libraries
setwd('/scratch/bc3lc/gcam-core-iamcompact-xin')
libP <- .libPaths()
.libPaths(c(libP,"/scratch/bc3lc/R-libs/4.1"))

library(dplyr)
library(tidyr)
library(rgcam)

# ## gather all the project files
# sub_prj_names <- c(list.files('/scratch/bc3lc/gcam-core-iamcompact-xin/', pattern = 'database_basexdb_sppnr_plus'))
# for (it in sub_prj_names) {
#   print(it)
#   assign(gsub("\\.dat$", "", it),
#          rgcam::loadProject(it))
#   print(length(rgcam::listQueries(get(gsub("\\.dat$", "", it)), anyscen = F)))
#   print(length(rgcam::listScenarios(get(gsub("\\.dat$", "", it)))))
#   print('---------------------------------------')
# }
# prj_gathered <- rgcam::mergeProjects(prjname = 'gath_all_sppnr_plus1.dat', prjlist = sub_prj_names)
# prj_gathered <- rgcam::mergeProjects(prjname = 'gath_all_spp_plus1.dat', prjlist = gsub("\\.dat$", "", sub_prj_names))
# rgcam::saveProject(prova, file = 'gath_all_snr_plus1.dat')
# rgcam::saveProject(prj_gathered, file = 'gath_all_sppnr_all1.dat')

## extract queries and save them in an RData file
sub_prj_names <- c(list.files('/scratch/bc3lc/gcam-core-iamcompact-xin/', pattern = 'gath_all'))
print(sub_prj_names)
print(sub_prj_names[args])
prj <<- rgcam::loadProject(sub_prj_names[args])
# year_s <- 1990
# year_e <- 2050
# source('diets_analysis/module_queries_extraction.R')
# source('diets_analysis/module_data.R')
load_queries(paste0("queries_all_", sub_prj_names[args[1]], ".RData"))

### rfasst module
source('diets_analysis/module_rfasst.R')
final_db_year <- 2050
# prj <- rgcam::loadProject('diets_analysis/outputs/gath_all_3.dat')
desired_scen <- rgcam::listScenarios(prj)
compute_premature_mortalities(desired_scen, paste0("queries_mort_", sub_prj_names[args[1]]))

