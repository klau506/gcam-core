#### prj_gathering.R
####
#### Main script to gather all the GCAM projects and extract the queries

#### INPUTS
# -

## Set the working directory and load libraries
setwd('/scratch/bc3LC/gcam-core-iamcompact-xin')
libP <- .libPaths()
.libPaths(c(libP,"/scratch/bc3LC/R-libs/4.1"))

library(dplyr)
library(tidyr)
library(rgcam)

## gather all the project files
sub_prj_names <- c(list.files('/scratch/bc3LC/gcam-core-iamcompact-xin/', pattern = '^spp_|^snr_'))
for (it in sub_prj_names) {
  print(it)
  assign(gsub("\\.dat$", "", it),
         rgcam::loadProject(it))
}
prj_gathered <<- rgcam::mergeProjects(prjname = 'gath_all.dat', prjlist = sub_prj_names)
rgcam::saveProject(prj_gathered, file = 'gath_all.dat')

## extract queries and save them in an RData file
prj <<- prj_gathered
rm(prj_gathered)
year_s <- 1990
year_e <- 2050
source('diets_analysis/module_queries_extraction.R')
source('diets_analysis/module_data.R')
load_queries("queries_ref_2.RData")

### rfasst module
source('diets_analysis/module_rfasst.R')
final_db_year <- 2050
prj <- rgcam::loadProject('diets_analysis/outputs/ref.dat')
desired_scen = rgcam::listScenarios(prj)
compute_premature_mortalities(desired_scen, 'queries_ref_2')

