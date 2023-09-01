## script to produce all the outputs (figures and tables) to do a system-wide
## analysis of the FVV scenarios

#### Libraries =================================================================
# ==============================================================================
library(rgcam)
library(dplyr)
library(ggplot2)
library(rfasst)
#####

#### Paths =====================================================================
# ==============================================================================
setwd('C:/GCAM/GCAM_7.0_Claudia/gcam-core/input/gcamdata')

gcam_path <<- substr(getwd(), start = 1, stop = regexpr("gcam-core/", getwd()) + 9)
tmp_output_data_path <<- paste0(gcam_path, "/input/gcamdata/outputs_binomial/")
figures_path <<- paste0(gcam_path, "/input/gcamdata/figures_binomial/")
folder_analysis_path <<- paste0(gcam_path, "input/gcamdata/study7_analysis/")

arg1 = commandArgs(trailingOnly = TRUE)[1]
arg2 = commandArgs(trailingOnly = TRUE)[2]

db_path <<- paste0(gcam_path, "output")
db_name_base <<- 'behaviour_basexdb_v2'
prj_name <<- paste0('behavioral_change_v2_x5_',arg1,'_',arg2,'.dat')
query_path <<- paste0(gcam_path, "input/gcamdata/study7_analysis/data/")
queries <<- 'queries_beh.xml'
desired_scen <<- c(paste0("Flex.ds.beh", arg1:arg2))

iso_gcam_regions <- read.csv(paste0(folder_analysis_path,"data/iso_GCAM_regID.csv"), skip = 6)
id_gcam_regions <- read.csv(paste0(folder_analysis_path,"data/gcam_id_to_region.csv")) %>%
  rename('GCAM_region_id' = 'ï..GCAM_region_ID')

source(paste0(folder_analysis_path,'zzz.R'))
#####

#### SYSTEM-WIDE EFFECTS SECTION ===============================================
#### Create prj ================================================================
# ==============================================================================

# if prj does not exist, create it. Load it otherwise
if (!file.exists(prj_name)) {

  print('create prj')
  ## select db according to the scenario
  for (sc in desired_scen) {
    print(sc)

    db_name = find_db_name(sc)

    ## create prj
    conn <- localDBConn(db_path, db_name)
    prj <<- addScenario(conn, prj_name, sc,
                        paste0(query_path, queries),
                        clobber = FALSE)

    # add 'nonCO2' large query
    fill_queries(db_path, db_name, prj_name, sc)

  }

  saveProject(prj, file = prj_name)

} else {
  ## load prj
  print('load prj')
  prj <<- loadProject(prj_name)
  listQueries(prj)
  listScenarios(prj)
}


#### Data preprocess ===========================================================
# ==============================================================================

year_s = 2000
year_e = 2100
final_db_year <<- 2100
selected_scen = desired_scen

# # load queries
# load_queries()

# # compute premature mortalities
mort = load_premature_mortalities() %>%
  rename('value' = 'mort',
         'fasst_region' = 'region')
