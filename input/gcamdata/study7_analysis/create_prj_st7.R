#### PREPROCESS ================================================================
# ==============================================================================

# setwd to file location === #####
setwd('C:\\GCAM\\GCAM_7.0_Claudia\\gcam-core\\input\\gcamdata\\study7_analysis')
.libPaths('C:\\Users\\claudia.rodes\\Documents\\R\\win-library')

# read arguments
args = commandArgs(trailingOnly=TRUE)
db_it = args[1]
print(db_it)

# load libraries and paths' variables, and extra functions and styles
source('load_libs_paths.R')
source('utils_data.R')
source('utils_style.R')

# load basic data
load_mapping_data()

# load project
prj_name = paste0('st7_',db_it,'.dat')
list_scen_it = list_scen[[paste0('list_scen_',db_it)]]
prj_ref = load_prj(prj_name,list_scen_it)

# # load queries
# year_s = 2000
# year_e = 2050
# selected_scen = list_scen_reference_calibrate
#
# load_queries(onlyFoodConsumption = TRUE)
# rm(prj)
# gc()
