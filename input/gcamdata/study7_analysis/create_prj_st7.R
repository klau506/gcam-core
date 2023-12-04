#### PREPROCESS ================================================================
# ==============================================================================

# setwd to file location === #####
gcam_folder = 'gcam-core-iamcompact'
setwd(paste0('C:\\GCAM\\GCAM_7.0_Claudia\\',gcam_folder,'\\input\\gcamdata\\study7_analysis'))
.libPaths('C:\\Users\\claudia.rodes\\Documents\\R\\win-library\\4.1 - gcamdata_CP/')

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
prj_name = paste0('st7_',sub("^database_", "", db_it),'.dat')
list_scen_it = list_scen[[paste0('list_scen_',sub("^database_", "", db_it))]]
prj = load_prj(prj_name,list_scen_it)

# load queries
year_s = 2000
year_e = 2050
final_db_year <<- year_e
selected_year = 2030

load_mapping_data()
selected_scen = list_scen_it

load_queries()
dt = save_queries(queries_file_name = 'REF_queries_all.RData')
rm(prj)
gc()
