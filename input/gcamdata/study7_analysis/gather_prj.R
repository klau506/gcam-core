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

db_path <<- paste0(gcam_path, "output")
db_name_base <<- 'behaviour_basexdb_v2'
prj_name <<- paste0('behavioral_change_v2_x5_ref_1_25.dat')
query_path <<- paste0(gcam_path, "input/gcamdata/study7_analysis/data/")
queries <<- 'queries_beh.xml'
desired_scen <<- c('Reference', paste0("Flex.ds.beh", 1:25))

iso_gcam_regions <- read.csv(paste0(folder_analysis_path,"data/iso_GCAM_regID.csv"), skip = 6)
id_gcam_regions <- read.csv(paste0(folder_analysis_path,"data/gcam_id_to_region.csv"))
colnames(id_gcam_regions) = c('GCAM_region_id', 'region')


source(paste0(folder_analysis_path,'zzz.R'))


#### Gather subprj =============================================================
# ==============================================================================
# prj
sub_prj_names = c('behavioral_change_v2_x5_1_5.dat','behavioral_change_v2_x5_6_10.dat','behavioral_change_v2_x5_11_15.dat',
                  'behavioral_change_v2_x5_16_20.dat','behavioral_change_v2_x5_21_25.dat','behavioral_change_v2_x5_ref.dat')
prj <<- rgcam::mergeProjects(prj_name, sub_prj_names, saveProj = TRUE)


# mort
sub_mort_names = c('mort_behavioral_change_v2_x5_1_5.RData','mort_behavioral_change_v2_x5_6_10.RData','mort_behavioral_change_v2_x5_11_15.RData',
                   'mort_behavioral_change_v2_x5_16_20.RData','mort_behavioral_change_v2_x5_21_25.RData','mort_behavioral_change_v2_x5_ref.RData')
mort = get(load(paste0(tmp_output_data_path,sub_mort_names[1])))
for (i in sub_mort_names[2:length(sub_mort_names)]) {
  mort = dplyr::bind_rows(mort,get(load(paste0(tmp_output_data_path,i))))
}
save(mort, file = paste0(tmp_output_data_path,'mort_',sub("\\.dat$", "", prj_name),'.RData'))


# crop loss
sub_crop_loss_names = c('_behavioral_change_v2_x5_1_5.RData','_behavioral_change_v2_x5_6_10.RData','_behavioral_change_v2_x5_11_15.RData',
                   '_behavioral_change_v2_x5_16_20.RData','_behavioral_change_v2_x5_21_25.RData','_behavioral_change_v2_x5_ref.RData')
for (pref in c('ryl.aot40','ryl.mi','prod.loss','rev.loss')) {
  tmp_crop_loss_names = paste0(pref, sub_crop_loss_names)
  crop_loss = get(load(paste0(tmp_output_data_path,tmp_crop_loss_names[1])))
  for (i in tmp_crop_loss_names[2:length(tmp_crop_loss_names)]) {
    crop_loss = dplyr::bind_rows(crop_loss,get(load(paste0(tmp_output_data_path,i))))
  }
  save(crop_loss, file = paste0(tmp_output_data_path,pref,'_',sub("\\.dat$", "", prj_name),'.RData'))
}
ryl.aot40 = get(load(paste0(tmp_output_data_path,'ryl.aot40_',sub("\\.dat$", "", prj_name),'.RData')))
ryl.mi = get(load(paste0(tmp_output_data_path,'ryl.mi_',sub("\\.dat$", "", prj_name),'.RData')))
prod.loss = get(load(paste0(tmp_output_data_path,'prod.loss_',sub("\\.dat$", "", prj_name),'.RData')))
rev.loss = get(load(paste0(tmp_output_data_path,'rev.loss_',sub("\\.dat$", "", prj_name),'.RData')))

crop_loss = list(ryl.aot40 = ryl.aot40, ryl.mi = ryl.mi, prod.loss = prod.loss, rev.loss = rev.loss)
save(crop_loss, file = paste0(tmp_output_data_path,'crop_loss_',sub("\\.dat$", "", prj_name),'.RData'))

