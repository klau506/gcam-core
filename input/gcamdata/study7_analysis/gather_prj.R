
#### Paths =====================================================================
# ==============================================================================
setwd('C:\\GCAM\\GCAM_7.0_Claudia\\gcam-core\\input\\gcamdata\\study7_analysis')
.libPaths('C:\\Users\\claudia.rodes\\Documents\\R\\win-library\\4.1')

source('load_libs_paths.R')
source('utils_data.R')
source('utils_style.R')

prj_name = 'snr_gathered2.dat'


#### Gather subprj =============================================================
# ==============================================================================
# prj
sub_prj_names <- c(list.files('.', pattern = '^st7_snr_'), 'snr_reference.dat')
for (it in sub_prj_names) {
  print(it)
  assign(gsub("\\.dat$", "", it),
         rgcam::loadProject(it))
}
prj_gathered <<- rgcam::mergeProjects(prjname = prj_name, prjlist = sub_prj_names)
rgcam::saveProject(prj_gathered, file = paste0(outputs_path, "/", prj_name))


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

