library(rgcam)
library(dplyr)
library(ggplot2)

basic_path = 'C:/GCAM/GCAM_7.0_Claudia/gcam-core/input/gcamdata'
gcam_path <- substr(basic_path, start = 1, stop = regexpr("gcam-core/", basic_path) + 9)
tmp_output_data_path = paste0(gcam_path, "/input/gcamdata/outputs_binomial/")
figures_path = paste0(gcam_path, "/input/gcamdata/figures_binomial/")
folder_analysis_path = paste0(gcam_path, "input/gcamdata/study7_analysis/")
source(paste0(folder_analysis_path,'zzz.R'))
source(paste0(folder_analysis_path,'fun_cobenefits.R'))

db_path <- paste0(gcam_path, "output")
db_name_base = 'behaviour_basexdb'
prj_name = 'behavioral_change_x5.dat'
query_path = paste0(gcam_path, "input/gcamdata/study7_analysis/data/")
queries = 'queries_beh.xml'
desired_scen = c('Reference', paste0("Flex.ds.beh", 1:25))

final_db_year = 2100

## select db accordingn to the scenario
for (sc in desired_scen) {
  if (sc == 'Reference') {
    db_name = paste0(db_name_base,'_ref')
  } else {
    sc_num <- sub(".*beh(.*)$", "\\1", sc)

    if (sc_num <= 5) {
      db_name = paste0(db_name_base,'_1_5')
    } else if (sc_num > 5 & sc_num <= 10) {
      db_name = paste0(db_name_base,'_6_10')
    } else if (sc_num > 10 & sc_num <= 15) {
      db_name = paste0(db_name_base,'_11_15')
    } else if (sc_num > 15 & sc_num <= 20) {
      db_name = paste0(db_name_base,'_16_20')
    } else if (sc_num > 20 & sc_num <= 25) {
      db_name = paste0(db_name_base,'_21_25')
    }
  }

  ## create prj
  conn <- localDBConn(db_path, db_name)
  prj <<- addScenario(conn, prj_name, sc,
                      paste0(query_path, queries),
                      clobber = FALSE)

  # add 'nonCO2' large query
  fill_queries(db_path, db_name, prj_name, sc)

}

saveProject(prj, file = prj_name)


## add premature mortalities
mort = add_mort_scen('Reference')
for (i in unique(desired_scen)[2:length(desired_scen)]) {
  mort = dplyr::bind_rows(mort,add_mort_scen(i))
}
save(mort, file = paste0(tmp_output_data_path,'mort_',prj_name,'.RData'))
