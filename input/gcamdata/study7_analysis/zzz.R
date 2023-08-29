### rename scenarios
rename_scen = function(df){

  df<- df %>%
    mutate(scenario = if_else(scenario == "Diets_Ref", "Reference", scenario),
           scenario = if_else(scenario == "Diets_Trade.adj21", "Trade", scenario),
           scenario = if_else(scenario == "Diets_Protein", "Protein", scenario))

  return(invisible(df))
}

### protein type & scenario type palette
protein_scenario_palette = c('plant protein.Reference' = '#00771E',
                             'plant protein.Behavior change' = '#00DB37',
                             'animal protein.Reference' = '#8A0000',
                             'animal protein.Behavior change' = '#DB0000')

### scen color palette
mypal_scen = c("Reference" = "grey4", "Protein" = "seagreen4", "Trade" = "orange1")
c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)



### compute pm and o3 premature mortalities
library(rfasst)
add_mort_scen = function(scen) {
  pm.mort = dplyr::bind_rows(m3_get_mort_pm25(db_path,query_path,db_name,prj_name,scen,queries,final_db_year = final_db_year, saveOutput=F, map=F)) %>%
    dplyr::mutate('scenario' = scen)
  pm.mort = tidyr::pivot_longer(pm.mort, cols = c('BURNETT2014_medium',
                                                  'BURNETT2018WITH_high',
                                                  'BURNETT2018WITH_low',
                                                  'BURNETT2018WITH_medium',
                                                  'BURNETT2018WITHOUT_high',
                                                  'BURNETT2018WITHOUT_low',
                                                  'BURNETT2018WITHOUT_medium',
                                                  'GBD2016_high',
                                                  'GBD2016_low',
                                                  'GBD2016_medium'), names_to = 'method', values_to = 'mort_by_disease')
  pm.mort = pm.mort %>%
    dplyr::group_by(region,year,scenario,method) %>%
    dplyr::summarise('mort' = sum(mort_by_disease))

  o3.mort = dplyr::bind_rows(m3_get_mort_o3(db_path,query_path,db_name,prj_name,scen,queries,final_db_year = final_db_year, saveOutput=F, map=F)) %>%
    dplyr::mutate('scenario' = scen)
  o3.mort = tidyr::pivot_longer(o3.mort, cols = c('mort_o3_jer_med',
                                                  'mort_o3_jer_low',
                                                  'mort_o3_jer_high',
                                                  'mort_o3_gbd2016_med',
                                                  'mort_o3_gbd2016_low',
                                                  'mort_o3_gbd2016_high'), names_to = 'method', values_to = 'mort_by_disease')
  o3.mort = o3.mort %>%
    dplyr::group_by(region,year,scenario,method) %>%
    dplyr::summarise('mort' = sum(mort_by_disease))

  mort = dplyr::bind_rows(pm.mort %>% dplyr::mutate('pollutant' = 'pm25'),
                          o3.mort %>% dplyr::mutate('pollutant' = 'o3'))

  return(mort)
}



nonCO2_emissions_list = c('BC','BC_AWB','C2F6','CF4','CH4','CH4_AGR','CH4_AWB','CO','CO_AWB','H2',
                   'H2_AWB','HFC125','HFC134a','HFC143a','HFC152a','HFC227ea','HFC23','HFC236fa',
                   'HFC245fa','HFC32','HFC365mfc','HFC43','N2O','N2O_AGR','N2O_AWB','NH3','NH3_AGR',
                   'NH3_AWB','NMVOC','NMVOC_AGR','NMVOC_AWB','NOx','NOx_AGR','NOx_AWB','OC','OC_AWB',
                   'PM10','PM2.5','SF6','SO2_1','SO2_1_AWB','SO2_2','SO2_2_AWB','SO2_3','SO2_3_AWB',
                   'SO2_4','SO2_4_AWB')

data_query = function(type, db_path, db_name, prj_name, sc) {
  dt = data.frame()
  xml <- xml2::read_xml(paste0(folder_analysis_path,"data/queries_beh_nonCO2.xml"))
  qq <- xml2::xml_find_first(xml, paste0("//*[@title='", type, "']"))

  for (emis in nonCO2_emissions_list) {
    qq_sec = gsub("current_emis", emis, qq)

    prj_tmp = rgcam::addSingleQuery(
      conn = rgcam::localDBConn(db_path,
                                db_name,migabble = FALSE),
      proj = prj_name,
      qn = type,
      query = qq_sec,
      scenario = sc,
      regions = NULL,
      clobber = TRUE,
      transformations = NULL,
      saveProj = FALSE,
      warn.empty = FALSE
    )

    tmp = data.frame(prj_tmp[[sc]][type])
    if (nrow(tmp) > 0) {
      dt = dplyr::bind_rows(dt,tmp)
    }
    rm(prj_tmp)
  }

  # Rename columns
  new_colnames <- sub(".*\\.(.*)", "\\1", names(dt))
  names(dt) <- new_colnames

  return(dt)
}


fill_queries = function(db_path, db_name, prj_name, sc) {
  # add nonCO2 query manually (it is too big to use the usual method)
    print('nonCO2 emissions by sector')
    dt_sec = data_query('nonCO2 emissions by sector (excluding resource production)', db_path, db_name, prj_name, sc)
    prj_tmp <- rgcam::addQueryTable(project = prj_name, qdata = dt_sec,
                                    queryname = 'nonCO2 emissions by sector (excluding resource production)', clobber = FALSE)
    prj <<- rgcam::mergeProjects(prj_name, list(prj,prj_tmp), clobber = TRUE, saveProj = FALSE)
}

