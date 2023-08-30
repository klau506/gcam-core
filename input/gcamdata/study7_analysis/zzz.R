
### protein type & scenario type palette
protein_scenario_palette = c('plant protein.Reference' = '#00771E',
                             'plant protein.Behavior change' = '#00DB37',
                             'animal protein.Reference' = '#8A0000',
                             'animal protein.Behavior change' = '#DB0000')

### staples vs non-staples & scenario type palette
staples_non_staples_scenario_palette =
  c('Behavior change.FoodDemand_NonStaples' = '#0085A5',
    'Behavior change.FoodDemand_Staples' = '#BDB400',
    'Reference.FoodDemand_NonStaples' = '#002787',
    'Reference.FoodDemand_Staples' = '#876400')
staples_non_staples_order_palette =
c('Behavior change.FoodDemand_Staples',
  'Reference.FoodDemand_Staples',
  'Behavior change.FoodDemand_NonStaples',
  'Reference.FoodDemand_NonStaples')

### staples vs non-staples & scenario type palette
beef_dairy_scenario_palette =
  c('Behavior change.Beef' = '#CCCCFF',
    'Behavior change.Dairy' = '#DE3163',
    'Behavior change.Pork' = '#6495ED',
    'Behavior change.Poultry' = '#40E0D0',
    'Behavior change.SheepGoat' = '#72F3B0',
    'Reference.Beef' = '#6666A8',
    'Reference.Dairy' = '#92002A',
    'Reference.Pork' = '#2355AF',
    'Reference.Poultry' = '#007F72',
    'Reference.SheepGoat' = '#209A5A')
beef_dairy_order_palette =
c('Behavior change.Beef',
  'Behavior change.Pork',
  'Behavior change.Poultry',
  'Behavior change.SheepGoat',
  'Behavior change.Dairy',
  'Reference.Beef',
  'Reference.Pork',
  'Reference.Poultry',
  'Reference.SheepGoat',
  'Reference.Dairy')

### scen color palette
mypal_scen = c("Reference" = "grey4", "Behavior change" = "seagreen4")
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
add_mort_scen = function(scen) {
  db_name = find_db_name(scen)

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

data_query = function(type, db_path, db_name, prj_name, scenarios) {
  dt = data.frame()
  xml <- xml2::read_xml(paste0(folder_analysis_path,"data/queries_beh_nonCO2.xml"))
  qq <- xml2::xml_find_first(xml, paste0("//*[@title='", type, "']"))

  emiss_list = unique(my_pol$Pollutant)
  for (sc in scenarios) {
    while (length(emiss_list) > 0) {
      current_emis = emiss_list[1:min(21,length(emiss_list))]
      qq_sec = gsub("current_emis", paste0("(@name = '", paste(current_emis, collapse = "' or @name = '"), "')"), qq)

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

      if (length(emiss_list) > 21) {
        emiss_list <- emiss_list[(21 + 1):length(emiss_list)]
      } else {
        emiss_list = c()
      }
    }
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


### given the scenario name, find the corresponding db
find_db_name = function(sc) {
  if (sc == 'Reference') {
    db_name = paste0(db_name_base,'_ref')
  } else {
    sc_num <- as.numeric(sub(".*beh(.*)$", "\\1", sc))

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

  return(db_name)
}

## compute premature mortalities difference between Reference and Behavior scenarios
compute_premature_mortalities_diff = function(mort) {

  mort_simpl = mort %>%
    filter(scenario %in% desired_scen) %>%
    filter(year >= year_s, year <= year_e) %>%
    # compute mean mortality across all imp fun
    dplyr::group_by(region,year,scenario,pollutant) %>%
    dplyr::mutate('ci50_mort' = median(as.numeric(mort), na.rm = TRUE)) %>%
    dplyr::mutate('ci5_mort' = quantile(mort, probs= 0.05, na.rm = TRUE)) %>%
    dplyr::mutate('ci95_mort' = quantile(mort, probs= 0.95, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    # add o3 and pm25 mortalities
    dplyr::group_by(region,year,scenario) %>%
    dplyr::mutate('annual_regional_mort_ci50' = sum(ci50_mort)) %>%
    dplyr::mutate('annual_regional_mort_ci5' = sum(ci5_mort)) %>%
    dplyr::mutate('annual_regional_mort_ci95' = sum(ci95_mort))

  # mortality difference (in %)
  mort_diff_percentage = tidyr::pivot_wider(mort_simpl, names_from = 'scenario', values_from = 'total_mort') %>%

    # mutate(across(starts_with("col"), ~ ref - ., .names = "diff_{.col}"))


    dplyr::mutate('diff_protein' = 100*(Diets_Ref  - Diets_Protein)/Diets_Ref) %>%
    dplyr::mutate('diff_trade' = 100*(Diets_Ref - Diets_Trade.adj21)/Diets_Ref)
  mort_diff_percentage = tidyr::pivot_longer(mort_diff_percentage %>% dplyr::select(region,year,diff_protein,diff_trade),
                                             cols = c('diff_protein','diff_trade')) %>%
    dplyr::rename('scenario' = 'name') %>%
    dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
    dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))

  # mortality difference (in absolute numbers)
  mort_diff_absNum = tidyr::pivot_wider(mort_simpl, names_from = 'scenario', values_from = 'total_mort') %>%
    dplyr::mutate('diff_protein' = Diets_Ref  - Diets_Protein) %>%
    dplyr::mutate('diff_trade' = Diets_Ref - Diets_Trade.adj21)
  mort_diff_absNum = tidyr::pivot_longer(mort_diff_absNum %>% dplyr::select(region,year,diff_protein,diff_trade),
                                         cols = c('diff_protein','diff_trade')) %>%
    dplyr::rename('scenario' = 'name') %>%
    dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
    dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))

}

## compute premature mortalities using rfasst
load_premature_mortalities = function() {
  if (!file.exists(paste0(tmp_output_data_path,'mort_',prj_name,'.RData'))) {
    mort = add_mort_scen(desired_scen[1])
    for (i in unique(desired_scen)[2:length(desired_scen)]) {
      mort = dplyr::bind_rows(mort,add_mort_scen(i))
    }
    save(mort, file = paste0(tmp_output_data_path,'mort_',prj_name,'.RData'))
  } else {
    load(paste0(tmp_output_data_path,'mort_',prj_name,'.RData'))
  }

  return(mort)
}


## food items
food_sector = c('Beef','Corn','Dairy','FiberCrop','FodderHerb','Fruits','Legumes',
                'MiscCrop','NutsSeeds','OilCrop','Pork','Poultry','Rice','RootTuber',
                'SheepGoat','Soybean','SugarCrop','Vegetables','Wheat','OilPalm',
                'FodderGrass')


### load all the necessary queries to perform the analysis
load_queries = function() {

  ###### ==== food and agriculture ====
  food_demand_world <<- rgcam::getQuery(prj, "food demand") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(scenario, input, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  food_demand_regional <<- rgcam::getQuery(prj, "food demand") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(region, scenario, input, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  food_consumption_world <<- rgcam::getQuery(prj, "food consumption by type (specific)") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(scenario, technology, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  food_consumption_regional <<- rgcam::getQuery(prj, "food consumption by type (specific)") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(region, scenario, technology, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  ag_production <<- rgcam::getQuery(prj, "ag production by crop type") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(scenario, sector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  food_demand_prices_world <<- rgcam::getQuery(prj, "food demand prices") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(Units, scenario, input, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  food_demand_prices_regional <<- rgcam::getQuery(prj, "food demand prices") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(Units, region, scenario, input, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  ag_prices <<- rgcam::getQuery(prj, "ag import vs. domestic prices") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(scenario, sector, subsector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  ag_prices_world <<- rgcam::getQuery(prj, "ag regional prices (weighted average b/t domestic and imported prices)") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(scenario, sector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  ag_prices_regional <<- rgcam::getQuery(prj, "ag regional prices (weighted average b/t domestic and imported prices)") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(scenario, region, sector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  ag_import_vs_domestic <<- rgcam::getQuery(prj, "ag import vs. domestic supply (Regional Armington competition)") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(scenario, sector, subsector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate('type' = ifelse(substr(subsector, 1, 8) == "domestic", "domestic",
                           ifelse(substr(subsector, 1, 8) == "imported", "imported", NA))) %>%
    filter(scenario %in% selected_scen) %>%
    filter(!is.na(type)) %>%
    filter(year >= year_s, year <= year_e)

  ag_meet_dairy_prices_world <<- rgcam::getQuery(prj, "meat and dairy prices") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(scenario, sector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  ag_meet_dairy_prices_regional <<- rgcam::getQuery(prj, "meat and dairy prices") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(scenario, region, sector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  ###### ==== water ====
  water_withdrawals <<- rgcam::getQuery(prj, "water withdrawals by region") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(scenario, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  water_consumption <<- rgcam::getQuery(prj, "water consumption by subsector") %>%
    filter(scenario %in% selected_scen, sector %in% food_sector) %>%
    group_by(scenario, sector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  water_consumption_world <<- rgcam::getQuery(prj, "water consumption by subsector") %>%
    filter(scenario %in% selected_scen, sector %in% food_sector) %>%
    group_by(scenario, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  water_consumption_regional <<- rgcam::getQuery(prj, "water consumption by subsector") %>%
    filter(scenario %in% selected_scen, sector %in% food_sector) %>%
    group_by(scenario, year, region) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  water_consumption_regional_sectorial <<- rgcam::getQuery(prj, "water consumption by subsector") %>%
    filter(scenario %in% selected_scen, sector %in% food_sector) %>%
    group_by(scenario, region, sector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)


  ###### ==== emissions ====
  GWP <- readr::read_csv(paste0(folder_analysis_path,"data/GWP_AR5.csv"))

  co2_emiss <<- getQuery(prj,"CO2 emissions by region") %>%
    group_by(scenario, region, year, Units) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(ghg = "CO2") %>%
    filter(year >= year_s, year <= year_e)

  luc <<- getQuery(prj,"LUC emissions by region") %>%
    group_by(Units, scenario, region, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(ghg = "LUC CO2",
           Units = "MTC") %>%
    filter(year >= year_s, year <= year_e)

  nonco2 <<- getQuery(prj,"nonCO2 emissions by region") %>%
    filter(year >= year_s, year <= year_e)


  ###### ==== GHG emissions ====
  ghg_by_ghg <<- bind_rows(luc,co2_emiss,nonco2) %>%
    left_join(GWP, by = c("Units", "ghg")) %>%
    mutate(value = value * GWP,
           Units = "MtCO2e") %>%
    group_by(group, scenario, year, Units) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    filter(!is.na(group))

  ghg_regional <<- bind_rows(luc,co2_emiss,nonco2) %>%
    left_join(GWP, by = c("Units", "ghg")) %>%
    mutate(value = value * GWP,
           Units = "MtCO2e") %>%
    group_by(scenario, region, year, Units) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup()

  ghg_world <<- bind_rows(luc,co2_emiss,nonco2) %>%
    left_join(GWP, by = c("Units", "ghg")) %>%
    mutate(value = value * GWP,
           Units = "MtCO2e") %>%
    group_by(scenario, year, Units) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup()

}
