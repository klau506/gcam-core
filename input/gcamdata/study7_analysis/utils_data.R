
# load mapping data
load_mapping_data = function() {
  db_scen_mapping <<- read.csv(paste0(inputs_path,"mappings/db_scen_mapping.csv"), header = TRUE) %>%
    as.data.table()
  iso_gcam_regions <<- read.csv(paste0(inputs_path,"mappings/iso_GCAM_regID.csv"), skip = 6)
  id_gcam_regions <<- read.csv(paste0(inputs_path,"mappings/gcam_id_to_region.csv"))
  colnames(id_gcam_regions) = c('GCAM_region_ID', 'region')
  country_gcam_regions <- read.csv(paste0(inputs_path,"mappings/country_to_gcam_id.csv"))
  regions_key <<- dplyr::left_join(country_gcam_regions, id_gcam_regions, by = "GCAM_region_ID") %>%
    dplyr::select(-1)

  list_scen = list()
  for(tt in unique(db_scen_mapping$scen_type)) {
    tmp_list <<- db_scen_mapping$scen_name[db_scen_mapping$scen_type == tt]
    list_scen[[paste0('list_scen_',tt)]] = tmp_list
  }
  list_scen <<- list_scen

  ## food items
  food_sector <<- c('Beef','Corn','Dairy','FiberCrop','FodderHerb','Fruits','Legumes',
                    'MiscCrop','NutsSeeds','OilCrop','Pork','Poultry','Rice','RootTuber',
                    'SheepGoat','Soybean','SugarCrop','Vegetables','Wheat','OilPalm',
                    'FodderGrass')
  # staple commodities
  staples <<- c("Corn", "OtherGrain", "Rice", "RootTuber", "Wheat")
  # animal commodities
  animal_commodities <<- c("Beef", "Dairy", "OtherMeat_Fish", "Pork", "Poultry", "SheepGoat")
  # plant protein commodities
  plant_prot_commodities <<- c("Legumes", "NutsSeeds")

}



# load nutritional data
load_nutritional_data = function(pop_all_reg) {
  # Map of the supplysectors and technologies
  food_subsector <<- read.csv(paste0(inputs_path,"nutrition/food_subsector.csv"))

  # Read in MDER (calculated exogenously, FAO data)
  mder <<- read.csv(paste0(inputs_path,"nutrition/MDER.csv")) %>%
    rename(mder_units = unit) %>%
    mutate(mder_units = 'kcal/capita/day')
  # Macronutrients: this comes from module_aglu_L100.FAO_SUA_connection
  gcamdata_macro <<- read.csv(paste0(inputs_path,"nutrition/gcamdata_macronutrient.csv"))
  gcamdata_macro[is.na(gcamdata_macro)] <<- 0

  # Gram of Protein & Fat Per Kcal data
  GramProteinFatPerKcal <<- readr::read_csv(paste0(inputs_path,"nutrition/GramProteinFatPerKcal.csv"))


  # micronutrients data
  micronutrients <<- read.csv(file = paste0(inputs_path,'nutrition/micronutrients_computed.csv')) %>%
    select(-1)

  # compute socioeconomic data
  # Socio-econ data
  ssp_data <<- read.csv(paste0(inputs_path,"nutrition/SSP2_population_by_demographic.csv"), skip = 1)
  ssp_data_clean <- iso_gcam_regions %>%
    select(-region_GCAM3, -GCAM_region_ID) %>%
    left_join(ssp_data, by = "iso", multiple = 'all') %>%
    select(-MODEL, -REGION) %>%
    rename(scenario = SCENARIO,
           variable = VARIABLE,
           unit = UNIT)
  # Remove X from year columns
  colnames(ssp_data_clean) <- gsub("X", "", colnames(ssp_data_clean))
  # Pivot longer
  ssp_data_long <<- ssp_data_clean %>%
    tidyr::pivot_longer(cols = 6:24, names_to = "year", values_to = "value") %>%
    mutate(value = value * 1e6,
           unit = "total population")
  # Isolate reference (total) population
  reference_pop <<- ssp_data_long %>%
    filter(variable == "Population") %>%
    rename(total_pop = value) %>%
    select(iso, year, total_pop)
  # Join and calculate demographic shares of population
  ssp_data_final <<- ssp_data_long %>%
    filter(variable != "Population") %>%
    left_join(reference_pop, by = c("iso", "year")) %>%
    mutate(demo_share = value / total_pop) %>%
    rename(sub_pop = value)  %>%
    # Remove total male and total female pop, we want by age/sex
    filter(!(variable %in% c("Population|Male", "Population|Female"))) %>%
    rename(pop_units = unit)
  # Get population by sex and age
  # Population weighting
  total_regional_pop <<- ssp_data_final %>%
    filter(year >= year_s, year <= year_e) %>%
    select(-scenario, -iso) %>%
    # get GCAM regions instead of country names
    left_join(regions_key, by = "country_name") %>%
    # get total regional population
    group_by(year, GCAM_region_ID, country_name, region) %>%
    # isolate total population by country
    distinct(total_pop) %>%
    group_by(year, GCAM_region_ID, region) %>%
    # sum for total regional population
    mutate(total_regional_pop = sum(total_pop)) %>%
    ungroup()

  total_regional_pop <<- ssp_data_final %>%
    filter(year >= year_s, year <= year_e) %>%
    select(-scenario, -iso) %>%
    # get GCAM regions instead of country names
    left_join(regions_key, by = "country_name") %>%
    # get total regional population
    left_join(total_regional_pop) %>%
    # weight each country by its population over total regional pop
    group_by(country_name, year) %>%
    mutate(weight = total_pop / total_regional_pop) %>%
    # get GCAM population
    left_join(mutate(pop_all_reg, year = as.character(year)), by = c("region", "year"),
              multiple = "all") %>%
    # compute GCAM population by sex and age for each country
    mutate(weighted_demographics = demo_share * weight * population)

  if ('final_share' %in% colnames(total_regional_pop)) {
    weighted_pop_sex_age <<- total_regional_pop %>%
      select(-pop_units, -sub_pop, -total_pop, -demo_share, -weight) %>%
      group_by(scenario, final_share, peak_year, slope, scen_type, variable, year, region) %>%
      # sum the weighted averages for each country into GCAM regions
      summarize(pop_sex_age = sum(weighted_demographics)) %>%
      ungroup()
  } else {
    weighted_pop_sex_age <<- total_regional_pop %>%
      select(-pop_units, -sub_pop, -total_pop, -demo_share, -weight) %>%
      group_by(scenario, scen_type, variable, year, region) %>%
      # sum the weighted averages for each country into GCAM regions
      summarize(pop_sex_age = sum(weighted_demographics)) %>%
      ungroup()
  }
}



# load both mapping and nutritional basic data
load_basic_data = function() {
  load_mapping_data()
  load_nutritional_data()
}


# compute premature mortality due to AP through rfasst
calc_mort_rfasst = function(scen) {

  rm(mort.data)

  for (i in 1:length(scen)) {
    print(i)
    rm(list = clean_pkg_outputs())
    pm.mort = rfasst::m3_get_mort_pm25(prj_name = 'rfasst_diets.dat',
                                           rdata_name = paste0('C:/GCAM/GCAM_7.0_Claudia/',gcam_folder,'/input/gcamdata/study7_analysis/outputs/rfasst_queries_all.RData'),
                                           scen_name = scen[i], final_db_year = 2050, map = F) %>%
    dplyr::mutate('scenario' = scen[i])
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

    o3.mort = rfasst::m3_get_mort_o3(prj_name = 'rfasst_diets.dat',
                                     rdata_name = paste0('C:/GCAM/GCAM_7.0_Claudia/',gcam_folder,'/input/gcamdata/study7_analysis/outputs/rfasst_queries_all.RData'),
                                     scen_name = scen[i], final_db_year = 2050, map = F) %>%
    dplyr::mutate('scenario' = scen[i])
    o3.mort = tidyr::pivot_longer(o3.mort, cols = c('mort_o3_jer_med',
                                                    'mort_o3_jer_low',
                                                    'mort_o3_jer_high',
                                                    'mort_o3_gbd2016_med',
                                                    'mort_o3_gbd2016_low',
                                                    'mort_o3_gbd2016_high'), names_to = 'method', values_to = 'mort_by_disease')
    o3.mort = o3.mort %>%
      dplyr::group_by(region,year,scenario,method) %>%
      dplyr::summarise('mort' = sum(mort_by_disease))

    mort.tmp = dplyr::bind_rows(pm.mort %>% dplyr::mutate('pollutant' = 'pm25'),
                                o3.mort %>% dplyr::mutate('pollutant' = 'o3'))

    if(exists('mort.data')) {
      mort.data = rbind(mort.data, mort.tmp)
    } else {
      mort.data = mort.tmp
    }
  }

  rm(list = clean_pkg_outputs())
  save(mort.data, file = paste0(outputs_path, 'mort.data.RData'))
  return(mort.data)
}



# compute crop loss due to AP through rfasst
calc_croploss_rfasst = function(scen) {

  rm(croploss.data)

  for (i in 1:length(scen)) {
    print(i)
    rm(list = clean_pkg_outputs())
    ryl.mi.croploss = rfasst::m4_get_ryl_mi(prj_name = 'rfasst_diets.dat',
                                    rdata_name = paste0('C:/GCAM/GCAM_7.0_Claudia/',gcam_folder,'/input/gcamdata/study7_analysis/outputs/rfasst_queries_all.RData'),
                                    scen_name = scen[i], final_db_year = 2050, map = F) %>%
    dplyr::mutate('scenario' = scen[i])

    if(exists('croploss.data')) {
      croploss.data = rbind(croploss.data, ryl.mi.croploss)
    } else {
      croploss.data = ryl.mi.croploss
    }
  }

  rm(list = clean_pkg_outputs())
  save(croploss.data, file = paste0(outputs_path, 'croploss.data.RData'))
  return(croploss.data)
}



# find the db name given the scenario name
find_db_name = function(scenario) {
  return(db_scen_mapping$db_name[db_scen_mapping$scen_name == scenario])
}



# load or create rgcam project
load_prj = function(prj_name, prj_desired_scen, onlyFoodConsumption = FALSE) {
  db_path = paste0('C:\\GCAM\\GCAM_7.0_Claudia\\',gcam_folder,'\\output')
  query_path = paste0(inputs_path, 'queries/')
  queries <<- if_else(onlyFoodConsumption, 'queries_beh_foodConsumption.xml', 'queries_beh.xml')

  if (!file.exists(prj_name)) {
    print('create prj')
    for (sc in prj_desired_scen) {
      print(sc)
      db_name = find_db_name(sc)

      ## create prj
      conn <- rgcam::localDBConn(db_path, db_name)
      prj <<- rgcam::addScenario(conn, prj_name, sc, saveProj = FALSE,
                                 paste0(query_path, queries),
                                 clobber = FALSE)

      # add 'nonCO2' large query
      if (!onlyFoodConsumption) prj <<- fill_queries(db_path, db_name, prj_name, sc, prj)
    }

    saveProject(prj, file = prj_name)

  } else {
    ## load prj
    print('load prj')
    prj <<- loadProject(prj_name)
  }

  return(prj)
}


# aux function of fill_queries to load heavy queries
data_query = function(type, db_path, db_name, prj_name, scenarios) {
  dt = data.frame()
  xml <- xml2::read_xml(paste0(inputs_path, 'queries/queries_beh_nonCO2.xml'))
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




# load heavy queries
fill_queries = function(db_path, db_name, prj_name, sc, prj) {
  # add nonCO2 query manually (it is too big to use the usual method)
  print('nonCO2 emissions by sector')
  dt_sec = data_query('nonCO2 emissions by sector (excluding resource production)', db_path, db_name, prj_name, sc)
  prj_tmp <- rgcam::addQueryTable(project = 'prj_tmp1.dat', qdata = dt_sec, saveProj = FALSE, clobber = TRUE,
                                  queryname = 'nonCO2 emissions by sector (excluding resource production)')
  prj <- rgcam::mergeProjects(prj_name, list(prj,prj_tmp), clobber = TRUE)

  return(prj)
}




# if query exists, append the new scenarios
update_query = function(data, data_name, rfasst = FALSE) {

  if (!rfasst) {
    data = data %>%
      tidyr::separate(scenario, into = c("scen_type", "scen_type2", "final_share", "peak_year", "slope"), sep = "_", remove = FALSE) %>%
      dplyr::select(-scen_type2)
  }

  if (exists(as.character(data_name))) {
    data = rbind(data, get(data_name))
  }

  if (exists('list_queries')) {
    list_queries <<- c(list_queries, data_name)
  } else {
    list_queries <<- c(data_name)
  }


  return(invisible(data))
}


# remove Scenario Specific Type Columns, ie, final_share, peak_year, slope
# it is useful for the REF scenarios, where all these cols are NA
remove_sstcols = function(data) {
  data = data %>%
    dplyr::select(-c('final_share','peak_year','slope'))

  return(data)
}

save_queries = function(queries_file_name = 'no_name.RData') {
  dt = list()
  for (q in list_queries) {
    dt[[q]] = get(q)
  }
  save(dt, file = paste0(outputs_path, queries_file_name))

  return(dt)
}

save_rfasst_queries = function() {
  dt = list()
  for (q in list_queries) {
    dt[[q]] = get(q)
  }
  save(dt, file = paste0(outputs_path, 'rfasst_queries_all.RData'))

  return(dt)
}


load_rfasst_queries = function() {

  rfasst_queries = c('International Aviation emissions',
                     'International Shipping emissions',
                     'nonCO2 emissions by resource production',
                     'Ag Commodity Prices',
                     'prices of all markets',
                     'ag production by subsector (land use region)',
                     'ag production by crop type',
                     'nonCO2 emissions by sector (excluding resource production)')

  prj1 = 'C:/GCAM/GCAM_7.0_Claudia/gcam-core-spp/input/gcamdata/study7_analysis/spp_gathered.dat'
  prj2 = 'C:/GCAM/GCAM_7.0_Claudia/gcam-core-spp/input/gcamdata/study7_analysis/spp_reference.dat'
  prj3 = 'C:/GCAM/GCAM_7.0_Claudia/gcam-core/input/gcamdata/study7_analysis/snr_gathered2.dat'
  prj4 = 'C:/GCAM/GCAM_7.0_Claudia/gcam-core/input/gcamdata/study7_analysis/snr_reference.dat'
  rm(list_queries)
  for (prj in c('prj1','prj2','prj3','prj4')) {
    assign(prj,rgcam::loadProject(get(prj)))
    for (qn in rfasst_queries) {
      print(qn)
      assign(qn,
             rgcam::getQuery(get(prj), qn) %>%
               update_query(., qn, rfasst = TRUE)
      )
    }
  }

  save_rfasst_queries()
}


# load queries
load_queries = function(onlyFoodConsumption = FALSE, onlyVegetativeCarbonStock = FALSE) {

  if (onlyFoodConsumption) {
    food_consumption_world <<- rgcam::getQuery(prj, "food consumption by type (specific)") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(Units, scenario, subsector...4, subsector...5, subsector...6, technology, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      rename(nestingSector1 = subsector...4) %>%
      tidyr::separate(nestingSector1, into = c("nestingSector1", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
      rename(nestingSector2 = subsector...5) %>%
      tidyr::separate(nestingSector2, into = c("nestingSector2", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
      rename(nestingSector3 = subsector...6) %>%
      tidyr::separate(nestingSector3, into = c("nestingSector3", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'food_consumption_world')

    food_consumption_regional <<- rgcam::getQuery(prj, "food consumption by type (specific)") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(Units, region, scenario, subsector...4, subsector...5, subsector...6, technology, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      rename(nestingSector1 = subsector...4) %>%
      tidyr::separate(nestingSector1, into = c("nestingSector1", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
      rename(nestingSector2 = subsector...5) %>%
      tidyr::separate(nestingSector2, into = c("nestingSector2", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
      rename(nestingSector3 = subsector...6) %>%
      tidyr::separate(nestingSector3, into = c("nestingSector3", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'food_consumption_regional')

  } else if (onlyVegetativeCarbonStock) {
    carbon_stock_world <<- getQuery(prj,"vegetative carbon stock by region") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(Units, scenario, year, landleaf) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'carbon_stock_world')

    carbon_stock_regional <<- getQuery(prj,"vegetative carbon stock by region") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(Units, scenario, year, region, landleaf) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'carbon_stock_regional')

  } else {

    if (!exists('list_queries')) {
      list_queries <<- c()
    }

    ###### ==== food and agriculture ====
    food_demand_world <<- rgcam::getQuery(prj, "food demand") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(scenario, input, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'food_demand_world')

    food_demand_regional <<- rgcam::getQuery(prj, "food demand") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(region, scenario, input, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'food_demand_regional')

    # staple and non-staple calories
    staples_nonstaples_world <<- getQuery(prj, "food demand") %>%
      filter(scenario %in% selected_scen) %>%
      filter(year >= year_s, year <= year_e) %>%
      mutate(input = gsub("FoodDemand_", "", input)) %>%
      group_by(scenario, Units, year, input) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      tidyr::pivot_wider(names_from = input, values_from = value) %>%
      left_join(getQuery(prj, "food demand") %>%
                  filter(scenario %in% selected_scen) %>%
                  filter(year >= year_s, year <= year_e) %>%
                  select(-input) %>%
                  group_by(scenario, Units, year) %>%
                  summarize(Total = sum(value))) %>%
      update_query(., 'staples_nonstaples_world')

    staples_nonstaples_regional <<- getQuery(prj, "food demand") %>%
      filter(scenario %in% selected_scen) %>%
      filter(year >= year_s, year <= year_e) %>%
      mutate(input = gsub("FoodDemand_", "", input)) %>%
      tidyr::pivot_wider(names_from = input, values_from = value) %>%
      left_join(getQuery(prj, "food demand") %>%
                  filter(scenario %in% selected_scen) %>%
                  filter(year >= year_s, year <= year_e) %>%
                  select(-input) %>%
                  group_by(scenario, region, Units, year) %>%
                  summarize(Total = sum(value))) %>%
      select(-c('gcam-consumer', 'nodeinput')) %>%
      update_query(., 'staples_nonstaples_regional')


    food_consumption_world <<- rgcam::getQuery(prj, "food consumption by type (specific)") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(Units, scenario, subsector...4, subsector...5, technology, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      rename(nestingSector1 = subsector...4) %>%
      tidyr::separate(nestingSector1, into = c("nestingSector1", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
      rename(nestingSector2 = subsector...5) %>%
      tidyr::separate(nestingSector2, into = c("nestingSector2", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'food_consumption_world')

    food_consumption_regional <<- rgcam::getQuery(prj, "food consumption by type (specific)") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(Units, region, scenario, subsector...4, subsector...5, technology, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      rename(nestingSector1 = subsector...4) %>%
      tidyr::separate(nestingSector1, into = c("nestingSector1", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
      rename(nestingSector2 = subsector...5) %>%
      tidyr::separate(nestingSector2, into = c("nestingSector2", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'food_consumption_regional')

    ag_production_world <<- rgcam::getQuery(prj, "ag production by crop type") %>%
      filter(scenario %in% selected_scen, sector %in% food_sector) %>%
      group_by(scenario, sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'ag_production_world')

    ag_production_regional <<- rgcam::getQuery(prj, "ag production by crop type") %>%
      filter(scenario %in% selected_scen, sector %in% food_sector) %>%
      group_by(region, scenario, sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'ag_production_regional')

    food_demand_prices_world <<- rgcam::getQuery(prj, "food demand prices") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(Units, scenario, input, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'food_demand_prices_world')

    food_demand_prices_regional <<- rgcam::getQuery(prj, "food demand prices") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(Units, region, scenario, input, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'food_demand_prices_regional')

    ag_prices <<- rgcam::getQuery(prj, "ag import vs. domestic prices") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(scenario, sector, subsector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'ag_prices')

    ag_prices_world <<- rgcam::getQuery(prj, "ag regional prices (weighted average b/t domestic and imported prices)") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(scenario, sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'ag_prices_world')

    ag_prices_regional <<- rgcam::getQuery(prj, "ag regional prices (weighted average b/t domestic and imported prices)") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(scenario, region, sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'ag_prices_regional')

    ag_import_vs_domestic_world <<- rgcam::getQuery(prj, "ag import vs. domestic supply (Regional Armington competition)") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(Units, scenario, sector, subsector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(subsector = ifelse(substr(subsector, 1, 8) == "domestic", "domestic",
                                ifelse(substr(subsector, 1, 8) == "imported", "imported", NA)),
             sector = sub(".*\\s", "", sector)) %>%
      filter(scenario %in% selected_scen) %>%
      filter(!is.na(subsector)) %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'ag_import_vs_domestic_world')

    ag_import_vs_domestic_regional <<- rgcam::getQuery(prj, "ag import vs. domestic supply (Regional Armington competition)") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(region, Units, scenario, sector, subsector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(subsector = ifelse(substr(subsector, 1, 8) == "domestic", "domestic",
                                ifelse(substr(subsector, 1, 8) == "imported", "imported", NA)),
             sector = sub(".*\\s", "", sector)) %>%
      filter(scenario %in% selected_scen) %>%
      filter(!is.na(subsector)) %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'ag_import_vs_domestic_regional')

    ag_meet_dairy_prices_world <<- rgcam::getQuery(prj, "meat and dairy prices") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(scenario, sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'ag_meet_dairy_prices_world')

    ag_meet_dairy_prices_regional <<- rgcam::getQuery(prj, "meat and dairy prices") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(scenario, region, sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'ag_meet_dairy_prices_regional')

    ag_meet_dairy_production_world <<- rgcam::getQuery(prj, "meat and dairy production by tech") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(Units, scenario, sector, subsector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'ag_meet_dairy_production_world')

    ag_meet_dairy_production_regional <<- rgcam::getQuery(prj, "meat and dairy production by tech") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(Units, scenario, sector, subsector, year, region) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'ag_meet_dairy_production_regional')

    feed_consumption_world <<- rgcam::getQuery(prj, "feed consumption by region") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(scenario, Units, year, input) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'feed_consumption_world')

    feed_consumption_regional <<- rgcam::getQuery(prj, "feed consumption by region") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(region, scenario, Units, year, input) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'feed_consumption_regional')


    ###### ==== water ====
    water_withdrawals_world <<- rgcam::getQuery(prj, "water withdrawals by region") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(scenario, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'water_withdrawals_world')

    water_withdrawals_regional <<- rgcam::getQuery(prj, "water withdrawals by region") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(region, scenario, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'water_withdrawals_regional')

    water_consumption <<- rgcam::getQuery(prj, "water consumption by subsector") %>%
      filter(scenario %in% selected_scen, sector %in% food_sector) %>%
      group_by(Units, scenario, sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'water_consumption')

    water_consumption_world <<- rgcam::getQuery(prj, "water consumption by subsector") %>%
      filter(scenario %in% selected_scen, sector %in% food_sector) %>%
      group_by(Units, scenario, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'water_consumption_world')

    water_consumption_regional <<- rgcam::getQuery(prj, "water consumption by subsector") %>%
      filter(scenario %in% selected_scen, sector %in% food_sector) %>%
      group_by(Units, scenario, year, region) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'water_consumption_regional')

    water_consumption_regional_sectorial <<- rgcam::getQuery(prj, "water consumption by subsector") %>%
      filter(scenario %in% selected_scen, sector %in% food_sector) %>%
      group_by(scenario, region, sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'water_consumption_regional_sectorial')

    water_irr_rfd_world <<- rgcam::getQuery(prj, "land allocation by crop and water source") %>%
      filter(!is.na(water)) %>%
      filter(scenario %in% selected_scen) %>%
      filter(!crop %in% c('biomassGrass','biomassTree')) %>%
      dplyr::mutate(crop = sub("C4$", "", crop)) %>%
      dplyr::mutate(crop = sub("Tree$", "", crop)) %>%
      group_by(Units, scenario, year, water, crop) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'water_irr_rfd_world')

    water_irr_rfd_regional <<- rgcam::getQuery(prj, "land allocation by crop and water source") %>%
      filter(!is.na(water)) %>%
      filter(scenario %in% selected_scen) %>%
      filter(!crop %in% c('biomassGrass','biomassTree')) %>%
      dplyr::mutate(crop = sub("C4$", "", crop)) %>%
      dplyr::mutate(crop = sub("Tree$", "", crop)) %>%
      group_by(region, Units, scenario, year, water, crop) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'water_irr_rfd_regional')


    ###### ==== emissions ====
    GWP <<- readr::read_csv(paste0(inputs_path,"mappings/GWP_AR5.csv"))

    co2_emiss <<- getQuery(prj,"CO2 emissions by region") %>%
      group_by(scenario, region, year, Units) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(ghg = "CO2") %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'co2_emiss')

    luc <<- getQuery(prj,"LUC emissions by region") %>%
      group_by(Units, scenario, region, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(ghg = "LUC CO2",
             Units = "MTC") %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'luc')

    nonco2 <<- getQuery(prj,"nonCO2 emissions by region") %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'nonco2')

    nonco2_luc <<- getQuery(prj,"nonCO2 emissions by sector (excluding resource production)") %>%
      filter(year >= year_s, year <= year_e) %>%
      filter(sector %in% food_sector) %>%
      filter(ghg %in% c('CH4_AGR','N2O_AGR')) %>%
      mutate(ghg = ifelse(ghg == 'CH4_AGR', 'CH4','N2O')) %>%
      mutate(Units = 'Mt')%>% #Tg is equivalent to Mt
      update_query(., 'nonco2_luc')


    ###### ==== GHG emissions ====
    ghg_by_ghg_world <<- bind_rows(luc,co2_emiss,nonco2) %>%
      left_join(GWP, by = c("Units", "ghg")) %>%
      mutate(value = value * GWP,
             Units = "MtCO2e") %>%
      group_by(group, scenario, year, Units) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      ungroup() %>%
      filter(!is.na(group)) %>%
      update_query(., 'ghg_by_ghg_world')

    ghg_by_ghg_regional <<- bind_rows(luc,co2_emiss,nonco2) %>%
      left_join(GWP, by = c("Units", "ghg")) %>%
      mutate(value = value * GWP,
             Units = "MtCO2e") %>%
      group_by(region, group, scenario, year, Units) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      ungroup() %>%
      filter(!is.na(group)) %>%
      update_query(., 'ghg_by_ghg_regional')

    ghg_regional <<- bind_rows(luc,co2_emiss,nonco2) %>%
      left_join(GWP, by = c("Units", "ghg")) %>%
      mutate(value = value * GWP,
             Units = "MtCO2e") %>%
      group_by(scenario, region, year, Units) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      ungroup() %>%
      update_query(., 'ghg_regional')

    ghg_world <<- bind_rows(luc,co2_emiss,nonco2) %>%
      left_join(GWP, by = c("Units", "ghg")) %>%
      mutate(value = value * GWP,
             Units = "MtCO2e") %>%
      group_by(scenario, year, Units) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      ungroup() %>%
      update_query(., 'ghg_world')



    ###### ===== land use ======
    land_use_world <<- getQuery(prj,"aggregated land allocation") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(Units, scenario, year, landleaf) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      mutate(land_use_type = ifelse(landleaf %in% c("forest (managed)", "forest (unmanaged)"), 'Forest',
                                    ifelse(landleaf %in% c('crops','biomass','otherarable'), 'Cropland',
                                           ifelse(landleaf %in% c("pasture (grazed)","pasture (other)"), 'Pasture',
                                                  ifelse(landleaf %in% c("shrubs","grass"), 'Shrubs & Grass',
                                                         'Other Natural'))))) %>%
      update_query(., 'land_use_world')

    land_use_regional <<- getQuery(prj,"aggregated land allocation") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(Units, scenario, year, region, landleaf) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      mutate(land_use_type = ifelse(landleaf %in% c("forest (managed)", "forest (unmanaged)"), 'Forest',
                                    ifelse(landleaf %in% c('crops','biomass','otherarable'), 'Cropland',
                                           ifelse(landleaf %in% c("pasture (grazed)","pasture (other)"), 'Pasture',
                                                  ifelse(landleaf %in% c("shrubs","grass"), 'Shrubs & Grass',
                                                         'Other Natural'))))) %>%
      update_query(., 'land_use_regional')

    land_crop_world <<- getQuery(prj,"land allocation by crop") %>%
      dplyr::mutate(landleaf = sub("C4$", "", landleaf)) %>%
      dplyr::mutate(landleaf = sub("Tree$", "", landleaf)) %>%
      group_by(Units, scenario, year, landleaf) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(scenario %in% selected_scen) %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'land_crop_world')

    land_crop_regional <<- getQuery(prj,"land allocation by crop") %>%
      dplyr::mutate(landleaf = sub("C4$", "", landleaf)) %>%
      dplyr::mutate(landleaf = sub("Tree$", "", landleaf)) %>%
      group_by(Units, scenario, region, year, landleaf) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(scenario %in% selected_scen) %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'land_crop_regional')


    carbon_stock_world <<- getQuery(prj,"vegetative carbon stock by region") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(Units, scenario, year, landleaf) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'carbon_stock_world')

    carbon_stock_regional <<- getQuery(prj,"vegetative carbon stock by region") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(Units, scenario, year, region, landleaf) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'carbon_stock_regional')


    fertilizer_consumption_world <<- getQuery(prj,"fertilizer consumption by crop type") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(Units, scenario, year, sector) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'fertilizer_consumption_world')

    fertilizer_consumption_regional <<- getQuery(prj,"fertilizer consumption by crop type") %>%
      filter(scenario %in% selected_scen) %>%
      group_by(Units, scenario, year, region, sector) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'fertilizer_consumption_regional')


    ###### ===== population ======
    # population by region
    pop_all_regions <<- getQuery(prj, "population by region") %>%
      filter(year >= year_s, year <= year_e) %>%
      mutate(value = value * 1000) %>% # Convert from thous ppl to total ppl
      select(-Units) %>%
      rename(population = value) %>%
      update_query(., 'pop_all_regions')

  }

}


