#' update_query
#'
#' Function to append (new) scenarios to existing data-lists with the queries extraction
#' @param data dataset
#' @param data_name query name
#' @return append dataset to the desired query name in the data-list
update_query = function(data, data_name) {

  data = data %>%
    tidyr::separate(scenario, into = c("scen_type", "scen_path",
                                       "final_share",
                                       "peak_year_title", "peak_year",
                                       "slope_title", "slope"),
                    sep = "_", remove = FALSE) %>%
    dplyr::select(-peak_year_title) %>%
    dplyr::select(-slope_title)

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


#' load_queries
#'
#' Function to load the project queries and store them in an RData list
#' @param queries_name name of the queries file to be saved
#' @return data-list (and saves it)
load_queries = function(queries_name) {

    if (!exists('list_queries')) {
      list_queries <<- c()
    }

    ###### ==== food and agriculture ====
    food_demand_world <<- rgcam::getQuery(prj, "food demand") %>%
      group_by(scenario, input, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'food_demand_world')

    food_demand_regional <<- rgcam::getQuery(prj, "food demand") %>%
      group_by(region, scenario, input, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'food_demand_regional')

    # staple and non-staple calories
    staples_nonstaples_world <<- getQuery(prj, "food demand") %>%
      filter(year >= year_s, year <= year_e) %>%
      mutate(input = gsub("FoodDemand_", "", input)) %>%
      group_by(scenario, Units, year, input) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      tidyr::pivot_wider(names_from = input, values_from = value) %>%
      left_join(getQuery(prj, "food demand") %>%
                              filter(year >= year_s, year <= year_e) %>%
                  select(-input) %>%
                  group_by(scenario, Units, year) %>%
                  summarize(Total = sum(value))) %>%
      update_query(., 'staples_nonstaples_world')

    staples_nonstaples_regional <<- getQuery(prj, "food demand") %>%
      filter(year >= year_s, year <= year_e) %>%
      mutate(input = gsub("FoodDemand_", "", input)) %>%
      tidyr::pivot_wider(names_from = input, values_from = value) %>%
      left_join(getQuery(prj, "food demand") %>%
                              filter(year >= year_s, year <= year_e) %>%
                  select(-input) %>%
                  group_by(scenario, region, Units, year) %>%
                  summarize(Total = sum(value))) %>%
      select(-c('gcam-consumer', 'nodeinput')) %>%
      update_query(., 'staples_nonstaples_regional')


    food_consumption_world <<- rgcam::getQuery(prj, "food consumption by type (specific)") %>%
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

    ag_production_world <<- rgcam::getQuery(prj, "ag production by crop type") %>%
      filter(sector %in% food_sector) %>%
      group_by(scenario, sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'ag_production_world')

    ag_production_regional <<- rgcam::getQuery(prj, "ag production by crop type") %>%
      filter(sector %in% food_sector) %>%
      group_by(region, scenario, sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'ag_production_regional')

    food_demand_prices_world <<- rgcam::getQuery(prj, "food demand prices") %>%
      group_by(Units, scenario, input, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'food_demand_prices_world')

    food_demand_prices_regional <<- rgcam::getQuery(prj, "food demand prices") %>%
      group_by(Units, region, scenario, input, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'food_demand_prices_regional')

    ag_prices <<- rgcam::getQuery(prj, "ag import vs. domestic prices") %>%
      group_by(scenario, sector, subsector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'ag_prices')

    ag_prices_world <<- rgcam::getQuery(prj, "ag regional prices (weighted average b/t domestic and imported prices)") %>%
      group_by(scenario, sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'ag_prices_world')

    ag_prices_regional <<- rgcam::getQuery(prj, "ag regional prices (weighted average b/t domestic and imported prices)") %>%
      group_by(scenario, region, sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'ag_prices_regional')

    ag_import_vs_domestic_world <<- rgcam::getQuery(prj, "ag import vs. domestic supply (Regional Armington competition)") %>%
      group_by(Units, scenario, sector, subsector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(subsector = ifelse(substr(subsector, 1, 8) == "domestic", "domestic",
                                ifelse(substr(subsector, 1, 8) == "imported", "imported", NA)),
             sector = sub(".*\\s", "", sector)) %>%
      filter(!is.na(subsector)) %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'ag_import_vs_domestic_world')

    ag_import_vs_domestic_regional <<- rgcam::getQuery(prj, "ag import vs. domestic supply (Regional Armington competition)") %>%
      group_by(region, Units, scenario, sector, subsector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(subsector = ifelse(substr(subsector, 1, 8) == "domestic", "domestic",
                                ifelse(substr(subsector, 1, 8) == "imported", "imported", NA)),
             sector = sub(".*\\s", "", sector)) %>%
      filter(!is.na(subsector)) %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'ag_import_vs_domestic_regional')

    ag_meet_dairy_prices_world <<- rgcam::getQuery(prj, "meat and dairy prices") %>%
      group_by(scenario, sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'ag_meet_dairy_prices_world')

    ag_meet_dairy_prices_regional <<- rgcam::getQuery(prj, "meat and dairy prices") %>%
      group_by(scenario, region, sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'ag_meet_dairy_prices_regional')

    ag_meet_dairy_production_world <<- rgcam::getQuery(prj, "meat and dairy production by tech") %>%
      group_by(Units, scenario, sector, subsector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'ag_meet_dairy_production_world')

    ag_meet_dairy_production_regional <<- rgcam::getQuery(prj, "meat and dairy production by tech") %>%
      group_by(Units, scenario, sector, subsector, year, region) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'ag_meet_dairy_production_regional')

    feed_consumption_world <<- rgcam::getQuery(prj, "feed consumption by region") %>%
      group_by(scenario, Units, year, input) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'feed_consumption_world')

    feed_consumption_regional <<- rgcam::getQuery(prj, "feed consumption by region") %>%
      group_by(region, scenario, Units, year, input) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'feed_consumption_regional')


    ###### ==== water ====
    water_withdrawals_world <<- rgcam::getQuery(prj, "water withdrawals by region") %>%
      group_by(scenario, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'water_withdrawals_world')

    water_withdrawals_regional <<- rgcam::getQuery(prj, "water withdrawals by region") %>%
      group_by(region, scenario, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'water_withdrawals_regional')

    water_consumption <<- rgcam::getQuery(prj, "water consumption by subsector") %>%
      filter(sector %in% food_sector) %>%
      group_by(Units, scenario, sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'water_consumption')

    water_consumption_world <<- rgcam::getQuery(prj, "water consumption by subsector") %>%
      filter(sector %in% food_sector) %>%
      group_by(Units, scenario, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'water_consumption_world')

    water_consumption_regional <<- rgcam::getQuery(prj, "water consumption by subsector") %>%
      filter(sector %in% food_sector) %>%
      group_by(Units, scenario, year, region) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'water_consumption_regional')

    water_consumption_regional_sectorial <<- rgcam::getQuery(prj, "water consumption by subsector") %>%
      filter(sector %in% food_sector) %>%
      group_by(scenario, region, sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'water_consumption_regional_sectorial')

    water_irr_rfd_world <<- rgcam::getQuery(prj, "land allocation by crop and water source") %>%
      filter(!is.na(water)) %>%
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
      filter(!crop %in% c('biomassGrass','biomassTree')) %>%
      dplyr::mutate(crop = sub("C4$", "", crop)) %>%
      dplyr::mutate(crop = sub("Tree$", "", crop)) %>%
      group_by(region, Units, scenario, year, water, crop) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'water_irr_rfd_regional')

    resource_supply_curves_world <<- rgcam::getQuery(prj, "resource supply curves") %>%
      group_by(region, Units, scenario, year, resource, subresource, grade) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'resource_supply_curves_world')

    resource_supply_curves_regional <<- rgcam::getQuery(prj, "resource supply curves") %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'resource_supply_curves_regional')

    basin_level_available_runoff_world <<- rgcam::getQuery(prj, "Basin level available runoff") %>%
      group_by(region, Units, scenario, year, subresource, Basin) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'basin_level_available_runoff_world')

    basin_level_available_runoff_regional <<- rgcam::getQuery(prj, "Basin level available runoff") %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'basin_level_available_runoff_regional')


    ###### ==== emissions ====
    GWP <<- readr::read_csv(file.path("diets_analysis","inputs","mappings/GWP_AR5.csv"))

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


    ###### ==== Climate variables ====
    global_mean_temperature <<- getQuery(prj,"global mean temperature") %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'global_mean_temperature')

    net_terrestrial_C_uptake <<- getQuery(prj,"net terrestrial C uptake") %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'net_terrestrial_C_uptake')


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
      filter(landleaf != 'urban') %>%
      group_by(Units, scenario, year, landleaf) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      mutate(land_use_type = ifelse(landleaf %in% c("UnmanagedHardwood_Forest", "UnmanagedSoftwood_Forest", 'Softwood_Forest', 'Hardwood_Forest', 'ProtectedUnmanagedHardwood_Forest', 'ProtectedUnmanagedSoftwood_Forest'), 'Forest',
                                    ifelse(landleaf %in% c('crops','biomass','otherarable'), 'Cropland',
                                           ifelse(landleaf %in% c("pasture (grazed)","pasture (other)"), 'Pasture',
                                                  ifelse(landleaf %in% c("shrubs","grass"), 'Shrubs & Grass',
                                                         'Other Natural'))))) %>%
      update_query(., 'land_use_world')

    land_use_regional <<- getQuery(prj,"aggregated land allocation") %>%
      filter(landleaf != 'urban') %>%
      group_by(Units, scenario, year, region, landleaf) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      mutate(land_use_type = ifelse(landleaf %in% c("UnmanagedHardwood_Forest", "UnmanagedSoftwood_Forest", 'Softwood_Forest', 'Hardwood_Forest', 'ProtectedUnmanagedHardwood_Forest', 'ProtectedUnmanagedSoftwood_Forest'), 'Forest',
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
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'land_crop_world')

    land_crop_regional <<- getQuery(prj,"land allocation by crop") %>%
      dplyr::mutate(landleaf = sub("C4$", "", landleaf)) %>%
      dplyr::mutate(landleaf = sub("Tree$", "", landleaf)) %>%
      group_by(Units, scenario, region, year, landleaf) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'land_crop_regional')

    detailed_land_allocation_world <<- getQuery(prj,"detailed land allocation") %>%
      group_by(Units, scenario, year, landleaf) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'detailed_land_allocation_world')

    detailed_land_allocation_regional <<- getQuery(prj,"detailed land allocation") %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'detailed_land_allocation_regional')


    carbon_stock_world <<- getQuery(prj,"vegetative carbon stock by region") %>%
      group_by(Units, scenario, year, landleaf) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'carbon_stock_world')

    carbon_stock_regional <<- getQuery(prj,"vegetative carbon stock by region") %>%
      group_by(Units, scenario, year, region, landleaf) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'carbon_stock_regional')


    fertilizer_consumption_world <<- getQuery(prj,"fertilizer consumption by crop type") %>%
      group_by(Units, scenario, year, sector) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year >= year_s, year <= year_e) %>%
      update_query(., 'fertilizer_consumption_world')

    fertilizer_consumption_regional <<- getQuery(prj,"fertilizer consumption by crop type") %>%
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


    ###### ===== rumin percentage ======
    rumin_percentage <<- rgcam::getQuery(prj, "food consumption by type (specific)") %>%
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

      dplyr::filter(nestingSector2 == 'Animal') %>%
      dplyr::mutate(is_rumin = ifelse(nestingSector3 == 'Ruminant',TRUE,FALSE)) %>%
      dplyr::select(scenario, region, is_rumin, year, Units, value) %>%
      # compute the total protein and rumin protein Pcal by region
      dplyr::group_by(scenario, region, is_rumin, year, Units) %>%
      dplyr::summarise(across(everything(), sum)) %>%
      dplyr::ungroup() %>%
      # compute the ruminant % (ruminant/total ANIMAL protein food consumption)
      dplyr::arrange(is_rumin) %>%

      dplyr::group_by(scenario, region, year, Units) %>%
      dplyr::summarise(across(value, function(x) return(x[2]/(x[1]+x[2])))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Units = 'Percentage') %>%
      update_query(., 'rumin_percentage')

    ###### ===== plant percentage ======
    plant_percentage <<- rgcam::getQuery(prj, "food consumption by type (specific)") %>%
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

      dplyr::filter(nestingSector1 == 'Protein') %>%
      dplyr::mutate(is_plant = ifelse(nestingSector2 == 'Plant',TRUE,FALSE)) %>%
      dplyr::select(scenario, region, is_plant, year, Units, value) %>%
      # compute the total and plant Pcal by region
      dplyr::group_by(scenario, region, is_plant, year, Units) %>%
      dplyr::summarise(across(everything(), sum)) %>%
      dplyr::ungroup() %>%
      # compute the plant % (plant/total food consumption)
      dplyr::group_by(scenario, region, year, Units) %>%
      dplyr::summarise(across(value, function(x) return(x[2]/(x[1]+x[2])))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Units = 'Percentage') %>%
      update_query(., 'plant_percentage')




    ###### ===== SAVE ======
    dt = list()
    for (q in list_queries) {
      dt[[q]] = get(q)
    }
    save(dt, file = file.path("diets_analysis","outputs",queries_name))

    return(dt)
}
