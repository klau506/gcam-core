
### protein type & scenario type palette
protein_scenario_palette = c('plant protein.Reference' = '#00771E',
                             'plant protein.Behavior change' = '#00DB37',
                             'animal protein.Reference' = '#8A0000',
                             'animal protein.Behavior change' = '#DB0000')

### macronutrients & scenario type palette
macronutrients_scenario_palette =
  c('Behavior change.gFatPerCapita' = '#de99d5',
    'Behavior change.gProteinPerCapita' = '#6cc2f0',
    'Reference.gFatPerCapita' = '#871c79',
    'Reference.gProteinPerCapita' = '#236e96')
macronutrients_order_palette =
  c('Behavior change.gFatPerCapita',
    'Reference.gFatPerCapita',
    'Behavior change.gProteinPerCapita',
    'Reference.gProteinPerCapita')

### kcal intake vs mder & scenario type palette
kcal_mder_scenario_palette =
  c('Behavior change.consumption' = '#de99d5',
    'Behavior change.mder' = '#6cc2f0',
    'Reference.consumption' = '#871c79')
kcal_mder_order_palette =
  c('Behavior change.consumption',
    'Reference.consumption',
    'Behavior change.mder')
kcal_mder_labels_palette =
  c('Behavior change.DES',
    'Reference.DES',
    'DER')


### staples vs non-staples & scenario type palette
staples_vs_nonstaples_scenario_palette =
  c('Behavior change.FoodDemand_NonStaples' = '#0085A5',
    'Behavior change.FoodDemand_Staples' = '#BDB400',
    'Reference.FoodDemand_NonStaples' = '#002787',
    'Reference.FoodDemand_Staples' = '#876400')
staples_vs_nonstaples_order_palette =
c('Behavior change.FoodDemand_Staples',
  'Reference.FoodDemand_Staples',
  'Behavior change.FoodDemand_NonStaples',
  'Reference.FoodDemand_NonStaples')

### irr vs rfd & scenario type palette
irr_rfd_scenario_palette =
  c('Behavior change.IRR' = '#36e35f',
    'Behavior change.RFD' = '#4a90ff',
    'Reference.IRR' = '#017a1e',
    'Reference.RFD' = '#002787')
irr_rfd_order_palette =
c('Behavior change.RFD',
  'Reference.RFD',
  'Behavior change.IRR',
  'Reference.IRR')

### land use type palette
land_use_scenario_palette =
  c('Cropland' = 'chocolate4',
    'Pasture' = '#E6AB02',
    'Forest' = 'darkgreen',
    'Shrubs & Grass' = '#66A61E',
    'Other Natural' = '#b1e378')
land_use_order_palette =
c('Cropland',
  'Pasture',
  'Forest',
  'Shrubs & Grass',
  'Other Natural')

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

### beef mixed vs pastoral & scenario type palette
beef_type_scenario_palette =
  c('Behavior change.Mixed' = '#d3d62d',
    'Behavior change.Pastoral' = '#2dd64f',
    'Reference.Mixed' = '#888a1e',
    'Reference.Pastoral' = '#1e8a3b')
beef_type_order_palette =
c('Behavior change.Mixed',
  'Behavior change.Pastoral',
  'Reference.Mixed',
  'Reference.Pastoral')

### beef mixed vs pastoral & scenario type palette DIFF
beef_type_diff_scenario_palette =
  c('Mixed' = '#d3d62d',
    'Pastoral' = '#2dd64f')
beef_type_diff_order_palette =
c('Mixed',
  'Pastoral')

### food items & scenario type palette
food_items_scenario_palette =
  c('Behavior change.Corn' = '#CCCCFF',
    'Behavior change.FiberCrop' = '#DE3163',
    'Behavior change.FodderGrass' = '#6495ED',
    'Behavior change.FodderHerb' = '#40E0D0',
    'Behavior change.Fruits' = '#72F3B0',
    'Behavior change.Legumes' = '#E77BE3',
    'Behavior change.MiscCrop' = '#E77B95',
    'Behavior change.NutsSeeds' = '#F1ED62',
    'Behavior change.OilCrop' = '#F1B662',
    'Behavior change.OilPalm' = '#EE814B',
    'Behavior change.Rice' = '#8BEE4B',
    'Behavior change.RootTuber' = '#62F7EC',
    'Behavior change.Soybean' = '#5BD3F7',
    'Behavior change.SugarCrop' = '#7B7DF4',
    'Behavior change.Vegetables' = '#B170F2',
    'Behavior change.Wheat' = '#5050FF',
    'Reference.Corn' = '#6666A8',
    'Reference.FiberCrop' = '#92002A',
    'Reference.FodderGrass' = '#2355AF',
    'Reference.FodderHerb' = '#007F72',
    'Reference.Fruits' = '#209A5A',
    'Reference.Legumes' = '#9A2096',
    'Reference.MiscCrop' = '#BC0B36',
    'Reference.NutsSeeds' = '#BCB70B',
    'Reference.OilCrop' = '#BD730B',
    'Reference.OilPalm' = '#BD460B',
    'Reference.Rice' = '#56C012',
    'Reference.RootTuber' = '#0CC4B7',
    'Reference.Soybean' = '#0C9AC4',
    'Reference.SugarCrop' = '#080BC6',
    'Reference.Vegetables' = '#6708C6',
    'Reference.Wheat' = '#000080')
food_items_order_palette =
c('Behavior change.Corn',
  'Behavior change.FiberCrop',
  'Behavior change.FodderGrass',
  'Behavior change.FodderHerb',
  'Behavior change.Fruits',
  'Behavior change.Legumes',
  'Behavior change.MiscCrop',
  'Behavior change.NutsSeeds',
  'Reference.Corn',
  'Reference.FiberCrop',
  'Reference.FodderGrass',
  'Reference.FodderHerb',
  'Reference.Fruits',
  'Reference.Legumes',
  'Reference.MiscCrop',
  'Reference.NutsSeeds',
  'Behavior change.OilCrop',
  'Behavior change.OilPalm',
  'Behavior change.Rice',
  'Behavior change.RootTuber',
  'Behavior change.Soybean',
  'Behavior change.SugarCrop',
  'Behavior change.Vegetables',
  'Behavior change.Wheat',
  'Reference.OilCrop',
  'Reference.OilPalm',
  'Reference.Rice',
  'Reference.RootTuber',
  'Reference.Soybean',
  'Reference.SugarCrop',
  'Reference.Vegetables',
  'Reference.Wheat')

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

# return vector of colors: color_positive if the position of the value in the values'
# vector is positive, color_negative otherwise
color_by_sign = function(values) {
  color_positive = 6
  color_negative = 7
  color_vector <- ifelse(values > 0, color_positive, color_negative)
  return(color_vector)
}


rename_scen = function(data) {

  if ('scenario' %in% colnames(data)) {
    data = data %>%
      mutate(scenario_type = if_else(scenario == 'RvsM_SW_newDS_Reference', 'Reference',
                                     if_else(scenario == 'RvsM_RreducedRandom_logitProtein6_fishNewNest_v1', 'RredRandom_6',
                                             if_else(scenario == 'RvsM_RreducedRandom_logitProtein3_fishNewNest_v1', 'RredRandom_3', scenario)))) %>%
      mutate(scenario = scenario_type)
  } else {
    data = data %>%
      rename('Reference' = 'RvsM_SW_newDS_Reference',
             'Rred_Mincr' = 'RvsM_SW_newDS_FuelPrefElast_0.45',
             'Rred_RandomIncr' = 'RvsM_RreducedRandom_0.45')
  }

  return(invisible(data))
}

### compute pm and o3 premature mortalities
add_mort_scen = function(scen) {
  db_name = find_db_name(scen)

  pm.mort = dplyr::bind_rows(m3_get_mort_pm25(prj_name=prj_name,scen_name=scen,final_db_year=final_db_year,saveOutput=F,map=F)) %>%
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

  o3.mort = dplyr::bind_rows(m3_get_mort_o3(prj_name=prj_name,scen_name=scen,final_db_year=final_db_year,saveOutput=F,map=F)) %>%
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

  return(o3.mort)
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
    prj <<- rgcam::mergeProjects(prj_name, list(prj,prj_tmp), clobber = FALSE)
}


### given the scenario name, find the corresponding db
find_db_name = function(arg1, arg2) {
  db_name = paste0(db_name_base, arg1, '_', arg2)

  return(db_name)
}


## compute premature mortalities using rfasst
load_premature_mortalities = function() {
  if (!file.exists(paste0(tmp_output_data_path,'mort_',sub("\\.dat$", "", prj_name),'.RData'))) {
    mort = add_mort_scen(desired_scen[1])
    for (i in unique(desired_scen)[2:length(desired_scen)]) {
      print(i)
      mort = dplyr::bind_rows(mort,add_mort_scen(i))
    }
    # o3.mort = mort
    save(mort, file = paste0(tmp_output_data_path,'mort_',sub("\\.dat$", "", prj_name),'.RData'))

  save(mort, file = paste0(tmp_output_data_path,'mort_',sub("\\.dat$", "", prj_name),'.RData'))
  } else {
    print('load mort')
    load(paste0(tmp_output_data_path,'mort_',sub("\\.dat$", "", prj_name),'.RData'))
  }

  return(mort)
}


## compute crop loss using rfasst
load_crop_loss = function() {
  if (!file.exists(paste0(tmp_output_data_path,'crop_loss_',sub("\\.dat$", "", prj_name),'.RData'))) {

    # Relative yield losses (Using AOT40 as O3 exposure indicator)
    ryl.aot40 = dplyr::bind_rows(m4_get_ryl_aot40(db_path=NULL,db_name=NULL,prj_name=prj_name,scen_name=desired_scen[1],final_db_year=final_db_year,saveOutput=F,map=F)) %>%
      dplyr::mutate('scenario' = desired_scen[1])
    # Relative yield losses (Using Mi as O3 exposure indicator)
    ryl.mi = dplyr::bind_rows(m4_get_ryl_mi(db_path=NULL,db_name=NULL,prj_name=prj_name,scen_name=desired_scen[1],final_db_year=final_db_year,saveOutput=F,map=F)) %>%
      dplyr::mutate('scenario' = desired_scen[1])
    # Production losses (includes losses using both AOT40 and Mi)
    prod.loss = dplyr::bind_rows(m4_get_prod_loss(db_path=NULL,db_name=NULL,prj_name=prj_name,scen_name=desired_scen[1],final_db_year=final_db_year,saveOutput=F,map=F)) %>%
      dplyr::mutate('scenario' = desired_scen[1])
    # Revenue losses (includes losses using both AOT40 and Mi)
    rev.loss = dplyr::bind_rows(m4_get_rev_loss(db_path=NULL,db_name=NULL,prj_name=prj_name,scen_name=desired_scen[1],final_db_year=final_db_year,saveOutput=F,map=F)) %>%
      dplyr::mutate('scenario' = desired_scen[1])

    for (i in unique(desired_scen)[2:length(desired_scen)]) {
      print(i)
      ryl.aot40 = rbind(ryl.aot40,
                        dplyr::bind_rows(m4_get_ryl_aot40(db_path=NULL,db_name=NULL,prj_name=prj_name,scen_name=desired_scen[1],final_db_year=final_db_year,saveOutput=F,map=F)) %>%
                          dplyr::mutate('scenario' = i))
      print(unique(ryl.aot40$scenario))

      ryl.mi = rbind(ryl.mi,
                     dplyr::bind_rows(m4_get_ryl_mi(db_path=NULL,db_name=NULL,prj_name=prj_name,scen_name=desired_scen[1],final_db_year=final_db_year,saveOutput=F,map=F)) %>%
                       dplyr::mutate('scenario' = i))

      prod.loss = rbind(prod.loss,
                        dplyr::bind_rows(m4_get_prod_loss(db_path=NULL,db_name=NULL,prj_name=prj_name,scen_name=desired_scen[1],final_db_year=final_db_year,saveOutput=F,map=F)) %>%
                          dplyr::mutate('scenario' = i))

      rev.loss = rbind(rev.loss,
                       dplyr::bind_rows(m4_get_rev_loss(db_path=NULL,db_name=NULL,prj_name=prj_name,scen_name=desired_scen[1],final_db_year=final_db_year,saveOutput=F,map=F)) %>%
                         dplyr::mutate('scenario' = i))
    }
    save(ryl.aot40, file = paste0(tmp_output_data_path,'ryl.aot40_',sub("\\.dat$", "", prj_name),'.RData'))
    save(ryl.mi, file = paste0(tmp_output_data_path,'ryl.mi_',sub("\\.dat$", "", prj_name),'.RData'))
    save(prod.loss, file = paste0(tmp_output_data_path,'prod.loss_',sub("\\.dat$", "", prj_name),'.RData'))
    save(rev.loss, file = paste0(tmp_output_data_path,'rev.loss_',sub("\\.dat$", "", prj_name),'.RData'))
    return(NULL)
  } else {
    print('load crop_loss')
    crop_loss = get(load(paste0(tmp_output_data_path,'crop_loss_',sub("\\.dat$", "", prj_name),'.RData')))
    return(crop_loss)
  }
}

## process crop_loss subdatasets changing the format of the crop_names column
process_crop_loss = function(crop_loss) {
  crop_loss = crop_loss %>%
    rename('fasst_region' = 'region')
  if ('pollutant' %in% colnames(crop_loss)) {
    crop_loss = crop_loss %>%
      rename('crop_name' = 'pollutant') %>%
      mutate(crop_name = gsub("^[A-Z]+_(.*)$", "\\1", crop_name),
             crop_name = tolower(crop_name),
             crop_name = paste0(toupper(substr(crop_name, 1, 1)), substr(crop_name, 2, nchar(crop_name))))
  }
  return(crop_loss)
}

## food items
food_sector = c('Beef','Corn','Dairy','FiberCrop','FodderHerb','Fruits','Legumes',
                'MiscCrop','NutsSeeds','OilCrop','Pork','Poultry','Rice','RootTuber',
                'SheepGoat','Soybean','SugarCrop','Vegetables','Wheat','OilPalm',
                'FodderGrass')

# staple commodities
staples = c("Corn", "OtherGrain", "Rice", "RootTuber", "Wheat")

# animal commodities
animal_commodities = c("Beef", "Dairy", "OtherMeat_Fish", "Pork", "Poultry", "SheepGoat")



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
                summarize(Total = sum(value)))

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
    select(-c('gcam-consumer', 'nodeinput'))


  food_consumption_world <<- rgcam::getQuery(prj, "food consumption by type (specific)") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(Units, scenario, subsector...5, subsector...6, technology, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    rename(nestingSubector = subsector...6) %>%
    separate(nestingSubector, into = c("nestingSubector", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
    rename(nestingSector = subsector...5) %>%
    separate(nestingSector, into = c("nestingSector", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
    filter(year >= year_s, year <= year_e)

  food_consumption_regional <<- rgcam::getQuery(prj, "food consumption by type (specific)") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(Units, region, scenario, subsector...5, subsector...6, technology, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    rename(nestingSubector = subsector...6) %>%
    separate(nestingSubector, into = c("nestingSubector", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
    rename(nestingSector = subsector...5) %>%
    separate(nestingSector, into = c("nestingSector", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
    filter(year >= year_s, year <= year_e)

  ag_production_world <<- rgcam::getQuery(prj, "ag production by crop type") %>%
    filter(scenario %in% selected_scen, sector %in% food_sector) %>%
    group_by(scenario, sector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  ag_production_regional <<- rgcam::getQuery(prj, "ag production by crop type") %>%
    filter(scenario %in% selected_scen, sector %in% food_sector) %>%
    group_by(region, scenario, sector, year) %>%
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
    filter(year >= year_s, year <= year_e)

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

  ag_meet_dairy_production_world <<- rgcam::getQuery(prj, "meat and dairy production by tech") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(Units, scenario, sector, subsector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  ag_meet_dairy_production_regional <<- rgcam::getQuery(prj, "meat and dairy production by tech") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(Units, scenario, sector, subsector, year, region) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  feed_consumption_world <<- rgcam::getQuery(prj, "feed consumption by region") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(scenario, Units, year, input) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  feed_consumption_regional <<- rgcam::getQuery(prj, "feed consumption by region") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(region, scenario, Units, year, input) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)


  ###### ==== water ====
  water_withdrawals_world <<- rgcam::getQuery(prj, "water withdrawals by region") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(scenario, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  water_withdrawals_regional <<- rgcam::getQuery(prj, "water withdrawals by region") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(region, scenario, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  water_consumption <<- rgcam::getQuery(prj, "water consumption by subsector") %>%
    filter(scenario %in% selected_scen, sector %in% food_sector) %>%
    group_by(Units, scenario, sector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  water_consumption_world <<- rgcam::getQuery(prj, "water consumption by subsector") %>%
    filter(scenario %in% selected_scen, sector %in% food_sector) %>%
    group_by(Units, scenario, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  water_consumption_regional <<- rgcam::getQuery(prj, "water consumption by subsector") %>%
    filter(scenario %in% selected_scen, sector %in% food_sector) %>%
    group_by(Units, scenario, year, region) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  water_consumption_regional_sectorial <<- rgcam::getQuery(prj, "water consumption by subsector") %>%
    filter(scenario %in% selected_scen, sector %in% food_sector) %>%
    group_by(scenario, region, sector, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  water_irr_rfd_world <<- rgcam::getQuery(prj, "land allocation by crop and water source") %>%
    filter(!is.na(water)) %>%
    filter(scenario %in% selected_scen) %>%
    filter(!crop %in% c('biomassGrass','biomassTree')) %>%
    dplyr::mutate(crop = sub("C4$", "", crop)) %>%
    dplyr::mutate(crop = sub("Tree$", "", crop)) %>%
    group_by(Units, scenario, year, water, crop) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  water_irr_rfd_regional <<- rgcam::getQuery(prj, "land allocation by crop and water source") %>%
    filter(!is.na(water)) %>%
    filter(scenario %in% selected_scen) %>%
    filter(!crop %in% c('biomassGrass','biomassTree')) %>%
    dplyr::mutate(crop = sub("C4$", "", crop)) %>%
    dplyr::mutate(crop = sub("Tree$", "", crop)) %>%
    group_by(region, Units, scenario, year, water, crop) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)


  ###### ==== emissions ====
  GWP <<- readr::read_csv(paste0(folder_analysis_path,"data/GWP_AR5.csv"))

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

  nonco2_luc <<- getQuery(prj,"nonCO2 emissions by sector (excluding resource production)") %>%
    filter(year >= year_s, year <= year_e) %>%
    filter(sector %in% food_sector) %>%
    filter(ghg %in% c('CH4_AGR','N2O_AGR')) %>%
    mutate(ghg = ifelse(ghg == 'CH4_AGR','CH4','N2O')) %>%
    mutate(Units = 'Mt') #Tg is equivalent to Mt


  ###### ==== GHG emissions ====
  ghg_by_ghg_world <<- bind_rows(luc,co2_emiss,nonco2) %>%
    left_join(GWP, by = c("Units", "ghg")) %>%
    mutate(value = value * GWP,
           Units = "MtCO2e") %>%
    group_by(group, scenario, year, Units) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    filter(!is.na(group))

  ghg_by_ghg_regional <<- bind_rows(luc,co2_emiss,nonco2) %>%
    left_join(GWP, by = c("Units", "ghg")) %>%
    mutate(value = value * GWP,
           Units = "MtCO2e") %>%
    group_by(region, group, scenario, year, Units) %>%
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
                                                       'Other Natural')))))

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
                                                       'Other Natural')))))

  land_crop_world <<- getQuery(prj,"land allocation by crop") %>%
    dplyr::mutate(landleaf = sub("C4$", "", landleaf)) %>%
    dplyr::mutate(landleaf = sub("Tree$", "", landleaf)) %>%
    group_by(Units, scenario, year, landleaf) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(scenario %in% selected_scen) %>%
    filter(year >= year_s, year <= year_e)

  land_crop_regional <<- getQuery(prj,"land allocation by crop") %>%
    dplyr::mutate(landleaf = sub("C4$", "", landleaf)) %>%
    dplyr::mutate(landleaf = sub("Tree$", "", landleaf)) %>%
    group_by(Units, scenario, region, year, landleaf) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(scenario %in% selected_scen) %>%
    filter(year >= year_s, year <= year_e)


  # carbon_stock_world <<- getQuery(prj,"vegetative carbon stock by region") %>%
  #   filter(scenario %in% selected_scen) %>%
  #   group_by(Units, scenario, year, landleaf) %>%
  #   summarise(value = sum(value)) %>%
  #   ungroup() %>%
  #   filter(year >= year_s, year <= year_e)
  #
  # carbon_stock_regional <<- getQuery(prj,"vegetative carbon stock by region") %>%
  #   filter(scenario %in% selected_scen) %>%
  #   group_by(Units, scenario, year, region, landleaf) %>%
  #   summarise(value = sum(value)) %>%
  #   ungroup() %>%
  #   filter(year >= year_s, year <= year_e)

  fertilizer_consumption_world <<- getQuery(prj,"fertilizer consumption by crop type") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(Units, scenario, year, sector) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)

  fertilizer_consumption_regional <<- getQuery(prj,"fertilizer consumption by crop type") %>%
    filter(scenario %in% selected_scen) %>%
    group_by(Units, scenario, year, region, sector) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(year >= year_s, year <= year_e)


  ###### ===== population ======
  # population by region
  pop_all_regions <<- getQuery(prj, "population by region") %>%
    filter(year >= year_s, year <= year_e) %>%
    mutate(value = value * 1000) %>% # Convert from thous ppl to total ppl
    select(-Units) %>%
    rename(population = value)


}



remove_attributes <- function(x) {
  for (c in colnames(x)) {
    attr(x[[c]], "names") <- NULL
  }
  return(x)
}
