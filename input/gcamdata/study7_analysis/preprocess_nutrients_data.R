# Code in this script can be run based on the inputs in the
# module_aglu_L100.FAO_SUA_connection

devtools::load_all()
inputs_of("module_aglu_L100.FAO_SUA_connection") %>% load_from_cache() -> all_data

# We have protein and fat data here but not used in GCAM (dropped here)
DF_Macronutrient_FoodItem2 %>%
  tidyr::gather(macronutrient, value, calperg:proteinperc) %>%
  # Join max regional conversion for adjustments later
  left_join(
    FAO_Food_MacronutrientRate_2010_2019_MaxValue,
    by = c("GCAM_commodity", "macronutrient")
  ) %>%
  # In rare cases, primary equivalent resulted in lower food mass consumption
  # mainly due to a higher-than-one-extraction rate, e.g., beer of barley
  # or small discrepancies (or possibly representing real processing)
  # thus, the cal per g conversion is larger than the max of the conversion
  # of the corresponding SUA items
  # I.e., a few OtherGrain cases (e.g., Indonesia) and a Mexico soybean case;
  # they all have relatively small consumption/impacts
  # But we use the max of the conversion of the corresponding SUA items to limit the value here
  # mainly for avoiding too different macronutrient rates across regions
  mutate(value = pmin(value, max_macronutrient_value)) %>%
  select(-max_macronutrient_value) %>%
  # There are sill NA values e.g., palm oil is not consumed in Canada
  # And fiber crop is not consumed in few regions
  # Fill in NA with world mean
  dplyr::group_by_at(vars(-GCAM_region_ID, -value)) %>%
  mutate(value = if_else(is.na(value), mean(value, na.rm = T), value)) %>%
  ungroup() %>%
  #filter(macronutrient == "calperg") %>%
  spread(macronutrient, value) ->
  DF_Macronutrient_FoodItem3_calperg



DF_Macronutrient_FoodItem4 <-
  L100.FAO_SUA_APE_balance %>% filter(element == "Food") %>%
  filter(year %in% 2015) %>%
  rename(Mt = value) %>%
  left_join_error_no_match(DF_Macronutrient_FoodItem3_calperg,
                           by = c("GCAM_commodity", "GCAM_region_ID")) %>%
  mutate(Kcalperg = calperg / 1000,
         MKcal =  Kcalperg * Mt * 1000) %>%
  select(-calperg) %>%
  mutate(gProteinPerKcal = proteinperc / Kcalperg,
         gFatPerKcal = fatperc / Kcalperg)


DF_Macronutrient_FoodItem4 %>%
  readr::write_csv(paste0(folder_analysis_path,"data/GramProteinFatPerKcal.csv"))





## =========== Micronutrients =================

raw_data <- read.csv(paste0(folder_analysis_path,"data/USDA data final.csv"))
rni_data <- read.csv(paste0(folder_analysis_path,"data/rni.csv"))

# Clean up column names
colnames(raw_data) <- c("Food", "GCAM_commodity", "Calories (kcal)", "Protein (g)",
                        "Carbohydrate (g)", "Sugars (g)", "Fiber (g)", "Total fat (g)",
                        "Fatty acids saturated (g)", "Fatty acids monounsaturated (g)",
                        "Fatty acids polyunsaturated (g)", "Cholesterol (mg)",
                        "Retinol (mcg)", "Vitamin A (mcg)", "Alpha carotene (mcg)",
                        "Beta carotene (mcg)", "Cryptoxanthin, beta (mcg)",
                        "Lycopene (mcg)", "Lutein and zeaxanthin (mcg)", "Thiamin (mg)",
                        "Riboflavin (mg)", "Niacin (mg)", "Vitamin B6 (mg)",
                        "Folic acid (mcg)", "Folate (mcg)", "Folate DFE (mcg)", # "Folate food (mcg)" = Folate
                        "Folate total (mcg)", "Choline (mg)", "Vitamin B12 (mcg)",
                        "Added vitamin B12 (mcg)", "Vitamin C (mg)",
                        "Vitamin D (mcg)", "Vitamin E alpha-tocopherol (mg)", # vitamin d2 and d3 = vitamin d
                        "Added vitamin E (mg)", "Vitamin K (mcg)", "Calcium (mg)", # Vitamin K phylloquinone = Vitamin K
                        "Phosphorus (mg)", "Magnesium (mg)", "Iron (mg)", "Zinc (mg)",
                        "Copper (mg)", "Selenium (mcg)", "Potassium (mg)", "Sodium (mg)",
                        "Caffeine (mg)", "Theobromine (mg)", "Alcohol (g)", "4:0 (g)",
                        "6:0 (g)", "8:0 (g)", "10:0 (g)", "12:0 (g)", "14:0 (g)",
                        "16:0 (g)", "18:0 (g)", "16:1 (g)", "18:1 (g)", "20:1 (g)",
                        "22:1 (g)", "18:2 (g)", "18:3 (g)", "18:4 (g)", "20:4 (g)",
                        "20:5 n3 (g)", "22:5 n3 (g)", "22:6 n3 (g)", "Water (g)")

# Average over individual food items to get a representative value for each commodity
average_data <- raw_data %>%
  filter(`GCAM_commodity` != "") %>%
  pivot_longer(cols = 3:67, names_to = "Nutrient") %>%
  group_by(`GCAM_commodity`, Nutrient) %>%
  summarize(average = median(value)) %>%
  filter_all(all_vars(!stringr::str_detect(., ":"))) %>%
  pivot_wider(names_from = Nutrient, values_from = average) %>%
  pivot_longer(-c('GCAM_commodity','Calories (kcal)'), names_to = 'nutrient', values_to = 'nutrient_value') %>%
  mutate(nutrient_value = nutrient_value/`Calories (kcal)`) %>%
  mutate(
    nutrient_name = stringr::str_split(nutrient, " \\(") %>%
      sapply(function(x) x[1]),
    nutrient_units = stringr::str_split(nutrient, " \\(") %>%
      sapply(function(x) sub("\\)$", "", x[2])),
    nutrient_units = paste0(nutrient_units,'/kcal')
  ) %>%
  select(-c(`Calories (kcal)`,nutrient))


# total micronutrients consumption
micronutrients_consumption = left_join(food_consumption_regional %>%
                                         # TODO: find data of nutritional values of FiberCrop (introduce it in the average_data)
                                         filter(technology != 'FiberCrop') %>%
                                         left_join(pop_all_regions, by = c("year", "scenario", "region")) %>%
                                         # convert from Pcal to kcal/day
                                         mutate(value = (value * 1e12) / (population * 365),
                                                Units = "kcal/capita/day") %>%
                                         # rename columns
                                         rename('GCAM_commodity' = 'technology',
                                                'consumption' = 'value'),
                                       average_data,
                                       by = 'GCAM_commodity') %>%
  mutate('total_micronutrient_intake' = consumption * nutrient_value) %>%
  group_by(region,scenario,year,nutrient_name,nutrient_units) %>%
  summarise(total_micronutrient_intake = sum(total_micronutrient_intake, na.rm = TRUE)) %>%
  mutate(nutrient_units = stringr::str_replace(nutrient_units, "/kcal", "/capita/day")) %>%
  mutate(year = as.numeric(as.character(year)))

# RNI
micronutrients_RNI = merge(rni_data %>%
                             rename('nutrient_name' = 'micronutrient',
                                    'units_rni' = 'Units') %>%
                             mutate(nutrient_name = tolower(nutrient_name)) %>%
                             select(-data_source),
                           weighted_pop_sex_age,
                           by = 'variable') %>%
  mutate(bySocioGroup_rni = as.numeric(mean_requirement * pop_sex_age)) %>%
  group_by(nutrient_name,units_rni,year,region) %>%
  summarise(byReg_rni = sum(bySocioGroup_rni),
            pop = sum(pop_sex_age),
            byRegC_rni = byReg_rni/pop) %>%
  mutate(units_rni = stringr::str_replace(units_rni, "/day", "/capita/day")) %>%
  mutate(year = as.numeric(as.character(year)))



micronutrients = merge(micronutrients_RNI %>%
                         mutate(nutrient_name = tolower(nutrient_name)),
                       micronutrients_consumption %>%
                         mutate(nutrient_name = tolower(nutrient_name)),
                       by = c('region','year','nutrient_name'))

write.csv(micronutrients, file = paste0(folder_analysis_path,'data/micronutrients_data.csv'))
