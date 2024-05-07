

for (i in 2:length(queries_all_list)) {
print(queries_all_list[i])
queries_all <- get(load(file.path('diets_analysis/outputs',queries_all_list[i])))
food_consumption_world <- queries_all$food_consumption_world
colnames(food_consumption_world) <- c('Units', 'scenario', 'region',
                                      'subsector...4', 'subsector...5', 'subsector...6',
                                      'technology', 'year', 'value')
food_consumption_world <- food_consumption_world %>%
  group_by(Units, scenario, `subsector...4`, `subsector...5`, `subsector...6`, technology, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  rename(nestingSector1 = `subsector...4`) %>%
  tidyr::separate(nestingSector1, into = c("nestingSector1", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
  rename(nestingSector2 = `subsector...5`) %>%
  tidyr::separate(nestingSector2, into = c("nestingSector2", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
  rename(nestingSector3 = `subsector...6`) %>%
  tidyr::separate(nestingSector3, into = c("nestingSector3", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
  filter(year >= year_s, year <= year_e) %>%
  update_query(., 'food_consumption_world')
queries_all$food_consumption_world <- food_consumption_world
food_consumption_regional <- queries_all$food_consumption_regional
colnames(food_consumption_regional) <- c('Units', 'scenario', 'region',
                                         'subsector...4', 'subsector...5', 'subsector...6',
                                         'technology', 'year', 'value')
food_consumption_regional <- food_consumption_regional %>%
  group_by(Units, region, scenario, `subsector...4`, `subsector...5`, `subsector...6`, technology, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  rename(nestingSector1 = `subsector...4`) %>%
  tidyr::separate(nestingSector1, into = c("nestingSector1", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
  rename(nestingSector2 = `subsector...5`) %>%
  tidyr::separate(nestingSector2, into = c("nestingSector2", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
  rename(nestingSector3 = `subsector...6`) %>%
  tidyr::separate(nestingSector3, into = c("nestingSector3", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
  filter(year >= year_s, year <= year_e) %>%
  update_query(., 'food_consumption_regional')
queries_all$food_consumption_regional <- food_consumption_regional
rumin_percentage <- queries_all$rumin_percentage
colnames(rumin_percentage) <- c('Units', 'scenario', 'region',
                                'subsector...4', 'subsector...5', 'subsector...6',
                                'technology', 'year', 'value')
rumin_percentage <- rumin_percentage %>%
  group_by(Units, region, scenario, `subsector...4`, `subsector...5`, `subsector...6`, technology, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  rename(nestingSector1 = `subsector...4`) %>%
  tidyr::separate(nestingSector1, into = c("nestingSector1", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
  rename(nestingSector2 = `subsector...5`) %>%
  tidyr::separate(nestingSector2, into = c("nestingSector2", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
  rename(nestingSector3 = `subsector...6`) %>%
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
queries_all$rumin_percentage <- rumin_percentage
plant_percentage <- queries_all$plant_percentage
colnames(plant_percentage) <- c('Units', 'scenario', 'region',
                                'subsector...4', 'subsector...5', 'subsector...6',
                                'technology', 'year', 'value')
plant_percentage <- plant_percentage %>%
  group_by(Units, region, scenario, `subsector...4`, `subsector...5`, `subsector...6`, technology, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  rename(nestingSector1 = `subsector...4`) %>%
  tidyr::separate(nestingSector1, into = c("nestingSector1", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
  rename(nestingSector2 = `subsector...5`) %>%
  tidyr::separate(nestingSector2, into = c("nestingSector2", "rest"), sep = ",", extra = "merge") %>% select(-rest) %>%
  rename(nestingSector3 = `subsector...6`) %>%
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
queries_all$plant_percentage <- plant_percentage
save(queries_all, file = file.path('diets_analysis/outputs', queries_all_list[i]))
rm(queries_all)
gc()
}
