##### Load libraries & set path ------------------------------------------------
setwd('C:/GCAM/GCAM_7.0_Claudia/gcam-core-iamcompact/diets_analysis')
if(!dir.exists('figures')) dir.create('figures')

# .libPaths('C:/Users/claudia.rodes/Documents/R/win-library/4.1-gcamdata_CP/')
library(dplyr)
library(magrittr)


##### Load food consumption data ---------------------------------------------------
outputs_folder <- 'C:/GCAM/GCAM_7.0_Claudia/gcam-core-iamcompact/output/diets_calibration_outputs'

# csv_files <- list.files(outputs_folder, pattern = "^food_SW_", full.names = TRUE)
csv_files <- list.files(outputs_folder, pattern = "^food_SW_ref|food_SW_calib_snr_plus", full.names = TRUE)
# csv_files <- list.files(outputs_folder, full.names = TRUE)

data_list <- lapply(csv_files, function(file) read.csv(file, skip = 1))
food_consumption <- do.call(rbind, data_list)
food_consumption$scenario <- sapply(strsplit(as.character(food_consumption$scenario), ","), function(x) x[1])
food_consumption$subsector.1 <- sapply(strsplit(as.character(food_consumption$subsector.1), ","), function(x) x[1])
food_consumption$subsector.2 <- sapply(strsplit(as.character(food_consumption$subsector.2), ","), function(x) x[1])
row.names(food_consumption) <- NULL


##### Plot food consumption data ---------------------------------------------------
rumin_percentage <- food_consumption %>%
  dplyr::filter(subsector.1 == 'Animal') %>%
  dplyr::mutate(is_rumin = ifelse(subsector.2 == 'Ruminant',TRUE,FALSE)) %>%
  dplyr::select(scenario,region,is_rumin,X1990:X2050,Units) %>%
  # compute the total protein and rumin protein Pcal by region
  dplyr::group_by(scenario, region, is_rumin, Units) %>%
  dplyr::summarise(across(everything(), sum)) %>%
  dplyr::ungroup() %>%
  # compute the ruminant % (ruminant/total ANIMAL protein food consumption)
  dplyr::group_by(scenario, region, Units) %>%
  dplyr::summarise(across(tidyr::starts_with('X'), function(x) return(x[2]/(x[1]+x[2])))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Units = 'Percentage')


rumin_percentage_check <- rumin_percentage %>%
  tidyr::pivot_longer(cols = X1990:X2050, names_to = 'year', values_to = 'value') %>%
  filter(scenario %in% c('snr_plus_70_v38', 'REF_IAMCOMPACT+XIN'),
         year == "X2050") %>%
  tidyr::pivot_wider(names_from = 'scenario', values_from = 'value') %>%
  dplyr::mutate(decrease = `REF_IAMCOMPACT+XIN` - snr_plus_70_v38)
View(rumin_percentage_check)

rumin_percentage_check2 <- rumin_percentage %>%
  tidyr::pivot_longer(cols = X1990:X2050, names_to = 'year', values_to = 'value') %>%
  filter(scenario %in% c('snr_plus_80_v29', 'REF_IAMCOMPACT+XIN'),
         year == "X2050") %>%
  tidyr::pivot_wider(names_from = 'scenario', values_from = 'value') %>%
  dplyr::mutate(decrease = `REF_IAMCOMPACT+XIN` - snr_plus_80_v29)
View(rumin_percentage_check2)



##### Plot food consumption data ---------------------------------------------------
plant_percentage <- food_consumption %>%
  dplyr::filter(subsector == 'Protein') %>%
  dplyr::mutate(is_plant = ifelse(subsector.1 == 'Plant',TRUE,FALSE)) %>%
  dplyr::select(scenario,region,is_plant,X1990:X2050,Units) %>%
  # compute the total and plant Pcal by region
  dplyr::group_by(scenario, region, is_plant, Units) %>%
  dplyr::summarise(across(everything(), sum)) %>%
  dplyr::ungroup() %>%
  # compute the plant % (plant/total food consumption)
  dplyr::group_by(scenario, region, Units) %>%
  dplyr::summarise(across(tidyr::starts_with('X'), function(x) return(x[2]/(x[1]+x[2])))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Units = 'Percentage')


plant_percentage_check <- plant_percentage %>%
  tidyr::pivot_longer(cols = X1990:X2050, names_to = 'year', values_to = 'value') %>%
  filter(scenario %in% c('sppnr_plus_70_v1', 'REF_IAMCOMPACT+XIN'),
         year == "X2050") %>%
  tidyr::pivot_wider(names_from = 'scenario', values_from = 'value') %>%
  mutate(increse = sppnr_plus_70_v1 - `REF_IAMCOMPACT+XIN` )
View(plant_percentage_check)


