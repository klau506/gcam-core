##### Load libraries & set path ------------------------------------------------
setwd('C:/GCAM/GCAM_7.0_Claudia/gcam-core-iamcompact/diets_analysis')

.libPaths('C:/Users/claudia.rodes/Documents/R/win-library/4.1-gcamdata_no_CP/')
library(dplyr)


##### Load calibration files ---------------------------------------------------
outputs_folder <- 'C:/GCAM/GCAM_7.0_Claudia/gcam-core-iamcompact/output/diets_calibration_outputs'

## 1. food consumption
csv_files <- list.files(outputs_folder, pattern = "^food", full.names = TRUE)

data_list <- lapply(csv_files, function(file) read.csv(file, skip = 1))
food_consumption <- do.call(rbind, data_list)
food_consumption$scenario <- sapply(strsplit(as.character(food_consumption$scenario), ","), function(x) x[1])
food_consumption$subsector.1 <- sapply(strsplit(as.character(food_consumption$subsector.1), ","), function(x) x[1])
food_consumption$subsector.2 <- sapply(strsplit(as.character(food_consumption$subsector.2), ","), function(x) x[1])
row.names(food_consumption) <- NULL

## 2. share weights
csv_files <- list.files(outputs_folder, pattern = "^sw", full.names = TRUE)

data_list <- lapply(csv_files, function(file) read.csv(file, skip = 1))
share_weights <- do.call(rbind, data_list) %>%
  dplyr::select(scenario, region, subsector, X1990:X2050) %>%
  dplyr::filter(subsector == 'Plant')
share_weights$scenario <- sapply(strsplit(as.character(share_weights$scenario), ","), function(x) x[1])
row.names(share_weights) <- NULL

## Create a csv file to modify manually the sw and be able to calibrate
share_weights_csv <- share_weights %>%
  dplyr::filter(scenario == 'REF_IAMCOMPACT+XIN') %>%
  dplyr::select(-scenario) %>%
  dplyr::mutate(supplysector = 'FoodDemand_NonStaples',
                subsector_nest1 = 'Protein',           # Later, manually, we need to change this name to "nesting-subsector"
                subsector_nest2 = 'Plant') %>%         # Later, manually, we need to change this name to "nesting-subsector"
  tidyr::pivot_longer(cols = 'X1990':'X2050', names_to = 'year', values_to = 'share-weight') %>%
  dplyr::mutate(year = gsub('X','',year)) %>%
  dplyr::select(region ,supplysector, subsector_nest1, subsector_nest2, year, `share-weight`) %>%
  # modify the sw: in 2020 x2, in 2025 x3, in 2030 x4...
  dplyr::mutate(year = as.numeric(year)) %>%
  dplyr::mutate(multiplier = ifelse(year > 2015, 1 + (year - 2010)/5 * 0.1, 1)) %>%
  dplyr::mutate(`share-weight` = multiplier * `share-weight`) %>%
  dplyr::select(region, supplysector, subsector_nest1, subsector_nest2, year, `share-weight`)

write.csv(share_weights_csv, 'share_weights_to_xml.csv', row.names = F)

##### Compute the regional conversion factor -----------------------------------
# compute the plant consumption percentage (plant / total food consumption)
plant_percentage <- food_consumption %>%
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

plant_percentage_mod <- plant_percentage %>%
  tidyr::pivot_longer(cols = X1990:X2050, names_to = 'year', values_to = 'plant_percentage') %>%
  dplyr::mutate(year = gsub("X","",year)) %>%
  dplyr::filter(year >= 2020) %>%
  dplyr::group_by(scenario, region, Units) %>%
  dplyr::mutate(plant_per_increase = plant_percentage / lag(plant_percentage)) %>%
  dplyr::ungroup()

# extract the share weights
share_weights_percentage <- share_weights %>%
  tidyr::pivot_longer(cols = 'X1990':'X2050', names_to = 'year', values_to = 'share_weight') %>%
  dplyr::mutate(year = gsub('X','',year)) %>%
  dplyr::filter(year >= 2020) %>%
  dplyr::group_by(scenario, region, subsector) %>%
  dplyr::mutate(share_weight_increase = share_weight / lag(share_weight))

# compute the regional conversion factor
conversion_factor <- merge(plant_percentage_mod %>%
                             dplyr::select(-Units),
                           share_weights_percentage,
                           by = c('scenario', 'region', 'year')) %>%
  dplyr::filter(year > 2020, scenario != 'REF_IAMCOMPACT+XIN') %>%
  dplyr::mutate(conv_factor = plant_per_increase / share_weight_increase) %>%
  dplyr::group_by(scenario, region) %>%
  dplyr::mutate(std_conv_factor = median(conv_factor)) %>%
  dplyr::ungroup() %>%
  dplyr::select(region, conv_factor = std_conv_factor) %>%
  dplyr::distinct() %>%
  tibble::as_tibble() %>%
  add_comments("Source: calibrate_sw_plant.R")

write.csv(conversion_factor, 'conversion_factor.csv', row.names = F)


##### Compute the REF plant protein consumption (plant protein consumption / total protein intake) -----------------------------------

plant_percentage_csv <- food_consumption %>%
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
  dplyr::mutate(Units = 'Percentage') %>%
  dplyr::filter(scenario == 'REF_IAMCOMPACT+XIN')
write.csv(plant_percentage_csv, 'diets_plant_percentage_REF.csv', row.names = F)

share_weights_csv <- share_weights %>%
  dplyr::mutate(Units = 'Unitless') %>%
  dplyr::filter(scenario == 'REF_IAMCOMPACT+XIN')
write.csv(share_weights_csv, 'diets_plant_sw_REF.csv', row.names = F)



