##### Load libraries & set path ------------------------------------------------
setwd('C:/GCAM/GCAM_7.0_Claudia/gcam-core-iamcompact/diets_analysis')
if(!dir.exists('figures')) dir.create('figures')

.libPaths('C:/Users/claudia.rodes/Documents/R/win-library/4.1-gcamdata_CP/')
library(dplyr)
library(magrittr)


##### Load food consumption data ---------------------------------------------------
outputs_folder <- 'C:/GCAM/GCAM_7.0_Claudia/gcam-core-iamcompact/output/diets_calibration_outputs'

csv_files <- list.files(outputs_folder, pattern = "^food_REF|*multip.factor", full.names = TRUE)
# csv_files <- list.files(outputs_folder, pattern = "^food_SW_check_spp|^food_REF", full.names = TRUE)

data_list <- lapply(csv_files, function(file) read.csv(file, skip = 1))
food_consumption <- do.call(rbind, data_list)
food_consumption$scenario <- sapply(strsplit(as.character(food_consumption$scenario), ","), function(x) x[1])
food_consumption$subsector.1 <- sapply(strsplit(as.character(food_consumption$subsector.1), ","), function(x) x[1])
food_consumption$subsector.2 <- sapply(strsplit(as.character(food_consumption$subsector.2), ","), function(x) x[1])
row.names(food_consumption) <- NULL


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
  tidyr::pivot_longer(cols = X1990:X2050, names_to = 'year', values_to = 'value')

View(plant_percentage_check)





# function to check the food consumption of snr to see if it meets the designed
# scenario
check_food_consumption_snr = function() {
  data = bind_rows(read.csv("C:\\GCAM\\GCAM_7.0_Claudia\\gcam-core-iamcompact\\exe\\DB\\FoodConsSpecific_SNR_IAMCOMPACT_reference.csv", skip = 1),
                   read.csv("C:\\GCAM\\GCAM_7.0_Claudia\\gcam-core-iamcompact\\exe\\DB\\FoodConsSpecific_SNR_IAMCOMPACT_all50_2035_0.805.csv", skip = 1)) %>%
    rename_scen() %>%
    select(-X) %>%
    clean_csv_food()
  selected_scen = unique(data$scenario)

  share_snr_data = data %>%
    # subset protein
    dplyr::filter(nestingSector1 == 'Protein', nestingSector2 != 'noR') %>%
    select(Units, scenario, scen_type, nestingSector3, year, value) %>% unique() %>%
    # sum consumption by animal vs noR protein
    group_by(Units, scenario, scen_type, nestingSector3, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    # compute noR_share
    group_by(Units, scenario, year, scen_type) %>%
    summarise(noR_share = 100 * sum(value[nestingSector3 != "Rumiant"]) / sum(value)) %>%
    ungroup() %>%
    filter(scenario %in% selected_scen) %>% rename_scen()

  pl_protein_share_world = ggplot(data = share_snr_data) +
    geom_line(aes(x = year, y = noR_share, group = scenario, color = scen_type), alpha = 1, linewidth = 2) +
    # scale
    # scale_color_manual(values = scen_palette_refVsAllSnr, name = 'Scenario') +
    # labs
    labs(y = 'share of noR protein (%)', x = '') +
    ggtitle('World noR share consumption') +
    # theme
    theme_light() +
    theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
          strip.background = element_blank(),
          strip.text = element_text(color = 'black', size = 40),
          axis.text.x = element_text(size=30),
          axis.text.y = element_text(size=30),
          legend.text = element_text(size = 35),
          legend.title = element_text(size = 40),
          title = element_text(size = 40))
  ggsave(pl_protein_share_world, file = paste0(figures_path,dir_name,"/",'pl_protein_noR_share_world.pdf'),
         # width = 2000, height = 1000, units = 'mm', limitsize = F)
         width = 500, height = 500, units = 'mm')


  # share noRumiant consumption regional
  share_snr_data = data %>%
    # subset protein
    dplyr::filter(nestingSector1 == 'Protein', nestingSector2 != 'noR') %>%
    select(Units, region, scenario, scen_type, nestingSector3, year, value) %>% unique() %>%
    # sum consumption by animal vs noR protein
    group_by(Units, region, scenario, scen_type, nestingSector3, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    # compute noR_share
    group_by(Units, region, scenario, year, scen_type) %>%
    summarise(noR_share = 100 * sum(value[nestingSector3 != "Rumiant"]) / sum(value)) %>%
    ungroup() %>%
    filter(scenario %in% selected_scen) %>% rename_scen()

  pl_protein_share_regional = ggplot(data = share_snr_data) +
    geom_line(aes(x = year, y = noR_share, group = scenario, color = scen_type), alpha = 1, linewidth = 2) +
    # facet
    facet_wrap(. ~ region, scales = 'free') +
    # scale
    # scale_color_manual(values = scen_palette_refVsSnr, name = 'Scenario') +
    # labs
    labs(y = 'share of noR protein (%)', x = '') +
    ggtitle('Regional noR share consumption free scales') +
    # theme
    theme_light() +
    theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
          strip.background = element_blank(),
          strip.text = element_text(color = 'black', size = 40),
          axis.text.x = element_text(size=30),
          axis.text.y = element_text(size=30),
          legend.text = element_text(size = 35),
          legend.title = element_text(size = 40),
          title = element_text(size = 40))
  ggsave(pl_protein_share_regional, file = paste0(figures_path,dir_name,"/",'pl_protein_noR_share_reg_freeS.pdf'),
         width = 2000, height = 1000, units = 'mm', limitsize = F)

}
check_food_consumption_snr()


check_food_consumption_ref = function() {
  data = bind_rows(read.csv("C:\\GCAM\\GCAM_7.0_Claudia\\gcam-core-iamcompact\\exe\\DB\\FoodConsSpecific_SNR_IAMCOMPACT_reference.csv", skip = 1),
                   read.csv("C:\\GCAM\\GCAM_7.0_Claudia\\gcam-core-iamcompact\\exe\\DB\\FoodConsSpecific_SPP_IAMCOMPACT_reference - -6.csv", skip = 1)) %>%
    rename_scen() %>%
    select(-X) %>%
    clean_csv_food()
  selected_scen = unique(data$scenario)

  share_protein_data = data %>%
    # subset protein
    dplyr::filter(nestingSector1 == 'Protein') %>%
    select(Units, scenario, scen_type, technology, year, value) %>% unique() %>%
    # sum consumption by animal vs noR protein
    group_by(Units, scenario, scen_type, technology, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(scenario %in% selected_scen) %>% rename_scen()

  share_protein_data_diff = share_protein_data %>%
    select(-scen_type) %>%
    filter(year == 2050) %>%
    tidyr::pivot_wider(names_from = scenario, values_from = value) %>%
    mutate(diff = SNR_IAMCOMPACT_reference - SPP_IAMCOMPACT_reference )

  pl_protein_share_world = ggplot(data = share_protein_data) +
    geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 1, linewidth = 2) +
    facet_wrap(. ~ technology) +
    # scale
    # scale_color_manual(values = scen_palette_refVsAllSnr, name = 'Scenario') +
    # labs
    labs(y = 'consumption (Pcal)', x = '') +
    ggtitle('World protein sources consumption') +
    # theme
    theme_light() +
    theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
          strip.background = element_blank(),
          strip.text = element_text(color = 'black', size = 40),
          axis.text.x = element_text(size=30),
          axis.text.y = element_text(size=30),
          legend.text = element_text(size = 35),
          legend.title = element_text(size = 40),
          title = element_text(size = 40))
  ggsave(pl_protein_share_world, file = paste0(figures_path,dir_name,"/",'pl_protein_sources_ref_world.pdf'),
         # width = 2000, height = 1000, units = 'mm', limitsize = F)
         width = 500, height = 500, units = 'mm')

  share_protein_data = data %>%
    # subset protein
    dplyr::filter(nestingSector1 == 'Protein') %>%
    select(Units, scenario, scen_type, technology, year, value, region) %>% unique() %>%
    # sum consumption by animal vs noR protein
    group_by(Units, scenario, scen_type, technology, year, region) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(scenario %in% selected_scen) %>% rename_scen()

  pl_protein_share_world = ggplot(data = share_protein_data) +
    geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 1, linewidth = 2) +
    facet_grid(region ~ technology) +
    # scale
    # scale_color_manual(values = scen_palette_refVsAllSnr, name = 'Scenario') +
    # labs
    labs(y = 'consumption (Pcal)', x = '') +
    ggtitle('World protein sources consumption') +
    # theme
    theme_light() +
    theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
          strip.background = element_blank(),
          strip.text = element_text(color = 'black', size = 40),
          axis.text.x = element_text(size=30),
          axis.text.y = element_text(size=30),
          legend.text = element_text(size = 35),
          legend.title = element_text(size = 40),
          title = element_text(size = 40))
  ggsave(pl_protein_share_world, file = paste0(figures_path,dir_name,"/",'pl_protein_sources_ref_reg.pdf'),
         width = 2000, height = 2000, units = 'mm', limitsize = F)



}
check_food_consumption_ref()

fuelPrefElast = rbind(
  get(load(paste0('../outputs/L203.FuelPrefElast_snr.RData')))$snr_all50_2035_0.805 %>%
    mutate(scenario = 'SNR_IAMCOMPACT_all50_2035_0.805'),
  get(load(paste0('../outputs/L203.FuelPrefElast_snr.RData')))$snr_all50_2035_0.805 %>%
    mutate(scenario = 'SNR_IAMCOMPACT_reference') %>%
    mutate(fuelprefElasticity = 0)
)

share_snr_data_reg = data %>%
  # subset protein
  dplyr::filter(nestingSector1 == 'Protein', nestingSector2 != 'noR') %>%
  select(Units, region, scenario, nestingSector3, year, value) %>% unique() %>%
  # sum consumption by animal vs noR protein
  group_by(Units, region, scenario, nestingSector3, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  # compute noR_share
  group_by(Units, region, scenario, year) %>%
  summarise(noR_share = 100 * sum(value[nestingSector3 != "Rumiant"]) / sum(value)) %>%
  ungroup() %>%
  filter(scenario %in% selected_scen) %>% rename_scen() %>%
  # compute factor
  left_join(fuelPrefElast %>%
              select(region, fuelprefElasticity, scenario, year = year.fillout) %>%
              mutate(year = as.character(year)),
            by = c('scenario','region', 'year')) %>%
  filter(year >= 2015) %>%
  group_by(year, region) %>%
  mutate(factor = fuelprefElasticity[which(scenario == 'SNR_IAMCOMPACT_all50_2035_0.805')] /
           (noR_share[which(scenario == 'SNR_IAMCOMPACT_all50_2035_0.805')] - noR_share[which(scenario == 'SNR_IAMCOMPACT_reference')])) %>%
  mutate(gap = fuelprefElasticity / SNR_CONVERSION_FACTOR)

