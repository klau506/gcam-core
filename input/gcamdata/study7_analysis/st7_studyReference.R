#### PREPROCESS ================================================================
# ==============================================================================

# setwd to file location === #####
setwd('C:\\GCAM\\GCAM_7.0_Claudia\\gcam-core-iamcompact\\input\\gcamdata\\study7_analysis')
# .libPaths('C:\\Users\\claudia.rodes\\Documents\\R\\win-library\\4.1 - gcamdata_CP/')

# load libraries and paths' variables, and extra functions and styles
source('load_libs_paths.R')
source('utils_data.R')
source('utils_style.R')

# load basic data
load_mapping_data()

# load project
prj_ref = load_prj('st7_Reference_Calibrate.dat',list_scen_reference_calibrate,
                   onlyFoodConsumption = TRUE)

# load queries
year_s = 2000
year_e = 2050
selected_scen = list_scen_reference_calibrate

load_queries(onlyFoodConsumption = TRUE)
rm(prj)
gc()

#### SNR =======================================================================
# ==============================================================================

# load calibration project
assign('dt', get(load(file.path(outputs_path,'prj_calibration_queries_all.RData'))))

# select year and scenario palette
selected_year = 2030
scen_palette = scen_palette_calibrateSppFuelPrefElast

# create figures' subdirectory
dir_name = 'st7_Calibrate'
if (!dir.exists(paste0(figures_path,dir_name))) dir.create(paste0(figures_path,dir_name))
if (!dir.exists(paste0(outputs_path,dir_name))) dir.create(paste0(outputs_path,dir_name))
if (!dir.exists(paste0(outputs_path,dir_name,'/snr'))) dir.create(paste0(outputs_path,dir_name,'/snr'))

# share noRumiant consumption world
share_snr_data = food_consumption_world %>%
  # subset protein
  dplyr::filter(nestingSector1 == 'Protein', nestingSector2 != 'noR') %>%
  select(Units, scenario, nestingSector3, year, value) %>% unique() %>%
  # sum consumption by animal vs noR protein
  group_by(Units, scenario, nestingSector3, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  # compute noR_share
  group_by(Units, scenario, year) %>%
  summarise(noR_share = 100 * sum(value[nestingSector3 != "Rumiant"]) / sum(value)) %>%
  ungroup() %>%
  filter(scenario %in% selected_scen) %>% rename_scen()

pl_protein_share_world = ggplot(data = share_snr_data) +
  geom_line(aes(x = year, y = noR_share, color = scenario), alpha = 1, linewidth = 2) +
  # scale
  # scale_color_manual(values = scen_palette, name = 'Scenario') +
  # scale_fill_manual(values = scen_palette, name = 'Scenario') +
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
       width = 500, height = 500, units = 'mm')

print(summary(share_snr_data %>%
                tidyr::pivot_wider(names_from = scenario, values_from = noR_share) %>%
                mutate(diff_between_refs = `REF_IAMCOMPACT+XIN` - `SNR_IAMCOMPACT+XIN_all20_2035_0405`) %>%
                pull(diff_between_refs)))
###    Min.   1st Qu.   Median    Mean    3rd Qu.    Max.
### -2.13920 -1.86612 -1.13435 -1.04331 -0.08818  0.00000



# regional
share_snr_data = food_consumption_regional %>%
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
  filter(scenario %in% selected_scen) %>% rename_scen()

pl_protein_share_reg = ggplot(data = share_snr_data) +
  geom_line(aes(x = year, y = noR_share, color = scenario), alpha = 1, linewidth = 2) +
  # scale
  # scale_color_manual(values = scen_palette, name = 'Scenario') +
  # scale_fill_manual(values = scen_palette, name = 'Scenario') +
  facet_wrap(. ~ region, scales = 'free') +
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
ggsave(pl_protein_share_reg, file = paste0(figures_path,dir_name,"/",'pl_protein_noR_share_reg_freeS.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = F)



#### STUDY CURRENT TRENDS AND %SHARES TO CALIBRATE THE SCENARIOS ===============
# ==============================================================================

# share noR consumption world
share_snr_data_reg = food_consumption_regional %>%
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
  mutate(factor = 0.45/noR_share)

tmp_share_snr_data_reg = share_snr_data_reg %>% filter(scenario == 'REF_IAMCOMPACT+XIN' & year == 2015)
write.csv(tmp_share_snr_data_reg, file = paste0(outputs_path, dir_name, '/snr/st7_Reference_Calibration_reference_values_noR_share_2015.csv'))
write.csv(tmp_share_snr_data_reg %>%
            select(region,noR_share),
          row.names = F,
          file = paste0(outputs_path, dir_name, '/snr/st7_Reference_Calibration_reference_values_noR_share_2015_clean.csv'))

pl_protein_share_regional = ggplot(data = share_snr_data_reg %>%
                                     filter(year == 2015)) +
  geom_bar(stat = 'identity', aes(x = reorder(region, -noR_share), y = noR_share, fill = scenario), alpha = 1) +
  # scale
  scale_color_manual(values = scen_palette, name = 'Scenario') +
  scale_fill_manual(values = scen_palette, name = 'Scenario') +
  # labs
  labs(y = 'share of noR protein (%)', x = '') +
  ggtitle('Regional noR share consumption in 2015') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        axis.text.x = element_text(size=30, angle = 90, hjust = 1, vjust = 0.25),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_protein_share_regional, file = paste0(figures_path,dir_name,"/",'pl_protein_noR_share_bars_regional.pdf'),
       width = 500, height = 500, units = 'mm')


#### CALIBRATE SPP AND FUELPREFELAST PARAMETER =================================
# ==============================================================================

## calibrate relation between spp and fuelPrefElast
share_noR_data = food_consumption_world %>%
  # subset protein
  dplyr::filter(nestingSector1 == 'Protein', nestingSector2 != 'noR') %>%
  select(Units, scenario, nestingSector3, year, value) %>% unique() %>%
  # sum consumption by animal vs noR protein
  group_by(Units, scenario, nestingSector3, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  # compute noR_share
  group_by(Units, scenario, year) %>%
  summarise(noR_share = 100 * sum(value[nestingSector3 != "Rumiant"]) / sum(value)) %>%
  ungroup() %>%
  filter(scenario %in% selected_scen) %>% rename_scen()
write.csv(share_noR_data, file = paste0(outputs_path, dir_name, '/snr/st7_calibrate_snr_fuelPrefElast.csv'))
conversion_factor = 0.45/share_noR_data[share_noR_data$year == '2050' & share_noR_data$scenario == 'RvsM_Rreduced_0.45',]$noR_share
print(conversion_factor)
### conversion factor: fuelPrefElast = snr_f * 0.005805109



#### STUDY 2015 MICRO & MACRO NUTRIENTS INTAKE =================================
# ==============================================================================





#### FIGURES ===================================================================
# ==============================================================================

# select year and scenario palette
selected_year = 2030
scen_palette = scen_palette_calibrateSppFuelPrefElast

# create figures' subdirectory
dir_name = 'st7_Reference_Calibrate'
if (!dir.exists(paste0(figures_path,dir_name))) dir.create(paste0(figures_path,dir_name))
if (!dir.exists(paste0(outputs_path,dir_name))) dir.create(paste0(outputs_path,dir_name))
if (!dir.exists(paste0(outputs_path,dir_name,'/spp'))) dir.create(paste0(outputs_path,dir_name,'/spp'))

# share plant consumption world
share_spp_data = food_consumption_world %>%
  # subset protein
  dplyr::filter(nestingSector1 == 'Protein') %>%
  select(Units, scenario, nestingSector2, year, value) %>% unique() %>%
  # sum consumption by animal vs plant protein
  group_by(Units, scenario, nestingSector2, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  # compute plant_share
  group_by(Units, scenario, year) %>%
  summarise(plant_share = 100 * sum(value[nestingSector2 == "Plant"]) / sum(value)) %>%
  ungroup() %>%
  filter(scenario %in% selected_scen) %>% rename_scen()

pl_protein_share_world = ggplot(data = share_spp_data) +
  geom_line(aes(x = year, y = plant_share, color = scenario), alpha = 1, linewidth = 2) +
  # scale
  # scale_color_manual(values = scen_palette, name = 'Scenario') +
  # scale_fill_manual(values = scen_palette, name = 'Scenario') +
  # labs
  labs(y = 'share of plant protein (%)', x = '') +
  ggtitle('World plant share consumption') +
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
ggsave(pl_protein_share_world, file = paste0(figures_path,dir_name,"/",'pl_protein_plant_share_world.pdf'),
       width = 500, height = 500, units = 'mm')

print(summary(share_spp_data %>%
                tidyr::pivot_wider(names_from = scenario, values_from = plant_share) %>%
                mutate(diff_between_refs = `REF_IAMCOMPACT+XIN` - `SPP_IAMCOMPACT+XIN_all60_2035_0405_prova_0005`) %>%
                pull(diff_between_refs)))
###     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
### -37.801 -34.769 -30.678 -23.125  -6.696   0.000


# share plant consumption world
share_spp_data = food_consumption_regional %>%
  # filter(region == 'Africa_Eastern') %>%
  # subset protein
  dplyr::filter(nestingSector1 == 'Protein') %>%
  select(Units, scenario, nestingSector2, year, value, region) %>% unique() %>%
  # sum consumption by animal vs plant protein
  group_by(Units, scenario, nestingSector2, year, region) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  # compute plant_share
  group_by(Units, scenario, year, region) %>%
  summarise(plant_share = 100 * sum(value[nestingSector2 == "Plant"]) / sum(value)) %>%
  ungroup() %>%
  filter(scenario %in% selected_scen) %>% rename_scen()

pl_protein_share_regional = ggplot(data = share_spp_data) +
  geom_line(aes(x = year, y = plant_share, color = scenario), alpha = 1, linewidth = 2) +
  facet_wrap(. ~ region, scales = 'free') +
  # scale
  # scale_color_manual(values = scen_palette, name = 'Scenario') +
  # scale_fill_manual(values = scen_palette, name = 'Scenario') +
  # labs
  labs(y = 'share of plant protein (%)', x = '') +
  ggtitle('Regional plant share consumption') +
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
ggsave(pl_protein_share_regional, file = paste0(figures_path,dir_name,"/",'pl_protein_plant_share_regional.pdf'),
       width = 1000, height = 1000, units = 'mm')



#### STUDY CURRENT TRENDS AND %SHARES TO CALIBRATE THE SCENARIOS ===============
# ==============================================================================

# share plant consumption world
share_spp_data_reg = food_consumption_regional %>%
  # subset protein
  dplyr::filter(nestingSector1 == 'Protein', scenario == 'REF_IAMCOMPACT+XIN') %>%
  select(Units, region, scenario, nestingSector2, year, value) %>% unique() %>%
  # sum consumption by animal vs noR protein
  group_by(Units, region, scenario, nestingSector2, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  # compute plant_share
  group_by(Units, region, scenario, year) %>%
  summarise(plant_share = 100 * sum(value[nestingSector2 == "Plant"]) / sum(value)) %>%
  ungroup() %>%
  filter(scenario %in% selected_scen) %>% rename_scen()

write.csv(share_spp_data_reg %>%
            filter(year == 2015, scenario == 'REF_IAMCOMPACT+XIN'),
          file = paste0(outputs_path, dir_name, '/spp/st7_Reference_Calibration_reference_values_plant_share_2015.csv'))
write.csv(share_spp_data_reg %>%
            filter(year %in% c(2015, 2050), scenario == 'REF_IAMCOMPACT+XIN') %>%
            select(region,year,plant_share) %>%
            tidyr::pivot_wider(names_from = 'year', values_from = 'plant_share') %>%
            rename('spp_i' = '2015', 'spp_f' = '2050'),
          row.names = F,
          file = paste0(outputs_path, dir_name, '/spp/st7_Reference_Calibration_reference_values_plant_share_2015_2050_clean.csv'))


pl_protein_share_regional = ggplot(data = share_spp_data_reg %>%
                                     filter(year == 2015)) +
  geom_bar(stat = 'identity', aes(x = reorder(region, -plant_share), y = plant_share, fill = scenario), alpha = 1) +
  # scale
  # scale_color_manual(values = scen_palette, name = 'Scenario') +
  # scale_fill_manual(values = scen_palette, name = 'Scenario') +
  # labs
  labs(y = 'share of plant protein (%)', x = '') +
  ggtitle('Regional plant share consumption in 2015') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        axis.text.x = element_text(size=30, angle = 90, hjust = 1, vjust = 0.25),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_protein_share_regional, file = paste0(figures_path,dir_name,"/",'pl_protein_plant_share_bars_regional_2015.pdf'),
       width = 500, height = 500, units = 'mm')
pl_protein_share_regional = ggplot(data = share_spp_data_reg %>%
                                     filter(year == 2050)) +
  geom_bar(stat = 'identity', aes(x = reorder(region, -plant_share), y = plant_share, fill = scenario), alpha = 1) +
  # scale
  # scale_color_manual(values = scen_palette, name = 'Scenario') +
  # scale_fill_manual(values = scen_palette, name = 'Scenario') +
  # labs
  labs(y = 'share of plant protein (%)', x = '') +
  ggtitle('Regional plant share consumption in 2050') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        axis.text.x = element_text(size=30, angle = 90, hjust = 1, vjust = 0.25),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_protein_share_regional, file = paste0(figures_path,dir_name,"/",'pl_protein_plant_share_bars_regional_2050.pdf'),
       width = 500, height = 500, units = 'mm')


#### CALIBRATE SPP AND FUELPREFELAST PARAMETER =================================
# ==============================================================================

## calibrate relation between spp and fuelPrefElast
share_spp_data = food_consumption_regional %>%
  filter(region == 'USA') %>%
  # subset protein
  dplyr::filter(nestingSector1 == 'Protein') %>%
  select(Units, scenario, region, nestingSector2, year, value) %>% unique() %>%
  # sum consumption by animal vs noR protein
  group_by(Units, scenario, nestingSector2, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  # compute plant_share
  group_by(Units, scenario, year) %>%
  summarise(plant_share = 100 * sum(value[nestingSector2 == "Plant"]) / sum(value)) %>%
  ungroup() %>%
  filter(scenario %in% selected_scen) %>% rename_scen()
write.csv(share_spp_data, file = paste0(outputs_path, dir_name, '/spp/st7_calibrate_spp_fuelPrefElast.csv'))
conversion_factor = 0.45/share_spp_data[share_spp_data$year == '2050' & share_spp_data$scenario == 'SPP_fuelPrefElast_0.45',]$plant_share
print(conversion_factor)
### conversion factor: fuelPrefElast = spp_f * 0.005805109



#### STUDY 2015 MICRO & MACRO NUTRIENTS INTAKE =================================
# ==============================================================================

