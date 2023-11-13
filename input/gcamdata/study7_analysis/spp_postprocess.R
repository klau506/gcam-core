#### PREPROCESS ================================================================
# ==============================================================================

# setwd to file location === #####
setwd('C:\\GCAM\\GCAM_7.0_Claudia\\gcam-core-spp\\input\\gcamdata\\study7_analysis')
.libPaths('C:\\Users\\claudia.rodes\\Documents\\R\\win-library\\4.1')

# load libraries and paths' variables, and extra functions and styles
source('load_libs_paths.R')
source('utils_data.R')
source('utils_style.R')

# # load projects
# prj_gathered = rgcam::loadProject("spp_gathered.dat")
# desired_scen <<- c('St7_Reference_original', db_scen_mapping %>% filter(grepl("spp", scen_name)) %>% pull(scen_name))


#### SYSTEM-WIDE EFFECTS SECTION ===============================================

#### Data preprocess ===========================================================
# ==============================================================================

load(paste0(outputs_path, 'snr_spp_queries.RData'))
desired_scen = unique(dt$food_demand_world$scenario)
selected_scen = desired_scen

# load basic data
year_s = 2000
year_e = 2050
final_db_year <<- year_e
selected_year = 2030
dir_name = 'SPP'

load_mapping_data()
load_nutritional_data()

# rfasst mortality
# assign('prj_rd', get(load('C:/GCAM/GCAM_7.0_Claudia/gcam-core-spp/input/gcamdata/study7_analysis/outputs/rfasst_queries_all.RData')))
# dt.mort = calc_mort_rfasst(selected_scen)
dt.mort = get(load(paste0(outputs_path, 'mort.data.RData'))) %>%
  mutate(pollutant = if_else(grepl("mort_o3", method), 'O3', "PM25"))

# rfasst crop loss
# assign('prj_rd', get(load('C:/GCAM/GCAM_7.0_Claudia/gcam-core-spp/input/gcamdata/study7_analysis/outputs/rfasst_queries_all.RData')))
# dt.croploss = calc_croploss_rfasst(selected_scen)
dt.croploss = get(load(paste0(outputs_path, 'croploss.data.RData')))


base_scen_spp = 'spp_p20_2025_1d205'
base_scen_snr = 'snr_all50_2035_0d005'
ref_scen_spp = 'St7_Reference_original'
ref_scen_snr = 'St7_Reference_R-M-F'

# load queries
# load(paste0(outputs_path, '/spp_queries.RData'))

# prj = prj_gathered
# load_queries()

## join spp and snr datasets
# spp_data = dt[-42] #spp_data = dt[dt != 'carbon_stock_world']
# assign('snr_data', get(load(paste0('C:/GCAM/GCAM_7.0_Claudia/gcam-core/input/gcamdata/study7_analysis/outputs/snr_queries.RData'))))
# spp_data[['food_consumption_world']] = spp_data[['food_consumption_world']] %>%
#   mutate(nestingSector3 = nestingSector2)
# spp_data[['food_consumption_regional']] = spp_data[['food_consumption_regional']] %>%
#   mutate(nestingSector3 = nestingSector2)
# dt = Map(rbind,
#          c(spp_data),
#          c(snr_data))
# save(dt, file = paste0(outputs_path, 'snr_spp_queries.RData'))


#
# # compute crop_loss due to AP
# crop_loss = load_crop_loss()
# crop_loss = lapply(crop_loss, process_crop_loss)
#
# # clean memory
# rm(prj)
# gc()


#### FIGURES ===================================================================
# ==============================================================================

# select year and scenario palette
selected_year = 2030
scen_palette = scen_palette_refVsAllSpp

# create figures' subdirectory
dir_name = 'SPP'
if (!dir.exists(paste0(figures_path,dir_name))) dir.create(paste0(figures_path,dir_name))
if (!dir.exists(paste0(outputs_path,dir_name))) dir.create(paste0(outputs_path,dir_name))

# share plant consumption world
share_spp_data = dt$food_consumption_world %>%
  filter(!startsWith(scenario, 'snr')) %>%
  filter(scenario != 'St7_Reference_R-M-F') %>%
  # subset protein
  dplyr::filter(nestingSector1 == 'Protein') %>%
  select(Units, scenario, scen_type, nestingSector2, year, value) %>% unique() %>%
  # sum consumption by animal vs plant protein
  group_by(Units, scenario, scen_type, nestingSector2, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  # compute plant_share
  group_by(Units, scenario, scen_type, year) %>%
  summarise(plant_share = 100 * sum(value[nestingSector2 == "Plant"]) / sum(value)) %>%
  ungroup() %>%
  # filter(scenario %in% selected_scen) %>%
  rename_scen()

pl_protein_share_world = ggplot(data = share_spp_data %>%
                                  mutate(scen_type = substr(scen_type, 1, 3)) %>%
                                  group_by(scen_type, year) %>%
                                  mutate(min_value = min(plant_share),
                                         max_value = max(plant_share))) +
  geom_line(aes(x = year, y = plant_share, group = scenario, color = scen_type), alpha = 1, linewidth = 2) +
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
  # scale
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario', labels = scen_palette_refVsSppVsSnr.labs) +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario', labels = scen_palette_refVsSppVsSnr.labs) +
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
        title = element_text(size = 40)) +
  geom_line(data = share_spp_data %>%
              filter(scenario == 'spp_p20_2025_1d205'), #snr_all30_2045_0d005
            aes(year, plant_share), linewidth = 3, color = 'black')
ggsave(pl_protein_share_world, file = paste0(figures_path,dir_name,"/",'pl_protein_plant_share_world_withMeanScen.png'),
       width = 750, height = 450, units = 'mm')
# share noR consumption world
share_snr_data = dt$food_consumption_world %>%
  filter(!startsWith(scenario, 'spp')) %>%
  filter(scenario != 'St7_Reference_original') %>%
  # subset protein
  dplyr::filter(nestingSector1 == 'Protein') %>%
  select(Units, scenario, scen_type, nestingSector3, year, value) %>% unique() %>%
  # sum consumption by animal vs plant protein
  group_by(Units, scenario, scen_type, nestingSector3, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  # compute noR_share
  group_by(Units, scenario, scen_type, year) %>%
  summarise(noR_share = 100 * sum(value[nestingSector3 == "Rumiant"]) / sum(value)) %>%
  ungroup() %>%
  # filter(scenario %in% selected_scen) %>%
  rename_scen()

pl_protein_share_world = ggplot(data = share_snr_data %>%
                                  filter(!startsWith(scen_type, 'snr_p')) %>%
                                  mutate(scen_type = substr(scen_type, 1, 3)) %>%
                                  group_by(scen_type, year) %>%
                                  mutate(min_value = min(noR_share),
                                         max_value = max(noR_share))) +
  geom_line(aes(x = year, y = noR_share, group = scenario, color = scen_type), alpha = 1, linewidth = 2) +
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
  # scale
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario', labels = scen_palette_refVsSppVsSnr.labs) +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario', labels = scen_palette_refVsSppVsSnr.labs) +
  # labs
  labs(y = 'share of noR protein (%)', x = '') +
  ggtitle('World noRumiant share consumption') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  geom_line(data = share_snr_data %>%
              filter(scenario == 'snr_all50_2035_0d005'), #snr_all30_2045_0d005
            aes(year, noR_share), linewidth = 3, color = 'black')
ggsave(pl_protein_share_world, file = paste0(figures_path,dir_name,"/",'pl_protein_noR_share_world_withMeanScen.png'),
       width = 750, height = 450, units = 'mm')


# share plant consumption regional
share_spp_data = dt$food_consumption_regional %>%
  # subset protein
  dplyr::filter(nestingSector1 == 'Protein') %>%
  select(Units, region, scenario, scen_type, nestingSector2, year, value) %>% unique() %>%
  # sum consumption by animal vs plant protein
  group_by(Units, region, scenario, scen_type, nestingSector2, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  # compute plant_share
  group_by(Units, region, scenario, scen_type, year) %>%
  summarise(plant_share = 100 * sum(value[nestingSector2 == "Plant"]) / sum(value)) %>%
  ungroup() %>%
  filter(scenario %in% selected_scen) %>% rename_scen()

pl_protein_share_regional = ggplot(data = share_spp_data) +
  geom_line(aes(x = year, y = plant_share, group = scenario, color = scen_type), alpha = 1, linewidth = 2) +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # scale
  scale_color_manual(values = scen_palette_refVsAllSpp, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsAllSpp, name = 'Scenario') +
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
ggsave(pl_protein_share_regional, file = paste0(figures_path,dir_name,"/",'pl_protein_plant_share_reg_freeS.pdf'),
       width = 2000, height = 1000, units = 'mm', limitsize = F)

#### SNR
# share noRumiant consumption world
share_snr_data = dt$food_consumption_regional %>%
  filter(!startsWith(scenario, 'spp')) %>%
  filter(scenario != ref_scen_spp) %>%
  # subset protein
  dplyr::filter(nestingSector1 == 'Protein', nestingSector2 != 'noR') %>%
  select(Units, scenario, scen_type, nestingSector3, year, value, region) %>% unique() %>%
  # sum consumption by animal vs noR protein
  group_by(Units, scenario, scen_type, nestingSector3, year, region) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  # compute noR_share
  group_by(Units, scenario, year, scen_type, region) %>%
  summarise(noR_share = 100 * sum(value[nestingSector3 != "Rumiant"]) / sum(value)) %>%
  ungroup() %>%
  filter(scenario %in% selected_scen) %>% rename_scen()

pl_protein_share_world = ggplot(data = share_snr_data %>%
                                  mutate(scen_type = substr(scen_type, 1, 3)) %>%
                                  filter(region %in% c('EU-12','China')) %>%
                                  order_facets()) +
  geom_line(aes(x = year, y = noR_share, group = scenario, color = scen_type), alpha = 1, linewidth = 2) +
  facet_wrap(. ~ region) +
  # scale
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
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
        title = element_text(size = 40)) +
  geom_line(data = share_snr_data %>%
              filter(scenario == base_scen_snr) %>%
              mutate(scen_type = substr(scen_type, 1, 3)) %>%
              filter(region %in% c('EU-12','China')) %>%
              order_facets(),
            aes(x = year, y = noR_share, group = scenario), color = 'black', alpha = 1, linewidth = 2)

ggsave(pl_protein_share_world, file = paste0(figures_path,dir_name,"/",'pl_protein_noR_share_EU12_China.pdf'),
       width = 500, height = 500, units = 'mm')
# ggsave(pl_protein_share_world, file = paste0(figures_path,dir_name,"/",'pl_protein_noR_share_regional.pdf'),
       # width = 2000, height = 1000, units = 'mm', limitsize = F)

#### System-wide effects figures ===============================================
# ==============================================================================


#### Fig: food consumption, production & demand ====
# =============================
## WORLD
# R vs M vs Plant vs Fish
# plot 6 elem
plt_food_consumption_world = ggplot(data = dt$food_consumption_world %>%
                                      dplyr::filter(technology %in% c(animal_commodities, plant_prot_commodities)) %>%
                                      filter(!startsWith(scenario, 'spp')) %>% filter(scenario != ref_scen_spp) %>%
                                      group_by(Units, scenario, technology, year) %>% summarise(value = sum(value)) %>% ungroup() %>%
                                      mutate(scen_type = substr(scenario,1,3)) %>%
                                      group_by(technology,year,scen_type) %>%
                                      dplyr::mutate(median_value = median(value)) %>%
                                      dplyr::mutate(min_value = min(value)) %>%
                                      dplyr::mutate(max_value = max(value)) %>%
                                      order_facets()) +
  geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
  facet_wrap(. ~ technology, nrow = 1, scales = 'free') +
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  # labs
  labs(y = 'Pcal', x = '') +
  ggtitle('World food consumption') +
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
ggsave(plt_food_consumption_world, file = paste0(figures_path,dir_name,"/",'pl1_food_consumption_6elem_world.pdf'),
       width = 1000, height = 300, units = 'mm')


# REGIONAL
reg = 'India'
plt_food_consumption_world = ggplot(data = dt$food_consumption_regional %>%
                                      dplyr::filter(technology %in% c(animal_commodities, plant_prot_commodities)) %>%
                                      filter(!startsWith(scenario, 'spp')) %>% filter(scenario != ref_scen_spp) %>%
                                      group_by(Units, scenario, technology, year, region) %>%
                                      summarise(value = sum(value)) %>% ungroup() %>%
                                      mutate(scen_type = substr(scenario,1,3)) %>%
                                      group_by(technology,year,scen_type,region) %>%
                                      dplyr::mutate(median_value = median(value)) %>%
                                      dplyr::mutate(min_value = min(value)) %>%
                                      dplyr::mutate(max_value = max(value)) %>%
                                      order_facets() %>%
                                      filter(region == reg)) +
  geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
  facet_wrap(. ~ technology, nrow = 1) +
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  # labs
  labs(y = 'Pcal', x = '') +
  ggtitle(paste0(reg,' food consumption')) +
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
ggsave(plt_food_consumption_world, file = paste0(figures_path,dir_name,"/",'pl1_food_consumption_6elem_',reg,'.pdf'),
       width = 1000, height = 300, units = 'mm')



## WORLD
# subset relevant items
pltD_food_consumption = food_consumption_world %>%
  dplyr::filter(technology %in% c('Beef','Pork','Poultry','OtherMeat_Fish','Legumes','NutsSeeds')) %>%
  dplyr::mutate(technology = ifelse(technology == 'OtherMeat_Fish', 'Other Meat and Fish',
                                    ifelse(technology == 'NutsSeeds', 'Nuts and Seeds', technology))) %>%
  rename_scen()

pltD_food_consumption$technology = factor(pltD_food_consumption$technology, levels = c('Beef','Pork','Poultry','Other Meat and Fish','Legumes','Nuts and Seeds'))

# plot 6 elem
plt_food_consumption_world = ggplot(data = pltD_food_consumption %>%
                                      group_by(technology,year,scen_type) %>%
                                      dplyr::mutate(median_value = median(value)) %>%
                                      dplyr::mutate(min_value = min(value)) %>%
                                      dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
  facet_wrap(. ~ technology, nrow = 1, scales = 'free') +
  # scale_color_manual(values = mypal_scen, name = 'Scenario') +
  # scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = 'Pcal', x = '') +
  ggtitle('World food consumption') +
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
ggsave(plt_food_consumption_world, file = paste0(figures_path,dir_name,"/",'pl1_food_consumption_6elem_world.pdf'),
       width = 1000, height = 300, units = 'mm')

## REGIONAL
# subset relevant items
pltD_food_consumption = food_consumption_regional %>%
  dplyr::filter(technology %in% c('Beef','Pork','Poultry','OtherMeat_Fish','Legumes','NutsSeeds')) %>%
  dplyr::mutate(technology = ifelse(technology == 'OtherMeat_Fish', 'Other Meat and Fish',
                                    ifelse(technology == 'NutsSeeds', 'Nuts and Seeds', technology))) %>%
  rename_scen()

pltD_food_consumption$technology = factor(pltD_food_consumption$technology, levels = c('Beef','Pork','Poultry','Other Meat and Fish','Legumes','Nuts and Seeds'))

# plot 6 elem
plt_food_consumption_regional = ggplot(data = pltD_food_consumption %>%
                                         group_by(region,technology,year,scen_type) %>%
                                         dplyr::mutate(median_value = median(value)) %>%
                                         dplyr::mutate(min_value = min(value)) %>%
                                         dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
  facet_grid(region ~ technology) +
  # scale_color_manual(values = mypal_scen, name = 'Scenario') +
  # scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = 'Pcal', x = '') +
  ggtitle('Regional food consumption (fixed scales)') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(plt_food_consumption_regional, file = paste0(figures_path,dir_name,"/",'pl1_food_consumption_6elem_regional_fixedScales.pdf'),
       width = 2000, height = 2500, units = 'mm', limitsize = FALSE)

plt_food_consumption_regional = ggplot(data = pltD_food_consumption %>%
                                         group_by(region,technology,year,scen_type) %>%
                                         dplyr::mutate(median_value = median(value)) %>%
                                         dplyr::mutate(min_value = min(value)) %>%
                                         dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
  facet_grid(region ~ technology, scales = 'free') +
  # scale_color_manual(values = mypal_scen, name = 'Scenario') +
  # scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = 'Pcal', x = '') +
  ggtitle('Regional food consumption (free scales)') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(plt_food_consumption_regional, file = paste0(figures_path,dir_name,"/",'pl1_food_consumption_6elem_regional_freeScales.pdf'),
       width = 2000, height = 2500, units = 'mm', limitsize = FALSE)




## WORLD
# plot all food elem
plt_food_consumption_world = ggplot(data = food_consumption_world %>%
                                      rename_scen() %>%
                                      group_by(technology,year,scen_type) %>%
                                      dplyr::mutate(median_value = median(value)) %>%
                                      dplyr::mutate(min_value = min(value)) %>%
                                      dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
  facet_wrap(. ~ technology, scales = 'free') +
  # scale_color_manual(values = mypal_scen, name = 'Scenario') +
  # scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = 'Pcal', x = '') +
  ggtitle('World food consumption') +
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
ggsave(plt_food_consumption_world, file = paste0(figures_path,dir_name,"/",'pl1_food_consumption_allElem_world.pdf'),
       width = 1000, height = 1000, units = 'mm')

## REGIONAL
# plot all food elem
plt_food_consumption_regional = ggplot(data = food_consumption_regional %>%
                                         rename_scen() %>%
                                         group_by(region,technology,year,scen_type) %>%
                                         dplyr::mutate(median_value = median(value)) %>%
                                         dplyr::mutate(min_value = min(value)) %>%
                                         dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
  facet_grid(region ~ technology) +
  # scale_color_manual(values = mypal_scen, name = 'Scenario') +
  # scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = 'Pcal', x = '') +
  ggtitle('Regional food consumption (fixed scales)') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(plt_food_consumption_regional, file = paste0(figures_path,dir_name,"/",'pl1_food_consumption_allElem_regional_fixedScales.pdf'),
       width = 2500, height = 2500, units = 'mm', limitsize = FALSE)

plt_food_consumption_regional = ggplot(data = food_consumption_regional %>%
                                         rename_scen() %>%
                                         group_by(region,technology,year,scen_type) %>%
                                         dplyr::mutate(median_value = median(value)) %>%
                                         dplyr::mutate(min_value = min(value)) %>%
                                         dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
  facet_grid(region ~ technology, scales = 'free') +
  # scale_color_manual(values = mypal_scen, name = 'Scenario') +
  # scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = 'Pcal', x = '') +
  ggtitle('Regional food consumption (free scales)') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(plt_food_consumption_regional, file = paste0(figures_path,dir_name,"/",'pl1_food_consumption_allElem_regional_freeScales.pdf'),
       width = 2500, height = 2500, units = 'mm', limitsize = FALSE)



### WORLD
pl_food_demand_staplesVsNonStapes_world = ggplot(data = food_demand_world %>%
                                                   rename_scen() %>%
                                                   group_by(input,year,scen_type) %>%
                                                   dplyr::mutate(median_value = median(value)) %>%
                                                   dplyr::mutate(min_value = min(value)) %>%
                                                   dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,input,scenario), color = interaction(scen_type,input)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type,input)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type,input)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
  #                    breaks = staples_vs_nonstaples_order_palette) +
  # scale_fill_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
  #                   breaks = staples_vs_nonstaples_order_palette) +
  # labs
  labs(y = 'Pcal', x = '', title = 'Annual World food demand') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 35),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2))
ggsave(pl_food_demand_staplesVsNonStapes_world, file = paste0(figures_path,dir_name,"/",'pl1_food_demand_staplesVsNonStapes_world.pdf'),
       width = 550, height = 500, units = 'mm', limitsize = FALSE)

### REGIONAL
## (free scales)
pl_food_demand_staplesVsNonStapes_regional = ggplot(data = food_demand_regional %>%
                                                      rename_scen() %>%
                                                      group_by(region,input,year,scen_type) %>%
                                                      dplyr::mutate(median_value = median(value)) %>%
                                                      dplyr::mutate(min_value = min(value)) %>%
                                                      dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,input,scenario), color = interaction(scen_type,input)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type,input)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type,input)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
  #                    breaks = staples_vs_nonstaples_order_palette) +
  # scale_fill_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
  #                   breaks = staples_vs_nonstaples_order_palette) +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(y = 'Pcal', x = '', title = 'Annual Regional food demand (free scales)') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 35),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2))
ggsave(pl_food_demand_staplesVsNonStapes_regional, file = paste0(figures_path,dir_name,"/",'pl1_food_demand_staplesVsNonStapes_regional_freeScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)


## (fixed scales)
pl_food_demand_staplesVsNonStapes_regional = ggplot(data = food_demand_regional %>%
                                                      rename_scen() %>%
                                                      group_by(region,input,year,scen_type) %>%
                                                      dplyr::mutate(median_value = median(value)) %>%
                                                      dplyr::mutate(min_value = min(value)) %>%
                                                      dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,input,scenario), color = interaction(scen_type,input)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type,input)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type,input)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
  #                    breaks = staples_vs_nonstaples_order_palette) +
  # scale_fill_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
  #                   breaks = staples_vs_nonstaples_order_palette) +
  # facet
  facet_wrap(. ~ region) +
  # labs
  labs(y = 'Pcal', x = '', title = 'Annual Regional food demand (fixed scales)') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2))
ggsave(pl_food_demand_staplesVsNonStapes_regional, file = paste0(figures_path,dir_name,"/",'pl1_food_demand_staplesVsNonStapes_regional_fixedScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)


### WORLD
pl_ag_production_world = ggplot(data = ag_production_world %>%
                                  rename_scen() %>%
                                  group_by(sector,year,scen_type) %>%
                                  dplyr::mutate(median_value = median(value)) %>%
                                  dplyr::mutate(min_value = min(value)) %>%
                                  dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,sector,scenario), color = interaction(scen_type,sector)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type,sector)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type,sector)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = food_items_scenario_palette, name = 'Scenario',
  #                    breaks = food_items_order_palette) +
  # scale_fill_manual(values = food_items_scenario_palette, name = 'Scenario',
  #                   breaks = food_items_order_palette) +
  # labs
  labs(y = 'Pcal', x = '', title = 'Annual World ag production') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(ncol = 4), color = guide_legend(ncol = 4))
ggsave(pl_ag_production_world, file = paste0(figures_path,dir_name,"/",'pl1_ag_production_world.pdf'),
       width = 750, height = 500, units = 'mm', limitsize = FALSE)

### REGIONAL
## (free scales)
pl_ag_production_regional = ggplot(data = ag_production_regional %>%
                                     rename_scen() %>%
                                     group_by(region,sector,year,scen_type) %>%
                                     dplyr::mutate(median_value = median(value)) %>%
                                     dplyr::mutate(min_value = min(value)) %>%
                                     dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,sector,scenario), color = interaction(scen_type,sector)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type,sector)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type,sector)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = food_items_scenario_palette, name = 'Scenario',
  #                    breaks = food_items_order_palette) +
  # scale_fill_manual(values = food_items_scenario_palette, name = 'Scenario',
  #                   breaks = food_items_order_palette) +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(y = 'Pcal', x = '', title = 'Annual Regional ag production (free scales)') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(ncol = 4), color = guide_legend(ncol = 4))
ggsave(pl_ag_production_regional, file = paste0(figures_path,dir_name,"/",'pl1_ag_production_regional_freeScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)


## (fixed scales)
pl_ag_production_regional = ggplot(data = ag_production_regional %>%
                                     rename_scen() %>%
                                     group_by(region,sector,year,scen_type) %>%
                                     dplyr::mutate(median_value = median(value)) %>%
                                     dplyr::mutate(min_value = min(value)) %>%
                                     dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,sector,scenario), color = interaction(scen_type,sector)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type,sector)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type,sector)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = food_items_scenario_palette, name = 'Scenario',
  #                    breaks = food_items_order_palette) +
  # scale_fill_manual(values = food_items_scenario_palette, name = 'Scenario',
  #                   breaks = food_items_order_palette) +
  # facet
  facet_wrap(. ~ region) +
  # labs
  labs(y = 'Pcal', x = '', title = 'Annual Regional ag production (fixed scales)') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(ncol = 4), color = guide_legend(ncol = 4))
ggsave(pl_ag_production_regional, file = paste0(figures_path,dir_name,"/",'pl1_ag_production_regional_fixedScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)

#####

#### Fig: food price =============================== DONE
# =============================
food_econ_basket_bill_regional = dt$food_consumption_regional %>%
  filter(region != 'South America_Northern') %>%
  left_join(food_subsector %>%
              rename('technology' = 'subsector')) %>%
  # Pcal to kcal/capita/day
  left_join(dt$pop_all_regions, by = c("year", "scenario", "t0", "k", "scen_type", "region")) %>%
  # convert from Pcal to kcal/day
  mutate(value = (value * 1e12) / (population * 365),
         Units = "kcal/capita/day") %>%
  # total staples and nonstaples kcal consumption
  group_by(Units,region,scenario,t0,k,scen_type,year,supplysector) %>%
  summarise(consumption = sum(value)) %>%
  # compute the expenditure by supplysector
  left_join(dt$food_demand_prices_regional %>%
              mutate(price = value * 1e3,
                     units_price = '2005$/kcal/day') %>%
              select(-c(Units,value)) %>%
              rename('supplysector' = 'input'), by = c('region','year','supplysector','scenario', "t0", "k", 'scen_type')) %>%
  mutate(expenditure = consumption * price,
         units_expenditure = '2005$/capita/day') %>%
  # total expenditure (staples + nonstaples)
  group_by(units_expenditure,region,scenario,t0,k,scen_type,year) %>%
  summarise(expenditure = sum(expenditure)) %>%
  ungroup()

# ABS diff
rm(pld_food_econ_basket_bill_diffAbs_regional)
for (s_type in c('spp','snr')) {
  tmp = food_econ_basket_bill_regional %>%
    filter(scenario == get(paste0('ref_scen_',s_type)) | startsWith(scenario, s_type)) %>%
    mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
    select(-c(t0, k, scen_type)) %>%
    tidyr::pivot_wider(names_from = 'scenario', values_from = 'expenditure') %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches('^spp|^snr')), list(diff = ~ . - St7_Reference)) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(c("units_expenditure","region","year", ends_with('_diff'))) %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches('^spp|^snr'), names_to = 'scenario') %>%
    # compute median
    dplyr::group_by(region,units_expenditure,year) %>%
    dplyr::summarise(median_value = median(value),
                     min_value = quantile(value, 0.10),
                     max_value = quantile(value, 0.90)) %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    mutate(scen_type = s_type)
  if (exists('pld_food_econ_basket_bill_diffAbs_regional')) {
    pld_food_econ_basket_bill_diffAbs_regional = pld_food_econ_basket_bill_diffAbs_regional %>%
      rbind(tmp)
  } else {
    pld_food_econ_basket_bill_diffAbs_regional = tmp
  }
  pl_food_econ_basket_bill_diffAbs_regional = ggplot(data = pld_food_econ_basket_bill_diffAbs_regional %>%
                                                       filter(scen_type == s_type) %>%
                                                       order_facets()) +
    geom_bar(stat = 'identity', aes(x = reorder(region, median_value), y = median_value, fill = scen_type), alpha = 1) +
    geom_errorbar(aes(x = region, ymin = min_value, ymax = max_value), width=0.3, colour="#757575", alpha=1, linewidth=1.2) +
    scale_fill_manual(name = 'Scenario',
                      values = scen_palette_refVsSppVsSnr,
                      labels = scen_palette_refVsSppVsSnr.labs) +  # labs
    labs(y = '$ per capita/day', x = '', title = 'Annual regional percentual change in food basket per capita expenditure') +
    # theme
    theme_light() +
    theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
          strip.background = element_blank(),
          strip.text = element_text(color = 'black', size = 40),
          strip.text.y = element_text(angle = 0),
          axis.text.x = element_text(size=30, angle = 90, hjust = 1, vjust = 0.25),
          axis.text.y = element_text(size=30),
          legend.text = element_text(size = 35),
          legend.title = element_text(size = 40),
          title = element_text(size = 40)) +
    guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2))
  # ggsave(pl_food_econ_basket_bill_diffAbs_regional, file = paste0(figures_path,dir_name,"/",'pl2_food_econ_basket_bill_diffAbs_regional_',s_type,'.png'),
  #        width = 750, height = 450, units = 'mm', limitsize = FALSE)
  assign(paste0('pl_food_econ_basket_bill_diffAbs_regional_',s_type), pl_food_econ_basket_bill_diffAbs_regional)
  png(paste0(figures_path,dir_name,"/",'pl2_food_econ_basket_bill_diffAbs_regional_',s_type,'.png'), width = 3401.575, height = 2267.717)
  print(pl_food_econ_basket_bill_diffAbs_regional)
  dev.off()
}

leg = ggpubr::get_legend(ggplot(data = pld_food_econ_basket_bill_diffAbs_regional %>%
                                  order_facets()) +
                           geom_bar(stat = 'identity', aes(x = reorder(region, median_value), y = median_value, fill = scen_type), alpha = 1) +
                           scale_fill_manual(name = 'Scenario',
                                             values = scen_palette_refVsSppVsSnr,
                                             labels = scen_palette_refVsSppVsSnr.labs) +
                           theme_light() +
                           theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
                                 legend.text = element_text(size = 35),
                                 legend.title = element_text(size = 40)))
blank_p <- patchwork::plot_spacer() + theme_void()
pl_food_econ_basket_bill_diffAbs_regional = cowplot::ggdraw() +
  cowplot::draw_plot(pl_food_econ_basket_bill_diffAbs_regional_spp +
                       ylim(-101000, 7000) + ggtitle('') +
                       geom_text(aes(x = 2, y = 3000, label = 'SPP'), color = "black", size = 13) +
                       theme(legend.position = 'none'), x = 0.01, y = 0.075, width = 0.48, height = 0.95) +
  cowplot::draw_plot(pl_food_econ_basket_bill_diffAbs_regional_snr +
                       ylim(-101000, 7000) + ggtitle('') +
                       geom_text(aes(x = 2, y = 3000, label = 'SNR'), color = "black", size = 13) +
                       theme(legend.position = 'none',
                             axis.title.y = element_blank()), x = 0.5, y = 0.075, width = 0.48, height = 0.95) +
  cowplot::draw_plot(cowplot::plot_grid(leg,blank_p,nrow=1), x = 0.25, y = -0.45, width = 1, height = 1)

pl_food_econ_basket_bill_diffAbs_regional = ggpubr::annotate_figure(pl_food_econ_basket_bill_diffAbs_regional,
                                                 top = ggpubr::text_grob("Annual regional percentual change in food basket per capita expenditure",
                                                                         color = "black", size = 40))
png(paste0(figures_path,dir_name,"/",'pl2_food_econ_basket_bill_diffAbs_regional.png'), width = 3401.575, height = 2267.717)
print(pl_food_econ_basket_bill_diffAbs_regional)
dev.off()


# PER diff
rm(pld_food_econ_basket_bill_diffPer_regional)
for (s_type in c('spp','snr')) {
  tmp = food_econ_basket_bill_regional %>%
    filter(scenario == get(paste0('ref_scen_',s_type)) | startsWith(scenario, s_type)) %>%
    mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
    select(-c(t0, k, scen_type)) %>%
    tidyr::pivot_wider(names_from = 'scenario', values_from = 'expenditure') %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches('^spp|^snr')), list(diff = ~ 100*(. - St7_Reference)/St7_Reference)) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(c("units_expenditure","region","year", ends_with('_diff'))) %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches('^spp|^snr'), names_to = 'scenario') %>%
    # compute median
    dplyr::group_by(region,units_expenditure,year) %>%
    dplyr::summarise(median_value = median(value),
                     min_value = quantile(value, 0.10),
                     max_value = quantile(value, 0.90)) %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    mutate(scen_type = s_type)
  if (exists('pld_food_econ_basket_bill_diffPer_regional')) {
    pld_food_econ_basket_bill_diffPer_regional = pld_food_econ_basket_bill_diffPer_regional %>%
      rbind(tmp)
  } else {
    pld_food_econ_basket_bill_diffPer_regional = tmp
  }
  pl_food_econ_basket_bill_diffPer_regional = ggplot(data = pld_food_econ_basket_bill_diffPer_regional %>%
                                                       filter(scen_type == s_type) %>%
                                                       order_facets()) +
    geom_bar(stat = 'identity', aes(x = reorder(region, median_value), y = median_value, fill = scen_type), alpha = 1) +
    geom_errorbar(aes(x = region, ymin = min_value, ymax = max_value), width=0.3, colour="#757575", alpha=1, linewidth=1.2) +
    scale_fill_manual(name = 'Scenario',
                      values = scen_palette_refVsSppVsSnr,
                      labels = scen_palette_refVsSppVsSnr.labs) +  # labs
    labs(y = '% per capita/day', x = '', title = 'Annual regional percentual change in food basket per capita expenditure') +
    # theme
    theme_light() +
    theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
          strip.background = element_blank(),
          strip.text = element_text(color = 'black', size = 40),
          strip.text.y = element_text(angle = 0),
          axis.text.x = element_text(size=30, angle = 90, hjust = 1, vjust = 0.25),
          axis.text.y = element_text(size=30),
          legend.text = element_text(size = 35),
          legend.title = element_text(size = 40),
          title = element_text(size = 40)) +
    guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2))
  # ggsave(pl_food_econ_basket_bill_diffPer_regional, file = paste0(figures_path,dir_name,"/",'pl2_food_econ_basket_bill_diffPer_regional_',s_type,'.png'),
  #        width = 750, height = 450, units = 'mm', limitsize = FALSE)
  assign(paste0('pl_food_econ_basket_bill_diffPer_regional_',s_type), pl_food_econ_basket_bill_diffPer_regional)
  png(paste0(figures_path,dir_name,"/",'pl2_food_econ_basket_bill_diffPer_regional_',s_type,'.png'), width = 3401.575, height = 2267.717)
  print(pl_food_econ_basket_bill_diffPer_regional)
  dev.off()
}

leg = ggpubr::get_legend(ggplot(data = pld_food_econ_basket_bill_diffPer_regional %>%
                                  order_facets()) +
                           geom_bar(stat = 'identity', aes(x = reorder(region, median_value), y = median_value, fill = scen_type), alpha = 1) +
                           scale_fill_manual(name = 'Scenario',
                                             values = scen_palette_refVsSppVsSnr,
                                             labels = scen_palette_refVsSppVsSnr.labs) +
                           theme_light() +
                           theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
                                 legend.text = element_text(size = 35),
                                 legend.title = element_text(size = 40)))
blank_p <- patchwork::plot_spacer() + theme_void()
pl_food_econ_basket_bill_diffPer_regional = cowplot::ggdraw() +
  cowplot::draw_plot(pl_food_econ_basket_bill_diffPer_regional_spp +
                       ylim(-12, 2.5) + ggtitle('') +
                       geom_text(aes(x = 2, y = 2.5, label = 'SPP'), color = "black", size = 13) +
                       theme(legend.position = 'none'), x = 0.01, y = 0.075, width = 0.48, height = 0.95) +
  cowplot::draw_plot(pl_food_econ_basket_bill_diffPer_regional_snr +
                       ylim(-12, 2.5) + ggtitle('') +
                       geom_text(aes(x = 2, y = 2.5, label = 'SNR'), color = "black", size = 13) +
                       theme(legend.position = 'none',
                             axis.title.y = element_blank()), x = 0.5, y = 0.075, width = 0.48, height = 0.95) +
  cowplot::draw_plot(cowplot::plot_grid(leg,blank_p,nrow=1), x = 0.25, y = -0.45, width = 1, height = 1)

pl_food_econ_basket_bill_diffPer_regional = ggpubr::annotate_figure(pl_food_econ_basket_bill_diffPer_regional,
                                                 top = ggpubr::text_grob("Annual regional percentual change in food basket per capita expenditure",
                                                                         color = "black", size = 40))
png(paste0(figures_path,dir_name,"/",'pl2_food_econ_basket_bill_diffPer_regional.png'), width = 3401.575, height = 2267.717)
print(pl_food_econ_basket_bill_diffPer_regional)
dev.off()


# world
food_econ_basket_bill_world = dt$food_consumption_regional %>%
  filter(region != 'South America_Northern') %>%
  left_join(food_subsector %>%
              rename('technology' = 'subsector')) %>%
  # compute total consumption by technology
  group_by(Units, scenario, t0, k, technology, year, scen_type, supplysector) %>%
  summarise(value = sum(value)) %>%
  # Pcal to kcal/capita/day
  left_join(dt$pop_all_regions %>%
              group_by(scenario, t0, k, year, scen_type) %>%
              summarise(population = sum(population)),
            by = c("year", "scenario", "t0", "k", "scen_type")) %>%
  # convert from Pcal to kcal/day
  mutate(value = (value * 1e12) / (population * 365),
         Units = "kcal/capita/day") %>%
  # total staples and nonstaples kcal consumption
  group_by(Units,scenario,t0,k,scen_type,year,supplysector) %>%
  summarise(consumption = sum(value)) %>%
  # compute the expenditure by supplysector
  left_join(dt$food_demand_prices_regional %>%
              mutate(price = value * 1e3,
                     units_price = '2005$/kcal/day') %>%
              select(-c(Units,value)) %>%
              group_by(scenario, t0, k, input, year, scen_type, units_price) %>%
              summarise(price = median(price)) %>%
              rename('supplysector' = 'input'),
            by = c('year','supplysector','scenario', "t0", "k", 'scen_type')) %>%
  mutate(expenditure = consumption * price,
         units_expenditure = '2005$/capita/day') %>%
  # total expenditure (staples + nonstaples)
  group_by(units_expenditure,scenario,t0,k,scen_type,year) %>%
  summarise(expenditure = sum(expenditure)) %>%
  ungroup()

rm(pld_food_econ_basket_bill_diffPer_world)
for (s_type in c('spp','snr')) {
  tmp = food_econ_basket_bill_world %>%
    filter(scenario == get(paste0('ref_scen_',s_type)) | startsWith(scenario, s_type)) %>%
    mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
    select(-c(scen_type,t0,k)) %>%
    tidyr::pivot_wider(names_from = 'scenario', values_from = 'expenditure') %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches('^spp|^snr')), list(diff = ~ 100*(. - St7_Reference)/St7_Reference)) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(c("units_expenditure","year", ends_with('_diff'))) %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches('^spp|^snr'), names_to = 'scenario') %>%
    # compute median
    dplyr::group_by(units_expenditure,year) %>%
    dplyr::mutate(median_value = median(value),
                  min_value = min(value),
                  max_value = max(value)) %>%
    mutate(scen_type = s_type)
  if (exists('pld_food_econ_basket_bill_diffPer_world')) {
    pld_food_econ_basket_bill_diffPer_world = pld_food_econ_basket_bill_diffPer_world %>%
      rbind(tmp)
  } else {
    pld_food_econ_basket_bill_diffPer_world = tmp
  }
  pl_food_econ_basket_bill_diffPer_world = ggplot(data = pld_food_econ_basket_bill_diffPer_world %>%
                                                       filter(scen_type == s_type) %>%
                                                       order_facets()) +
    geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
    geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line
    geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
    scale_color_manual(name = 'Scenario', values = scen_palette_refVsSppVsSnr, labels = scen_palette_refVsSppVsSnr.labs) +  # labs
    scale_fill_manual(name = 'Scenario', values = scen_palette_refVsSppVsSnr, labels = scen_palette_refVsSppVsSnr.labs) +  # labs
    labs(y = '% per capita/day', x = '', title = 'Annual world percentual change in food basket per capita expenditure') +
    # theme
    theme_light() +
    theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
          strip.background = element_blank(),
          strip.text = element_text(color = 'black', size = 40),
          strip.text.y = element_text(angle = 0),
          axis.text.x = element_text(size=30),
          axis.text.y = element_text(size=30),
          legend.text = element_text(size = 35),
          legend.title = element_text(size = 40),
          title = element_text(size = 40)) +
    guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2))
  # ggsave(pl_food_econ_basket_bill_diffPer_world, file = paste0(figures_path,dir_name,"/",'pl2_food_econ_basket_bill_diffPer_world_',s_type,'.png'),
  #        width = 750, height = 450, units = 'mm', limitsize = FALSE)
  assign(paste0('pl_food_econ_basket_bill_diffPer_world_',s_type), pl_food_econ_basket_bill_diffPer_world)
  png(paste0(figures_path,dir_name,"/",'pl2_food_econ_basket_bill_diffPer_world_',s_type,'.png'), width = 3401.575, height = 2267.717)
  print(pl_food_econ_basket_bill_diffPer_world)
  dev.off()
}

leg = ggpubr::get_legend(ggplot(data = pld_food_econ_basket_bill_diffPer_world %>%
                                  order_facets()) +
                           geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
                           geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line
                           geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
                           scale_color_manual(name = 'Scenario', values = scen_palette_refVsSppVsSnr, labels = scen_palette_refVsSppVsSnr.labs) +  # labs
                           scale_fill_manual(name = 'Scenario', values = scen_palette_refVsSppVsSnr, labels = scen_palette_refVsSppVsSnr.labs) +  # labs
                           theme_light() +
                           theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
                                 legend.text = element_text(size = 35),
                                 legend.title = element_text(size = 40)))
blank_p <- patchwork::plot_spacer() + theme_void()
pl_food_econ_basket_bill_diffPer_world = cowplot::ggdraw() +
  cowplot::draw_plot(pl_food_econ_basket_bill_diffPer_world_spp +
                       ylim(-15, 2) + ggtitle('') +
                       geom_text(aes(x = 2010, y = 0.15, label = 'SPP'), color = "black", size = 13) +
                       theme(legend.position = 'none'), x = 0.01, y = 0.075, width = 0.48, height = 0.95) +
  cowplot::draw_plot(pl_food_econ_basket_bill_diffPer_world_snr +
                       ylim(-15, 2) + ggtitle('') +
                       geom_text(aes(x = 2010, y = 0.15, label = 'SNR'), color = "black", size = 13) +
                       theme(legend.position = 'none',
                             axis.title.y = element_blank()), x = 0.5, y = 0.075, width = 0.48, height = 0.95) +
  cowplot::draw_plot(cowplot::plot_grid(leg,blank_p,nrow=1), x = 0.25, y = -0.45, width = 1, height = 1)

pl_food_econ_basket_bill_diffPer_world = ggpubr::annotate_figure(pl_food_econ_basket_bill_diffPer_world,
                                                                    top = ggpubr::text_grob("Annual median world percentual change in food basket per capita expenditure",
                                                                                            color = "black", size = 40))
png(paste0(figures_path,dir_name,"/",'pl2_food_econ_basket_bill_diffPer_world.png'), width = 3401.575, height = 2267.717)
print(pl_food_econ_basket_bill_diffPer_world)
dev.off()
#####

#### Fig: GHG emissions ============================ DONE
# =============================
## WORLD
## -- annual trend
ghg_world = dt$ghg_world %>%
  filter(scen_type != 'St7_Reference') %>%
  rbind(dt$ghg_world %>%
          filter(scen_type == 'St7_Reference') %>%
          group_by(Units, year) %>%
          mutate(t0 = NA, k = NA, scenario = 'St7_Reference',
                 value = mean(value)) %>%
          ungroup() %>%
          distinct())

pl_ghg_world <- ggplot(data = ghg_world %>%
                         rename_scen() %>%
                         dplyr::mutate(scen_type = substr(scenario, 1, 3)) %>%
                         dplyr::group_by(Units,year,scen_type) %>%
                         dplyr::mutate(median_value = median(value)) %>%
                         dplyr::mutate(min_value = min(value)) %>%
                         dplyr::mutate(max_value = max(value)) %>%
                         order_facets(.)) +
  geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario', labels = scen_palette_refVsSppVsSnr.labs) +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario', labels = scen_palette_refVsSppVsSnr.labs) +
  labs(x = '', y = expression(MtCO[2]), title = 'World GHG emissions') +
  guides(color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
  theme_light() +
  theme(panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',
        strip.background =element_rect(fill="white"),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
png(paste0(figures_path,dir_name, '/pl2_ghg_world.png'), width = 3401.575, height = 2267.717)
print(pl_ghg_world)
dev.off()


## -- map (abs difference)
rm(ghg_diffAbs_regional_all)
for (s_type in c('spp','snr')) {
  ghg_diffAbs_regional = tidyr::pivot_wider(dt$ghg_regional %>%
                             filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                             mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
                             select(-c(scen_type,t0,k)), names_from = 'scenario', values_from = 'value') %>%
    # compute difference between Reference and runs
    rename_scen() %>%
    dplyr::mutate_at(vars(matches('^snr|^spp')), list(diff = ~ . - St7_Reference)) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-c('St7_Reference')) %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches("^snr|^spp"), names_to = 'scenario') %>%
    # compute median
    dplyr::group_by(Units,year,scenario,region) %>%
    dplyr::summarise(median_value = median(diff)) %>%
    dplyr::mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
    dplyr::select(-scenario) %>% distinct() %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    # merge with GCAM regions
    dplyr::mutate('GCAM Region' = region) %>%
    inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
    # merge with world data
    dplyr::rename('adm0_a3' = 'ISO 3')

  if (exists('ghg_diffAbs_regional_all')) {
    ghg_diffAbs_regional_all = ghg_diffAbs_regional_all %>%
      rbind(ghg_diffAbs_regional)
  } else {
    ghg_diffAbs_regional_all = ghg_diffAbs_regional
  }

  pl_ghg_diffAbs_map <- ggplot() +
    # color map by regions
    geom_sf(data = order_facets(merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                        dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                        dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                        dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                      ghg_diffAbs_regional, by = 'adm0_a3')),
            aes(fill = -median_value)) +
    scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                         mid = '#f7f7f7', midpoint = 0,
                         name = expression(paste(MtCO[2],' difference'))) +
    # theme
    guides(fill = guide_colorbar(title.position = "left")) +
    theme_light() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#ffffff",
                                          colour = "#ffffff"),
          legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
          legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
    # title
    labs(title = paste('Abs GHG avoided emissions in', selected_year))
  png(paste0(figures_path,dir_name, '/pl2_ghg_diffAbs_map_',s_type,'.png'), width = 3401.575, height = 2267.717)
  print(pl_ghg_diffAbs_map)
  dev.off()
}

# plot
pl_ghg_diffAbs_map <- ggplot() +
  # color map by regions
  geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                          dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                          dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                          dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                        ghg_diffAbs_regional_all, by = 'adm0_a3') %>%
            order_facets(.),
          aes(fill = -median_value)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste(MtCO[2],' difference'))) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  facet_wrap(. ~ scen_type, scales = 'fixed') +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # title
  labs(title = paste('Abs GHG avoided emissions in', selected_year))
png(paste0(figures_path,dir_name, '/pl2_ghg_diffAbs_map.png'), width = 3401.575, height = 1267.717)
print(pl_ghg_diffAbs_map)
dev.off()



## -- map (per difference)
rm(ghg_diffPer_regional_all)
for (s_type in c('spp','snr')) {
  ghg_diffPer_regional = tidyr::pivot_wider(dt$ghg_regional %>%
                             filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                             mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
                             select(-c(scen_type,t0,k)), names_from = 'scenario', values_from = 'value') %>%
    # compute difference between Reference and runs
    rename_scen() %>%
    dplyr::mutate_at(vars(matches('^snr|^spp')), list(diff = ~ . - St7_Reference)) %>%
    dplyr::mutate(per_diff = 100*diff/St7_Reference) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-c('St7_Reference')) %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches("^snr|^spp"), names_to = 'scenario') %>%
    dplyr::select(Units,year,scenario,region,per_diff) %>%
    dplyr::mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
    dplyr::select(-scenario) %>% distinct() %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    # merge with GCAM regions
    dplyr::mutate('GCAM Region' = region) %>%
    inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
    # merge with world data
    dplyr::rename('adm0_a3' = 'ISO 3')

  if (exists('ghg_diffPer_regional_all')) {
    ghg_diffPer_regional_all = ghg_diffPer_regional_all %>%
      rbind(ghg_diffPer_regional)
  } else {
    ghg_diffPer_regional_all = ghg_diffPer_regional
  }

  pl_ghg_diffPer_map <- ggplot() +
    # color map by regions
    geom_sf(data = order_facets(merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                        dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                        dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                        dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                      ghg_diffPer_regional, by = 'adm0_a3')),
            aes(fill = -per_diff)) +
    scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                         mid = '#f7f7f7', midpoint = 0,
                         name = expression(paste(MtCO[2],' difference'))) +
    # theme
    guides(fill = guide_colorbar(title.position = "left")) +
    theme_light() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#ffffff",
                                          colour = "#ffffff"),
          legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
          legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
    # title
    labs(title = paste('Per GHG avoided emissions in', selected_year))
  png(paste0(figures_path,dir_name, '/pl2_ghg_diffPer_map_',s_type,'.png'), width = 3401.575, height = 2267.717)
  print(pl_ghg_diffPer_map)
  dev.off()
}

# plot
pl_ghg_diffPer_map <- ggplot() +
  # color map by regions
  geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                         dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                         dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                         dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                       ghg_diffPer_regional_all, by = 'adm0_a3') %>%
            order_facets(.),
          aes(fill = -per_diff)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = paste('Difference (%)')) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  facet_wrap(. ~ scen_type, scales = 'fixed') +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # title
  labs(title = paste('Per GHG avoided emissions in', selected_year))
png(paste0(figures_path,dir_name, '/pl2_ghg_diffPer_map.png'), width = 3401.575, height = 1267.717)
print(pl_ghg_diffPer_map)
dev.off()


## -- ghg emission by type (abs difference)
rm(ghg_by_ghg_diffAbs_world)
for (s_type in c('spp','snr')) {
  tmp = tidyr::pivot_wider(dt$ghg_by_ghg_world %>%
                                                  filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                                                  mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
                                                  select(-c(scen_type,t0,k)), names_from = 'scenario', values_from = 'value') %>%
    # compute difference between Reference and runs
    rename_scen() %>%
    dplyr::mutate_at(vars(matches('^snr|^spp')), list(diff = ~ . - St7_Reference)) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-c('St7_Reference')) %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches("^snr|^spp"), names_to = 'scenario') %>%
    # compute median
    dplyr::group_by(group,Units,year,scenario) %>%
    dplyr::summarise(median_value = median(diff)) %>%
    dplyr::mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
    dplyr::select(-scenario) %>% distinct()
  if (exists('ghg_by_ghg_diffAbs_world')) {
    ghg_by_ghg_diffAbs_world = ghg_by_ghg_diffAbs_world %>%
      rbind(tmp)
  } else {
    ghg_by_ghg_diffAbs_world = tmp
  }

  pl_ghg_diffAbs_world_bars <- ggplot() +
    # barchart
    geom_bar(data = ghg_by_ghg_diffAbs_world |> filter(year == selected_year,
                                                       scen_type == toupper(s_type)) %>%
               order_facets(.),
             aes(x = 0, y = median_value, fill = as.factor(group)),
             stat = "identity", color = NA, width = 0.5,
             position = position_stack(reverse = TRUE)) +
    scale_fill_brewer(palette = 'Paired', name = '') +
    facet_wrap(. ~ scen_type) +
    # horizontal line at y = 0
    geom_hline(yintercept = 0, linewidth = 1.2) +
    facet_wrap(.~scen_type, scales = 'fixed') +
    labs(x = '', y = expression(MtCO[2])) +
    theme_light() +
    theme(panel.grid.major.y = element_line(color = 'grey20'),
          panel.grid.major.x = element_blank(),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "transparent",
                                         colour = 'grey',linewidth = 2),
          panel.background = element_rect(fill = "transparent"),
          legend.position = 'bottom', legend.direction = 'horizontal',
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="transparent"),
          axis.text.x = element_text(size=25),
          axis.text.y = element_text(size=30),
          legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(1.5,'cm'),
          legend.text = element_text(size = 35),
          legend.title = element_text(size = 40),
          title = element_text(size = 40))
  png(paste0(figures_path,dir_name, '/pl2_ghg_diffAbs_bars_world_',s_type,'.png'), width = 3401.575, height = 2267.717)
  print(pl_ghg_diffAbs_world_bars)
  dev.off()
  assign(paste0('pl_ghg_',s_type),pl_ghg_diffAbs_world_bars)
}

pl_ghg_diffAbs_world_bars = ggplot() +
  # barchart
  geom_bar(data = ghg_by_ghg_diffAbs_world |> filter(year == selected_year) %>%
             order_facets(.),
           aes(x = 0, y = median_value, fill = as.factor(group)),
           stat = "identity", color = NA, width = 0.5,
           position = position_stack(reverse = TRUE)) +
  scale_fill_brewer(palette = 'Paired', name = '') +
  facet_wrap(. ~ scen_type) +
  # horizontal line at y = 0
  geom_hline(yintercept = 0, linewidth = 1.2) +
  facet_wrap(.~scen_type, scales = 'fixed') +
  labs(x = '', y = expression(MtCO[2])) +
  theme_light() +
  theme(panel.grid.major.y = element_line(color = 'grey20'),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = 'grey',linewidth = 2),
        panel.background = element_rect(fill = "transparent"),
        legend.position = 'bottom', legend.direction = 'horizontal',
        strip.text = element_text(size = 50, color = 'black'),
        strip.background =element_rect(fill="transparent"),
        axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=30),
        legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(1.5,'cm'),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
png(paste0(figures_path,dir_name, '/pl_ghg_diffAbs_world_bars.png'), width = 3401.575, height = 2267.717)
print(pl_ghg_diffAbs_world_bars)
dev.off()


## -- ghg emission by type (per difference)
rm(ghg_by_ghg_diffPer_world)
for (s_type in c('spp','snr')) {
  tmp = tidyr::pivot_wider(dt$ghg_by_ghg_world %>%
                             filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                             mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
                             select(-c(scen_type,t0,k)), names_from = 'scenario', values_from = 'value') %>%
    # compute difference between Reference and runs
    rename_scen() %>%
    dplyr::mutate_at(vars(matches('^snr|^spp')), list(diff = ~ . - St7_Reference)) %>%
    dplyr::mutate(per_diff = 100*diff/St7_Reference) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-c('St7_Reference')) %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches("^snr|^spp"), names_to = 'scenario') %>%
    dplyr::select(group,Units,year,scenario,per_diff) %>%
    dplyr::mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
    dplyr::select(-scenario) %>% distinct()
  if (exists('ghg_by_ghg_diffPer_world')) {
    ghg_by_ghg_diffPer_world = ghg_by_ghg_diffPer_world %>%
      rbind(tmp)
  } else {
    ghg_by_ghg_diffPer_world = tmp
  }

  pl_ghg_diffPer_world_bars <- ggplot() +
    # barchart
    geom_bar(data = ghg_by_ghg_diffPer_world |> filter(year == selected_year,
                                                       scen_type == toupper(s_type)) %>%
               order_facets(),
             aes(x = 0, y = per_diff, fill = as.factor(group)),
             stat = "identity", color = NA, width = 0.5,
             position = position_stack(reverse = TRUE)) +
    scale_fill_brewer(palette = 'Paired', name = '') +
    facet_wrap(. ~ scen_type) +
    # horizontal line at y = 0
    geom_hline(yintercept = 0, linewidth = 1.2) +
    facet_wrap(.~scen_type, scales = 'fixed') +
    labs(x = '', y = expression(MtCO[2])) +
    theme_light() +
    theme(panel.grid.major.y = element_line(color = 'grey20'),
          panel.grid.major.x = element_blank(),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "transparent",
                                         colour = 'grey',linewidth = 2),
          panel.background = element_rect(fill = "transparent"),
          legend.position = 'bottom', legend.direction = 'horizontal',
          strip.text = element_text(size = 30, color = 'black'),
          strip.background =element_rect(fill="transparent"),
          axis.text.x = element_text(size=25),
          axis.text.y = element_text(size=30),
          legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(1.5,'cm'),
          legend.text = element_text(size = 35),
          legend.title = element_text(size = 40),
          title = element_text(size = 40))
  png(paste0(figures_path,dir_name, '/pl2_ghg_diffPer_bars_world_',s_type,'.png'), width = 3401.575, height = 2267.717)
  print(pl_ghg_diffPer_world_bars)
  dev.off()
  assign(paste0('pl_ghg_',s_type),pl_ghg_diffPer_world_bars)
}

pl_ghg_diffPer_world_bars = ggplot() +
  # barchart
  geom_bar(data = ghg_by_ghg_diffPer_world |> filter(year == selected_year) %>%
             order_facets(),
           aes(x = 0, y = per_diff, fill = as.factor(group)),
           stat = "identity", color = NA, width = 0.5,
           position = position_stack(reverse = TRUE)) +
  scale_fill_brewer(palette = 'Paired', name = '') +
  facet_wrap(. ~ scen_type) +
  # horizontal line at y = 0
  geom_hline(yintercept = 0, linewidth = 1.2) +
  facet_wrap(.~scen_type, scales = 'fixed') +
  labs(x = '', y = expression(MtCO[2])) +
  theme_light() +
  theme(panel.grid.major.y = element_line(color = 'grey20'),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = 'grey',linewidth = 2),
        panel.background = element_rect(fill = "transparent"),
        legend.position = 'bottom', legend.direction = 'horizontal',
        strip.text = element_text(size = 30, color = 'black'),
        strip.background =element_rect(fill="transparent"),
        axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=30),
        legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(1.5,'cm'),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
png(paste0(figures_path,dir_name, '/pl_ghg_diffPer_world_bars.png'), width = 3401.575, height = 2267.717)
print(pl_ghg_diffPer_world_bars)
dev.off()

#####

#### Fig: avoided deaths =========================== DONE
# =============================
### MAPS
## -- map (abs difference)
rm(mort_diffAbs_regional_all)
for (s_type in c('spp','snr')) {
  mort_diffAbs_regional = tidyr::pivot_wider(dt.mort %>%
                                               filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                                               mutate(scenario = ifelse(startsWith(scenario, 'St7_Reference'), 'St7_Reference', scenario)) %>%
                                               # consider mean deaths from pm25 and o3
                                               group_by(region, year, scenario, pollutant) %>%
                                               summarise(value = median(mort)) %>%
                                               # add pm + o3 deaths
                                               group_by(region, year, scenario) %>%
                                               summarise(value = sum(value)) %>%
                                               rename(fasst_region = region) %>%
                                               ungroup(),
                                             names_from = 'scenario', values_from = 'value') %>%
    rename_scen() %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches("^snr|^spp")), list(diff = ~ . - St7_Reference)) %>%
    # mutate(per_diff = 100*diff/St7_Reference)
    dplyr::filter(!is.na(diff)) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-matches("^snr|^spp|^St7")) %>%
    # add a column with the scenario type
    mutate(scen_type = s_type) %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    # merge with GCAM regions
    left_join(rfasst::fasst_reg %>%
                dplyr::rename('ISO3' = 'subRegionAlt'), by = 'fasst_region',
              multiple = 'all') %>%
    # merge with world data
    dplyr::rename('adm0_a3' = 'ISO3')

  if (exists('mort_diffAbs_regional_all')) {
    mort_diffAbs_regional_all = mort_diffAbs_regional_all %>%
      rbind(mort_diffAbs_regional)
  } else {
    mort_diffAbs_regional_all = mort_diffAbs_regional
  }

  pl_mort_diffAbs_regional_map <- ggplot() +
    # color map by regions
    geom_sf(data = order_facets(merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                        dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                        dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                        dplyr::filter(!adm0_a3 %in% c("ATA","FJI")) %>%
                                        rowwise(),
                                      mort_diffAbs_regional, by = 'adm0_a3')),
            aes(fill = -diff)) +
    scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                         mid = '#f7f7f7', midpoint = 0,
                         name = expression(paste("Avoided premature mortalities (nº)","\n"))) +
    # theme
    guides(fill = guide_colorbar(title.position = "left")) +
    theme_light() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#ffffff",
                                          colour = "#ffffff"),
          legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
          legend.text = element_text(size = 30, angle = 90, vjust = 0.5, hjust=0.5), legend.title = element_text(size = 30, vjust = 0.95),
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
    # title
    labs(title = paste0("Annual avoided deaths in ", selected_year))
  png(paste0(figures_path,dir_name, '/pl2_mort_diffAbs_regional_map_',s_type,'.png'), width = 3401.575, height = 2267.717)
  print(pl_mort_diffAbs_regional_map)
  dev.off()
}

pl_mort_diffAbs_regional_map <- ggplot() +
  # color map by regions
  geom_sf(data = order_facets(merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                      dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                      dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                      dplyr::filter(!adm0_a3 %in% c("ATA","FJI")) %>%
                                      rowwise(),
                                    mort_diffAbs_regional_all, by = 'adm0_a3')),
          aes(fill = -diff)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Avoided premature mortalities (nº)","\n"))) +
  # facet
  facet_wrap(. ~ scen_type) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 30, angle = 90, vjust = 0.5, hjust=0.5), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # title
  labs(title = paste0("Annual avoided deaths in ", selected_year))
png(paste0(figures_path,dir_name, '/pl2_mort_diffAbs_regional_map.png'), width = 3401.575, height = 1267.717)
print(pl_mort_diffAbs_regional_map)
dev.off()

## -- map (per difference)
rm(mort_diffPer_regional_all)
for (s_type in c('spp','snr')) {
  mort_diffPer_regional = tidyr::pivot_wider(dt.mort %>%
                                               filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                                               mutate(scenario = ifelse(startsWith(scenario, 'St7_Reference'), 'St7_Reference', scenario)) %>%
                                               # consider mean deaths from pm25 and o3
                                               group_by(region, year, scenario, pollutant) %>%
                                               summarise(value = median(mort)) %>%
                                               # add pm + o3 deaths
                                               group_by(region, year, scenario) %>%
                                               summarise(value = sum(value)) %>%
                                               rename(fasst_region = region) %>%
                                               ungroup(),
                                             names_from = 'scenario', values_from = 'value') %>%
    rename_scen() %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches("^snr|^spp")), list(diff = ~ ifelse(St7_Reference != 0, 100 * (. - St7_Reference) / St7_Reference, 0))) %>%
    # mutate(per_diff = 100*diff/St7_Reference)
    dplyr::filter(!is.na(diff)) %>%
    # clean the dataset and keep only the "difference" columns
    # dplyr::select(-matches("^snr|^spp|^St7")) %>%
    rename(Ref_deaths = St7_Reference,
           Scen_deaths = get(paste0('base_scen_',s_type))) %>%
    # add a column with the scenario type
    mutate(scen_type = s_type) %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    # merge with GCAM regions
    left_join(rfasst::fasst_reg %>%
                dplyr::rename('ISO3' = 'subRegionAlt'), by = 'fasst_region',
              multiple = 'all') %>%
    # merge with world data
    dplyr::rename('adm0_a3' = 'ISO3')

  if (exists('mort_diffPer_regional_all')) {
    mort_diffPer_regional_all = mort_diffPer_regional_all %>%
      rbind(mort_diffPer_regional)
  } else {
    mort_diffPer_regional_all = mort_diffPer_regional
  }

  pl_mort_diffPer_regional_map <- ggplot() +
    # color map by regions
    geom_sf(data = order_facets(merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                        dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                        dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                        dplyr::filter(!adm0_a3 %in% c("ATA","FJI")) %>%
                                        rowwise(),
                                      mort_diffPer_regional, by = 'adm0_a3')),
            aes(fill = -diff)) +
    scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                         mid = '#f7f7f7', midpoint = 0,
                         name = expression(paste("Avoided premature mortalities (%)","\n"))) +
    # theme
    guides(fill = guide_colorbar(title.position = "left")) +
    theme_light() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#ffffff",
                                          colour = "#ffffff"),
          legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
          legend.text = element_text(size = 30), legend.title = element_text(size = 30, vjust = 0.95),
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
    # title
    labs(title = paste0("Annual avoided deaths in ", selected_year))
  png(paste0(figures_path,dir_name, '/pl2_mort_diffPer_regional_map_',s_type,'.png'), width = 3401.575, height = 2267.717)
  print(pl_mort_diffPer_regional_map)
  dev.off()
}

pl_mort_diffPer_regional_map <- ggplot() +
  # color map by regions
  geom_sf(data = order_facets(merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                      dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                      dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                      dplyr::filter(!adm0_a3 %in% c("ATA","FJI")) %>%
                                      rowwise(),
                                    mort_diffPer_regional_all, by = 'adm0_a3')),
          aes(fill = -diff)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Avoided premature mortalities (%)","\n"))) +
  # facet
  facet_wrap(. ~ scen_type) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 30), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # title
  labs(title = paste0("Annual avoided deaths in ", selected_year))
png(paste0(figures_path,dir_name, '/pl2_mort_diffPer_regional_map.png'), width = 3401.575, height = 1267.717)
print(pl_mort_diffPer_regional_map)
dev.off()


## -- map (abs difference population weighted)
population_by_fasst_region = total_regional_pop %>%
  select(year, country_name, country_pop = total_pop, GCAM_region_ID, region) %>%
  distinct(.) %>%
  dplyr::filter(year == selected_year) %>%
  left_join(iso_gcam_regions, by = c('GCAM_region_ID','country_name')) %>%
  rename('adm0_a3' = 'iso') %>%
  mutate(adm0_a3 = toupper(adm0_a3)) %>%
  # merge with GCAM regions
  left_join(rfasst::fasst_reg %>%
              dplyr::rename('adm0_a3' = 'subRegionAlt'), by = 'adm0_a3',
            multiple = 'all') %>%
  # compute total population by fasst-region
  group_by(year, fasst_region) %>%
  summarise(fasst_region_pop = sum(country_pop))

rm(mort_diffAbs_regional_all)
for (s_type in c('spp','snr')) {
  mort_diffAbs_regional = tidyr::pivot_wider(dt.mort %>%
                                               filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                                               mutate(scenario = ifelse(startsWith(scenario, 'St7_Reference'), 'St7_Reference', scenario)) %>%
                                               # consider mean deaths from pm25 and o3
                                               group_by(region, year, scenario, pollutant) %>%
                                               summarise(value = median(mort)) %>%
                                               # add pm + o3 deaths
                                               group_by(region, year, scenario) %>%
                                               summarise(value = sum(value)) %>%
                                               rename(fasst_region = region) %>%
                                               ungroup(),
                                             names_from = 'scenario', values_from = 'value') %>%
    rename_scen() %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches("^snr|^spp")), list(diff = ~ . - St7_Reference)) %>%
    dplyr::filter(!is.na(diff)) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-matches("^snr|^spp|^St7")) %>%
    # add a column with the scenario type
    mutate(scen_type = s_type) %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    # add population nº
    dplyr::left_join(population_by_fasst_region, by = c('year','fasst_region')) %>%
    # weight the avoided premature deaths
    mutate(diff_w = diff / (fasst_region_pop/1e3)) %>%
    # merge with GCAM regions
    left_join(rfasst::fasst_reg %>%
                dplyr::rename('ISO3' = 'subRegionAlt'), by = 'fasst_region',
              multiple = 'all') %>%
    # merge with world data
    dplyr::rename('adm0_a3' = 'ISO3')


  if (exists('mort_diffAbs_regional_all')) {
    mort_diffAbs_regional_all = mort_diffAbs_regional_all %>%
      rbind(mort_diffAbs_regional)
  } else {
    mort_diffAbs_regional_all = mort_diffAbs_regional
  }

  mort_diffAbs_regional = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                  dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                  dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                  dplyr::filter(!adm0_a3 %in% c("ATA","FJI")) %>%
                                  rowwise(),
                                mort_diffAbs_regional %>%
                                  rename(value = diff_w), by = 'adm0_a3')

  pl_mort_diffAbs_regional_map <- ggplot() +
    # color map by regions
    geom_sf(data = order_facets(mort_diffAbs_regional), aes(fill = -value)) +
    scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                         mid = '#f7f7f7', midpoint = 0,
                         name = expression(paste("Avoided premature mortalities per 1K people","\n"))) +
    # theme
    guides(fill = guide_colorbar(title.position = "left")) +
    theme_light() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#ffffff",
                                          colour = "#ffffff"),
          legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
          legend.text = element_text(size = 30, angle = 90, vjust = 0.5, hjust=0.5), legend.title = element_text(size = 30, vjust = 0.95),
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
    # title
    labs(title = paste0("Annual avoided deaths in ", selected_year))
  png(paste0(figures_path,dir_name, '/pl2_mort_diffAbsW_regional_map_',s_type,'.png'), width = 3401.575, height = 2267.717)
  print(pl_mort_diffAbs_regional_map)
  dev.off()
}

mort_diffAbs_regional_all = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                dplyr::filter(!adm0_a3 %in% c("ATA","FJI")) %>%
                                rowwise(),
                              mort_diffAbs_regional_all %>%
                                rename('value' = 'diff_w'), by = 'adm0_a3')

pl_mort_diffAbs_regional_map <- ggplot() +
  # color map by regions
  geom_sf(data = order_facets(mort_diffAbs_regional_all), aes(fill = -value)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Avoided premature mortalities per 1K people","\n"))) +
  # facet
  facet_wrap(. ~ scen_type) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 30), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # title
  labs(title = paste0("Annual avoided deaths in ", selected_year))
png(paste0(figures_path,dir_name, '/pl2_mort_diffAbsW_regional_map.png'), width = 3401.575, height = 1267.717)
print(pl_mort_diffAbs_regional_map)
dev.off()

## WORLD
plt_mort_world = ggplot(data = dt.mort %>%
                          # statistics by region
                          group_by(region, year, scenario, pollutant) %>%
                          summarise(value = median(mort)) %>%
                          ungroup() %>%
                          # sum all regions
                          group_by(year, scenario, pollutant) %>%
                          summarise(value = sum(value)) %>%
                          ungroup() %>% dplyr::distinct(.) %>%
                          # compute statistics by scen_type
                          rename_scen() %>%
                          mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
                          group_by(year, scen_type, pollutant) %>%
                          mutate(min_value = min(value),
                                 max_value = max(value),
                                 median_value = median(value)) %>%
                          ungroup() %>%
                          order_facets() %>%
                          # filter(scen_type != 'REF') %>%
                          mutate(year = as.numeric(year))) +
  # geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line
  # geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
  # geom_text(aes(x = year, y = median_value, label = scen_type), vjust = -0.5, hjust = -0.5) +
  facet_wrap(. ~ pollutant, scales = 'free') +
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario', labels = scen_palette_refVsSppVsSnr.labs) +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario', labels = scen_palette_refVsSppVsSnr.labs) +
  # labs
  labs(y = 'Nº people', x = '', title = 'Annual World premature deaths due to AP') +
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
png(paste0(figures_path,dir_name, '/pl2_mort_world.png'), width = 3401.575, height = 2267.717)
print(plt_mort_world)
dev.off()

## REGIONAL
# (free scales)
plt_mort_regional = ggplot(data = dt.mort %>%
                             # statistics by region
                             group_by(region, year, scenario, pollutant) %>%
                             summarise(value = median(mort)) %>%
                             ungroup() %>% dplyr::distinct(.) %>%
                             # compute statistics by scen_type
                             rename_scen() %>%
                             mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
                             group_by(region, year, scen_type, pollutant) %>%
                             mutate(min_value = min(value),
                                    max_value = max(value),
                                    median_value = median(value)) %>%
                             ungroup() %>%
                             order_facets() %>%
                             # filter(scen_type != 'REF') %>%
                             mutate(year = as.numeric(year))) +
  # geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line
  # geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
  # geom_text(aes(x = year, y = median_value, label = scen_type), vjust = -0.5, hjust = -0.5) +
  facet_grid(region ~ pollutant, scales = 'free') +
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario', labels = scen_palette_refVsSppVsSnr.labs) +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario', labels = scen_palette_refVsSppVsSnr.labs) +
  # labs
  labs(y = 'Nº people', x = '', title = 'Annual World premature deaths due to AP') +
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
png(paste0(figures_path,dir_name, '/pl2_mort_regional.png'), width = 2267.717, height = 5267.717)
print(plt_mort_regional)
dev.off()


#####

#### Fig: water consumption and withdrawals ======== DONE
# =============================
# water consumption
### MAPS
## -- map (abs difference)
rm(water_consumption_diffAbs_regional_all)
for (s_type in c('spp','snr')) {
  water_consumption_diffAbs_regional = tidyr::pivot_wider(dt$water_consumption_regional %>%
                                                            filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                                                            mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
                                                            select(-c(scen_type,t0,k)),
                                                          names_from = 'scenario', values_from = 'value') %>%
    rename_scen() %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches("^snr|^spp")), list(diff = ~ . - St7_Reference)) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-'St7_Reference') %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches("^snr|^spp"), names_to = 'scenario') %>%
    # scen_type
    dplyr::mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    # merge with GCAM regions
    dplyr::mutate('GCAM Region' = region) %>%
    inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
    # merge with world data
    dplyr::rename('adm0_a3' = 'ISO 3') %>%
    ungroup()

  if (exists('water_consumption_diffAbs_regional_all')) {
    water_consumption_diffAbs_regional_all = water_consumption_diffAbs_regional_all %>%
      rbind(water_consumption_diffAbs_regional)
  } else {
    water_consumption_diffAbs_regional_all = water_consumption_diffAbs_regional
  }

  water_consumption_diffAbs_regional = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                               dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                               dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                               dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                             water_consumption_diffAbs_regional, by = 'adm0_a3')
  # plot
  pl_water_consumption_diffAbs_map <- ggplot() +
    # color map by regions
    geom_sf(data = water_consumption_diffAbs_regional, aes(fill = diff)) +
    scale_fill_gradient2(low = "#0DA800", high = "#C60000",
                         mid = '#f7f7f7', midpoint = 0,
                         name = expression(km^3)) +
    # theme
    guides(fill = guide_colorbar(title.position = "left")) +
    theme_light() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#ffffff",
                                          colour = "#ffffff"),
          legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
          legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
    # facet
    facet_wrap(. ~ scen_type) +
    labs(title = 'Regional water savings (Scen - Ref)')
  png(paste0(figures_path,dir_name,"/",'pl2_water_consumption_diffAbs_map_', s_type, '.png'), width = 3401.575, height = 2267.717)
  print(pl_water_consumption_diffAbs_map)
  dev.off()

}

pl_water_consumption_diffAbs_map <- ggplot() +
  # color map by regions
  geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                         dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                         dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                         dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                       water_consumption_diffAbs_regional_all, by = 'adm0_a3')
          , aes(fill = diff)) +
  scale_fill_gradient2(low = "#0DA800", high = "#C60000",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(km^3)) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # facet
  facet_wrap(. ~ scen_type) +
  labs(title = 'Regional water savings (Scen - Ref)')
png(paste0(figures_path,dir_name,"/",'pl2_water_consumption_diffAbs_map.png'), width = 3401.575, height = 2267.717)
print(pl_water_consumption_diffAbs_map)
dev.off()



# (per diff)
rm(water_consumption_diffPer_regional_all)
for (s_type in c('spp','snr')) {
  water_consumption_diffPer_regional = tidyr::pivot_wider(dt$water_consumption_regional %>%
                                                      filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                                                      mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
                                                      select(-c(scen_type,t0,k)),
                                                    names_from = 'scenario', values_from = 'value') %>%
    rename_scen() %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches("^snr|^spp")), list(diff = ~ ifelse(. - St7_Reference != 0, 100*(. - St7_Reference)/St7_Reference, 0))) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-'St7_Reference') %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches("^snr|^spp"), names_to = 'scenario') %>%
    # scen_type
    dplyr::mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    # merge with GCAM regions
    dplyr::mutate('GCAM Region' = region) %>%
    inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
    # merge with world data
    dplyr::rename('adm0_a3' = 'ISO 3') %>%
    ungroup()

  if (exists('water_consumption_diffPer_regional_all')) {
    water_consumption_diffPer_regional_all = water_consumption_diffPer_regional_all %>%
      rbind(water_consumption_diffPer_regional)
  } else {
    water_consumption_diffPer_regional_all = water_consumption_diffPer_regional
  }

  # plot
  pl_water_consumption_diffPer_map <- ggplot() +
    # color map by regions
    geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                           dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                           dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                           dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                         water_consumption_diffPer_regional, by = 'adm0_a3'),
            aes(fill = diff)) +
    scale_fill_gradient2(low = "#0DA800", high = "#C60000",
                         mid = '#f7f7f7', midpoint = 0,
                         name = expression(paste('Percentual difference'))) +
    # theme
    guides(fill = guide_colorbar(title.position = "left")) +
    theme_light() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#ffffff",
                                          colour = "#ffffff"),
          legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
          legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
    # facet
    facet_wrap(. ~ scen_type) +
    labs(title = 'Regional water savings (Scen - Ref)')
  png(paste0(figures_path,dir_name,"/",'pl2_water_consumption_diffPer_map_', s_type, '.png'), width = 3401.575, height = 2267.717)
  print(pl_water_consumption_diffPer_map)
  dev.off()
}

pl_water_consumption_diffPer_map <- ggplot() +
  # color map by regions
  geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                         dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                         dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                         dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                       water_consumption_diffPer_regional_all, by = 'adm0_a3')
          , aes(fill = diff)) +
  scale_fill_gradient2(low = "#0DA800", high = "#C60000",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste('Percentual difference'))) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # facet
  facet_wrap(. ~ scen_type) +
  labs(title = 'Regional water savings (Scen - Ref)')
png(paste0(figures_path,dir_name,"/",'pl2_water_consumption_diffPer_map.png'), width = 3401.575, height = 2267.717)
print(pl_water_consumption_diffPer_map)
dev.off()



# ## -- bars (abs difference)
# water_consumption_diffAbs_regional_sectorial = water_consumption_regional_sectorial %>%
#   rename_scen() %>%
#   # compute regional-sectorial consumption
#   dplyr::filter(sector %in% food_sector) %>%
#   group_by(year,scenario,sector,region) %>%
#   summarise(value = sum(value)) %>% ungroup() %>%
#   # compute difference between Reference and runs
#   tidyr::pivot_wider(names_from = 'scenario', values_from = 'value') %>%
#   dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ . - Reference)) %>%
#   # clean the dataset and keep only the "difference" columns
#   dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
#   # reshape dataset
#   tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
#   # compute median
#   dplyr::group_by(region,year,sector,scenario) %>%
#   dplyr::summarise(median_value = median(value)) %>%
#   # filter desired year
#   dplyr::filter(year == selected_year)
#
#
# pl_water_consumption_diffAbs_regional_sectorial_bars <- ggplot() +
#   # barchart
#   geom_bar(data = water_consumption_diffAbs_regional_sectorial |> filter(year == selected_year),
#            aes(x = region, y = median_value, fill = as.factor(sector)),
#            stat = "identity", color = NA, width = 0.5,
#            position = position_stack(reverse = TRUE)) +
#   scale_fill_manual(values = c25, name = '') +
#   # horizontal line at y = 0
#   geom_hline(yintercept = 0, linewidth = 1.2) +
#   facet_wrap(.~scenario) +
#   labs(x = '', y = expression(paste("Annual Water flows difference (billion ",m^3,")","\n"))) +
#   theme_light() +
#   theme(panel.grid.major.y = element_line(color = 'grey20'),
#         panel.grid.major.x = element_blank(),
#         panel.border = element_blank(),
#         plot.background = element_rect(fill = "transparent",
#                                        colour = 'white',linewidth = 0),
#         panel.background = element_rect(fill = "transparent"),
#         legend.key.size = unit(2,'cm'), legend.position = 'bottom', legend.direction = 'horizontal',
#         strip.text = element_text(size = 20, color = 'black'),
#         strip.background =element_rect(fill="transparent"),
#         axis.text.x = element_text(size=30, angle = 90),
#         axis.text.y = element_text(size=30),
#         legend.text = element_text(size = 35),
#         legend.title = element_text(size = 40),
#         title = element_text(size = 40)) +
#   guides(fill = guide_legend(nrow = 3)) +
#   # title
#   labs(title = paste0('Abs diff of regional sectorial water consumption in ',selected_year))
# ggsave(pl_water_consumption_diffAbs_regional_sectorial_bars, file = paste0(figures_path,dir_name, '/pl2_water_consumption_diffAbs_regional_sectorial_bars.pdf'),
#        width = 700, height = 500, units = 'mm')
#
# ## -- bars (per difference)
# water_consumption_diffPer_regional_sectorial = water_consumption_regional_sectorial %>%
#   rename_scen() %>%
#   # compute regional-sectorial consumption
#   dplyr::filter(sector %in% food_sector) %>%
#   group_by(year,scenario,sector,region) %>%
#   summarise(value = sum(value)) %>% ungroup() %>%
#   # compute difference between Reference and runs
#   tidyr::pivot_wider(names_from = 'scenario', values_from = 'value') %>%
#   dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ 100*(. - Reference)/Reference)) %>%
#   # clean the dataset and keep only the "difference" columns
#   dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
#   # reshape dataset
#   tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
#   # compute median
#   dplyr::group_by(region,year,sector,scenario) %>%
#   dplyr::summarise(median_value = median(value)) %>%
#   # filter desired year
#   dplyr::filter(year == selected_year)
#
#
# pl_water_consumption_diffPer_regional_sectorial_bars <- ggplot() +
#   # barchart
#   geom_bar(data = water_consumption_diffPer_regional_sectorial |> filter(year == selected_year),
#            aes(x = region, y = median_value, fill = as.factor(sector)),
#            stat = "identity", color = NA, width = 0.5,
#            position = position_stack(reverse = TRUE)) +
#   scale_fill_manual(values = c25, name = '') +
#   # horizontal line at y = 0
#   geom_hline(yintercept = 0, linewidth = 1.2) +
#   facet_wrap(.~scenario) +
#   labs(x = '', y = expression(paste("Annual Water flows % difference","\n"))) +
#   theme_light() +
#   theme(panel.grid.major.y = element_line(color = 'grey20'),
#         panel.grid.major.x = element_blank(),
#         panel.border = element_blank(),
#         plot.background = element_rect(fill = "transparent",
#                                        colour = 'grey',linewidth = 2),
#         panel.background = element_rect(fill = "transparent"),
#         legend.position = 'bottom', legend.direction = 'horizontal',
#         strip.text = element_text(size = 20, color = 'black'),
#         strip.background =element_rect(fill="transparent"),
#         axis.text.x = element_text(size=30, angle = 90),
#         axis.text.y = element_text(size=30),
#         legend.text = element_text(size = 35),
#         legend.title = element_text(size = 40),
#         title = element_text(size = 40)) +
#   # title
#   labs(title = paste0('Per diff of regional sectorial water consumption in ',selected_year))
# ggsave(pl_water_consumption_diffPer_regional_sectorial_bars, file = paste0(figures_path,dir_name, '/pl2_water_consumption_diffPer_regional_sectorial_bars.pdf'),
#        width = 700, height = 500, units = 'mm')
#
#
#
#
#
# ### WORLD
# ## -- world water withdrawals
# pl_water_withdrawals_world <- ggplot(data = water_withdrawals_world %>%
#                                        rename_scen() %>%
#                                        dplyr::group_by(year, scen_type) %>%
#                                        mutate(median_value = median(value)) %>%
#                                        mutate(min_value = min(value)) %>%
#                                        mutate(max_value = max(value))) +
#   geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
#   geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line
#   geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
#   scale_color_manual(values = mypal_scen, name = 'Scenario') +
#   scale_fill_manual(values = mypal_scen, name = 'Scenario') +
#   # labs
#   labs(y = expression(paste("Annual Water flows (billion ",m^3,")")), x = '') +
#   # theme
#   theme_light() +
#   guides(color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
#   theme(plot.background = element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.position = 'bottom', legend.direction = 'horizontal',
#         strip.background = element_blank(),
#         axis.text.x = element_text(size=30),
#         axis.text.y = element_text(size=30),
#         axis.title = element_text(size=30),
#         legend.text = element_text(size = 35),
#         legend.title = element_text(size = 40),
#         title = element_text(size = 40)) +
#   # title
#   labs(title = 'Annual World Water withdrawals')
# ggsave(pl_water_withdrawals_world, file = paste0(figures_path,dir_name, '/pl2_water_withdrawals_world.pdf'), width = 500, height = 300, units = 'mm')
#
#
#
# ### REGIONAL
# ## -- regional water withdrawals (free scales)
# pl_water_withdrawals_regional <- ggplot(data = water_withdrawals_regional %>%
#                                           rename_scen() %>%
#                                           dplyr::group_by(year, scen_type, region) %>%
#                                           mutate(median_value = median(value)) %>%
#                                           mutate(min_value = min(value)) %>%
#                                           mutate(max_value = max(value))) +
#   geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
#   geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line
#   geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
#   scale_color_manual(values = mypal_scen, name = 'Scenario') +
#   scale_fill_manual(values = mypal_scen, name = 'Scenario') +
#   # facet
#   facet_wrap(. ~ region, scales = 'free') +
#   # labs
#   labs(y = expression(paste("Annual Water flows (billion ",m^3,")")), x = '') +
#   # theme
#   theme_light() +
#   guides(color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
#   theme(plot.background = element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.position = 'bottom', legend.direction = 'horizontal',
#         # strip.background = element_blank(),
#         axis.text.x = element_text(size=30),
#         axis.text.y = element_text(size=30),
#         axis.title = element_text(size=30),
#         legend.text = element_text(size = 35),
#         legend.title = element_text(size = 40),
#         title = element_text(size = 40)) +
#   # title
#   labs(title = 'Annual Water withdrawals (free scales)')
# ggsave(pl_water_withdrawals_regional + theme(strip.text = element_text(size = 30)), file = paste0(figures_path,dir_name, '/pl2_water_withdrawals_regional_freeScales.pdf'),
#        width = 800, height = 700, units = 'mm')
#
# ## -- regional water withdrawals (fixed scales)
# pl_water_withdrawals_regional <- ggplot(data = water_withdrawals_regional %>%
#                                           rename_scen() %>%
#                                           dplyr::group_by(year, scen_type, region) %>%
#                                           mutate(median_value = median(value)) %>%
#                                           mutate(min_value = min(value)) %>%
#                                           mutate(max_value = max(value))) +
#   geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
#   geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line
#   geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
#   scale_color_manual(values = mypal_scen, name = 'Scenario') +
#   scale_fill_manual(values = mypal_scen, name = 'Scenario') +
#   # facet
#   facet_wrap(. ~ region) +
#   # labs
#   labs(y = expression(paste("Annual Water flows (billion ",m^3,")")), x = '') +
#   # theme
#   theme_light() +
#   guides(color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
#   theme(plot.background = element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.position = 'bottom', legend.direction = 'horizontal',
#         # strip.background = element_blank(),
#         axis.text.x = element_text(size=30),
#         axis.text.y = element_text(size=30),
#         axis.title = element_text(size=30),
#         legend.text = element_text(size = 35),
#         legend.title = element_text(size = 40),
#         title = element_text(size = 40)) +
#   # title
#   labs(title = 'Annual Water withdrawals (fixed scales)')
# ggsave(pl_water_withdrawals_regional + theme(strip.text = element_text(size = 30)), file = paste0(figures_path,dir_name, '/pl2_water_withdrawals_regional_fixedScales.pdf'),
#        width = 800, height = 700, units = 'mm')

#####

#### Fig: CH4 ====================================== DONE
# =============================
## -- CH4 emissions
### WORLD
pl_ch4_world = ggplot(data = dt$nonco2_luc %>% filter(ghg == 'CH4') %>%
                        mutate(scen_type = substr(scenario, 1, 3)) %>%
                        group_by(Units,scenario,scen_type,ghg,year) %>%
                        summarise(value = sum(value)) %>%
                        ungroup() %>%
                        rename_scen() %>%
                        dplyr::group_by(year,scen_type) %>%
                        dplyr::mutate(median_value = median(value)) %>%
                        dplyr::mutate(min_value = min(value)) %>%
                        dplyr::mutate(max_value = max(value)) %>%
                        order_facets()) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,scenario), color = interaction(scen_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type)), linewidth = 2, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  # labs
  labs(y = expression(MtCH[4]), x = '', title = expression(paste('Annual World ',CH[4],' emissions'))) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
png(paste0(figures_path,dir_name, '/pl2_ch4_world.png'), width = 3401.575, height = 2267.717)
print(pl_ch4_world)
dev.off()

### REGIONAL
## (free scales)
pl_ch4_regional = ggplot(data = dt$nonco2_luc %>% filter(ghg == 'CH4') %>%
                           mutate(scen_type = substr(scenario, 1, 3)) %>%
                           group_by(region,Units,scenario,scen_type,ghg,year) %>%
                           summarise(value = sum(value)) %>%
                           ungroup() %>%
                           rename_scen() %>%
                           dplyr::group_by(region,year,scen_type) %>%
                           dplyr::mutate(median_value = median(value)) %>%
                           dplyr::mutate(min_value = min(value)) %>%
                           dplyr::mutate(max_value = max(value)) %>%
                           order_facets()) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,scenario), color = interaction(scen_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(y = expression(MtCH[4]), x = '', title = expression(paste('Annual World ',CH[4],' emissions (free scales)'))) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
png(paste0(figures_path,dir_name, '/pl2_ch4_regional_freeScales.png'), width = 3401.575, height = 3267.717)
print(pl_ch4_regional)
dev.off()

# (fixed scales)
pl_ch4_regional = ggplot(data = dt$nonco2_luc %>% filter(ghg == 'CH4') %>%
                           mutate(scen_type = substr(scenario, 1, 3)) %>%
                           group_by(region,Units,scenario,scen_type,ghg,year) %>%
                           summarise(value = sum(value)) %>%
                           ungroup() %>%
                           rename_scen() %>%
                           dplyr::group_by(region,year,scen_type) %>%
                           dplyr::mutate(median_value = median(value)) %>%
                           dplyr::mutate(min_value = min(value)) %>%
                           dplyr::mutate(max_value = max(value)) %>%
                           order_facets()) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,scenario), color = interaction(scen_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  # facet
  facet_wrap(. ~ region, scales = 'fixed') +
  # labs
  labs(y = expression(MtCH[4]), x = '', title = expression(paste('Annual World ',CH[4],' emissions (fixed scales)'))) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
png(paste0(figures_path,dir_name, '/pl2_ch4_regional_fixedScales.png'), width = 3401.575, height = 3267.717)
print(pl_ch4_regional)
dev.off()


## maps
rm(ch4_diffAbs_regional_all)
for (s_type in c('spp','snr')) {
  ch4_diffAbs_regional = tidyr::pivot_wider(dt$nonco2_luc %>% filter(ghg == 'CH4') %>%
                                                  filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                                                  mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
                                                  select(-c(scen_type,t0,k)) %>%
                                                  group_by(region,Units,scenario,ghg,year) %>%
                                                  summarise(value = sum(value)),
                                                names_from = 'scenario', values_from = 'value') %>%
    rename_scen() %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches("^snr|^spp")), list(diff = ~ . - St7_Reference)) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-'St7_Reference') %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches("^snr|^spp"), names_to = 'scenario') %>%
    # compute median
    dplyr::mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
    dplyr::group_by(Units,year,scen_type,region) %>%
    dplyr::summarise(median_value = sum(diff)) %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    # merge with GCAM regions
    dplyr::mutate('GCAM Region' = region) %>%
    inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
    # merge with world data
    dplyr::rename('adm0_a3' = 'ISO 3') %>%
    ungroup() %>%
    # fix southAm-Northern
    dplyr::mutate(median_value = ifelse(region == 'South America_Northern', 0, median_value))

  if (exists('ch4_diffAbs_regional_all')) {
    ch4_diffAbs_regional_all = ch4_diffAbs_regional_all %>%
      rbind(ch4_diffAbs_regional)
  } else {
    ch4_diffAbs_regional_all = ch4_diffAbs_regional
  }

  # plot
  pl_ch4_diffAbs_regional_map <- ggplot() +
    # color map by regions
    geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                           dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                           dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                           dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                         ch4_diffAbs_regional, by = 'adm0_a3') %>%
              order_facets(), aes(fill = -median_value)) +
    scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                         mid = '#f7f7f7', midpoint = 0,
                         name = 'Mt') +
    # theme
    guides(fill = guide_colorbar(title.position = "left")) +
    theme_light() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#ffffff",
                                          colour = "#ffffff"),
          legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
          legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
    # facet
    facet_wrap(. ~ scen_type)
  png(paste0(figures_path,dir_name, '/pl2_ch4_diffAbs_regional_map_', s_type, '.png'), width = 3401.575, height = 2267.717)
  print(pl_ch4_diffAbs_regional_map)
  dev.off()
}


pl_ch4_diffAbs_regional_map <- ggplot() +
  # color map by regions
  geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                         dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                         dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                         dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                       ch4_diffAbs_regional_all, by = 'adm0_a3') %>%
            order_facets(), aes(fill = -median_value)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = 'Mt') +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # facet
  facet_wrap(. ~ scen_type) +
  labs(title = "Global median avoided CH4 emissions according to SPP & SNR")

png(paste0(figures_path,dir_name, '/pl_ch4_diffAbs_regional_map.png'), width = 3401.575, height = 2267.717)
print(pl_ch4_diffAbs_regional_map)
dev.off()


## maps (per diff)
rm(ch4_diffPer_regional_all)
for (s_type in c('spp','snr')) {
  ch4_diffPer_regional = tidyr::pivot_wider(dt$nonco2_luc %>% filter(ghg == 'CH4') %>%
                                                  filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                                                  mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
                                                  select(-c(scen_type,t0,k)) %>%
                                                  group_by(region,Units,scenario,ghg,year) %>%
                                                  summarise(value = sum(value)),
                                                names_from = 'scenario', values_from = 'value') %>%
    rename_scen() %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches("^snr|^spp")), list(diff = ~ ifelse(St7_Reference != 0, 100 * (. - St7_Reference) / St7_Reference, 0))) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-'St7_Reference') %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches("^snr|^spp"), names_to = 'scenario') %>%
    # compute median
    dplyr::mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
    dplyr::group_by(Units,year,scen_type,region) %>%
    dplyr::summarise(median_value = sum(diff)) %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    # merge with GCAM regions
    dplyr::mutate('GCAM Region' = region) %>%
    inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
    # merge with world data
    dplyr::rename('adm0_a3' = 'ISO 3') %>%
    ungroup() %>%
    # fix southAm-Northern
    dplyr::mutate(median_value = ifelse(region == 'South America_Northern', 0, median_value))

  if (exists('ch4_diffPer_regional_all')) {
    ch4_diffPer_regional_all = ch4_diffPer_regional_all %>%
      rbind(ch4_diffPer_regional)
  } else {
    ch4_diffPer_regional_all = ch4_diffPer_regional
  }

  # plot
  pl_ch4_diffPer_regional_map <- ggplot() +
    # color map by regions
    geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                           dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                           dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                           dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                         ch4_diffPer_regional, by = 'adm0_a3') %>%
              order_facets(), aes(fill = -median_value)) +
    scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                         mid = '#f7f7f7', midpoint = 0,
                         name = 'Percentual difference') +
    # theme
    guides(fill = guide_colorbar(title.position = "left")) +
    theme_light() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#ffffff",
                                          colour = "#ffffff"),
          legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
          legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
    # facet
    facet_wrap(. ~ scen_type)
  png(paste0(figures_path,dir_name, '/pl2_ch4_diffPer_regional_map_', s_type, '.png'), width = 3401.575, height = 2267.717)
  print(pl_ch4_diffPer_regional_map)
  dev.off()
}


pl_ch4_diffPer_regional_map <- ggplot() +
  # color map by regions
  geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                         dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                         dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                         dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                       ch4_diffPer_regional_all, by = 'adm0_a3') %>%
            order_facets(), aes(fill = -median_value)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = 'Percentual difference') +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # facet
  facet_wrap(. ~ scen_type) +
  labs(title = "Global median avoided CH4 emissions according to SPP & SNR")

png(paste0(figures_path,dir_name, '/pl_ch4_diffPer_regional_map.png'), width = 3401.575, height = 2267.717)
print(pl_ch4_diffPer_regional_map)
dev.off()


#####

#### Fig: N2O ====================================== DONE
# =============================
## -- N2O emissions
### WORLD
pl_n2o_world = ggplot(data = dt$nonco2_luc %>% filter(ghg == 'N2O') %>%
                        mutate(scen_type = substr(scenario, 1, 3)) %>%
                        group_by(Units,scenario,scen_type,ghg,year) %>%
                        summarise(value = sum(value)) %>%
                        ungroup() %>%
                        rename_scen() %>%
                        dplyr::group_by(year,scen_type) %>%
                        dplyr::mutate(median_value = median(value)) %>%
                        dplyr::mutate(min_value = min(value)) %>%
                        dplyr::mutate(max_value = max(value)) %>%
                        order_facets()) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,scenario), color = interaction(scen_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type)), linewidth = 2, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  # labs
  labs(y = expression(MtCH[4]), x = '', title = expression(paste('Annual World ',CH[4],' emissions'))) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
png(paste0(figures_path,dir_name, '/pl2_n2o_world.png'), width = 3401.575, height = 2267.717)
print(pl_n2o_world)
dev.off()

### REGIONAL
## (free scales)
pl_n2o_regional = ggplot(data = dt$nonco2_luc %>% filter(ghg == 'N2O') %>%
                           mutate(scen_type = substr(scenario, 1, 3)) %>%
                           group_by(region,Units,scenario,scen_type,ghg,year) %>%
                           summarise(value = sum(value)) %>%
                           ungroup() %>%
                           rename_scen() %>%
                           dplyr::group_by(region,year,scen_type) %>%
                           dplyr::mutate(median_value = median(value)) %>%
                           dplyr::mutate(min_value = min(value)) %>%
                           dplyr::mutate(max_value = max(value)) %>%
                           order_facets()) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,scenario), color = interaction(scen_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(y = expression(MtCH[4]), x = '', title = expression(paste('Annual World ',CH[4],' emissions (free scales)'))) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
png(paste0(figures_path,dir_name, '/pl2_n2o_regional_freeScales.png'), width = 3401.575, height = 3267.717)
print(pl_n2o_regional)
dev.off()

# (fixed scales)
pl_n2o_regional = ggplot(data = dt$nonco2_luc %>% filter(ghg == 'N2O') %>%
                           mutate(scen_type = substr(scenario, 1, 3)) %>%
                           group_by(region,Units,scenario,scen_type,ghg,year) %>%
                           summarise(value = sum(value)) %>%
                           ungroup() %>%
                           rename_scen() %>%
                           dplyr::group_by(region,year,scen_type) %>%
                           dplyr::mutate(median_value = median(value)) %>%
                           dplyr::mutate(min_value = min(value)) %>%
                           dplyr::mutate(max_value = max(value)) %>%
                           order_facets()) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,scenario), color = interaction(scen_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  # facet
  facet_wrap(. ~ region, scales = 'fixed') +
  # labs
  labs(y = expression(MtCH[4]), x = '', title = expression(paste('Annual World ',CH[4],' emissions (fixed scales)'))) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
png(paste0(figures_path,dir_name, '/pl2_n2o_regional_fixedScales.png'), width = 3401.575, height = 3267.717)
print(pl_n2o_regional)
dev.off()



## maps
rm(n2o_diffAbs_regional_all)
for (s_type in c('spp','snr')) {
  n2o_diffAbs_regional = tidyr::pivot_wider(dt$nonco2_luc %>% filter(ghg == 'N2O') %>%
                                              filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                                              mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
                                              select(-c(scen_type,t0,k)) %>%
                                              group_by(region,Units,scenario,ghg,year) %>%
                                              summarise(value = sum(value)),
                                            names_from = 'scenario', values_from = 'value') %>%
    rename_scen() %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches("^snr|^spp")), list(diff = ~ . - St7_Reference)) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-'St7_Reference') %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches("^snr|^spp"), names_to = 'scenario') %>%
    # compute median
    dplyr::mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
    dplyr::group_by(Units,year,scen_type,region) %>%
    dplyr::summarise(median_value = sum(diff)) %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    # merge with GCAM regions
    dplyr::mutate('GCAM Region' = region) %>%
    inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
    # merge with world data
    dplyr::rename('adm0_a3' = 'ISO 3') %>%
    ungroup() %>%
    # fix southAm-Northern
    dplyr::mutate(median_value = ifelse(region == 'South America_Northern', 0, median_value))

  if (exists('n2o_diffAbs_regional_all')) {
    n2o_diffAbs_regional_all = n2o_diffAbs_regional_all %>%
      rbind(n2o_diffAbs_regional)
  } else {
    n2o_diffAbs_regional_all = n2o_diffAbs_regional
  }

  # plot
  pl_n2o_diffAbs_regional_map <- ggplot() +
    # color map by regions
    geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                           dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                           dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                           dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                         n2o_diffAbs_regional, by = 'adm0_a3') %>%
              order_facets(), aes(fill = -median_value)) +
    scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                         mid = '#f7f7f7', midpoint = 0,
                         name = 'Mt') +
    # theme
    guides(fill = guide_colorbar(title.position = "left")) +
    theme_light() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#ffffff",
                                          colour = "#ffffff"),
          legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
          legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
    # facet
    facet_wrap(. ~ scen_type)
  png(paste0(figures_path,dir_name, '/pl2_n2o_diffAbs_regional_map_', s_type, '.png'), width = 3401.575, height = 2267.717)
  print(pl_n2o_diffAbs_regional_map)
  dev.off()


## maps (per diff)
rm(n2o_diffPer_regional_all)
for (s_type in c('spp','snr')) {
  n2o_diffPer_regional = tidyr::pivot_wider(dt$nonco2_luc %>% filter(ghg == 'N2O') %>%
                                              filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                                              mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
                                              select(-c(scen_type,t0,k)) %>%
                                              group_by(region,Units,scenario,ghg,year) %>%
                                              summarise(value = sum(value)),
                                            names_from = 'scenario', values_from = 'value') %>%
    rename_scen() %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches("^snr|^spp")), list(diff = ~ ifelse(St7_Reference != 0, 100 * (. - St7_Reference) / St7_Reference, 0))) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-'St7_Reference') %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches("^snr|^spp"), names_to = 'scenario') %>%
    # compute median
    dplyr::mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
    dplyr::group_by(Units,year,scen_type,region) %>%
    dplyr::summarise(median_value = sum(diff)) %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    # merge with GCAM regions
    dplyr::mutate('GCAM Region' = region) %>%
    inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
    # merge with world data
    dplyr::rename('adm0_a3' = 'ISO 3') %>%
    ungroup() %>%
    # fix southAm-Northern
    dplyr::mutate(median_value = ifelse(region == 'South America_Northern', 0, median_value))

  if (exists('n2o_diffPer_regional_all')) {
    n2o_diffPer_regional_all = n2o_diffPer_regional_all %>%
      rbind(n2o_diffPer_regional)
  } else {
    n2o_diffPer_regional_all = n2o_diffPer_regional
  }

  # plot
  pl_n2o_diffPer_regional_map <- ggplot() +
    # color map by regions
    geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                           dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                           dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                           dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                         n2o_diffPer_regional, by = 'adm0_a3') %>%
              order_facets(), aes(fill = -median_value)) +
    scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                         mid = '#f7f7f7', midpoint = 0,
                         name = 'Percentual difference') +
    # theme
    guides(fill = guide_colorbar(title.position = "left")) +
    theme_light() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#ffffff",
                                          colour = "#ffffff"),
          legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
          legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
    # facet
    facet_wrap(. ~ scen_type)
  png(paste0(figures_path,dir_name, '/pl2_n2o_diffPer_regional_map_', s_type, '.png'), width = 3401.575, height = 2267.717)
  print(pl_n2o_diffPer_regional_map)
  dev.off()
}


pl_n2o_diffPer_regional_map <- ggplot() +
  # color map by regions
  geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                         dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                         dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                         dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                       n2o_diffPer_regional_all, by = 'adm0_a3') %>%
            order_facets(), aes(fill = -median_value)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = 'Percentual difference') +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # facet
  facet_wrap(. ~ scen_type) +
  labs(title = "Global median avoided N2O emissions according to SPP & SNR")

png(paste0(figures_path,dir_name, '/pl_n2o_diffPer_regional_map.png'), width = 3401.575, height = 2267.717)
print(pl_n2o_diffPer_regional_map)
dev.off()

#####

#### Fig: LUC CO2 ================================== DONE
# =============================
## -- LUC CO2 emissions
### WORLD
pl_luc_co2_world = ggplot(data = dt$luc %>%
                            mutate(scen_type = substr(scenario, 1, 3)) %>%
                            group_by(Units,scenario,scen_type,ghg,year) %>%
                            summarise(value = sum(value)) %>%
                            ungroup() %>%
                            rename_scen() %>%
                            dplyr::group_by(year,scen_type) %>%
                            dplyr::mutate(median_value = median(value)) %>%
                            dplyr::mutate(min_value = min(value)) %>%
                            dplyr::mutate(max_value = max(value)) %>%
                            order_facets()) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,scenario), color = interaction(scen_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  # labs
  labs(y = expression(paste(MtC)), x = '', title = expression(paste('Annual World LUC ',CO[2],' emissions (fixed scales)'))) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
png(paste0(figures_path,dir_name, '/pl2_luc_co2_world.png'), width = 3401.575, height = 2267.717)
print(pl_luc_co2_world)
dev.off()

### REGIONAL
## (free scales)
pl_luc_co2_regional = ggplot(data = dt$luc %>%
                               mutate(scen_type = substr(scenario, 1, 3)) %>%
                               group_by(region,Units,scenario,scen_type,ghg,year) %>%
                               summarise(value = sum(value)) %>%
                               ungroup() %>%
                               rename_scen() %>%
                               dplyr::group_by(region,year,scen_type) %>%
                               dplyr::mutate(median_value = median(value)) %>%
                               dplyr::mutate(min_value = min(value)) %>%
                               dplyr::mutate(max_value = max(value)) %>%
                               order_facets()) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,scenario), color = interaction(scen_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(y = expression(paste(MtC)), x = '', title = expression(paste('Annual World LUC ',CO[2],' emissions (fixed scales)'))) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
png(paste0(figures_path,dir_name, '/pl2_luc_co2_regional_freeScales.png'), width = 3401.575, height = 3267.717)
print(pl_luc_co2_regional)
dev.off()

# (fixed scales)
pl_luc_co2_regional = ggplot(data = dt$luc %>%
                               mutate(scen_type = substr(scenario, 1, 3)) %>%
                               group_by(region,Units,scenario,scen_type,ghg,year) %>%
                               summarise(value = sum(value)) %>%
                               ungroup() %>%
                               rename_scen() %>%
                               dplyr::group_by(region,year,scen_type) %>%
                               dplyr::mutate(median_value = median(value)) %>%
                               dplyr::mutate(min_value = min(value)) %>%
                               dplyr::mutate(max_value = max(value)) %>%
                               order_facets()) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,scenario), color = interaction(scen_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = mypal_scen, name = 'Scenario') +
  # scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # facet
  facet_wrap(. ~ region) +
  # labs
  labs(y = expression(paste(MtC)), x = '', title = expression(paste('Annual World LUC ',CO[2],' emissions (fixed scales)'))) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
png(paste0(figures_path,dir_name, '/pl2_luc_co2_regional_fixedScales.png'), width = 3401.575, height = 3267.717)
print(pl_luc_co2_regional)
dev.off()



## maps
rm(luc_diffAbs_regional_all)
for (s_type in c('spp','snr')) {
  luc_diffAbs_regional = tidyr::pivot_wider(dt$luc %>%
                                              filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                                              mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
                                              select(-c(scen_type,t0,k)) %>%
                                              group_by(region,Units,scenario,ghg,year) %>%
                                              summarise(value = sum(value)),
                                            names_from = 'scenario', values_from = 'value') %>%
    rename_scen() %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches("^snr|^spp")), list(diff = ~ . - St7_Reference)) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-'St7_Reference') %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches("^snr|^spp"), names_to = 'scenario') %>%
    # compute median
    dplyr::mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
    dplyr::group_by(Units,year,scen_type,region) %>%
    dplyr::summarise(median_value = sum(diff)) %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    # merge with GCAM regions
    dplyr::mutate('GCAM Region' = region) %>%
    inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
    # merge with world data
    dplyr::rename('adm0_a3' = 'ISO 3') %>%
    ungroup() %>%
    # fix southAm-Northern
    dplyr::mutate(median_value = ifelse(region == 'South America_Northern', 0, median_value))

  if (exists('luc_diffAbs_regional_all')) {
    luc_diffAbs_regional_all = luc_diffAbs_regional_all %>%
      rbind(luc_diffAbs_regional)
  } else {
    luc_diffAbs_regional_all = luc_diffAbs_regional
  }

  # plot
  pl_luc_diffAbs_regional_map <- ggplot() +
    # color map by regions
    geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                           dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                           dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                           dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                         luc_diffAbs_regional, by = 'adm0_a3') %>%
              order_facets(), aes(fill = -median_value)) +
    scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                         mid = '#f7f7f7', midpoint = 0,
                         name = 'MTC') +
    # theme
    guides(fill = guide_colorbar(title.position = "left")) +
    theme_light() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#ffffff",
                                          colour = "#ffffff"),
          legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
          legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
    # facet
    facet_wrap(. ~ scen_type)
  png(paste0(figures_path,dir_name, '/pl2_luc_diffAbs_regional_map_', s_type, '.png'), width = 3401.575, height = 2267.717)
  print(pl_luc_diffAbs_regional_map)
  dev.off()
}


pl_luc_diffAbs_regional_map <- ggplot() +
  # color map by regions
  geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                         dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                         dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                         dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                       luc_diffAbs_regional_all, by = 'adm0_a3') %>%
            order_facets(), aes(fill = -median_value)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = 'MTC') +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # facet
  facet_wrap(. ~ scen_type) +
  labs(title = "Global median avoided LUC CO2 emissions according to SPP & SNR")

png(paste0(figures_path,dir_name, '/pl_luc_diffAbs_regional_map.png'), width = 3401.575, height = 2267.717)
print(pl_luc_diffAbs_regional_map)
dev.off()


## maps (per diff)
rm(luc_diffPer_regional_all)
for (s_type in c('spp','snr')) {
  luc_diffPer_regional = tidyr::pivot_wider(dt$luc %>%
                                              filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                                              mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
                                              select(-c(scen_type,t0,k)) %>%
                                              group_by(region,Units,scenario,ghg,year) %>%
                                              summarise(value = sum(value)),
                                            names_from = 'scenario', values_from = 'value') %>%
    rename_scen() %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches("^snr|^spp")), list(diff = ~ ifelse(St7_Reference != 0, 100 * (. - St7_Reference) / St7_Reference, 0))) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-'St7_Reference') %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches("^snr|^spp"), names_to = 'scenario') %>%
    # compute median
    dplyr::mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
    dplyr::group_by(Units,year,scen_type,region) %>%
    dplyr::summarise(median_value = sum(diff)) %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    # merge with GCAM regions
    dplyr::mutate('GCAM Region' = region) %>%
    inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
    # merge with world data
    dplyr::rename('adm0_a3' = 'ISO 3') %>%
    ungroup() %>%
    # fix southAm-Northern
    dplyr::mutate(median_value = ifelse(region == 'South America_Northern', 0, median_value))

  if (exists('luc_diffPer_regional_all')) {
    luc_diffPer_regional_all = luc_diffPer_regional_all %>%
      rbind(luc_diffPer_regional)
  } else {
    luc_diffPer_regional_all = luc_diffPer_regional
  }

  # plot
  pl_luc_diffPer_regional_map <- ggplot() +
    # color map by regions
    geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                           dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                           dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                           dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                         luc_diffPer_regional, by = 'adm0_a3') %>%
              order_facets(), aes(fill = -median_value)) +
    scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                         mid = '#f7f7f7', midpoint = 0,
                         name = 'Percentual difference') +
    # theme
    guides(fill = guide_colorbar(title.position = "left")) +
    theme_light() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#ffffff",
                                          colour = "#ffffff"),
          legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
          legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
    # facet
    facet_wrap(. ~ scen_type)
  png(paste0(figures_path,dir_name, '/pl2_luc_diffPer_regional_map_', s_type, '.png'), width = 3401.575, height = 2267.717)
  print(pl_luc_diffPer_regional_map)
  dev.off()
}


pl_luc_diffPer_regional_map <- ggplot() +
  # color map by regions
  geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                         dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                         dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                         dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                       luc_diffPer_regional_all, by = 'adm0_a3') %>%
            order_facets(), aes(fill = -median_value)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = 'Percentual difference') +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # facet
  facet_wrap(. ~ scen_type) +
  labs(title = "Global median avoided LUC CO2 emissions according to SPP & SNR")

png(paste0(figures_path,dir_name, '/pl_luc_diffPer_regional_map.png'), width = 3401.575, height = 2267.717)
print(pl_luc_diffPer_regional_map)
dev.off()

#####

#### Fig: land use ================================= DONE
# =============================
### WORLD
# (abs diff)
rm(land_use_diffAbs_world_all)
for (s_type in c('spp','snr')) {
  land_use_diffAbs_world = tidyr::pivot_wider(dt$land_use_world %>%
                                                filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                                                mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
                                                select(-c(scen_type,t0,k)),
                                              names_from = 'scenario', values_from = 'value') %>%
    rename_scen() %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches("^snr|^spp")), list(diff = ~ . - St7_Reference)) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-'St7_Reference') %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches("^snr|^spp"), names_to = 'scenario') %>%
    # compute median
    dplyr::mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
    dplyr::group_by(Units,land_use_type,year,scen_type) %>%
    dplyr::summarise(median_value = sum(diff))

  if (exists('land_use_diffAbs_world_all')) {
    land_use_diffAbs_world_all = land_use_diffAbs_world_all %>%
      rbind(land_use_diffAbs_world)
  } else {
    land_use_diffAbs_world_all = land_use_diffAbs_world
  }

  pl_land_use_diffAbs_world = ggplot(data = order_facets(land_use_diffAbs_world)) +
    geom_area(aes(x = year, y = median_value, fill = land_use_type), alpha = 1) +  # Median area
    geom_hline(aes(yintercept = 0)) +
    scale_fill_manual(values = land_use_scenario_palette, name = 'Land Type',
                      breaks = land_use_order_palette) +
    facet_wrap(. ~ scen_type) +
    # labs
    labs(y = expression(paste('Thous. ', km^2, ' difference')), x = '', title = paste0('Global median land-use abs change between ',s_type,' and Reference')) +
    # theme
    theme_light() +
    theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
          strip.background = element_blank(),
          strip.text = element_text(color = 'black', size = 40),
          strip.text.y = element_text(angle = 0),
          axis.text.x = element_text(size=30),
          axis.text.y = element_text(size=30),
          legend.text = element_text(size = 35),
          legend.title = element_text(size = 40),
          title = element_text(size = 40))
  png(paste0(figures_path,dir_name,"/",'pl2_land_use_diffAbs_world_', s_type, '.png'), width = 3401.575, height = 2267.717)
  print(pl_land_use_diffAbs_world)
  dev.off()

  assign(paste0('plt_landuse_',s_type), pl_land_use_diffAbs_world)
}


pl_land_use_diffAbs_world = ggplot(data = order_facets(land_use_diffAbs_world_all)) +
  geom_area(aes(x = year, y = median_value, fill = land_use_type), alpha = 1) +  # Median area
  geom_hline(aes(yintercept = 0)) +
  scale_fill_manual(values = land_use_scenario_palette, name = 'Land Type',
                    breaks = land_use_order_palette) +
  facet_wrap(. ~ scen_type) +
  # labs
  labs(y = expression(paste('Thous. ', km^2, ' difference')), x = '', title = 'Global median land-use abs change between SPP & SNR and Reference') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
png(paste0(figures_path,dir_name,"/",'pl2_land_use_diffAbs_world.png'), width = 3401.575, height = 2267.717)
print(pl_land_use_diffAbs_world)
dev.off()


### REGIONAL
rm(land_use_diffAbs_regional_all)
for (s_type in c('spp','snr')) {
  land_use_diffAbs_regional = tidyr::pivot_wider(dt$land_use_regional %>%
                                                filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                                                mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
                                                select(-c(scen_type,t0,k)),
                                              names_from = 'scenario', values_from = 'value') %>%
    rename_scen() %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches("^snr|^spp")), list(diff = ~ . - St7_Reference)) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-'St7_Reference') %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches("^snr|^spp"), names_to = 'scenario') %>%
    # compute median
    dplyr::mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
    dplyr::group_by(Units,land_use_type,year,scen_type,region) %>%
    dplyr::summarise(median_value = sum(diff))

  if (exists('land_use_diffAbs_regional_all')) {
    land_use_diffAbs_regional_all = land_use_diffAbs_regional_all %>%
      rbind(land_use_diffAbs_regional)
  } else {
    land_use_diffAbs_regional_all = land_use_diffAbs_regional
  }

  pl_land_use_diffAbs_regional = ggplot(data = order_facets(land_use_diffAbs_regional)) +
    geom_area(aes(x = year, y = median_value, fill = land_use_type), alpha = 1) +  # Median area
    geom_hline(aes(yintercept = 0)) +
    scale_fill_manual(values = land_use_scenario_palette, name = 'Land Type',
                      breaks = land_use_order_palette) +
    facet_grid(region ~ scen_type, scales = 'free') +
    # labs
    labs(y = expression(paste('Thous. ', km^2, ' difference')), x = '', title = paste0('Global median land-use abs change between ',s_type,' and Reference')) +
    # theme
    theme_light() +
    theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
          strip.background = element_blank(),
          strip.text = element_text(color = 'black', size = 40),
          strip.text.y = element_text(angle = 0),
          axis.text.x = element_text(size=30),
          axis.text.y = element_text(size=30),
          legend.text = element_text(size = 35),
          legend.title = element_text(size = 40),
          title = element_text(size = 40))
  png(paste0(figures_path,dir_name,"/",'pl2_land_use_diffAbs_regional_freeS_', s_type, '.png'), width = 3401.575, height = 5267.717)
  print(pl_land_use_diffAbs_regional)
  dev.off()

  assign(paste0('plt_landuse_',s_type), pl_land_use_diffAbs_regional)
}


pl_land_use_diffAbs_regional = ggplot(data = order_facets(land_use_diffAbs_regional_all)) +
  geom_area(aes(x = year, y = median_value, fill = land_use_type), alpha = 1) +  # Median area
  geom_hline(aes(yintercept = 0)) +
  scale_fill_manual(values = land_use_scenario_palette, name = 'Land Type',
                    breaks = land_use_order_palette) +
  facet_grid(region ~ scen_type, scales = 'free') +
  # labs
  labs(y = expression(paste('Thous. ', km^2, ' difference')), x = '', title = 'Global median land-use abs change between SPP & SNR and Reference') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
png(paste0(figures_path,dir_name,"/",'pl2_land_use_diffAbs_regional_freeS.png'), width = 3401.575, height = 5267.717)
print(pl_land_use_diffAbs_regional)
dev.off()

#####

#### Fig: re/de-forestation ======================== DONE
# =============================
### WORLD
# (abs diff)
pl_aforestation_world = ggplot(data = dt$land_use_world %>%
                                 filter(land_use_type == 'Forest') %>%
                                 mutate(scen_type = substr(scenario, 1, 3)) %>%
                                 dplyr::group_by(Units,land_use_type,year,scenario,scen_type) %>%
                                 summarise(value = sum(value)) %>%
                                 dplyr::group_by(Units,land_use_type,year,scen_type) %>%
                                 dplyr::mutate(median_value = median(value),
                                               min_value = min(value),
                                               max_value = max(value)) %>%
                                 order_facets()) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,scenario), color = interaction(scen_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  # labs
  labs(y = expression(paste(MtC)), x = '', title = expression(paste('Annual World aforestation'))) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
png(paste0(figures_path,dir_name, '/pl2_aforestation_world.png'), width = 3401.575, height = 2267.717)
print(pl_aforestation_world)
dev.off()


## MAPS
## -- map (per difference)
rm(forestation_diffAbs_regional_all)
for (s_type in c('spp','snr')) {
  forestation_diffAbs_regional = tidyr::pivot_wider(dt$land_use_regional %>%
                                                      filter(land_use_type == 'Forest') %>%
                                                filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                                                mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
                                                select(-c(scen_type,t0,k)),
                                              names_from = 'scenario', values_from = 'value') %>%
    rename_scen() %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches("^snr|^spp")), list(diff = ~ . - St7_Reference)) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-'St7_Reference') %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches("^snr|^spp"), names_to = 'scenario') %>%
    # compute median
    dplyr::mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
    dplyr::group_by(Units,land_use_type,year,scen_type,region) %>%
    dplyr::summarise(median_value = sum(diff)) %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    # merge with GCAM regions
    dplyr::mutate('GCAM Region' = region) %>%
    inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
    # merge with world data
    dplyr::rename('adm0_a3' = 'ISO 3') %>%
    ungroup() %>%
    # fix southAm-Northern
    dplyr::mutate(median_value = ifelse(region == 'South America_Northern', 0, median_value))

  if (exists('forestation_diffAbs_regional_all')) {
    forestation_diffAbs_regional_all = forestation_diffAbs_regional_all %>%
      rbind(forestation_diffAbs_regional)
  } else {
    forestation_diffAbs_regional_all = forestation_diffAbs_regional
  }

  # plot
  pl_forestation_diffAbs_regional_map <- ggplot() +
    # color map by regions
    geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                           dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                           dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                           dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                         forestation_diffAbs_regional, by = 'adm0_a3') %>%
              order_facets(), aes(fill = median_value)) +
    scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                         mid = '#f7f7f7', midpoint = 0,
                         name = expression(paste('Thous ',km^2))) +
    # theme
    guides(fill = guide_colorbar(title.position = "left")) +
    theme_light() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#ffffff",
                                          colour = "#ffffff"),
          legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
          legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
    # facet
    facet_wrap(. ~ scen_type)
  png(paste0(figures_path,dir_name, '/pl2_forestation_diffAbs_regional_map_', s_type, '.png'), width = 3401.575, height = 2267.717)
  print(pl_forestation_diffAbs_regional_map)
  dev.off()
}


pl_forestation_diffAbs_regional_map <- ggplot() +
  # color map by regions
  geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                         dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                         dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                         dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                       forestation_diffAbs_regional_all, by = 'adm0_a3') %>%
            order_facets(), aes(fill = median_value)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste('Thous ',km^2))) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # facet
  facet_wrap(. ~ scen_type) +
  labs(title = "Global median re-forestation according to SPP & SNR")

png(paste0(figures_path,dir_name, '/pl_forestation_diffAbs_regional_map.png'), width = 3401.575, height = 2267.717)
print(pl_forestation_diffAbs_regional_map)
dev.off()


## -- map (per difference)
rm(forestation_diffPer_regional_all)
for (s_type in c('spp','snr')) {
  forestation_diffPer_regional = tidyr::pivot_wider(dt$land_use_regional %>%
                                                      filter(land_use_type == 'Forest') %>%
                                                      filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                                                      mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
                                                      select(-c(scen_type,t0,k)),
                                                    names_from = 'scenario', values_from = 'value') %>%
    rename_scen() %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches("^snr|^spp")), list(diff = ~ ifelse(St7_Reference != 0, 100*(. - St7_Reference)/St7_Reference, 0))) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-'St7_Reference') %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches("^snr|^spp"), names_to = 'scenario') %>%
    # compute median
    dplyr::mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
    dplyr::group_by(Units,land_use_type,year,scen_type,region) %>%
    dplyr::summarise(median_value = sum(diff)) %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    # merge with GCAM regions
    dplyr::mutate('GCAM Region' = region) %>%
    inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
    # merge with world data
    dplyr::rename('adm0_a3' = 'ISO 3') %>%
    ungroup() %>%
    # fix southAm-Northern
    dplyr::mutate(median_value = ifelse(region == 'South America_Northern', 0, median_value))

  if (exists('forestation_diffPer_regional_all')) {
    forestation_diffPer_regional_all = forestation_diffPer_regional_all %>%
      rbind(forestation_diffPer_regional)
  } else {
    forestation_diffPer_regional_all = forestation_diffPer_regional
  }

  forestation_diffPer_regional = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                         dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                         dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                         dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                       forestation_diffPer_regional, by = 'adm0_a3')
  # plot
  pl_forestation_diffPer_regional_map <- ggplot() +
    # color map by regions
    geom_sf(data = forestation_diffPer_regional %>%
              order_facets(), aes(fill = median_value)) +
    scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                         mid = '#f7f7f7', midpoint = 0,
                         name = expression(paste('Percentual difference'))) +
    # theme
    guides(fill = guide_colorbar(title.position = "left")) +
    theme_light() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#ffffff",
                                          colour = "#ffffff"),
          legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
          legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
    # facet
    facet_wrap(. ~ scen_type)
  png(paste0(figures_path,dir_name, '/pl2_forestation_diffPer_regional_map_', s_type, '.png'), width = 3401.575, height = 2267.717)
  print(pl_forestation_diffPer_regional_map)
  dev.off()
}

pl_forestation_diffPer_regional_map <- ggplot() +
  # color map by regions
  geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                         dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                         dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                         dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                       forestation_diffPer_regional_all, by = 'adm0_a3') %>%
            order_facets(), aes(fill = median_value)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste('Percentual difference'))) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # facet
  facet_wrap(. ~ scen_type) +
  labs(title = "Global median re-forestation according to SPP & SNR")

png(paste0(figures_path,dir_name, '/pl_forestation_diffPer_regional_map.png'), width = 3401.575, height = 2267.717)
print(pl_forestation_diffPer_regional_map)
dev.off()





forestation_diffAbs_regional = tidyr::pivot_wider(land_use_regional %>%
                                                    filter(landleaf %in% c('forest (managed)', 'forest (unmanaged)')),
                                                  names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ . - Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # sum all forest types
  dplyr::group_by(region,Units,land_use_type,year,scenario) %>%
  dplyr::summarise(value = sum(value)) %>%
  # compute median
  dplyr::group_by(region,Units,land_use_type,year) %>%
  dplyr::summarise(median_value = median(value)) %>%
  # filter desired year
  dplyr::filter(year == selected_year) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3')

forestation_diffAbs_regional = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                       dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                       dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                       dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                     forestation_diffAbs_regional, by = 'adm0_a3')
# plot
pl_forestation_diffAbs_regional_map <- ggplot() +
  # color map by regions
  geom_sf(data = forestation_diffAbs_regional, aes(fill = median_value)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste('thous. ',km^2,' difference'))) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # title
  labs(title = paste('Re-forestation (abs difference) in', selected_year))
ggsave(pl_forestation_diffAbs_regional_map, file = paste0(figures_path,dir_name, '/pl2_forestation_diffAbs_regional_map.pdf'), width = 500, height = 300, units = 'mm')


## -- map (per difference)
forestation_diffPer_regional = tidyr::pivot_wider(land_use_regional %>%
                                                    filter(landleaf %in% c('forest (managed)', 'forest (unmanaged)')),
                                                  names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ ifelse(. - Reference != 0, 100*(. - Reference)/Reference, 0))) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # sum all forest types
  dplyr::group_by(region,Units,land_use_type,year,scenario) %>%
  dplyr::summarise(value = sum(value)) %>%
  # compute median
  dplyr::group_by(region,Units,land_use_type,year) %>%
  dplyr::summarise(median_value = median(value)) %>%
  # filter desired year
  dplyr::filter(year == selected_year) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3')

forestation_diffPer_regional = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                       dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                       dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                       dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                     forestation_diffPer_regional, by = 'adm0_a3')
# plot
pl_forestation_diffPer_regional_map <- ggplot() +
  # color map by regions
  geom_sf(data = forestation_diffPer_regional, aes(fill = median_value)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = '% difference') +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # title
  labs(title = paste('Re-forestation (per difference) in', selected_year))
ggsave(pl_forestation_diffPer_regional_map, file = paste0(figures_path,dir_name, '/pl2_forestation_diffPer_regional_map.pdf'), width = 500, height = 300, units = 'mm')


#####

#### Fig: fertilizer demand ======================== DONE
# =============================
## WORLD
pl__fertilizer_demand_world = ggplot(data = dt$fertilizer_consumption_world %>%
                                       mutate(scen_type = substr(scenario, 1, 3)) %>%
                                       group_by(Units,scenario,scen_type,sector,year) %>%
                                       summarise(value = sum(value)) %>%
                                       ungroup() %>%
                                       group_by(Units,scenario,scen_type,year) %>%
                                       summarise(value = sum(value)) %>%
                                       ungroup() %>%
                                       rename_scen() %>%
                                       dplyr::group_by(year,scen_type) %>%
                                       dplyr::mutate(median_value = median(value)) %>%
                                       dplyr::mutate(min_value = min(value)) %>%
                                       dplyr::mutate(max_value = max(value)) %>%
                                       order_facets()) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,scenario), color = interaction(scen_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type)), linewidth = 2, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  # labs
  labs(y = 'Mt N', x = '', title = expression(paste('Annual global fertilizer demand'))) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
png(paste0(figures_path,dir_name, '/pl2_fertilizer_demand_world.png'), width = 3401.575, height = 2267.717)
print(pl__fertilizer_demand_world)
dev.off()

pl__fertilizer_demand_world = ggplot(data = dt$fertilizer_consumption_world %>%
                                       mutate(scen_type = substr(scenario, 1, 3)) %>%
                                       group_by(Units,scenario,scen_type,sector,year) %>%
                                       summarise(value = sum(value)) %>%
                                       ungroup() %>%
                                       rename_scen() %>%
                                       dplyr::group_by(year,scen_type,sector) %>%
                                       dplyr::mutate(median_value = median(value)) %>%
                                       dplyr::mutate(min_value = min(value)) %>%
                                       dplyr::mutate(max_value = max(value)) %>%
                                       order_facets()) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,scenario), color = interaction(scen_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type)), linewidth = 2, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  # facet
  facet_wrap(. ~ sector, ncol = 4) +
  # labs
  labs(y = 'Mt N', x = '', title = expression(paste('Annual global sectorial fertilizer demand'))) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
png(paste0(figures_path,dir_name, '/pl2_fertilizer_demand_bySector_world.png'), width = 3401.575, height = 2267.717)
print(pl__fertilizer_demand_world)
dev.off()


## REGIONAL
pl__fertilizer_demand_regional = ggplot(data = dt$fertilizer_consumption_regional %>%
                                       mutate(scen_type = substr(scenario, 1, 3)) %>%
                                       group_by(Units,scenario,scen_type,sector,year,region) %>%
                                       summarise(value = sum(value)) %>%
                                       ungroup() %>%
                                       group_by(Units,scenario,scen_type,year,region) %>%
                                       summarise(value = sum(value)) %>%
                                       ungroup() %>%
                                       rename_scen() %>%
                                       dplyr::group_by(year,scen_type,region) %>%
                                       dplyr::mutate(median_value = median(value)) %>%
                                       dplyr::mutate(min_value = min(value)) %>%
                                       dplyr::mutate(max_value = max(value)) %>%
                                       order_facets()) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,scenario), color = interaction(scen_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type)), linewidth = 2, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  # facet
  facet_wrap(. ~ region, ncol = 5) +
  # labs
  labs(y = expression(paste(MtC)), x = '', title = expression(paste('Annual global fertilizer demand'))) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
png(paste0(figures_path,dir_name, '/pl2_fertilizer_demand_regional.png'), width = 3401.575, height = 3267.717)
print(pl__fertilizer_demand_regional)
dev.off()

pl__fertilizer_demand_regional = ggplot(data = dt$fertilizer_consumption_regional %>%
                                       mutate(scen_type = substr(scenario, 1, 3)) %>%
                                       group_by(Units,scenario,scen_type,sector,year,region) %>%
                                       summarise(value = sum(value)) %>%
                                       ungroup() %>%
                                       rename_scen() %>%
                                       dplyr::group_by(year,scen_type,sector,region) %>%
                                       dplyr::mutate(median_value = median(value)) %>%
                                       dplyr::mutate(min_value = min(value)) %>%
                                       dplyr::mutate(max_value = max(value)) %>%
                                       order_facets()) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,scenario), color = interaction(scen_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type)), linewidth = 2, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario') +
  # facet
  facet_grid(region ~ sector) +
  # labs
  labs(y = expression(paste(MtC)), x = '', title = expression(paste('Annual global sectorial fertilizer demand'))) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
png(paste0(figures_path,dir_name, '/pl2_fertilizer_demand_bySector_regional.png'), width = 3401.575, height = 3267.717)
print(pl__fertilizer_demand_regional)
dev.off()


## maps
rm(fertilizer_demand_diffAbs_regional_all)
for (s_type in c('spp','snr')) {
  fertilizer_demand_diffAbs_regional = tidyr::pivot_wider(dt$fertilizer_consumption_regional %>%
                                                      filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                                                      mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
                                                      select(-c(scen_type,t0,k)),
                                                    names_from = 'scenario', values_from = 'value') %>%
    rename_scen() %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches("^snr|^spp")), list(diff = ~ . - St7_Reference)) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-'St7_Reference') %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches("^snr|^spp"), names_to = 'scenario') %>%
    # compute median
    dplyr::mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
    dplyr::group_by(Units,year,scen_type,region) %>%
    dplyr::summarise(median_value = sum(diff)) %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    # merge with GCAM regions
    dplyr::mutate('GCAM Region' = region) %>%
    inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
    # merge with world data
    dplyr::rename('adm0_a3' = 'ISO 3') %>%
    ungroup() %>%
    # fix southAm-Northern
    dplyr::mutate(median_value = ifelse(region == 'South America_Northern', 0, median_value))

  if (exists('fertilizer_demand_diffAbs_regional_all')) {
    fertilizer_demand_diffAbs_regional_all = fertilizer_demand_diffAbs_regional_all %>%
      rbind(fertilizer_demand_diffAbs_regional)
  } else {
    fertilizer_demand_diffAbs_regional_all = fertilizer_demand_diffAbs_regional
  }

  fertilizer_demand_diffAbs_regional = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                         dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                         dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                         dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                       fertilizer_demand_diffAbs_regional, by = 'adm0_a3')
  # plot
  pl_fertilizer_demand_diffAbs_regional_map <- ggplot() +
    # color map by regions
    geom_sf(data = fertilizer_demand_diffAbs_regional %>%
              order_facets(), aes(fill = -median_value)) +
    scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                         mid = '#f7f7f7', midpoint = 0,
                         name = 'Mt N') +
    # theme
    guides(fill = guide_colorbar(title.position = "left")) +
    theme_light() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#ffffff",
                                          colour = "#ffffff"),
          legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
          legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
    # facet
    facet_wrap(. ~ scen_type)
  png(paste0(figures_path,dir_name, '/pl2_fertilizer_demand_diffAbs_regional_map_', s_type, '.png'), width = 3401.575, height = 2267.717)
  print(pl_fertilizer_demand_diffAbs_regional_map)
  dev.off()
}


pl_fertilizer_demand_diffAbs_regional_map <- ggplot() +
  # color map by regions
  geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                         dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                         dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                         dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                       fertilizer_demand_diffAbs_regional_all, by = 'adm0_a3') %>%
            order_facets(), aes(fill = -median_value)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = 'Mt N') +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # facet
  facet_wrap(. ~ scen_type) +
  labs(title = "Global median avoided fertilizer demand according to SPP & SNR")

png(paste0(figures_path,dir_name, '/pl_fertilizer_demand_diffAbs_regional_map.png'), width = 3401.575, height = 2267.717)
print(pl_fertilizer_demand_diffAbs_regional_map)
dev.off()

## maps (per diff)
rm(fertilizer_demand_diffPer_regional_all)
for (s_type in c('spp','snr')) {
  fertilizer_demand_diffPer_regional = tidyr::pivot_wider(dt$fertilizer_consumption_regional %>%
                                                      filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                                                      mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
                                                      select(-c(scen_type,t0,k)),
                                                    names_from = 'scenario', values_from = 'value') %>%
    rename_scen() %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches("^snr|^spp")), list(diff = ~ ifelse(St7_Reference != 0, 100 * (. - St7_Reference)/St7_Reference, 0))) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-'St7_Reference') %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches("^snr|^spp"), names_to = 'scenario') %>%
    # compute median
    dplyr::mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
    dplyr::group_by(Units,year,scen_type,region) %>%
    dplyr::summarise(median_value = sum(diff)) %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    # merge with GCAM regions
    dplyr::mutate('GCAM Region' = region) %>%
    inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
    # merge with world data
    dplyr::rename('adm0_a3' = 'ISO 3') %>%
    ungroup() %>%
    # fix southAm-Northern
    dplyr::mutate(median_value = ifelse(region == 'South America_Northern', 0, median_value))

  if (exists('fertilizer_demand_diffPer_regional_all')) {
    fertilizer_demand_diffPer_regional_all = fertilizer_demand_diffPer_regional_all %>%
      rbind(fertilizer_demand_diffPer_regional)
  } else {
    fertilizer_demand_diffPer_regional_all = fertilizer_demand_diffPer_regional
  }

  fertilizer_demand_diffPer_regional = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                         dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                         dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                         dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                       fertilizer_demand_diffPer_regional, by = 'adm0_a3')
  # plot
  pl_fertilizer_demand_diffPer_regional_map <- ggplot() +
    # color map by regions
    geom_sf(data = fertilizer_demand_diffPer_regional %>%
              order_facets(), aes(fill = -median_value)) +
    scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                         mid = '#f7f7f7', midpoint = 0,
                         name = 'Percentual difference') +
    # theme
    guides(fill = guide_colorbar(title.position = "left")) +
    theme_light() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#ffffff",
                                          colour = "#ffffff"),
          legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
          legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
    # facet
    facet_wrap(. ~ scen_type)
  png(paste0(figures_path,dir_name, '/pl2_fertilizer_demand_diffPer_regional_map_', s_type, '.png'), width = 3401.575, height = 2267.717)
  print(pl_fertilizer_demand_diffPer_regional_map)
  dev.off()
}


pl_fertilizer_demand_diffPer_regional_map <- ggplot() +
  # color map by regions
  geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                         dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                         dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                         dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                       fertilizer_demand_diffPer_regional_all, by = 'adm0_a3') %>%
            order_facets(), aes(fill = -median_value)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = 'Percentual difference') +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # facet
  facet_wrap(. ~ scen_type) +
  labs(title = "Global median avoided fertilizer demand according to SPP & SNR")

png(paste0(figures_path,dir_name, '/pl_fertilizer_demand_diffPer_regional_map.png'), width = 3401.575, height = 2267.717)
print(pl_fertilizer_demand_diffPer_regional_map)
dev.off()

#####

#### Fig: irr vs rfd water ========================= DONE
# =============================
### WORLD
# irr vs rfd trend
for (w_type in c('IRR', 'RFD')) {
  pl_water_irr_rfd_world = ggplot(data = dt$water_irr_rfd_world %>%
                                    filter(water == w_type) %>%
                                    mutate(scen_type = substr(scen_type, 1, 3)) %>%
                                    group_by(Units, scenario, year, water) %>%
                                    mutate(value = sum(value)) %>%
                                    ungroup() %>%
                                    rename_scen() %>%
                                    dplyr::group_by(year, scen_type, water) %>%
                                    dplyr::mutate(median_value = median(value)) %>%
                                    dplyr::mutate(min_value = min(value)) %>%
                                    dplyr::mutate(max_value = max(value)) %>%
                                    order_facets()) +
    geom_line(aes(x = year, y = value, group = interaction(scen_type,scenario,water), color = interaction(water,scen_type)), alpha = 0.3) +  # All runs lines
    geom_line(aes(x = year, y = median_value, color = interaction(water,scen_type)), linewidth = 1.5, alpha = 1) +  # Median line
    geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(water,scen_type)), alpha = 0.15) +  # Shadow
    scale_color_manual(values = irr_rfd_scenario_palette, name = 'Scenario', labels = irr_rfd_scenario.labs,
                       drop = FALSE) +
    scale_fill_manual(values = irr_rfd_scenario_palette, name = 'Scenario', labels = irr_rfd_scenario.labs,
                      drop = FALSE) +
    # labs
    labs(y = expression(paste('thous.',km^2)), x = '', title = paste('Annual World IRR and RFD water consumption')) +
    # theme
    theme_light() +
    theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
          strip.background = element_blank(),
          strip.text = element_text(color = 'black', size = 40),
          strip.text.y = element_text(angle = 0),
          axis.text.x = element_text(size=30),
          axis.text.y = element_text(size=30),
          legend.text = element_text(size = 35),
          legend.title = element_text(size = 40),
          title = element_text(size = 40))
  if (w_type == 'IRR') {
    pl_water_irr_rfd_world = pl_water_irr_rfd_world +
      geom_text(aes(x = 2010, y = 2075, label = 'IRR'), color = "black", size = 13)
  } else {
    pl_water_irr_rfd_world = pl_water_irr_rfd_world +
      geom_text(aes(x = 2010, y = 9555, label = 'RFD'), color = "black", size = 13)
  }

  png(paste0(figures_path,dir_name,"/",'pl2_water_',w_type,'_world.png'), width = 3401.575, height = 2267.717)
  print(pl_water_irr_rfd_world)
  dev.off()

  assign(paste0('plt_water_',w_type), pl_water_irr_rfd_world)
}


pl_water_irr_rfd_world = ggplot(data = dt$water_irr_rfd_world %>%
                                  mutate(scen_type = substr(scen_type, 1, 3)) %>%
                                  group_by(Units, scenario, year, water) %>%
                                  mutate(value = sum(value)) %>%
                                  ungroup() %>%
                                  rename_scen() %>%
                                  dplyr::group_by(year, scen_type, water) %>%
                                  dplyr::mutate(median_value = median(value)) %>%
                                  dplyr::mutate(min_value = min(value)) %>%
                                  dplyr::mutate(max_value = max(value)) %>%
                                  order_facets()) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,scenario,water), color = interaction(water,scen_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(water,scen_type)), linewidth = 1.5, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(water,scen_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = irr_rfd_scenario_palette, name = 'Scenario', labels = irr_rfd_scenario.labs,
                     drop = FALSE) +
  scale_fill_manual(values = irr_rfd_scenario_palette, name = 'Scenario', labels = irr_rfd_scenario.labs,
                    drop = FALSE) +
  # labs
  labs(y = expression(paste('thous.',km^2)), x = '', title = paste('Annual World IRR and RFD water consumption')) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  geom_text(aes(x = 2010, y = 2075, label = 'IRR'), color = "black", size = 13) +
  geom_text(aes(x = 2010, y = 9555, label = 'RFD'), color = "black", size = 13)

pl_water_irr_rfd_world_up = pl_water_irr_rfd_world + ylim(8500,11500)
pl_water_irr_rfd_world_down = pl_water_irr_rfd_world + ylim(1900,2200)

pl_water_irr_rfd_world = ggpubr::ggarrange(
  pl_water_irr_rfd_world_up + theme(plot.title = element_blank()),
  pl_water_irr_rfd_world_down + theme(plot.title = element_blank()),
  ncol = 1, align = "v", common.legend = TRUE, heights = c(4,1),
  legend = 'bottom'
)
pl_water_irr_rfd_world = ggpubr::annotate_figure(pl_water_irr_rfd_world,
                                                 top = ggpubr::text_grob("Annual World IRR and RFD water consumption",
                                                                         color = "black", size = 40))


png(paste0(figures_path,dir_name, '/pl_water_irr_rfd_world.png'), width = 3401.575, height = 3067.717)
print(pl_water_irr_rfd_world)
dev.off()

## irr vs rfd ratio
pl_water_share_world = ggplot(data = dt$water_irr_rfd_world %>%
                                mutate(scen_type = substr(scen_type, 1, 3)) %>%
                                group_by(Units, scenario, scen_type, year, water) %>%
                                summarise(value = sum(value)) %>%
                                ungroup() %>%
                                rename_scen() %>%
                                tidyr::pivot_wider(names_from = water, values_from = value) %>%
                                dplyr::mutate(share = 100 * RFD / (RFD + IRR)) %>%
                                dplyr::group_by(year, scen_type) %>%
                                dplyr::mutate(median_value = median(share)) %>%
                                dplyr::mutate(min_value = min(share)) %>%
                                dplyr::mutate(max_value = max(share)) %>%
                                ungroup() %>%
                                order_facets()) +
  geom_line(aes(x = year, y = share, group = interaction(scen_type,scenario), color = interaction(scen_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario', labels = scen_palette_refVsSppVsSnr.labs,
                     drop = FALSE) +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario', labels = scen_palette_refVsSppVsSnr.labs,
                    drop = FALSE) +
  # labs
  labs(y = 'Unitless', x = '', title = paste('Annual World RFD water percentage (from total water consumption)')) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))

png(paste0(figures_path,dir_name, '/pl2_water_shareRFD_world.png'), width = 3401.575, height = 3001.575) #2267.717
print(pl_water_share_world)
dev.off()


## maps
rm(water_irr_reduction_diffAbs_regional_all)
for (s_type in c('spp','snr')) {
  water_irr_reduction_diffAbs_regional = tidyr::pivot_wider(dt$water_irr_rfd_regional %>%
                                                              filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                                                              mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
                                                              select(-c(scen_type,t0,k)) %>%
                                                              # compute total irr - rfd consumption
                                                              dplyr::group_by(Units,year,scenario,region,water) %>%
                                                              dplyr::summarise(value = sum(value)) %>%
                                                              # consider only IRR water
                                                              tidyr::pivot_wider(names_from = water, values_from = value) %>%
                                                              dplyr::mutate(value = IRR) %>%
                                                              select(-c(IRR,RFD)),
                                                          names_from = 'scenario', values_from = 'value') %>%
    rename_scen() %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches("^snr|^spp")), list(diff = ~ . - St7_Reference)) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-'St7_Reference') %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches("^snr|^spp"), names_to = 'scenario') %>%
    # compute scen_type
    dplyr::mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    # merge with GCAM regions
    dplyr::mutate('GCAM Region' = region) %>%
    inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
    # merge with world data
    dplyr::rename('adm0_a3' = 'ISO 3') %>%
    ungroup() %>%
    # fix southAm-Northern
    dplyr::mutate(diff = ifelse(region == 'South America_Northern', 0, diff))

  if (exists('water_irr_reduction_diffAbs_regional_all')) {
    water_irr_reduction_diffAbs_regional_all = water_irr_reduction_diffAbs_regional_all %>%
      rbind(water_irr_reduction_diffAbs_regional)
  } else {
    water_irr_reduction_diffAbs_regional_all = water_irr_reduction_diffAbs_regional
  }

  water_irr_reduction_diffAbs_regional = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                               dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                               dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                               dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                             water_irr_reduction_diffAbs_regional, by = 'adm0_a3')
  # plot
  pl_water_irr_reduction_diffAbs_regional_map <- ggplot() +
    # color map by regions
    geom_sf(data = water_irr_reduction_diffAbs_regional %>%
              order_facets(), aes(fill = -diff)) +
    scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                         mid = '#f7f7f7', midpoint = 0,
                         name = expression(paste('Thous ', km^2))) +
    # theme
    guides(fill = guide_colorbar(title.position = "left")) +
    theme_light() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#ffffff",
                                          colour = "#ffffff"),
          legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
          legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
    # facet
    facet_wrap(. ~ scen_type)
  png(paste0(figures_path,dir_name, '/pl2_water_irr_reduction_diffAbs_regional_map_', s_type, '.png'), width = 3401.575, height = 2267.717)
  print(pl_water_irr_reduction_diffAbs_regional_map)
  dev.off()
}


pl_water_irr_reduction_diffAbs_regional_map <- ggplot() +
  # color map by regions
  geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                         dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                         dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                         dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                       water_irr_reduction_diffAbs_regional_all, by = 'adm0_a3') %>%
            order_facets(), aes(fill = -diff)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste('Thous ', km^2))) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # facet
  facet_wrap(. ~ scen_type) +
  labs(title = "Global median avoided IRR water demand according to SPP & SNR")

png(paste0(figures_path,dir_name, '/pl_water_irr_reduction_diffAbs_regional_map.png'), width = 3401.575, height = 2267.717)
print(pl_water_irr_reduction_diffAbs_regional_map)
dev.off()

## maps (per diff)
rm(water_irr_reduction_diffPer_regional_all)
for (s_type in c('spp','snr')) {
  water_irr_reduction_diffPer_regional = tidyr::pivot_wider(dt$water_irr_rfd_regional %>%
                                                              filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                                                              mutate(scenario = ifelse(scen_type == 'St7_Reference', 'St7_Reference', scenario)) %>%
                                                              select(-c(scen_type,t0,k)) %>%
                                                              # compute total irr - rfd consumption
                                                              dplyr::group_by(Units,year,scenario,region,water) %>%
                                                              dplyr::summarise(value = sum(value)) %>%
                                                              # compute irr share (%)
                                                              tidyr::pivot_wider(names_from = water, values_from = value) %>%
                                                              # dplyr::mutate(value = 100 * IRR / (RFD + IRR)) %>%
                                                              mutate(value = IRR) %>%
                                                              select(-c(IRR,RFD)),
                                                          names_from = 'scenario', values_from = 'value') %>%
    rename_scen() %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches("^snr|^spp")), list(diff = ~ ifelse(St7_Reference != 0, 100 * (. - St7_Reference)/St7_Reference, 0))) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-'St7_Reference') %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches("^snr|^spp"), names_to = 'scenario') %>%
    # compute scen_type
    dplyr::mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    # merge with GCAM regions
    dplyr::mutate('GCAM Region' = region) %>%
    inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
    # merge with world data
    dplyr::rename('adm0_a3' = 'ISO 3') %>%
    ungroup() %>%
    # fix southAm-Northern
    dplyr::mutate(diff = ifelse(region == 'South America_Northern', 0, diff))

  if (exists('water_irr_reduction_diffPer_regional_all')) {
    water_irr_reduction_diffPer_regional_all = water_irr_reduction_diffPer_regional_all %>%
      rbind(water_irr_reduction_diffPer_regional)
  } else {
    water_irr_reduction_diffPer_regional_all = water_irr_reduction_diffPer_regional
  }

  water_irr_reduction_diffPer_regional = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                               dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                               dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                               dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                             water_irr_reduction_diffPer_regional, by = 'adm0_a3')
  # plot
  pl_water_irr_reduction_diffPer_regional_map <- ggplot() +
    # color map by regions
    geom_sf(data = water_irr_reduction_diffPer_regional %>%
              order_facets(), aes(fill = -diff)) +
    scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                         mid = '#f7f7f7', midpoint = 0,
                         name = 'Avoided percentage') +
    # theme
    guides(fill = guide_colorbar(title.position = "left")) +
    theme_light() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#ffffff",
                                          colour = "#ffffff"),
          legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
          legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
    # facet
    facet_wrap(. ~ scen_type)
  png(paste0(figures_path,dir_name, '/pl2_water_irr_reduction_diffPer_regional_map_', s_type, '.png'), width = 3401.575, height = 2267.717)
  print(pl_water_irr_reduction_diffPer_regional_map)
  dev.off()
}


pl_water_irr_reduction_diffPer_regional_map <- ggplot() +
  # color map by regions
  geom_sf(data = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                         dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                         dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                         dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                       water_irr_reduction_diffPer_regional_all, by = 'adm0_a3') %>%
            order_facets(), aes(fill = -diff)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = 'Avoided percentage') +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # facet
  facet_wrap(. ~ scen_type) +
  labs(title = "Global median avoided IRR water demand according to SPP & SNR")

png(paste0(figures_path,dir_name, '/pl_water_irr_reduction_diffPer_regional_map.png'), width = 3401.575, height = 2267.717)
print(pl_water_irr_reduction_diffPer_regional_map)
dev.off()



## water total
water_irr_rfd_world = dt$water_irr_rfd_world %>%
  filter(scen_type != 'St7_Reference') %>%
  rbind(dt$water_irr_rfd_world %>%
        filter(scen_type == 'St7_Reference') %>%
        group_by(Units, year, water, crop) %>%
        mutate(t0 = NA, k = NA, scenario = 'St7_Reference',
               value = mean(value)) %>%
        ungroup() %>%
        distinct())

pl_water_world = ggplot(data = water_irr_rfd_world %>%
                          mutate(scen_type = substr(scen_type, 1, 3)) %>%
                          group_by(Units, scenario, scen_type, year) %>%
                          summarise(value = sum(value)) %>%
                          ungroup() %>%
                          rename_scen() %>%
                          dplyr::group_by(year, scen_type) %>%
                          dplyr::mutate(median_value = median(value)) %>%
                          dplyr::mutate(min_value = min(value)) %>%
                          dplyr::mutate(max_value = max(value)) %>%
                          order_facets()) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,scenario), color = interaction(scen_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario', labels = scen_palette_refVsSppVsSnr.labs,
                     drop = FALSE) +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario', labels = scen_palette_refVsSppVsSnr.labs,
                    drop = FALSE) +
  # labs
  labs(y = expression(paste('thous.',km^2)), x = '', title = paste('Annual World water consumption')) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))

png(paste0(figures_path,dir_name, '/pl2_water_world.png'), width = 3401.575, height = 2267.717)
print(pl_water_world)
dev.off()

### REGIONAL
# irr vs rfd trend
pl_water_share_regional = ggplot(data = dt$water_irr_rfd_regional %>%
                                mutate(scen_type = substr(scen_type, 1, 3)) %>%
                                group_by(Units, scenario, scen_type, year, water, region) %>%
                                summarise(value = sum(value)) %>%
                                ungroup() %>%
                                rename_scen() %>%
                                tidyr::pivot_wider(names_from = water, values_from = value) %>%
                                dplyr::mutate(share = 100 * RFD / (RFD + IRR)) %>%
                                dplyr::group_by(year, scen_type, region) %>%
                                dplyr::mutate(median_value = median(share)) %>%
                                dplyr::mutate(min_value = min(share)) %>%
                                dplyr::mutate(max_value = max(share)) %>%
                                ungroup() %>%
                                order_facets()) +
  geom_line(aes(x = year, y = share, group = interaction(scen_type,scenario), color = interaction(scen_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario', labels = scen_palette_refVsSppVsSnr.labs,
                     drop = FALSE) +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario', labels = scen_palette_refVsSppVsSnr.labs,
                    drop = FALSE) +
  # facet
  facet_wrap(. ~ region) +
  # labs
  labs(y = 'Unitless', x = '', title = paste('Annual Regional RFD water percentage (from total water consumption)')) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))

png(paste0(figures_path,dir_name, '/pl2_water_shareRFD_regional.png'), width = 3401.575, height = 3267.717)
print(pl_water_share_regional)
dev.off()


# total water consumption regional
water_irr_rfd_regional = dt$water_irr_rfd_regional %>%
  filter(scen_type != 'St7_Reference') %>%
  rbind(dt$water_irr_rfd_regional %>%
          filter(scen_type == 'St7_Reference') %>%
          group_by(Units, year, water, crop, region) %>%
          mutate(t0 = NA, k = NA, scenario = 'St7_Reference',
                 value = mean(value)) %>%
          ungroup() %>%
          distinct())

pl_water_regional = ggplot(data = water_irr_rfd_regional %>%
                          mutate(scen_type = substr(scen_type, 1, 3)) %>%
                          group_by(Units, scenario, scen_type, year, region) %>%
                          summarise(value = sum(value)) %>%
                          ungroup() %>%
                          rename_scen() %>%
                          dplyr::group_by(year, scen_type, region) %>%
                          dplyr::mutate(median_value = median(value)) %>%
                          dplyr::mutate(min_value = min(value)) %>%
                          dplyr::mutate(max_value = max(value)) %>%
                          order_facets()) +
  geom_line(aes(x = year, y = value, group = interaction(scen_type,scenario), color = interaction(scen_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scen_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scen_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario', labels = scen_palette_refVsSppVsSnr.labs,
                     drop = FALSE) +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario', labels = scen_palette_refVsSppVsSnr.labs,
                    drop = FALSE) +
  # facet
  facet_wrap(. ~ region) +
  # labs
  labs(y = expression(paste('thous.',km^2)), x = '', title = paste('Annual regional water consumption')) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))

png(paste0(figures_path,dir_name, '/pl2_water_regional.png'), width = 3401.575, height = 2267.717)
print(pl_water_regional)
dev.off()




# waterfall
for (s_type in c('spp','snr')) {
  pld_water_irr_rfd_world = tidyr::pivot_wider(dt$water_irr_rfd_world %>%
                                                 filter(scenario %in% c(get(paste0('ref_scen_',s_type)), get(paste0('base_scen_',s_type)))) %>%
                                                 mutate(scenario = ifelse(scenario == get(paste0('ref_scen_',s_type)), 'St7_Reference', scenario)) %>%
                                                 select(-c(t0,k,scen_type)),
                                               names_from = 'scenario', values_from = 'value') %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches('^spp|^snr')), list(diff = ~ (. - St7_Reference))) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select('water','year','crop','diff') %>%
    # reshape dataset
    tidyr::pivot_longer(cols = 'diff', names_to = 'scenario') %>%
    # compute median by region and crop_name
    group_by(year, water, crop) %>%
    summarise(value = median(value)) %>%
    ungroup() %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    mutate(crop = tolower(crop)) %>%
    data.table::as.data.table()


  #irr
  dat = pld_water_irr_rfd_world %>%
    filter(water == 'IRR') %>%
    select(x = crop, y = value) %>%
    data.table::as.data.table()
  dat <- dat[order(dat$x), ]
  dat = dat %>%
    mutate(cum_sum = cumsum(y),
           cum_sum = ifelse(abs(y) > 1, cum_sum/2 + (cum_sum-y)/2, cum_sum + abs(y) + nchar(x)/2))


  pl_water_irr_world = waterfalls::waterfall(.data = dat, rect_text_labels = rep('',length(dat$x)), total_rect_text = 'Abs',
                                 calc_total = FALSE, fill_by_sign = FALSE, fill_colours = color_by_sign(dat$y)) +
    # text
    geom_text(data = dat, aes(x = x, y = cum_sum, label = x),
              angle = 90, color = "black", size = 7) +
    # labs
    labs(x = '', y = expression(paste('thous.',km^2)), x = '', title = paste('Annual World IRR and RFD water consumption by crop (free scales)')) +
    # theme
    theme_light() +
    theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
          strip.background = element_blank(),
          strip.text = element_text(color = 'black', size = 40),
          strip.text.y = element_text(angle = 0),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size=30),
          legend.text = element_text(size = 35),
          legend.title = element_text(size = 40),
          title = element_text(size = 40))

  # rfd
  dat = pld_water_irr_rfd_world %>%
    filter(water == 'RFD') %>%
    select(x = crop, y = value) %>%
    data.table::as.data.table()
  dat <- dat[order(dat$x), ]
  dat = dat %>%
    mutate(cum_sum = cumsum(y),
           cum_sum = ifelse(abs(y) > 10, cum_sum/2 + (cum_sum-y)/2, cum_sum + abs(y) + nchar(x)/2))

  pl_water_rfd_world = waterfalls::waterfall(.data = na.omit(dat), rect_text_labels = rep('',length(dat$x)), total_rect_text = 'Abs',
                                 calc_total = FALSE, fill_by_sign = FALSE, fill_colours = color_by_sign(dat$y)) +
    # text
    geom_text(data = dat, aes(x = x, y = cum_sum, label = x),
              angle = 90, color = "black", size = 7) +
    # labs
    labs(x = '', y = expression(paste('thous.',km^2)), x = '', title = paste('Annual World IRR and RFD water consumption by crop (free scales)')) +
    # theme
    theme_light() +
    theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
          strip.background = element_blank(),
          strip.text = element_text(color = 'black', size = 40),
          strip.text.y = element_text(angle = 0),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size=30),
          legend.text = element_text(size = 35),
          legend.title = element_text(size = 40),
          title = element_text(size = 40))

  if (s_type == 'snr') {
    pl_water_irr_world = pl_water_irr_world + ylim(0,65)
    pl_water_rfd_world = pl_water_rfd_world + ylim(0,65)
  } else {
    pl_water_irr_world = pl_water_irr_world + ylim(-200,170)
    pl_water_rfd_world = pl_water_rfd_world + ylim(-200,170)
  }
  pl_water_irr_rfd_waterfall = cowplot::ggdraw() +
    cowplot::draw_plot(pl_water_irr_world + labs(title = 'IRR'),
                       x = 0, y = 0, width = 0.49, height = 0.90) +
    cowplot::draw_plot(pl_water_rfd_world + labs(title = 'RFD', y = ''),
                       x = 0.5, y = 0, width = 0.49, height = 0.90) +
    cowplot::draw_plot_label(label = paste0("Annual World IRR and RFD abs difference (",toupper(s_type)," - REF)"), size = 45,
                             x = -0.25, y = 0.99)
  png(paste0(figures_path,dir_name, '/pl2_water_irr_rfd_waterfall_',s_type,'.png'), width = 2401.575, height = 1267.717)
  print(pl_water_irr_rfd_waterfall)
  dev.off()
}

#####

#### Fig: crop loss ================================ DONE
# =============================
### MAPS
## -- map (abs difference)
rm(croploss_diffPer_regional_all)
for (s_type in c('spp','snr')) {
  croploss_diffPer_regional = tidyr::pivot_wider(dt.croploss %>%
                                               filter(scenario %in% c(get(paste0('ref_scen_',s_type)),get(paste0('base_scen_',s_type)))) %>%
                                               mutate(scenario = ifelse(startsWith(scenario, 'St7_Reference'), 'St7_Reference', scenario)) %>%
                                               # consider mean crop loss from wheat, rice, maize, and soy
                                               group_by(region, year, scenario, pollutant) %>%
                                               summarise(value = median(value)) %>%
                                               # add crop losses
                                               group_by(region, year, scenario) %>%
                                               summarise(value = sum(value)) %>%
                                               rename(fasst_region = region) %>%
                                               ungroup(),
                                             names_from = 'scenario', values_from = 'value') %>%
    rename_scen() %>%
    # compute difference between Reference and runs
    dplyr::mutate_at(vars(matches("^snr|^spp")), list(diff = ~ . - St7_Reference)) %>%
    # mutate(per_diff = 100*diff/St7_Reference)
    dplyr::filter(!is.na(diff)) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-matches("^snr|^spp|^St7")) %>%
    # add a column with the scenario type
    mutate(scen_type = s_type) %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    # merge with GCAM regions
    left_join(rfasst::fasst_reg %>%
                dplyr::rename('ISO3' = 'subRegionAlt'), by = 'fasst_region',
              multiple = 'all') %>%
    # merge with world data
    dplyr::rename('adm0_a3' = 'ISO3')

  if (exists('croploss_diffPer_regional_all')) {
    croploss_diffPer_regional_all = croploss_diffPer_regional_all %>%
      rbind(croploss_diffPer_regional)
  } else {
    croploss_diffPer_regional_all = croploss_diffPer_regional
  }

  croploss_diffPer_regional = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                  dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                  dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                  dplyr::filter(!adm0_a3 %in% c("ATA","FJI")) %>%
                                  rowwise(),
                                croploss_diffPer_regional, by = 'adm0_a3')

  pl_croploss_diffPer_regional_map <- ggplot() +
    # color map by regions
    geom_sf(data = order_facets(croploss_diffPer_regional), aes(fill = -diff)) +
    scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                         mid = '#f7f7f7', midpoint = 0,
                         name = expression(paste("Avoided relative crop loss (%)","\n"))) +
    # theme
    guides(fill = guide_colorbar(title.position = "left")) +
    theme_light() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#ffffff",
                                          colour = "#ffffff"),
          legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
          legend.text = element_text(size = 30), legend.title = element_text(size = 30, vjust = 0.95),
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
    # title
    labs(title = paste0("Annual avoided relative crop loss in ", selected_year))
  png(paste0(figures_path,dir_name, '/pl2_croploss_diffPer_regional_map_',s_type,'.png'), width = 3401.575, height = 2267.717)
  print(pl_croploss_diffPer_regional_map)
  dev.off()
}

croploss_diffPer_regional = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                    dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                    dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                    dplyr::filter(!adm0_a3 %in% c("ATA","FJI")) %>%
                                    rowwise(),
                                  croploss_diffPer_regional_all, by = 'adm0_a3')

pl_croploss_diffPer_regional_map <- ggplot() +
  # color map by regions
  geom_sf(data = order_facets(croploss_diffPer_regional), aes(fill = -diff)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Avoided relative crop loss (%)","\n"))) +
  # facet
  facet_wrap(. ~ scen_type) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 30), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # title
  labs(title = paste0("Annual avoided relative crop loss in ", selected_year))
png(paste0(figures_path,dir_name, '/pl2_croploss_diffPer_regional_map.png'), width = 3401.575, height = 1267.717)
print(pl_croploss_diffPer_regional_map)
dev.off()



## WORLD
plt_croploss_world = ggplot(data = dt.croploss %>%
                          # statistics by region
                          group_by(region, year, scenario, pollutant) %>%
                          summarise(value = median(value)) %>%
                          ungroup() %>%
                          # sum all regions
                          group_by(year, scenario, pollutant) %>%
                          summarise(value = sum(value)) %>%
                          ungroup() %>% dplyr::distinct(.) %>%
                          # compute statistics by scen_type
                          rename_scen() %>%
                          mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
                          group_by(year, scen_type, pollutant) %>%
                          mutate(min_value = min(value),
                                 max_value = max(value),
                                 median_value = median(value)) %>%
                          ungroup() %>%
                          order_facets() %>%
                          # filter(scen_type != 'REF') %>%
                          mutate(year = as.numeric(year))) +
  # geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line
  # geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
  # geom_text(aes(x = year, y = median_value, label = scen_type), vjust = -0.5, hjust = -0.5) +
  facet_wrap(. ~ pollutant, scales = 'free') +
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario', labels = scen_palette_refVsSppVsSnr.labs) +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario', labels = scen_palette_refVsSppVsSnr.labs) +
  # labs
  labs(y = '% crop loss', x = '', title = 'Annual relative crop loss due to AP') +
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
png(paste0(figures_path,dir_name, '/pl2_croploss_world.png'), width = 3401.575, height = 2267.717)
print(plt_croploss_world)
dev.off()

## REGIONAL
# (free scales)
plt_croploss_regional = ggplot(data = dt.croploss %>%
                             # statistics by region
                             group_by(region, year, scenario, pollutant) %>%
                             summarise(value = median(value)) %>%
                             ungroup() %>% dplyr::distinct(.) %>%
                             # compute statistics by scen_type
                             rename_scen() %>%
                             mutate(scen_type = toupper(substr(scenario, 1, 3))) %>%
                             group_by(region, year, scen_type, pollutant) %>%
                             mutate(min_value = min(value),
                                    max_value = max(value),
                                    median_value = median(value)) %>%
                             ungroup() %>%
                             order_facets() %>%
                             # filter(scen_type != 'REF') %>%
                             mutate(year = as.numeric(year))) +
  # geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line
  # geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
  # geom_text(aes(x = year, y = median_value, label = scen_type), vjust = -0.5, hjust = -0.5) +
  facet_grid(region ~ pollutant, scales = 'fixed') +
  scale_color_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario', labels = scen_palette_refVsSppVsSnr.labs) +
  scale_fill_manual(values = scen_palette_refVsSppVsSnr, name = 'Scenario', labels = scen_palette_refVsSppVsSnr.labs) +
  # labs
  labs(y = '% crop loss', x = '', title = 'Annual relative crop loss due to AP') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 30),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
png(paste0(figures_path,dir_name, '/pl2_croploss_regional.png'), width = 2267.717, height = 5267.717)
print(plt_croploss_regional)
dev.off()


#####


### Fig: summary fig ==============================
# =============================

population_by_fasst_region = total_regional_pop %>%
  select(year, country_name, country_pop = total_pop, GCAM_region_ID, region) %>%
  distinct(.) %>%
  dplyr::filter(year == selected_year) %>%
  left_join(iso_gcam_regions, by = c('GCAM_region_ID','country_name')) %>%
  rename('adm0_a3' = 'iso') %>%
  mutate(adm0_a3 = toupper(adm0_a3)) %>%
  # merge with GCAM regions
  left_join(rfasst::fasst_reg %>%
              dplyr::rename('adm0_a3' = 'subRegionAlt'), by = 'adm0_a3',
            multiple = 'all') %>%
  # compute total population by fasst-region
  group_by(year, fasst_region) %>%
  mutate(fasst_region_pop = sum(country_pop)) %>%
  # compute the population-weigth by fasst_region
  mutate(pop_w = country_pop / fasst_region_pop)

## SDG 15 ================= DONE
#####
# forestation
data_summary_afforestation = forestation_diffPer_regional_all %>%
  select(Units, year, scen_type, region, value = median_value) %>%
  distinct(.) %>%
  # add column indicating the impact
  mutate(impact = 'afforestation',
         year = as.numeric(year)) %>%
  order_facets()

data_summary_croploss = croploss_diffPer_regional_all %>%
  # merge with ISO codes and GCAM regions
  left_join(rfasst::fasst_reg %>%
              dplyr::rename('ISO3' = 'subRegionAlt'), by = 'fasst_region',
            multiple = 'all') %>%
  right_join(iso_gcam_regions %>%
               dplyr::mutate(iso = toupper(iso)) %>%
               dplyr::rename('ISO3' = 'iso'),
             by = 'ISO3') %>%
  left_join(id_gcam_regions, by = 'GCAM_region_ID') %>%
  select(-c(adm0_a3,region_GCAM3)) %>% distinct(.) %>%
  # merge with weighted population
  left_join(population_by_fasst_region %>%
              select(year, GCAM_region_ID, fasst_region, ISO3 = adm0_a3, pop_w) %>%
              distinct(.),
            by = c('GCAM_region_ID','fasst_region','ISO3','year')) %>%
  filter(!is.na(pop_w)) %>%
  # compute country associated deaths
  mutate(country_av_deaths = -diff * pop_w) %>%
  # aggregate the deaths by GCAM-regions
  group_by(year, scen_type, GCAM_region_ID, region) %>%
  summarise(value = sum(country_av_deaths)) %>%
  # add column indicating that's "mort"
  mutate(impact = 'avoided crop loss',
         Units = '%',
         year = as.numeric(as.character(year))) %>%
  order_facets()


data_summary_fertilizer = fertilizer_demand_diffPer_regional_all %>%
  select(Units, year, scen_type, region, value = median_value) %>%
  distinct(.) %>%
  # add column indicating the impact
  mutate(impact = 'avoided fertilizer usage',
         year = as.numeric(year)) %>%
  order_facets()
#####

## SDG 6 ================== DONE
#####
# total water consumption
data_summary_water = water_consumption_diffPer_regional_all %>%
  select(Units, year, scen_type, region, value = diff) %>%
  mutate(value = -value) %>%
  distinct(.) %>%
  # add column indicating the impact
  mutate(impact = 'avoided water consumption',
         year = as.numeric(year))%>%
  order_facets()

# irr water consumption
data_summary_irr_water = water_irr_reduction_diffPer_regional_all %>%
  select(Units, year, scen_type, region, value = diff) %>%
  mutate(value = -value) %>%
  distinct(.) %>%
  # add column indicating the impact
  mutate(impact = 'avoided irrigated water consumption',
         Units = '%',
         year = as.numeric(year))%>%
  order_facets()

#####

## SDG 13 ================= DONE
#####
# total GHG
data_summary_ghg = ghg_diffPer_regional_all %>%
  select(Units, year, scen_type, region, value = per_diff) %>%
  mutate(value = -value) %>%
  distinct(.) %>%
  # add column indicating that's "ghg"
  mutate(impact = 'avoided GHG emissions',
         year = as.numeric(year)) %>%
  order_facets()

# agricultural CH4
data_summary_ch4 = ch4_diffPer_regional_all %>%
  select(Units, year, scen_type, region, value = median_value) %>%
  mutate(value = -value) %>%
  distinct(.) %>%
  # add column indicating that's "ch4"
  mutate(impact = 'avoided CH4 emissions',
         year = as.numeric(year)) %>%
  order_facets()

# agricultural N2O
data_summary_n2o = n2o_diffPer_regional_all %>%
  select(Units, year, scen_type, region, value = median_value) %>%
  mutate(value = -value) %>%
  distinct(.) %>%
  # add column indicating that's "n2o"
  mutate(impact = 'avoided N2O emissions',
         year = as.numeric(year)) %>%
  order_facets()

# agricultural LUC CO2
data_summary_luc_co2 = luc_diffPer_regional_all %>%
  select(Units, year, scen_type, region, value = median_value) %>%
  mutate(value = -value) %>%
  distinct(.) %>%
  # add column indicating that's "LUC CO2"
  mutate(impact = 'avoided LUC CO2 emissions',
         year = as.numeric(year)) %>%
  order_facets()
#####

## SDG 3 ================== DONE
#####
data_summary_mort = mort_diffPer_regional_all %>%
  # merge with ISO codes and GCAM regions
  left_join(rfasst::fasst_reg %>%
              dplyr::rename('ISO3' = 'subRegionAlt'), by = 'fasst_region',
            multiple = 'all') %>%
  right_join(iso_gcam_regions %>%
               dplyr::mutate(iso = toupper(iso)) %>%
               dplyr::rename('ISO3' = 'iso'),
             by = 'ISO3') %>%
  left_join(id_gcam_regions, by = 'GCAM_region_ID') %>%
  select(-c(adm0_a3,region_GCAM3)) %>% distinct(.) %>%
  # merge with weighted population
  left_join(population_by_fasst_region %>%
              select(year, GCAM_region_ID, fasst_region, ISO3 = adm0_a3, pop_w) %>%
              distinct(.),
            by = c('GCAM_region_ID','fasst_region','ISO3','year')) %>%
  filter(!is.na(pop_w)) %>%
  # compute country associated deaths
  mutate(country_ref_deaths = Ref_deaths * pop_w) %>%
  mutate(country_scen_deaths = Scen_deaths * pop_w) %>%
  # aggregate the deaths by GCAM-regions
  group_by(year, scen_type, GCAM_region_ID, region) %>%
  summarise(value_ref = sum(country_ref_deaths),
            value_scen = sum(country_scen_deaths)) %>%
  # compute percentual difference (avoided deaths)
  mutate(value = -100 * (value_scen - value_ref) / value_ref) %>%
  # add column indicating that's "mort"
  mutate(impact = 'avoided premautre deaths',
         year = as.numeric(year)) %>%
  order_facets()
#####

## plot ===================
data_summary = bind_rows(data_summary_mort,
                         data_summary_ghg,
                         data_summary_ch4,
                         data_summary_n2o,
                         data_summary_luc_co2,
                         data_summary_irr_water,
                         data_summary_water,
                         data_summary_afforestation,
                         data_summary_croploss,
                         data_summary_fertilizer) %>%
  filter(year == selected_year)
# data_summary$impact = factor(data_summary$impact,
#                              levels = c('avoided crop loss',
#                                         're-forestation',
#                                         'avoided water consumption',
#                                         'avoided irrigated water consumption',
#                                         'avoided GHG emissions',
#                                         # 'avoided LUC CO2 emissions',
#                                         'avoided CH4 emissions',
#                                         'avoided N2O emissions',
#                                         # 'avoided cropland area',
#                                         'avoided fertilizer usage',
#                                         'avoided premature deaths'))
data_summary$region = forcats::fct_rev(data_summary$region)

pl_summary = ggplot(data_summary %>%
                      filter(abs(value) < 100), aes(x = impact, y = region, fill = value)) +
  geom_tile(width = 1, height = 1) +
  coord_equal() +
  scale_fill_gradientn(colours = c('#C11717','#CFD835','white','#6CD060','#208A13'),
                       limits = c(-100, 100),
                       name = '% difference') +
  guides(fill = guide_colorbar(title.position = "top")) +
  scale_y_discrete(position = 'right') +
  # facet
  facet_wrap(. ~ scen_type) +
  # labs
  labs(y = '', x = '', title = 'Percentual regional difference of different system-wide effects') +
  # theme
  theme_light() +
  theme(legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30, angle = -45, hjust = 0),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40),
        legend.key.height = unit(1.5, "cm"),
        legend.key.width = unit(1, "cm"))
png(paste0(figures_path,dir_name, '/pl_summary.png'), width = 3001.575, height = 3001.575)
print(pl_summary)
dev.off()


data_summary$region = forcats::fct_rev(data_summary$region)
data_summary$impact = forcats::fct_rev(data_summary$impact)

pl_summary = ggplot(data_summary, aes(x = region, y = impact, fill = median_value)) +
  geom_tile(width = 1, height = 1) +
  coord_equal() +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = '% difference') +
  guides(fill = guide_colorbar(title.position = "top")) +
  scale_y_discrete(position = 'left') +
  # labs
  labs(y = '', x = '', title = 'Percentual regional difference of\ndifferent system-wide impacts') +
  # theme
  theme_light() +
  theme(legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=20, angle = -45, hjust = 0),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 30),
        title = element_text(size = 30),
        legend.key.height = unit(2, "cm"),
        legend.key.width = unit(1, "cm"))
ggsave(pl_summary, file = paste0(figures_path,dir_name,"/",'pl4_summary_horitzontal.pdf'),
       width = 500, height = 300, units = 'mm', limitsize = FALSE)


#####



#### SCENARIO DESIGN SECTION ===================================================
#### Create dataset ============================================================
# ==============================================================================

# if dataset does not exist, create it. Load it otherwise
if (!file.exists(paste0(tmp_output_data_path,'L202.flexitarian_population_v1_2050.RData'))) {
  # combine the data frames into a single dataset
  subdirectories <- c('V1_2050')
  subdirectories <- c('all_equal_2040_60-80', 'all_equal_2080_60-80')
  file_list <- list.files(tmp_output_data_path, pattern = "^L202.F", full.names = TRUE,
                          recursive = TRUE, include.dirs = TRUE)

  # filter the files in subdirectories specified
  filtered_files <- file_list[grepl(paste(subdirectories, collapse = '|'), file_list)]


  data_list <- list()
  for (f in filtered_files) {
    data <- get(load(f))
    data$id <- as.numeric(gsub(".*_(.*)\\.RData$", "\\1", f))
    run = strsplit(f, "/")[[1]]
    data$run <- run[length(run) - 1]
    data_list[[f]] <- data
  }
  all_data <- do.call(rbind, data_list) %>%
    group_by(year, region, run) %>%
    mutate(median_flex = median(flex),
           min_flex = min(flex),
           max_flex = max(flex)) %>%
    ungroup()
  save(all_data, file = paste0(tmp_output_data_path,'L202.flexitarian_population_v1_2050.RData'))
  scen_design = all_data
  rm(all_data)
} else {
  print('load flex data')
  scen_design = get(load(paste0(tmp_output_data_path,'L202.flexitarian_population_2040-2080_60-80.RData')))
}

scen_design = scen_design %>%
  select(year, region, population, flex, id, run, median_flex, min_flex, max_flex)
# load the parameters data
param = get(load(paste0(tmp_output_data_path,'/L201.Flexitarian_parameters.RData')))
#####


#### Scenarios curves ==========================================================
# ==============================================================================

#### Fig: reg by color =====================
# =============================
# flex percentage
ggplot(scen_design %>%
         filter(year >= year_s, year <= year_e) %>%
         mutate(flex_percentage = 100*flex/population) %>%
         mutate(median_flex_percentage = 100*median_flex/population) %>%
         mutate(min_flex_percentage = 100*min_flex/population) %>%
         mutate(max_flex_percentage = 100*max_flex/population)) +
  xlim(1993,2120) +
  geom_line(aes(x = year, y = flex_percentage, group = interaction(id, region), color = region), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_flex_percentage, color = region), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_flex_percentage, ymax = max_flex_percentage, fill = region), alpha = 0.15) +  # Shadow
  geom_text(data = scen_design %>%
              filter(year >= 2015) %>%
              mutate(median_flex_percentage = 100*median_flex/population) %>%
              group_by(region, id) %>%
              mutate(last_median_flex_percentage = median_flex_percentage[which(year == 2100)]) %>%
              ungroup() %>%
              select(year, region, last_median_flex_percentage) %>%
              distinct(., .keep_all = TRUE),
            aes(x = 2100, y = last_median_flex_percentage, label = region, color = region), hjust = -0.1, vjust = 0.5, size = 15) +
  # labs
  labs(x = ' ', y = 'Regional percentage of flexitarians', title = paste('Cumulative percentage of FVV')) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'none', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(file = paste0(figures_path,dir_name, "/pl0_originals/",'pl0_flex_percentage_ci.pdf'),  width = 900, height = 600, units = 'mm')

# cum flex
ggplot(scen_design %>%
         filter(year >= year_s, year <= year_e)) +
  xlim(1993,2120) +
  geom_line(aes(x = year, y = flex, group = interaction(id, region), color = region), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_flex, color = region), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_flex, ymax = max_flex, fill = region), alpha = 0.15) +  # Shadow
  geom_text(data = scen_design %>%
              filter(year >= 2015) %>%
              group_by(region, id) %>%
              mutate(last_median_flex = median_flex[which(year == 2100)]) %>%
              ungroup() %>%
              select(year, region, last_median_flex) %>%
              distinct(., .keep_all = TRUE),
            aes(x = 2100, y = last_median_flex, label = region, color = region), hjust = -0.1, vjust = 0.5, size = 15) +  # Text
  # labs
  labs(x = '', y = 'Regional nº of flexitarians', title = paste('Cumulative nº of FVV')) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'none', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(file = paste0(figures_path,dir_name, "/pl0_originals/",'pl0_flex_number_ci.pdf'), width = 900, height = 600, units = 'mm')

# new flex
ggplot(scen_design %>%
         filter(year >= year_s, year <= year_e) %>%
         group_by(region, id) %>%
         mutate(new_flex = flex - lag(flex)) %>%
         mutate(min_new_flex = min_flex - lag(min_flex)) %>%
         mutate(max_new_flex = max_flex - lag(max_flex)) %>%
         mutate(median_new_flex = median(new_flex))) +
  xlim(1993,2120) +
  geom_line(aes(x = year, y = new_flex, group = interaction(region, id), color = region), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_new_flex, color = region), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_new_flex, ymax = max_new_flex, fill = region), alpha = 0.15) +  # Shadow
  # text
  geom_text(data = scen_design %>%
              filter(year >= 2015) %>%
              group_by(region, id) %>%
              mutate(new_flex = flex - lag(flex)) %>%
              mutate(mid_new_flex = new_flex[which(year == 2030)]) %>%
              ungroup() %>% group_by(region) %>%
              mutate(mid_new_flex = median(mid_new_flex)),
            aes(x = 2030, y = mid_new_flex, label = region, color = region), hjust = -0.1, vjust = 0.5, size = 15) +
  # labs
  labs(x = '', y = 'Regional nº of flexitarians', title = paste('Nº of new FVV')) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'none', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(file = paste0(figures_path,dir_name, "/pl0_originals/",'pl0_flex_new_ci.pdf'),  width = 900, height = 600, units = 'mm')

#####


#### Fig: world fig ========================
# =============================
### WORLD
# flex percentage
ggplot(scen_design %>%
         filter(year >= year_s, year <= year_e) %>%
         group_by(year, id, run) %>%
         summarise(flex = sum(flex), population = sum(population)) %>%
         ungroup() %>%
         group_by(year, run) %>%
         mutate(median_flex = median(flex)) %>%
         mutate(min_flex = min(flex)) %>%
         mutate(max_flex = max(flex)) %>%
         ungroup() %>%
         mutate(flex_percentage = 100*flex/population) %>%
         mutate(median_flex_percentage = 100*median_flex/population) %>%
         mutate(min_flex_percentage = 100*min_flex/population) %>%
         mutate(max_flex_percentage = 100*max_flex/population)) +
  geom_line(aes(x = year, y = flex_percentage, group = interaction(id,run), color = run), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_flex_percentage, color = run), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_flex_percentage, ymax = max_flex_percentage, fill = run), alpha = 0.15) +  # Shadow
  # labs
  labs(x = ' ', y = 'Annual World percentage of flexitarians', title = paste('Cumulative percentage of flexitarians')) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(file = paste0(figures_path,dir_name,"/",'pl0_flex_percentage_world.pdf'), width = 500, height = 300, units = 'mm')

# cum flex
ggplot(scen_design %>%
         filter(year >= year_s, year <= year_e) %>%
         group_by(year, id, run) %>%
         summarise(flex = sum(flex), population = sum(population)) %>%
         ungroup() %>%
         group_by(year, run) %>%
         mutate(median_flex = median(flex)) %>%
         mutate(min_flex = min(flex)) %>%
         mutate(max_flex = max(flex)) %>%
         ungroup()) +
  geom_line(aes(x = year, y = flex, group = interaction(id, run), color = run), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_flex, color = run), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_flex, ymax = max_flex, fill = run), alpha = 0.15) +  # Shadow
  # labs
  labs(x = ' ', y = 'Annual World nº of flexitarians', title = paste('Cumulative nº of flexitarians')) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(file = paste0(figures_path,dir_name,"/",'pl0_flex_number_world.pdf'), width = 500, height = 300, units = 'mm')

# new flex
ggplot(scen_design %>%
         filter(year >= year_s, year <= year_e) %>%
         group_by(year, id, run) %>%
         summarise(flex = sum(flex), population = sum(population)) %>%
         ungroup() %>%
         group_by(year, run) %>%
         mutate(median_flex = median(flex)) %>%
         mutate(min_flex = min(flex)) %>%
         mutate(max_flex = max(flex)) %>%
         ungroup() %>%
         group_by(id, run) %>%
         mutate(new_flex = flex - lag(flex)) %>%
         mutate(min_new_flex = min_flex - lag(min_flex)) %>%
         mutate(max_new_flex = max_flex - lag(max_flex)) %>%
         mutate(median_new_flex = median(new_flex))) +
  geom_line(aes(x = year, y = new_flex, group = interaction(id, run), color = run), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_new_flex, color = run), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_new_flex, ymax = max_new_flex, fill = run), alpha = 0.15) +  # Shadow
  # vlines
  geom_vline(xintercept = 2040) +
  geom_vline(xintercept = 2080) +
  geom_vline(xintercept = 2030, color = 'gray') +
  geom_vline(xintercept = 2057, color = 'gray') +
  # labs
  labs(x = ' ', y = 'Annual World nº of new flexitarians', title = paste('Nº of new flexitarians')) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(file = paste0(figures_path,dir_name,"/",'pl0_flex_new_world.pdf'), width = 500, height = 300, units = 'mm')

#####


#### Fig: regional fig =====================
# =============================
### REGIONAL
# flex percentage (free scales)
pl = ggplot(scen_design %>%
              filter(year >= year_s, year <= year_e) %>%
              mutate(flex_percentage = 100*flex/population) %>%
              mutate(median_flex_percentage = 100*median_flex/population) %>%
              mutate(min_flex_percentage = 100*min_flex/population) %>%
              mutate(max_flex_percentage = 100*max_flex/population)) +
  geom_line(aes(x = year, y = flex_percentage, group = interaction(id, run), color = run), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_flex_percentage, color = run), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_flex_percentage, ymax = max_flex_percentage, fill = run), alpha = 0.15) +  # Shadow
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(x = '', y = 'Annual Regional percentage of flexitarians', title = paste('Cumulative percentage of flexitarians (free scales)'),
       color = 'Sensitivity scenario', fill = 'Sensitivity scenario') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 50),
        legend.title = element_text(size = 60),
        title = element_text(size = 60),
        legend.key.size = unit(2, "cm"))
ggsave(pl, file = paste0(figures_path,dir_name, "/pl0_2040-2080_60-80/",'pl0_flex_percentage_regional_freeScales.pdf'), width = 1000, height = 1000, units = 'mm')

# flex percentage (fixed scales)
pl = ggplot(scen_design %>%
              filter(year >= year_s, year <= year_e) %>%
              mutate(flex_percentage = 100*flex/population) %>%
              mutate(median_flex_percentage = 100*median_flex/population) %>%
              mutate(min_flex_percentage = 100*min_flex/population) %>%
              mutate(max_flex_percentage = 100*max_flex/population)) +
  geom_line(aes(x = year, y = flex_percentage, group = interaction(id, run), color = run), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_flex_percentage, color = run), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_flex_percentage, ymax = max_flex_percentage, fill = run), alpha = 0.15) +  # Shadow
  # facet
  facet_wrap(. ~ region) +
  # labs
  labs(x = '', y = 'Annual Regional percentage of flexitarians', title = paste('Cumulative percentage of flexitarians (fixed scales)'),
       color = 'Sensitivity scenario', fill = 'Sensitivity scenario') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 50),
        legend.title = element_text(size = 60),
        title = element_text(size = 60),
        legend.key.size = unit(2, "cm"))
ggsave(pl, file = paste0(figures_path,dir_name, "/pl0_2040-2080_60-80/",'pl0_flex_percentage_regional_fixedScales.pdf'), width = 1000, height = 1000, units = 'mm')



# cum flex (free scales)
pl = ggplot(scen_design %>%
              filter(year >= year_s, year <= year_e)) +
  geom_line(aes(x = year, y = flex, group = interaction(id, run), color = run), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_flex, color = run), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_flex, ymax = max_flex, fill = run), alpha = 0.15) +  # Shadow
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(x = '', y = 'Annual World nº of flexitarians', title = paste('Cumulative nº of flexitarians (free scales)'),
       color = 'Sensitivity scenario', fill = 'Sensitivity scenario') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 50),
        legend.title = element_text(size = 60),
        title = element_text(size = 60),
        legend.key.size = unit(2, "cm"))
ggsave(pl, file = paste0(figures_path,dir_name, "/pl0_2040-2080_60-80/",'pl0_flex_number_regional_freeScales.pdf'), width = 1000, height = 1000, units = 'mm')

# cum flex (fixed scales)
pl = ggplot(scen_design %>%
              filter(year >= year_s, year <= year_e)) +
  geom_line(aes(x = year, y = flex, group = interaction(id, run), color = run), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_flex, color = run), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_flex, ymax = max_flex, fill = run), alpha = 0.15) +  # Shadow
  # facet
  facet_wrap(. ~ region) +
  # labs
  labs(x = '', y = 'Annual World nº of flexitarians', title = paste('Cumulative nº of flexitarians (fixed scales)'),
       color = 'Sensitivity scenario', fill = 'Sensitivity scenario') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 50),
        legend.title = element_text(size = 60),
        title = element_text(size = 60),
        legend.key.size = unit(2, "cm"))
ggsave(pl, file = paste0(figures_path,dir_name, "/pl0_2040-2080_60-80/",'pl0_flex_number_regional_fixedScales.pdf'), width = 1000, height = 1000, units = 'mm')



# new flex (free scales)
pl = ggplot(scen_design %>%
              filter(year >= year_s, year <= year_e) %>%
              group_by(id, region, run) %>%
              mutate(new_flex = flex - lag(flex)) %>%
              mutate(min_new_flex = min_flex - lag(min_flex)) %>%
              mutate(max_new_flex = max_flex - lag(max_flex)) %>%
              mutate(median_new_flex = median(new_flex))) +
  geom_line(aes(x = year, y = new_flex, group = interaction(id, run), color = run), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_new_flex, color = run), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_new_flex, ymax = max_new_flex, fill = run), alpha = 0.15) +  # Shadow
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(x = ' ', y = 'Annual World nº of new flexitarians', title = paste('Nº of new flexitarians (free scales)'),
       color = 'Sensitivity scenario', fill = 'Sensitivity scenario') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 50),
        legend.title = element_text(size = 60),
        title = element_text(size = 60),
        legend.key.size = unit(2, "cm"))
ggsave(pl, file = paste0(figures_path,dir_name, "/pl0_2040-2080_60-80/",'pl0_flex_new_regional_freeScales.pdf'), width = 1000, height = 1000, units = 'mm')

# new flex (fixed scales)
pl = ggplot(scen_design %>%
              filter(year >= year_s, year <= year_e) %>%
              group_by(id, region, run) %>%
              mutate(new_flex = flex - lag(flex)) %>%
              mutate(min_new_flex = min_flex - lag(min_flex)) %>%
              mutate(max_new_flex = max_flex - lag(max_flex)) %>%
              mutate(median_new_flex = median(new_flex))) +
  geom_line(aes(x = year, y = new_flex, group = interaction(id, run), color = run), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_new_flex, color = run), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_new_flex, ymax = max_new_flex, fill = run), alpha = 0.15) +  # Shadow
  # facet
  facet_wrap(. ~ region) +
  # labs
  labs(x = ' ', y = 'Annual World nº of new flexitarians', title = paste('Nº of new flexitarians (fixed scales'),
       color = 'Sensitivity scenario', fill = 'Sensitivity scenario') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 50),
        legend.title = element_text(size = 60),
        title = element_text(size = 60))
ggsave(pl, file = paste0(figures_path,dir_name, "/pl0_2040-2080_60-80/",'pl0_flex_new_regional_fixedScales.pdf'), width = 1000, height = 1000, units = 'mm')

#####



#### NUTRITIONAL VALUES ========================================================
#### Basic data ================================================================
# ==============================================================================

if(!dir.exists(paste0(figures_path,dir_name))) dir.create(paste0(figures_path,dir_name))
if(!dir.exists(paste0(figures_path,dir_name,"/macronutrients/"))) dir.create(paste0(figures_path,dir_name,"/macronutrients/"))

# Dietary energy supply
# Units: kcal/capita/day
# By region
# Get total consumption in calories
dietary_energy_supply <- dt$food_consumption_regional %>%
  filter(year >= year_s, year <= year_e) %>%
  group_by(scenario, region, year) %>%
  # Aggregate staple and non-staple calories
  summarize(value = sum(value)) %>%
  left_join(dt$pop_all_regions, by = c("year", "scenario", "region")) %>%
  # Convert from Pcal to kcal/capita/day
  mutate(value = (value * 1e12) / (population * 365),
         units = "kcal/capita/day")
## Share of dietary energy supply from staples =================================
# Find share of staple and non-staple calories in total calories
share_diet_staples_region <- dt$staples_nonstaples_regional %>%
  mutate(staples_in_total = Staples/Total,
         nonstaples_in_total = NonStaples/Total,
         percent_staples_in_total = staples_in_total * 100)

## =========== ADESA (Average dietary supply adequacy) =============================================

# Calculate average dietary supply adequacy as:
# total regional calories / SUM_age,sex(calreq_a,s * pop_a,s)
total_regional_calories <- dietary_energy_supply


# join with MDER data, calculate caloric requirements by sex and age
adesa_denominator <- weighted_pop_sex_age %>%
  left_join(mder, by = "variable") %>%
  select(-std) %>%
  group_by(scenario, t0, k, scen_type, variable, year, region) %>%
  # compute a range because of differing physical activity levels
  summarize(cal_req_x_pop = mder * pop_sex_age,
            min_cal_req_x_pop = min * pop_sex_age,
            max_cal_req_x_pop = max * pop_sex_age) %>%
  # aggregate caloric requirements to get total regional values
  group_by(scenario, t0, k, scen_type, region, year) %>%
  summarize(denominator_sum = sum(cal_req_x_pop),
            min_denominator_sum = sum(min_cal_req_x_pop),
            max_denominator_sum = sum(max_cal_req_x_pop)) %>%
  mutate(year = as.numeric(year))

# add in regional calorie info, calculate ADESA
adesa <- left_join(adesa_denominator, total_regional_calories) %>%
  # select(-population) %>%
  group_by(year, region, scenario, t0, k, scen_type) %>%
  reframe(adesa = (value / denominator_sum) * population * 100, # convert to unitless and percentage
          min_adesa = (value / min_denominator_sum) * population * 100,
          max_adesa = (value / max_denominator_sum) * population * 100,
          .groups = "keep")

# check for ADESA trends and get rid of the scenarios with a down-going ADESA
pl = ggplot(adesa %>%
              filter(scen_type != 'St7_Reference') %>%
              rbind(adesa %>% filter(scen_type == 'St7_Reference') %>%
                      group_by(year, region) %>%
                      mutate(scenario = 'St7_Reference',
                             t0 = NA,
                             k = NA,
                             scen_type = 'St7_Reference',
                             adesa = mean(adesa),
                             min_adesa = mean(min_adesa),
                             max_adesa = mean(max_adesa),
                             .groups = 'keep')) %>%
              mutate(scen_type = substr(scen_type, 1, 3)) %>%
              group_by(scen_type, year) %>%
              mutate(mean_adesa = mean(adesa)) %>%
              ungroup() %>%
              group_by(scen_type, scenario, year) %>%
              mutate(adesa = mean(adesa)) %>%
              ungroup() %>%
              order_facets())+
  geom_line(aes(x = year, y = mean_adesa, color = scen_type), linewidth = 2, alpha = 1) +  # Median line
  geom_line(aes(x = year, y = adesa, group = scenario, color = scen_type), linewidth = 1, alpha = 0.8) +  # Median line
  scale_color_manual(values = scen_palette_refVsSppVsSnr, labels = scen_palette_refVsSppVsSnr.labs, name = 'Scenario') +
  labs(y = 'ADESA value', x = '') +
  ggtitle('ADESA World') +
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
png(paste0(figures_path,dir_name,"/",'pl_adesa_world.png'), width = 3401.575, height = 2267.717, res = 150)
print(pl)
dev.off()

# USA vs AffricaEastern
pl = ggplot(adesa  %>%
              filter(scen_type != 'St7_Reference') %>%
              rbind(adesa %>% filter(scen_type == 'St7_Reference') %>%
                      group_by(year, region) %>%
                      mutate(scenario = 'St7_Reference',
                             t0 = NA,
                             k = NA,
                             scen_type = 'St7_Reference',
                             adesa = mean(adesa),
                             min_adesa = mean(min_adesa),
                             max_adesa = mean(max_adesa),
                             .groups = 'keep')) %>%
              mutate(scen_type = substr(scen_type, 1, 3)) %>%
              group_by(scen_type, region, year) %>%
              mutate(mean_adesa = mean(adesa)) %>%
              ungroup() %>%
              order_facets() %>%
              filter(region %in% c('Africa_Eastern','USA')))+
  geom_line(aes(x = year, y = mean_adesa, color = scen_type), linewidth = 2, alpha = 1) +  # Median line
  geom_line(aes(x = year, y = adesa, group = scenario, color = scen_type), linewidth = 1, alpha = 0.8) +  # Median line
  scale_color_manual(values = scen_palette_refVsSppVsSnr, labels = scen_palette_refVsSppVsSnr.labs, name = 'Scenario') +
  # facet
  facet_wrap(. ~ region, scales = 'fixed') +
  labs(y = 'ADESA value', x = '') +
  ggtitle('ADESA regional values') +
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
png(paste0(figures_path,dir_name,"/",'pl_adesa_AffEvsUSA.png'), width = 3401.575, height = 2267.717)
print(pl)
dev.off()


# REGIONAL
# check for ADESA trends and get rid of the scenarios with a down-going ADESA
pl = ggplot(adesa  %>%
              filter(scen_type != 'St7_Reference') %>%
              rbind(adesa %>% filter(scen_type == 'St7_Reference') %>%
                      group_by(year, region) %>%
                      mutate(scenario = 'St7_Reference',
                             t0 = NA,
                             k = NA,
                             scen_type = 'St7_Reference',
                             adesa = mean(adesa),
                             min_adesa = mean(min_adesa),
                             max_adesa = mean(max_adesa),
                             .groups = 'keep')) %>%
              mutate(scen_type = substr(scen_type, 1, 3)) %>%
              group_by(scen_type, region, year) %>%
              mutate(mean_adesa = mean(adesa)) %>%
              ungroup() %>%
              order_facets())+
  geom_line(aes(x = year, y = mean_adesa, color = scen_type), linewidth = 2, alpha = 1) +  # Median line
  geom_line(aes(x = year, y = adesa, group = scenario, color = scen_type), linewidth = 1, alpha = 0.8) +  # Median line
  scale_color_manual(values = scen_palette_refVsSppVsSnr, labels = scen_palette_refVsSppVsSnr.labs, name = 'Scenario') +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  labs(y = 'ADESA value', x = '') +
  ggtitle('ADESA regional values') +
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
png(paste0(figures_path,dir_name,"/",'pl_adesa_reg_freeS.png'), width = 3401.575, height = 2267.717)
print(pl)
dev.off()

pl = ggplot(adesa  %>%
              filter(scen_type != 'St7_Reference') %>%
              rbind(adesa %>% filter(scen_type == 'St7_Reference') %>%
                      group_by(year, region) %>%
                      mutate(scenario = 'St7_Reference',
                             t0 = NA,
                             k = NA,
                             scen_type = 'St7_Reference',
                             adesa = mean(adesa),
                             min_adesa = mean(min_adesa),
                             max_adesa = mean(max_adesa),
                             .groups = 'keep')) %>%
              mutate(scen_type = substr(scen_type, 1, 3)) %>%
              group_by(scen_type, region, year) %>%
              mutate(mean_adesa = mean(adesa)) %>%
              ungroup() %>%
              order_facets())+
  geom_line(aes(x = year, y = mean_adesa, color = scen_type), linewidth = 2, alpha = 1) +  # Median line
  geom_line(aes(x = year, y = adesa, group = scenario, color = scen_type), linewidth = 1, alpha = 0.8) +  # Median line
  scale_color_manual(values = scen_palette_refVsSppVsSnr, labels = scen_palette_refVsSppVsSnr.labs, name = 'Scenario') +
  # facet
  facet_wrap(. ~ region, scales = 'fixed') +
  labs(y = 'ADESA value', x = '') +
  ggtitle('ADESA regional values') +
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
png(paste0(figures_path,dir_name,"/",'pl_adesa_reg_fixedS.png'), width = 3401.575, height = 2267.717)
print(pl)
dev.off()


## MICRO & MACROnutrients ======================================================
devtools::load_all()

## =========== Macronutrients (Protein and Fat) =================

## Macronutrient by Kcal of food consumption
macronutrients_basic = dt$food_consumption_regional %>%
  # rename columns
  rename('GCAM_commodity' = 'technology') %>%
  rename('consumption' = 'value') %>%
  # aggregate population data
  left_join(dt$pop_all_regions, by = c("year", "scenario", "t0", "k", "scen_type", "region"),
            multiple = "all") %>%
  # convert from Pcal to kcal/capita/day
  mutate(consumptionPerCapita = (consumption * 1e12) / (population * 365),
         Units = "kcal/capita/day") %>%
  left_join(GramProteinFatPerKcal %>%
              select(-year) %>%
              # match regions' id with regions' name
              left_join_keep_first_only(regions_key %>% select(-1), by = 'GCAM_region_ID'),
            by = c('region','GCAM_commodity'), multiple = "all") %>%
  # compute total Protein and Fat [g/capita/day]
  mutate(gProteinPerCapita = consumptionPerCapita * gProteinPerKcal,
         gFatPerCapita = consumptionPerCapita * gFatPerKcal)

## plot
# WORLD trend
pl_macronutrients_world = ggplot(data = macronutrients_basic %>%
                                   filter(scen_type != 'St7_Reference') %>%
                                   rbind(macronutrients_basic %>% filter(scen_type == 'St7_Reference') %>%
                                           group_by(Units, year, region, nestingSector1,
                                                    nestingSector2, nestingSector3,
                                                    GCAM_commodity, GCAM_region_ID,
                                                    element) %>%
                                           mutate(scenario = 'St7_Reference',
                                                  t0 = NA,
                                                  k = NA,
                                                  scen_type = 'St7_Reference',
                                                  consumption = mean(consumption),
                                                  population = mean(population),
                                                  consumptionPerCapita = mean(consumptionPerCapita),
                                                  Mt = mean(Mt),
                                                  fatperc = mean(fatperc),
                                                  proteinperc = mean(proteinperc),
                                                  Kcalperg = mean(Kcalperg),
                                                  gProteinPerKcal = mean(gProteinPerKcal),
                                                  gProteinPerCapita = mean(gProteinPerCapita),
                                                  gFatPerCapita = mean(gFatPerCapita),
                                                  MKcal = mean(MKcal)) %>%
                                           ungroup()) %>%
                                   mutate(scen_type = substr(scen_type, 1, 3)) %>%
                                   group_by(scenario, scen_type, year) %>%
                                   summarise(gProteinPerCapita = median(gProteinPerCapita),
                                             gFatPerCapita = median(gFatPerCapita)) %>%
                                   tidyr::pivot_longer(cols = gProteinPerCapita:gFatPerCapita, names_to = 'macronutrient') %>%
                                   group_by(scen_type, year, macronutrient) %>%
                                   mutate(min_value = min(value),
                                          max_value = max(value),
                                          median_value = median(value)) %>%
                                   ungroup() %>%
                                   order_facets() %>%
                                   mutate(macronutrient = factor(macronutrient, levels = c("gProteinPerCapita","gFatPerCapita")))) +
  geom_line(aes(x = year, y = value, group = interaction(macronutrient,scen_type,scenario), color = interaction(macronutrient,scen_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(macronutrient,scen_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(macronutrient,scen_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = macronutrients_scenario_palette, name = 'Scenario', labels = macronutrients_scenario.labs) +
  scale_fill_manual(values = macronutrients_scenario_palette, name = 'Scenario', labels = macronutrients_scenario.labs) +
  # text
  geom_text(aes(x = 2013, y = 155, label = "Protein"), color = "black", size = 13) +
  geom_text(aes(x = 2013, y = 135, label = "Fat"), color = "black", size = 13) +
  # labs
  labs(y = 'g/capita/day', x = '', title = 'World macronutrients intake') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(nrow = 2), color = guide_legend(nrow = 2))
png(paste0(figures_path,dir_name,"/",'pl4_macronutrients_world.png'), width = 3401.575, height = 2267.717, res = 150)
print(pl_macronutrients_world)
dev.off()

# REGIONAL trend
pl_macronutrients_regional = ggplot(data = macronutrients_basic %>%
                                      mutate(scen_type = substr(scen_type, 1, 3)) %>%
                                      group_by(scenario, scen_type, year, region) %>%
                                      summarise(gProteinPerCapita = median(gProteinPerCapita),
                                                gFatPerCapita = median(gFatPerCapita)) %>%
                                      tidyr::pivot_longer(cols = gProteinPerCapita:gFatPerCapita, names_to = 'macronutrient') %>%
                                      group_by(scen_type, year, macronutrient, region) %>%
                                      mutate(min_value = min(value),
                                             max_value = max(value),
                                             median_value = median(value)) %>%
                                      ungroup() %>%
                                      order_facets() %>%
                                      mutate(macronutrient = factor(macronutrient, levels = c("gProteinPerCapita","gFatPerCapita")))) +
  geom_line(aes(x = year, y = value, group = interaction(macronutrient,scen_type,scenario), color = interaction(macronutrient,scen_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(macronutrient,scen_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(macronutrient,scen_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = macronutrients_scenario_palette, name = 'Scenario', labels = macronutrients_scenario.labs) +
  scale_fill_manual(values = macronutrients_scenario_palette, name = 'Scenario', labels = macronutrients_scenario.labs) +
  # facet
  facet_wrap(. ~ region, scales = 'fixed') +
  # labs
  labs(y = 'g/capita/day', x = '', title = 'Regional macronutrients intake (fixed scale)') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(nrow = 2), color = guide_legend(nrow = 2))
png(paste0(figures_path,dir_name,"/",'pl4_macronutrients_regional_fixedScale.png'), width = 3401.575, height = 3267.717)
print(pl_macronutrients_regional)
dev.off()


pl_macronutrients_regional = ggplot(data = macronutrients_basic %>%
                                      mutate(scen_type = substr(scen_type, 1, 3)) %>%
                                      group_by(scenario, scen_type, year, region) %>%
                                      summarise(gProteinPerCapita = median(gProteinPerCapita),
                                                gFatPerCapita = median(gFatPerCapita)) %>%
                                      tidyr::pivot_longer(cols = gProteinPerCapita:gFatPerCapita, names_to = 'macronutrient') %>%
                                      group_by(scen_type, year, macronutrient, region) %>%
                                      mutate(min_value = min(value),
                                             max_value = max(value),
                                             median_value = median(value)) %>%
                                      ungroup() %>%
                                      order_facets() %>%
                                      mutate(macronutrient = factor(macronutrient, levels = c("gProteinPerCapita","gFatPerCapita")))) +
  geom_line(aes(x = year, y = value, group = interaction(macronutrient,scen_type,scenario), color = interaction(macronutrient,scen_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(macronutrient,scen_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(macronutrient,scen_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = macronutrients_scenario_palette, name = 'Scenario', labels = macronutrients_scenario.labs) +
  scale_fill_manual(values = macronutrients_scenario_palette, name = 'Scenario', labels = macronutrients_scenario.labs) +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(y = 'g/capita/day', x = '', title = 'Regional macronutrients intake (fixed scale)') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(nrow = 2), color = guide_legend(nrow = 2))
png(paste0(figures_path,dir_name,"/",'pl4_macronutrients_regional_freeScale.png'), width = 3401.575, height = 3267.717)
print(pl_macronutrients_regional)
dev.off()




## =========== Micronutrients (Vitmins & others) =================

micronutrients_ref = micronutrients %>%
  filter(startsWith(scenario, 'St7_Reference')) %>%
  group_by(region, year, nutrient_name,units_rni,nutrient_units) %>%
  mutate(byReg_rni = mean(byReg_rni),
         pop = mean(pop),
         byRegC_rni = mean(byRegC_rni),
         total_micronutrient_intake = mean(total_micronutrient_intake),
         scenario = 'St7_Reference') %>%
  ungroup() %>%
  distinct()

micronutrients = micronutrients %>%
  filter(!startsWith(scenario, 'St7_Reference')) %>%
  rbind(micronutrients_ref) %>%
  tidyr::separate(scenario, into = c("scen_type"), sep = "_", remove = FALSE)


###### plot
## -- bars (per difference)
micronutrients_diffPer_regional = micronutrients %>%
  # compute diff between intake and RNI
  mutate(diff = 100*(total_micronutrient_intake - byRegC_rni)/byRegC_rni) %>%
  # compute median by scenario type
  dplyr::group_by(scen_type,region,year,nutrient_name,nutrient_units) %>%
  dplyr::summarise(median_value = median(diff),
                   min_value = min(diff),
                   max_value = max(diff)) %>%
  # filter desired year
  dplyr::filter(year == selected_year)


pl_micronutrients_diffPer_regional_bars <- ggplot() +
  # barchart
  geom_bar(data = micronutrients_diffPer_regional %>%
             filter(scen_type == 'spp') %>%
             order_facets(),
           aes(x = as.factor(nutrient_name), y = median_value, fill = as.factor(nutrient_name)),
           stat = "identity", color = NA, width = 0.5) +
  scale_fill_manual(values = c25, name = '') +
  # facet
  facet_wrap(. ~ region, scales = 'fixed') +
  # horizontal line at y = 0
  geom_hline(yintercept = 0, linewidth = 1.2) +
  labs(x = '', y = 'Percentual difference between intake and RNI') +
  theme_light() +
  theme(panel.grid.major.y = element_line(color = 'grey20'),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = 'white',linewidth = 0),
        panel.background = element_rect(fill = "transparent"),
        legend.key.size = unit(2,'cm'), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.text = element_text(size = 20, color = 'black'),
        strip.background =element_rect(fill="transparent"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(nrow = 3)) +
  # title
  labs(title = paste('Percentual difference between intake and RNI in', selected_year, 'with Behavior change scen'))
png(paste0(figures_path,dir_name,"/",'pl4_micronutrients_diffPer_SPP_regional_bars.png'), width = 3401.575, height = 3267.717)
print(pl_micronutrients_diffPer_regional_bars)
dev.off()

pl_micronutrients_diffPer_regional_bars <- ggplot() +
  # barchart
  geom_bar(data = micronutrients_diffPer_regional %>%
             filter(scen_type == 'snr') %>%
             order_facets(),
           aes(x = as.factor(nutrient_name), y = median_value, fill = as.factor(nutrient_name)),
           stat = "identity", color = NA, width = 0.5) +
  scale_fill_manual(values = c25, name = '') +
  # facet
  facet_wrap(. ~ region, scales = 'fixed') +
  # horizontal line at y = 0
  geom_hline(yintercept = 0, linewidth = 1.2) +
  labs(x = '', y = 'Percentual difference between intake and RNI') +
  theme_light() +
  theme(panel.grid.major.y = element_line(color = 'grey20'),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = 'white',linewidth = 0),
        panel.background = element_rect(fill = "transparent"),
        legend.key.size = unit(2,'cm'), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.text = element_text(size = 20, color = 'black'),
        strip.background =element_rect(fill="transparent"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(nrow = 3)) +
  # title
  labs(title = paste('Percentual difference between intake and RNI in', selected_year, 'with Reference scen'))
png(paste0(figures_path,dir_name,"/",'pl4_micronutrients_diffPer_SNR_regional_bars.png'), width = 3401.575, height = 3267.717)
print(pl_micronutrients_diffPer_regional_bars)
dev.off()


## total diff world
micronutrients_diffPer_world = rbind(
  micronutrients %>%
    filter(scen_type != 'snr') %>%
    select(-scen_type) %>%
    # compute difference between Reference and runs
    tidyr::pivot_wider(names_from = 'scenario', values_from = 'total_micronutrient_intake') %>%
    dplyr::mutate_at(vars(matches("^snr|^spp")), list(diff = ~ 100*(. - St7_Reference)/St7_Reference)) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-c(matches("[0-9]$"),'St7_Reference')) %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches("^snr|^spp"), names_to = 'scenario') %>%
    # compute median by scenario type
    dplyr::group_by(year,nutrient_name,nutrient_units) %>%
    dplyr::summarise(median_value = median(value),
                     min_value = min(value),
                     max_value = max(value)) %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    mutate('scen_type' = 'spp'),
  micronutrients %>%
    filter(scen_type != 'spp') %>%
    select(-scen_type) %>%
    # compute difference between Reference and runs
    tidyr::pivot_wider(names_from = 'scenario', values_from = 'total_micronutrient_intake') %>%
    dplyr::mutate_at(vars(matches("^snr|^spp")), list(diff = ~ 100*(. - St7_Reference)/St7_Reference)) %>%
    # clean the dataset and keep only the "difference" columns
    dplyr::select(-c(matches("[0-9]$"),'St7_Reference')) %>%
    # reshape dataset
    tidyr::pivot_longer(cols = matches("^snr|^spp"), names_to = 'scenario') %>%
    # compute median by scenario type
    dplyr::group_by(year,nutrient_name,nutrient_units) %>%
    dplyr::summarise(median_value = median(value),
                     min_value = min(value),
                     max_value = max(value)) %>%
    # filter desired year
    dplyr::filter(year == selected_year) %>%
    mutate('scen_type' = 'snr')
)


pl_micronutrients_diffPer_world <- ggplot() +
  # barchart
  geom_bar(data = micronutrients_diffPer_world %>%
             order_facets(),
           aes(x = as.factor(nutrient_name), y = median_value, fill = as.factor(nutrient_name)),
           stat = "identity", color = NA, width = 0.5) +
  scale_fill_manual(values = c25, name = '') +
  facet_wrap(. ~ scen_type) +
  # horizontal line at y = 0
  geom_hline(yintercept = 0, linewidth = 1.2) +
  labs(x = '', y = 'Percentage') +
  theme_light() +
  theme(panel.grid.major.y = element_line(color = 'grey20'),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = 'white',linewidth = 0),
        panel.background = element_rect(fill = "transparent"),
        legend.key.size = unit(2,'cm'), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="transparent"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(nrow = 3)) +
  # title
  labs(title = paste('Percentual difference between intake\nby scenario and Ref in', selected_year))
png(paste0(figures_path,dir_name,"/",'pl4_micronutrients_diffPer_betweenScen_world_bars.png'), width = 3401.575, height = 2267.717, res = 150)
print(pl_micronutrients_diffPer_world)
dev.off()


