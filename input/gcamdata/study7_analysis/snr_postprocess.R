#### PREPROCESS ================================================================
# ==============================================================================

# setwd to file location === #####
setwd('C:\\GCAM\\GCAM_7.0_Claudia\\gcam-core\\input\\gcamdata\\study7_analysis')
.libPaths('C:\\Users\\claudia.rodes\\Documents\\R\\win-library\\4.1')

# load libraries and paths' variables, and extra functions and styles
source('load_libs_paths.R')
source('utils_data.R')
source('utils_style.R')

# load basic data
load_mapping_data()

desired_scen <<- c('St7_Reference_R-M-F', db_scen_mapping %>% filter(grepl("snr", scen_name)) %>% pull(scen_name))

# load projects
# prj_gathered = rgcam::loadProject("snr_gathered.dat")
# prj_ref = rgcam::loadProject("snr_reference.dat")
#####

#### SYSTEM-WIDE EFFECTS SECTION ===============================================

#### Data preprocess ===========================================================
# ==============================================================================

year_s = 2000
year_e = 2050
final_db_year <<- year_e
selected_scen = desired_scen
selected_year = 2030

# load queries
load(paste0(outputs_path, 'snr_queries_all.RData'))

# prj = prj_ref
# load_queries()
# prj = prj_gathered
# load_queries()

# # compute premature mortalities due to AP
# mort = load_premature_mortalities() %>%
#   rename('value' = 'mort',
#          'fasst_region' = 'region')
#
# mort_by_poll = mort %>%
#   rename_scen() %>%
#   group_by(year, fasst_region, scenario_type, pollutant) %>%
#   summarise(median_value = median(value),
#             min_value = quantile(value, probs= 0.05, na.rm = TRUE),
#             max_value = quantile(value, probs= 0.95, na.rm = TRUE)) %>%
#   ungroup()
#
# mort_total = mort_by_poll %>%
#   group_by(year, fasst_region, scenario_type) %>%
#   summarise(median_value = sum(median_value),
#             min_value = sum(min_value),
#             max_value = sum(max_value)) %>%
#   ungroup()
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
scen_palette = scen_palette_calibrateSppFuelPrefElast

# create figures' subdirectory
dir_name = 'SNR'
if (!dir.exists(paste0(figures_path,dir_name))) dir.create(paste0(figures_path,dir_name))
if (!dir.exists(paste0(outputs_path,dir_name))) dir.create(paste0(outputs_path,dir_name))

# share noRumiant consumption world
share_snr_data = dt$food_consumption_world %>%
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
  scale_color_manual(values = scen_palette_refVsAllSnr, name = 'Scenario') +
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
share_snr_data = dt$food_consumption_regional %>%
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
  scale_color_manual(values = scen_palette_refVsSnr, name = 'Scenario') +
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



 #### System-wide effects figures ===============================================
 # ==============================================================================



#### Fig: food consumption, production & demand ====
# =============================
## WORLD
# R vs M vs Plant vs Fish
pltD_food_consumption = food_consumption_world %>%
  dplyr::filter(technology %in% c('Rumiant','Monogastric','OtherMeat_Fish','Plant')) %>%
  group_by(Units, scenario, technology, year) %>% mutate(value_nestingSubector = sum(value)) %>% ungroup() %>%
  group_by(Units, scenario, technology, year) %>% mutate(value_nestingSector = sum(value)) %>% ungroup() %>%
  filter(scenario %in% selected_scen) %>% rename_scen()

pltD_food_consumption$technology = factor(pltD_food_consumption$technology, levels = c('Beef','Pork','Poultry','Other Meat and Fish','Legumes','Nuts and Seeds'))

# plot 6 elem
plt_food_consumption_world = ggplot(data = pltD_food_consumption %>%
                                      group_by(technology,year,scenario_type) %>%
                                      dplyr::mutate(median_value = median(value)) %>%
                                      dplyr::mutate(min_value = min(value)) %>%
                                      dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
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
                                      group_by(technology,year,scenario_type) %>%
                                      dplyr::mutate(median_value = median(value)) %>%
                                      dplyr::mutate(min_value = min(value)) %>%
                                      dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
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
                                         group_by(region,technology,year,scenario_type) %>%
                                         dplyr::mutate(median_value = median(value)) %>%
                                         dplyr::mutate(min_value = min(value)) %>%
                                         dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
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
                                         group_by(region,technology,year,scenario_type) %>%
                                         dplyr::mutate(median_value = median(value)) %>%
                                         dplyr::mutate(min_value = min(value)) %>%
                                         dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
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
                                      group_by(technology,year,scenario_type) %>%
                                      dplyr::mutate(median_value = median(value)) %>%
                                      dplyr::mutate(min_value = min(value)) %>%
                                      dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
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
                                         group_by(region,technology,year,scenario_type) %>%
                                         dplyr::mutate(median_value = median(value)) %>%
                                         dplyr::mutate(min_value = min(value)) %>%
                                         dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
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
                                         group_by(region,technology,year,scenario_type) %>%
                                         dplyr::mutate(median_value = median(value)) %>%
                                         dplyr::mutate(min_value = min(value)) %>%
                                         dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
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
                                                   group_by(input,year,scenario_type) %>%
                                                   dplyr::mutate(median_value = median(value)) %>%
                                                   dplyr::mutate(min_value = min(value)) %>%
                                                   dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,input,scenario), color = interaction(scenario_type,input)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,input)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,input)), alpha = 0.15) +  # Shadow
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
                                                      group_by(region,input,year,scenario_type) %>%
                                                      dplyr::mutate(median_value = median(value)) %>%
                                                      dplyr::mutate(min_value = min(value)) %>%
                                                      dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,input,scenario), color = interaction(scenario_type,input)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,input)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,input)), alpha = 0.15) +  # Shadow
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
                                                      group_by(region,input,year,scenario_type) %>%
                                                      dplyr::mutate(median_value = median(value)) %>%
                                                      dplyr::mutate(min_value = min(value)) %>%
                                                      dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,input,scenario), color = interaction(scenario_type,input)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,input)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,input)), alpha = 0.15) +  # Shadow
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
                                  group_by(sector,year,scenario_type) %>%
                                  dplyr::mutate(median_value = median(value)) %>%
                                  dplyr::mutate(min_value = min(value)) %>%
                                  dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,sector,scenario), color = interaction(scenario_type,sector)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,sector)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,sector)), alpha = 0.15) +  # Shadow
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
                                     group_by(region,sector,year,scenario_type) %>%
                                     dplyr::mutate(median_value = median(value)) %>%
                                     dplyr::mutate(min_value = min(value)) %>%
                                     dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,sector,scenario), color = interaction(scenario_type,sector)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,sector)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,sector)), alpha = 0.15) +  # Shadow
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
                                     group_by(region,sector,year,scenario_type) %>%
                                     dplyr::mutate(median_value = median(value)) %>%
                                     dplyr::mutate(min_value = min(value)) %>%
                                     dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,sector,scenario), color = interaction(scenario_type,sector)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,sector)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,sector)), alpha = 0.15) +  # Shadow
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

#### Fig: food price ===============================
# =============================
## REGIONAL
# by abs value
ag_prices_diffAbs_regional = tidyr::pivot_wider(ag_prices_regional, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  rename_scen() %>%
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ . - Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols,other_cols)) %>%
  # select desired sectors
  dplyr::filter(year == selected_year, sector %in% c('regional beef', 'regional pork', 'regional poultry', 'regional legumes',
                                                     'regional fruits', 'regional vegetables', 'regional oilcrop', 'regional root_tuber',
                                                     'regional corn', 'regional wheat', 'regional rice')) %>%
  # rename sectors
  dplyr::mutate(sector = gsub("_", " ", sector, fixed=TRUE)) %>%
  dplyr::mutate(sector = stringr::str_to_title(sub(".*regional ", "", sector))) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,sector,year,scenario) %>%
  dplyr::summarise(median_value = median(value))

pl_ag_prices_diffAbs_regional <- ggplot(data = ag_prices_diffAbs_regional) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
  geom_bar(aes(x=reorder(region,median_value), y=median_value, fill=sector),stat="identity",col='black',linewidth=0.2) +
  scale_fill_manual(name="",
                    values=c('#2C39FC','#3EA1DA','#1FDDED','#B800AC','#0E8600','#00BB19','#85E892','#B00000','#D05959','#D1D400','#CF8400'),
                    breaks=c("Beef","Pork","Poultry","Legumes","Rice","Corn","Wheat","Fruits","Vegetables","Oilcrop","Root Tuber")) +
  guides(fill = guide_legend(nrow = 2)) +
  geom_hline(yintercept = 0, linewidth = 1.5) +
  facet_wrap(. ~ scenario) +
  labs(y="$/Mt",x="",title = paste("Abs difference Behavior-Ref change in", selected_year))+
  theme_bw()+
  theme(
    strip.background = element_blank(),
    axis.text.x = element_text(size=30),
    axis.text.y = element_text(size=30),
    legend.text = element_text(size = 35),
    legend.title = element_text(size = 40),
    legend.position = 'bottom',
    title = element_text(size = 40)
  ) +
  coord_flip()
ggsave(pl_ag_prices_diffAbs_regional, file = paste0(figures_path,dir_name, '/pl1_ag_prices_diffAbs_regional.pdf'), width = 600, height = 700, units = 'mm')


# by percentage
ag_prices_diffPer_regional = tidyr::pivot_wider(ag_prices_regional, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  rename_scen() %>%
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # select desired sectors
  dplyr::filter(year == selected_year, sector %in% c('regional beef', 'regional pork', 'regional poultry', 'regional legumes',
                                                     'regional fruits', 'regional vegetables', 'regional oilcrop', 'regional root_tuber',
                                                     'regional corn', 'regional wheat', 'regional rice')) %>%
  # rename sectors
  dplyr::mutate(sector = gsub("_", " ", sector, fixed=TRUE)) %>%
  dplyr::mutate(sector = stringr::str_to_title(sub(".*regional ", "", sector))) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,sector,year,scenario) %>%
  dplyr::summarise(median_value = median(value))


pl_ag_prices_diffPer_regional <- ggplot(data = ag_prices_diffPer_regional) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
  geom_bar(aes(x=reorder(region,median_value), y=median_value, fill=sector),stat="identity",col='black',linewidth=0.2) +
  scale_fill_manual(name="",
                    values=c('#2C39FC','#3EA1DA','#1FDDED','#B800AC','#0E8600','#00BB19','#85E892','#B00000','#D05959','#D1D400','#CF8400'),
                    breaks=c("Beef","Pork","Poultry","Legumes","Rice","Corn","Wheat","Fruits","Vegetables","Oilcrop","Root Tuber")) +
  guides(fill = guide_legend(nrow = 2)) +
  geom_hline(yintercept = 0, linewidth = 1.5) +
  labs(y="% $/MT",x="",title = paste("Per difference Ref-Behavior change in", selected_year))+
  facet_wrap(. ~ scenario) +
  theme_bw()+
  theme(
    strip.background = element_blank(),
    axis.text.x = element_text(size=30),
    axis.text.y = element_text(size=30),
    legend.text = element_text(size = 35),
    legend.title = element_text(size = 40),
    legend.position = 'bottom',
    title = element_text(size = 40)
  ) +
  coord_flip()
ggsave(pl_ag_prices_diffPer_regional, file = paste0(figures_path,dir_name, '/pl1_ag_prices_diffPer_regional.pdf'), width = 600, height = 700, units = 'mm')




### WORLD
pl_food_demand_staplesVsNonStapes_world = ggplot(data = food_demand_prices_world %>%
                                                   rename_scen() %>%
                                                   group_by(input,year,scenario_type) %>%
                                                   dplyr::mutate(median_value = median(value)) %>%
                                                   dplyr::mutate(min_value = min(value)) %>%
                                                   dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,input,scenario), color = interaction(scenario_type,input)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,input)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,input)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
  #                    breaks = staples_vs_nonstaples_order_palette) +
  # scale_fill_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
  #                   breaks = staples_vs_nonstaples_order_palette) +
  # labs
  labs(y = '2005$/Mcal', x = '', title = 'Annual World food demand prices') +
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
ggsave(pl_food_demand_staplesVsNonStapes_world, file = paste0(figures_path,dir_name,"/",'pl1_food_demand_prices_staplesVsNonStapes_world.pdf'),
       width = 550, height = 500, units = 'mm', limitsize = FALSE)

### REGIONAL
pl_food_demand_staplesVsNonStapes_regional = ggplot(data = food_demand_prices_regional %>%
                                                      rename_scen() %>%
                                                      group_by(region,input,year,scenario_type) %>%
                                                      dplyr::mutate(median_value = median(value)) %>%
                                                      dplyr::mutate(min_value = min(value)) %>%
                                                      dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,input,scenario), color = interaction(scenario_type,input)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,input)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,input)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
  #                    breaks = staples_vs_nonstaples_order_palette) +
  # scale_fill_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
  #                   breaks = staples_vs_nonstaples_order_palette) +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(y = '2005$/Mcal', x = '', title = 'Annual Regional food demand prices (free scales)') +
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
ggsave(pl_food_demand_staplesVsNonStapes_regional, file = paste0(figures_path,dir_name,"/",'pl1_food_demand_prices_staplesVsNonStapes_regional_freeScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)

pl_food_demand_staplesVsNonStapes_regional = ggplot(data = food_demand_prices_regional %>%
                                                      rename_scen() %>%
                                                      group_by(region,input,year,scenario_type) %>%
                                                      dplyr::mutate(median_value = median(value)) %>%
                                                      dplyr::mutate(min_value = min(value)) %>%
                                                      dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,input,scenario), color = interaction(scenario_type,input)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,input)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,input)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
  #                    breaks = staples_vs_nonstaples_order_palette) +
  # scale_fill_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
  #                   breaks = staples_vs_nonstaples_order_palette) +
  # facet
  facet_wrap(. ~ region) +
  # labs
  labs(y = '2005$/Mcal', x = '', title = 'Annual Regional food demand prices (fixed scales)') +
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
ggsave(pl_food_demand_staplesVsNonStapes_regional, file = paste0(figures_path,dir_name,"/",'pl1_food_demand_prices_staplesVsNonStapes_regional_fixedScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)



#### econ. basket bill ---
### WORLD

# check food consumption
pl = ggplot(data = food_consumption_regional %>%
              left_join(food_subsector %>%
                          rename('technology' = 'subsector')) %>%
              # Pcal to kcal/capita/day
              left_join(pop_all_regions, by = c("year", "scenario", "region")) %>%
              # convert from Pcal to kcal/day
              mutate(value = (value * 1e12) / (population * 365),
                     Units = "kcal/capita/day") %>%
              # median by scenario_type
              rename_scen() %>%
              group_by(region,scenario_type,technology,year) %>%
              summarise(value = median(value)) %>%
              # diff by scenario_type
              pivot_wider(names_from = scenario_type, values_from = value) %>%
              pivot_longer(cols = other_cols, names_to = 'scenario') %>%
              mutate(diff = 100*(value - Reference)/Reference)) +
  geom_bar(aes(x = year, y = diff, fill = technology), position = 'stack', stat = 'identity') +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c25, name = 'Food items') +
  # facet
  facet_grid(region ~ scenario, scales = 'free') +
  # labs
  labs(y = '% (kcal/capita/day)', x = '', title = 'Annual regional median food consumption (free scales)') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(1, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(nrow = 2), color = guide_legend(nrow = 2))
ggsave(pl, file = paste0(figures_path,dir_name,"/",'pl5_food_consumption_by_tech_freeScales.pdf'),
       width = 1000, height = 1500, units = 'mm', limitsize = FALSE)

pl = ggplot(data = food_consumption_regional %>%
              left_join(food_subsector %>%
                          rename('technology' = 'subsector')) %>%
              # Pcal to kcal/capita/day
              left_join(pop_all_regions, by = c("year", "scenario", "region")) %>%
              # convert from Pcal to kcal/day
              mutate(value = (value * 1e12) / (population * 365),
                     Units = "kcal/capita/day") %>%
              # median by scenario_type
              rename_scen() %>%
              group_by(region,scenario_type,technology,year) %>%
              summarise(value = median(value)) %>%
              # diff by scenario_type
              pivot_wider(names_from = scenario_type, values_from = value) %>%
              pivot_longer(cols = other_cols, names_to = 'scenario') %>%
              mutate(diff = 100*(value - Reference)/Reference)) +
  geom_bar(aes(x = year, y = diff, fill = technology), position = 'stack', stat = 'identity') +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c25, name = 'Food items') +
  # facet
  facet_grid(region ~ scenario, scales = 'fixed') +
  # labs
  labs(y = '% (kcal/capita/day)', x = '', title = 'Annual regional median food consumption (fixed scales)') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(1, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(nrow = 2), color = guide_legend(nrow = 2))
ggsave(pl, file = paste0(figures_path,dir_name,"/",'pl5_food_consumption_by_tech_fixedScales.pdf'),
       width = 1000, height = 750, units = 'mm', limitsize = FALSE)



food_econ_basket_bill_regional = food_consumption_regional %>%
  left_join(food_subsector %>%
              rename('technology' = 'subsector')) %>%
  # Pcal to kcal/capita/day
  left_join(pop_all_regions, by = c("year", "scenario", "region")) %>%
  # convert from Pcal to kcal/day
  mutate(value = (value * 1e12) / (population * 365),
         Units = "kcal/capita/day") %>%
  # total staples and nonstaples kcal consumption
  group_by(Units,region,scenario,year,supplysector) %>%
  summarise(consumption = sum(value)) %>%
  # compute the expenditure by supplysector
  left_join(food_demand_prices_regional %>%
              mutate(price = value * 1e3,
                     units_price = '2005$/kcal/day') %>%
              select(-c(Units,value)) %>%
              rename('supplysector' = 'input'), by = c('region','year','supplysector','scenario')) %>%
  mutate(expenditure = consumption * price,
         units_expenditure = '2005$/capita/day') %>%
  # total expenditure (staples + nonstaples)
  group_by(units_expenditure,region,scenario,year) %>%
  summarise(expenditure = sum(expenditure))

pld_food_econ_basket_bill_regional = food_econ_basket_bill_regional %>%
  # add scenario_type column
  dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change'))
# reorder scenario_type items
pld_food_econ_basket_bill_regional$scenario_type <- factor(pld_food_econ_basket_bill_regional$scenario_type,levels = c("Reference", "Behavior change"))

pl_food_econ_basket_bill_world = ggplot(data = pld_food_econ_basket_bill_regional %>%
                                          filter(year == selected_year) %>%
                                          group_by(year,scenario_type) %>%
                                          dplyr::summarise(median_value = median(expenditure),
                                                           min_value = min(expenditure),
                                                           max_value = max(expenditure))) +
  geom_bar(aes(x = scenario_type, y = median_value, fill = scenario_type), stat = 'identity', alpha = 0.3) +
  geom_errorbar(aes(x = scenario_type, ymin = min_value, ymax = max_value), width=0.3, colour="#757575", alpha=1, linewidth=1.2) +
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = '2005$/capita/day', x = '', title = 'Annual World median food basket expenditure') +
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
ggsave(pl_food_econ_basket_bill_world, file = paste0(figures_path,dir_name,"/",'pl2_food_econ_basket_bill_world.pdf'),
       width = 550, height = 500, units = 'mm', limitsize = FALSE)

### REGIONAL
pl_food_econ_basket_bill_regional = ggplot(data = pld_food_econ_basket_bill_regional %>%
                                             filter(year == selected_year) %>%
                                             group_by(year,scenario_type,region) %>%
                                             dplyr::summarise(median_value = median(expenditure),
                                                              min_value = min(expenditure),
                                                              max_value = max(expenditure))) +
  geom_bar(aes(x = scenario_type, y = median_value, fill = scenario_type), stat = 'identity', alpha = 0.3) +
  geom_errorbar(aes(x = scenario_type, ymin = min_value, ymax = max_value), width=0.3, colour="#757575", alpha=1, linewidth=1.2) +
  # facet
  facet_wrap(. ~ region, scales = 'fixed') +
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = '2005$/capita/day', x = '', title = 'Annual regional median food basket expenditure (fixed scales)') +
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
ggsave(pl_food_econ_basket_bill_regional, file = paste0(figures_path,dir_name,"/",'pl2_food_econ_basket_bill_regional_fixedScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)


### REGIONAL diff
pld_food_econ_basket_bill_diffPer_regional = food_econ_basket_bill_regional %>%
  tidyr::pivot_wider(names_from = 'scenario', values_from = 'expenditure') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,units_expenditure,year) %>%
  dplyr::summarise(median_value = median(value),
                   min_value = min(value),
                   max_value = max(value)) %>%
  # filter desired year
  dplyr::filter(year == selected_year)

pl_food_econ_basket_bill_diffPer_regional = ggplot(data = pld_food_econ_basket_bill_diffPer_regional) +
  geom_bar(aes(x = region, y = median_value, fill = cut(median_value, c(-Inf,0,Inf))), stat = 'identity', alpha = 0.3) +
  # geom_errorbar(aes(x = scenario_type, ymin = min_value, ymax = max_value), width=0.3, colour="#757575", alpha=1, linewidth=1.2) +
  scale_fill_manual(name = '% food expenditure difference\n(BC-Ref)',
                    values = c('(-Inf,0]' = '#0DA800',
                               '(0, Inf]' = '#C60000'),
                    labels = c('Reduction', 'Increase')) +  # labs
  labs(y = '% per capita/day', x = '', title = 'Annual regional median food basket expenditure difference\n(between BC and Ref)') +
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
ggsave(pl_food_econ_basket_bill_diffPer_regional, file = paste0(figures_path,dir_name,"/",'pl2_food_econ_basket_bill_diffPer_regional.pdf'),
       width = 600, height = 500, units = 'mm', limitsize = FALSE)
#####

#### Fig: ghg emissions ============================
# =============================
## WORLD
## -- annual trend
pl_ghg_world <- ggplot(data = ghg_world %>%
                         rename_scen() %>%
                         dplyr::group_by(Units,year,scenario_type) %>%
                         dplyr::mutate(median_value = median(value)) %>%
                         dplyr::mutate(min_value = min(value)) %>%
                         dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = mypal_scen, name = 'Scenario') +
  # scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  labs(x = '', y = expression(MtCO[2]), title = 'World ghg emissions') +
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
ggsave(pl_ghg_world, file = paste0(figures_path,dir_name, '/pl2_ghg_world.pdf'), width = 400, height = 300, units = 'mm')


## -- map (abs difference)
ghg_diffAbs_regional = tidyr::pivot_wider(ghg_regional, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  rename_scen() %>%
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ . - Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,Units,year,scenario) %>%
  dplyr::summarise(median_value = -median(value)) %>%
  # filter desired year
  dplyr::filter(year == selected_year) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3')

ghg_diffAbs_regional = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                               dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                               dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                               dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                             ghg_diffAbs_regional, by = 'adm0_a3')

# plot
pl_ghg_diffAbs_map <- ggplot() +
  # color map by regions
  geom_sf(data = ghg_diffAbs_regional, aes(fill = median_value)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste(MtCO[2],' difference'))) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  facet_wrap(.~scenario, scales = 'fixed') +
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
ggsave(pl_ghg_diffAbs_map, file = paste0(figures_path,dir_name, '/pl2_ghg_diffAbs_map.pdf'), width = 500, height = 300, units = 'mm')



# percentage to add over each region
per_text = tidyr::pivot_wider(ghg_regional, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,Units,year) %>%
  summarise(percentage_text = paste0(as.character(round(-median(value), digits = 1)),'%')) %>%
  # filter desired year
  dplyr::filter(year == selected_year) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3')

ghg_diffAbs_regional = merge(ghg_diffAbs_regional,per_text, by = c('region','Units','year','GCAM Region','Country','adm0_a3'))

# plot
pl_ghg_diffAbs_map <- ggplot() +
  # color map by regions
  geom_sf(data = ghg_diffAbs_regional, aes(fill = median_value)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste(MtCO[2],' difference'))) +
  geom_text(data = mort_diffAbs_regional, aes(x = avg_x, y = avg_y, label = percentage_text),
            size = 5) +
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
ggsave(pl_ghg_diffAbs_map, file = paste0(figures_path,dir_name, '/pl2_ghg_diffAbs_with_text_map.pdf'), width = 500, height = 300, units = 'mm')




## -- map (per difference)
ghg_diffPer_regional = tidyr::pivot_wider(ghg_regional, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  rename_scen() %>%
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,Units,year,scenario) %>%
  dplyr::summarise(median_value = -median(value)) %>%
  # filter desired year
  dplyr::filter(year == selected_year) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3')

ghg_diffPer_regional = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                               dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                               dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                               dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                             ghg_diffPer_regional, by = 'adm0_a3')

# plot
pl_ghg_diffPer_map <- ggplot() +
  # color map by regions
  geom_sf(data = ghg_diffPer_regional, aes(fill = median_value)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste(MtCO[2],' % difference'))) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  facet_wrap(.~scenario, scales = 'fixed') +
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
  labs(title = paste('Per diff regional GHG emissions in', selected_year))
ggsave(pl_ghg_diffPer_map, file = paste0(figures_path,dir_name, '/pl2_ghg_diffPer_map.pdf'), width = 500, height = 300, units = 'mm')


## -- ghg emission by type (abs difference)
ghg_by_ghg_diffAbs_world = tidyr::pivot_wider(ghg_by_ghg_world, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  rename_scen() %>%
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ . - Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(group,Units,year,scenario) %>%
  dplyr::summarise(median_value = median(value))

pl_ghg_diffAbs_world_bars <- ggplot() +
  # barchart
  geom_bar(data = ghg_by_ghg_diffAbs_world |> filter(year == selected_year),
           aes(x = 0, y = median_value, fill = as.factor(group)),
           stat = "identity", color = NA, width = 0.5,
           position = position_stack(reverse = TRUE)) +
  scale_fill_brewer(palette = 'Paired', name = '') +
  # horizontal line at y = 0
  geom_hline(yintercept = 0, linewidth = 1.2) +
  facet_wrap(.~scenario, scales = 'fixed') +
  labs(x = '', y = expression(MtCO[2])) +
  theme_light() +
  theme(panel.grid.major.y = element_line(color = 'grey20'),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = 'grey',linewidth = 2),
        panel.background = element_rect(fill = "transparent"),
        legend.position = 'left', legend.direction = 'vertical',
        strip.text = element_text(size = 20, color = 'black'),
        strip.background =element_rect(fill="transparent"),
        axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  # title
  labs(title = paste('Abs diff of world\nghg emissions in', selected_year))
ggsave(pl_ghg_diffAbs_world_bars, file = paste0(figures_path,dir_name, '/pl2_ghg_diffAbs_bars_world.pdf'), width = 300, height = 300, units = 'mm')


## -- ghg emission by type (per difference)
ghg_by_ghg_diffPer_world = tidyr::pivot_wider(ghg_by_ghg_world, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  rename_scen() %>%
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(group,Units,year,scenario) %>%
  dplyr::summarise(median_value = median(value))

pl_ghg_diffPer_world_bars <- ggplot() +
  # barchart
  geom_bar(data = ghg_by_ghg_diffPer_world |> filter(year == selected_year),
           aes(x = 0, y = median_value, fill = as.factor(group)),
           stat = "identity", color = NA, width = 0.5,
           position = position_stack(reverse = TRUE)) +
  scale_fill_brewer(palette = 'Paired', name = '') +
  # horizontal line at y = 0
  geom_hline(yintercept = 0, linewidth = 1.2) +
  facet_wrap(.~scenario, scales = 'fixed') +
  labs(x = '', y = expression(paste('%', MtCO[2]))) +
  theme_light() +
  theme(panel.grid.major.y = element_line(color = 'grey20'),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = 'grey',linewidth = 2),
        panel.background = element_rect(fill = "transparent"),
        legend.position = 'left', legend.direction = 'vertical',
        strip.text = element_text(size = 20, color = 'black'),
        strip.background =element_rect(fill="transparent"),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  # title
  labs(title = paste('Per diff of world ghg emissions in', selected_year))
ggsave(pl_ghg_diffPer_world_bars, file = paste0(figures_path,dir_name, '/pl2_ghg_diffPer_bars_world.pdf'), width = 400, height = 150, units = 'mm')

#####

#### Fig: avoided deaths ===========================
# =============================
### MAPS
## -- map (abs difference)
mort_diffAbs_regional = tidyr::pivot_wider(mort, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ . - Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select('fasst_region','year','method','pollutant',matches("_diff$")) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median by region and pollutant
  group_by(year, fasst_region, pollutant) %>%
  summarise(median_value = median(value),
            min_value = quantile(value, probs= 0.05, na.rm = TRUE),
            max_value = quantile(value, probs= 0.95, na.rm = TRUE)) %>%
  ungroup() %>%
  # compute the total deaths by region (pm25 + o3)
  group_by(year, fasst_region) %>%
  summarise(median_value = -sum(median_value),
            min_value = -sum(min_value),
            max_value = -sum(max_value)) %>%
  ungroup() %>%
  # filter desired year
  dplyr::filter(year == selected_year) %>%
  # merge with GCAM regions
  left_join(rfasst::fasst_reg %>%
              dplyr::rename('ISO3' = 'subRegionAlt'), by = 'fasst_region',
            multiple = 'all') %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO3')

mort_diffAbs_regional = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                dplyr::filter(!adm0_a3 %in% c("ATA","FJI")) %>%
                                rowwise() %>%
                                mutate(
                                  avg_x = (st_bbox(geometry)$xmax + st_bbox(geometry)$xmin) / 2,
                                  avg_y = (st_bbox(geometry)$ymax + st_bbox(geometry)$ymin) / 2
                                ),
                              mort_diffAbs_regional, by = 'adm0_a3')

# plot
pl_mort_diffAbs_regional_map <- ggplot() +
  # color map by regions
  geom_sf(data = mort_diffAbs_regional, aes(fill = median_value)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Avoided deaths (nº)","\n"))) +
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
ggsave(pl_mort_diffAbs_regional_map, file = paste0(figures_path,dir_name, '/pl2_mort_diffAbs_regional_map.pdf'), width = 500, height = 300, units = 'mm')



# percentage to add over each region
per_text = tidyr::pivot_wider(mort, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ ifelse(. - Reference != 0, 100*(. - Reference)/Reference, 0))) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select('fasst_region','year','method','pollutant',matches("_diff$")) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median by region and pollutant
  group_by(year, fasst_region, pollutant) %>%
  summarise(median_value = median(value),
            min_value = quantile(value, probs= 0.05, na.rm = TRUE),
            max_value = quantile(value, probs= 0.95, na.rm = TRUE)) %>%
  ungroup() %>%
  # compute the total deaths by region (pm25 + o3)
  group_by(year, fasst_region) %>%
  summarise(percentage_text = paste0(as.character(round(-sum(median_value), digits = 1)),'%')) %>%
  ungroup() %>%
  # filter desired year
  dplyr::filter(year == selected_year) %>%
  # merge with GCAM regions
  left_join(rfasst::fasst_reg %>%
              dplyr::rename('ISO3' = 'subRegionAlt'), by = 'fasst_region',
            multiple = 'all') %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO3')


mort_diffAbs_regional = merge(mort_diffAbs_regional,per_text, by = c('year', 'fasst_region', 'adm0_a3'))

# plot
pl_mort_diffAbs_regional_map <- ggplot() +
  # color map by regions
  geom_sf(data = mort_diffAbs_regional, aes(fill = median_value)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Avoided deaths (nº)","\n"))) +
  geom_text(data = mort_diffAbs_regional, aes(x = avg_x, y = avg_y, label = percentage_text),
            size = 5) +
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
ggsave(pl_mort_diffAbs_regional_map, file = paste0(figures_path,dir_name, '/pl2_mort_diffAbs_with_text_regional_map.pdf'), width = 500, height = 300, units = 'mm')



## -- map (per difference)
mort_diffPer_regional = tidyr::pivot_wider(mort, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ ifelse(. - Reference != 0, 100*(. - Reference)/Reference, 0))) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select('fasst_region','year','method','pollutant',matches("_diff$")) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median by region and pollutant
  group_by(year, fasst_region, pollutant) %>%
  summarise(median_value = median(value),
            min_value = quantile(value, probs= 0.05, na.rm = TRUE),
            max_value = quantile(value, probs= 0.95, na.rm = TRUE)) %>%
  ungroup() %>%
  # compute the total deaths by region (pm25 + o3)
  group_by(year, fasst_region) %>%
  summarise(median_value = -sum(median_value),
            min_value = -sum(min_value),
            max_value = -sum(max_value)) %>%
  ungroup() %>%
  # filter desired year
  dplyr::filter(year == selected_year) %>%
  # merge with GCAM regions
  left_join(rfasst::fasst_reg %>%
              dplyr::rename('ISO3' = 'subRegionAlt'), by = 'fasst_region',
            multiple = 'all') %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO3')


mort_diffPer_regional = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                              mort_diffPer_regional, by = 'adm0_a3')

# plot
pl_mort_diffPer_regional_map <- ggplot() +
  # color map by regions
  geom_sf(data = mort_diffPer_regional, aes(fill = median_value)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Avoided deaths (%)","\n"))) +
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
ggsave(pl_mort_diffPer_regional_map, file = paste0(figures_path,dir_name, '/pl2_mort_diffPer_regional_map.pdf'), width = 500, height = 300, units = 'mm')



## WORLD
plt_mort_world = ggplot(data = mort %>%
                          # statistics by region
                          group_by(fasst_region, year, scenario, pollutant) %>%
                          summarise(min_value = quantile(value, probs= 0.05, na.rm = TRUE),
                                    max_value = quantile(value, probs= 0.95, na.rm = TRUE),
                                    value = median(value)) %>%
                          ungroup() %>%
                          # sum all regions
                          group_by(year, scenario, pollutant) %>%
                          summarise(min_value = sum(min_value),
                                    max_value = sum(max_value),
                                    value = sum(value)) %>%
                          ungroup() %>% dplyr::distinct(., .keep_all = TRUE) %>%
                          # compute statistics by scenario_type
                          rename_scen() %>%
                          group_by(year, scenario_type, pollutant) %>%
                          mutate(min_value = min(min_value),
                                 max_value = max(max_value),
                                 median_value = median(value)) %>%
                          ungroup()) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
  facet_wrap(. ~ pollutant, scales = 'free') +
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = 'Premature deaths', x = '', title = 'Annual World premature deaths') +
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
ggsave(plt_mort_world, file = paste0(figures_path,dir_name,"/",'pl2_mort_world.pdf'),
       width = 1000, height = 1000, units = 'mm')

## REGIONAL
# (free scales)
plt_mort_regional = ggplot(data = mort %>%
                             # statistics by region
                             group_by(fasst_region, year, scenario, pollutant) %>%
                             summarise(min_value = quantile(value, probs= 0.05, na.rm = TRUE),
                                       max_value = quantile(value, probs= 0.95, na.rm = TRUE),
                                       value = median(value)) %>%
                             ungroup() %>%
                             # compute statistics by scenario_type
                             rename_scen() %>%
                             group_by(fasst_region, year, scenario_type, pollutant) %>%
                             mutate(min_value = min(min_value),
                                    max_value = max(max_value),
                                    median_value = median(value)) %>%
                             ungroup()) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # facet
  facet_wrap(fasst_region ~ pollutant, scales = 'free', ncol = 4) +
  # labs
  labs(y = 'Premature deaths', x = '', title = 'Annual Regional premature deaths (free scales)') +
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
ggsave(plt_mort_regional, file = paste0(figures_path,dir_name,"/",'pl2_mort_regional_freeScales.pdf'),
       width = 1500, height = 4000, units = 'mm', limitsize = FALSE)

#####

#### Fig: water consumption and withdrawals ========
# =============================
# water consumption
### WORLD
## -- world water consumption
pl_water_consumption_world <- ggplot(data = water_consumption_world %>%
                                       rename_scen() %>%
                                       dplyr::group_by(year, scenario_type) %>%
                                       mutate(median_value = median(value)) %>%
                                       mutate(min_value = min(value)) %>%
                                       mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = mypal_scen, name = 'Scenario') +
  # scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = expression(paste("Annual Water flows (billion ",m^3,")")), x = '') +
  # theme
  theme_light() +
  guides(color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        axis.title = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  # title
  labs(title = 'Annual World Water consumption')
ggsave(pl_water_consumption_world, file = paste0(figures_path,dir_name, '/pl2_water_consumption_world.pdf'), width = 500, height = 300, units = 'mm')



### REGIONAL
## -- regional water consumption (free scales)
pl_water_consumption_regional <- ggplot(data = water_consumption_regional %>%
                                          rename_scen() %>%
                                          dplyr::group_by(year, scenario_type, region) %>%
                                          mutate(median_value = median(value)) %>%
                                          mutate(min_value = min(value)) %>%
                                          mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = mypal_scen, name = 'Scenario') +
  # scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(y = expression(paste("Annual Water flows (billion ",m^3,")")), x = '') +
  # theme
  theme_light() +
  guides(color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom', legend.direction = 'horizontal',
        # strip.background = element_blank(),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        axis.title = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  # title
  labs(title = 'Annual Water consumption (free scales)')
ggsave(pl_water_consumption_regional + theme(strip.text = element_text(size = 30)), file = paste0(figures_path,dir_name, '/pl2_water_consumption_regional_freeScales.pdf'),
       width = 800, height = 700, units = 'mm')

## -- regional water consumption (fixed scales)
pl_water_consumption_regional <- ggplot(data = water_consumption_regional %>%
                                          rename_scen() %>%
                                          dplyr::group_by(year, scenario_type, region) %>%
                                          mutate(median_value = median(value)) %>%
                                          mutate(min_value = min(value)) %>%
                                          mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = mypal_scen, name = 'Scenario') +
  # scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # facet
  facet_wrap(. ~ region) +
  # labs
  labs(y = expression(paste("Annual Water flows (billion ",m^3,")")), x = '') +
  # theme
  theme_light() +
  guides(color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom', legend.direction = 'horizontal',
        # strip.background = element_blank(),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        axis.title = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  # title
  labs(title = 'Annual Water consumption (fixed scales)')
ggsave(pl_water_consumption_regional + theme(strip.text = element_text(size = 30)), file = paste0(figures_path,dir_name, '/pl2_water_consumption_regional_fixedScales.pdf'),
       width = 800, height = 700, units = 'mm')


### MAPS
## -- map (abs difference)
water_consumption_diffAbs_regional = tidyr::pivot_wider(water_consumption_regional, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  rename_scen() %>%
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ . - Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,year,scenario) %>%
  dplyr::summarise(median_value = median(value)) %>%
  # filter desired year
  dplyr::filter(year == selected_year) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3')

water_consumption_diffAbs_regional = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                             dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                             dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                             dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                           water_consumption_diffAbs_regional, by = 'adm0_a3')

# plot
pl_water_consumption_diffAbs_map <- ggplot() +
  # color map by regions
  geom_sf(data = water_consumption_diffAbs_regional, aes(fill = median_value)) +
  scale_fill_gradient2(low = "#0DA800", high = "#C60000",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Annual Water flows difference (billion ",m^3,")","\n"))) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  facet_wrap(.~scenario, scales = 'fixed') +
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
  labs(title = paste0("Annual water consumption abs difference in ", selected_year))
ggsave(pl_water_consumption_diffAbs_map, file = paste0(figures_path,dir_name, '/pl2_water_consumption_diffAbs_map.pdf'), width = 500, height = 300, units = 'mm')


## -- map (per difference)
water_consumption_diffPer_regional = tidyr::pivot_wider(water_consumption_regional, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  rename_scen() %>%
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,year,scenario) %>%
  dplyr::summarise(median_value = median(value)) %>%
  # filter desired year
  dplyr::filter(year == selected_year) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3')

water_consumption_diffPer_regional = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                             dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                             dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                             dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                           water_consumption_diffPer_regional, by = 'adm0_a3')
# plot
pl_water_consumption_diffPer_map <- ggplot() +
  # color map by regions
  geom_sf(data = water_consumption_diffPer_regional, aes(fill = median_value)) +
  scale_fill_gradient2(low = "#0DA800", high = "#C60000",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Annual Water flows % difference","\n"))) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  facet_wrap(.~scenario, scales = 'fixed') +
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
  labs(title = paste("Annual water consumption % difference in", selected_year, "\n"))
ggsave(pl_water_consumption_diffPer_map, file = paste0(figures_path,dir_name, '/pl2_water_consumption_diffPer_map.pdf'), width = 500, height = 300, units = 'mm')



## -- bars (abs difference)
water_consumption_diffAbs_regional_sectorial = water_consumption_regional_sectorial %>%
  rename_scen() %>%
  # compute regional-sectorial consumption
  dplyr::filter(sector %in% food_sector) %>%
  group_by(year,scenario,sector,region) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  # compute difference between Reference and runs
  tidyr::pivot_wider(names_from = 'scenario', values_from = 'value') %>%
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ . - Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,year,sector,scenario) %>%
  dplyr::summarise(median_value = median(value)) %>%
  # filter desired year
  dplyr::filter(year == selected_year)


pl_water_consumption_diffAbs_regional_sectorial_bars <- ggplot() +
  # barchart
  geom_bar(data = water_consumption_diffAbs_regional_sectorial |> filter(year == selected_year),
           aes(x = region, y = median_value, fill = as.factor(sector)),
           stat = "identity", color = NA, width = 0.5,
           position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c25, name = '') +
  # horizontal line at y = 0
  geom_hline(yintercept = 0, linewidth = 1.2) +
  facet_wrap(.~scenario) +
  labs(x = '', y = expression(paste("Annual Water flows difference (billion ",m^3,")","\n"))) +
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
        axis.text.x = element_text(size=30, angle = 90),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(nrow = 3)) +
  # title
  labs(title = paste0('Abs diff of regional sectorial water consumption in ',selected_year))
ggsave(pl_water_consumption_diffAbs_regional_sectorial_bars, file = paste0(figures_path,dir_name, '/pl2_water_consumption_diffAbs_regional_sectorial_bars.pdf'),
       width = 700, height = 500, units = 'mm')

## -- bars (per difference)
water_consumption_diffPer_regional_sectorial = water_consumption_regional_sectorial %>%
  rename_scen() %>%
  # compute regional-sectorial consumption
  dplyr::filter(sector %in% food_sector) %>%
  group_by(year,scenario,sector,region) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  # compute difference between Reference and runs
  tidyr::pivot_wider(names_from = 'scenario', values_from = 'value') %>%
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,year,sector,scenario) %>%
  dplyr::summarise(median_value = median(value)) %>%
  # filter desired year
  dplyr::filter(year == selected_year)


pl_water_consumption_diffPer_regional_sectorial_bars <- ggplot() +
  # barchart
  geom_bar(data = water_consumption_diffPer_regional_sectorial |> filter(year == selected_year),
           aes(x = region, y = median_value, fill = as.factor(sector)),
           stat = "identity", color = NA, width = 0.5,
           position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c25, name = '') +
  # horizontal line at y = 0
  geom_hline(yintercept = 0, linewidth = 1.2) +
  facet_wrap(.~scenario) +
  labs(x = '', y = expression(paste("Annual Water flows % difference","\n"))) +
  theme_light() +
  theme(panel.grid.major.y = element_line(color = 'grey20'),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent",
                                       colour = 'grey',linewidth = 2),
        panel.background = element_rect(fill = "transparent"),
        legend.position = 'bottom', legend.direction = 'horizontal',
        strip.text = element_text(size = 20, color = 'black'),
        strip.background =element_rect(fill="transparent"),
        axis.text.x = element_text(size=30, angle = 90),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  # title
  labs(title = paste0('Per diff of regional sectorial water consumption in ',selected_year))
ggsave(pl_water_consumption_diffPer_regional_sectorial_bars, file = paste0(figures_path,dir_name, '/pl2_water_consumption_diffPer_regional_sectorial_bars.pdf'),
       width = 700, height = 500, units = 'mm')





### WORLD
## -- world water withdrawals
pl_water_withdrawals_world <- ggplot(data = water_withdrawals_world %>%
                                       rename_scen() %>%
                                       dplyr::group_by(year, scenario_type) %>%
                                       mutate(median_value = median(value)) %>%
                                       mutate(min_value = min(value)) %>%
                                       mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = expression(paste("Annual Water flows (billion ",m^3,")")), x = '') +
  # theme
  theme_light() +
  guides(color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        axis.title = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  # title
  labs(title = 'Annual World Water withdrawals')
ggsave(pl_water_withdrawals_world, file = paste0(figures_path,dir_name, '/pl2_water_withdrawals_world.pdf'), width = 500, height = 300, units = 'mm')



### REGIONAL
## -- regional water withdrawals (free scales)
pl_water_withdrawals_regional <- ggplot(data = water_withdrawals_regional %>%
                                          rename_scen() %>%
                                          dplyr::group_by(year, scenario_type, region) %>%
                                          mutate(median_value = median(value)) %>%
                                          mutate(min_value = min(value)) %>%
                                          mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(y = expression(paste("Annual Water flows (billion ",m^3,")")), x = '') +
  # theme
  theme_light() +
  guides(color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom', legend.direction = 'horizontal',
        # strip.background = element_blank(),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        axis.title = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  # title
  labs(title = 'Annual Water withdrawals (free scales)')
ggsave(pl_water_withdrawals_regional + theme(strip.text = element_text(size = 30)), file = paste0(figures_path,dir_name, '/pl2_water_withdrawals_regional_freeScales.pdf'),
       width = 800, height = 700, units = 'mm')

## -- regional water withdrawals (fixed scales)
pl_water_withdrawals_regional <- ggplot(data = water_withdrawals_regional %>%
                                          rename_scen() %>%
                                          dplyr::group_by(year, scenario_type, region) %>%
                                          mutate(median_value = median(value)) %>%
                                          mutate(min_value = min(value)) %>%
                                          mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # facet
  facet_wrap(. ~ region) +
  # labs
  labs(y = expression(paste("Annual Water flows (billion ",m^3,")")), x = '') +
  # theme
  theme_light() +
  guides(color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom', legend.direction = 'horizontal',
        # strip.background = element_blank(),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        axis.title = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  # title
  labs(title = 'Annual Water withdrawals (fixed scales)')
ggsave(pl_water_withdrawals_regional + theme(strip.text = element_text(size = 30)), file = paste0(figures_path,dir_name, '/pl2_water_withdrawals_regional_fixedScales.pdf'),
       width = 800, height = 700, units = 'mm')

#####

#### Fig: beef - dairy =============================
# =============================
## -- prices (meet and dairy)
### WORLD
pl_ag_meet_dairy_prices_world = ggplot(data = ag_meet_dairy_prices_world %>%
                                         rename_scen() %>%
                                         dplyr::group_by(sector,year,scenario_type) %>%
                                         dplyr::mutate(median_value = median(value)) %>%
                                         dplyr::mutate(min_value = min(value)) %>%
                                         dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,sector,scenario), color = interaction(scenario_type,sector)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,sector)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,sector)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = beef_dairy_scenario_palette, name = 'Scenario',
  #                    breaks = beef_dairy_order_palette) +
  # scale_fill_manual(values = beef_dairy_scenario_palette, name = 'Scenario',
  #                   breaks = beef_dairy_order_palette) +
  # labs
  labs(y = '2005$/Mcal', x = '', title = 'Annual World beef-dairy prices') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2))
ggsave(pl_ag_meet_dairy_prices_world, file = paste0(figures_path,dir_name,"/",'pl3_ag_meet_dairy_prices_world.pdf'),
       width = 550, height = 500, units = 'mm', limitsize = FALSE)

### REGIONAL
## (free scales)
pl_ag_meet_dairy_prices_regional = ggplot(data = ag_meet_dairy_prices_regional %>%
                                            rename_scen() %>%
                                            dplyr::group_by(region,sector,year,scenario_type) %>%
                                            dplyr::mutate(median_value = median(value)) %>%
                                            dplyr::mutate(min_value = min(value)) %>%
                                            dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,sector,scenario), color = interaction(scenario_type,sector)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,sector)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,sector)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = beef_dairy_scenario_palette, name = 'Scenario',
                     breaks = beef_dairy_order_palette) +
  scale_fill_manual(values = beef_dairy_scenario_palette, name = 'Scenario',
                    breaks = beef_dairy_order_palette) +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(y = '2005$/Mcal', x = '', title = 'Annual Regional beef-dairy prices (free scales)') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2))
ggsave(pl_ag_meet_dairy_prices_regional, file = paste0(figures_path,dir_name,"/",'pl3_ag_meet_dairy_prices_regional_freeScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)

# (fixed scales)
pl_ag_meet_dairy_prices_regional = ggplot(data = ag_meet_dairy_prices_regional %>%
                                            rename_scen() %>%
                                            dplyr::group_by(region,sector,year,scenario_type) %>%
                                            dplyr::mutate(median_value = median(value)) %>%
                                            dplyr::mutate(min_value = min(value)) %>%
                                            dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,sector,scenario), color = interaction(scenario_type,sector)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,sector)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,sector)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = beef_dairy_scenario_palette, name = 'Scenario',
                     breaks = beef_dairy_order_palette) +
  scale_fill_manual(values = beef_dairy_scenario_palette, name = 'Scenario',
                    breaks = beef_dairy_order_palette) +
  # facet
  facet_wrap(. ~ region) +
  # labs
  labs(y = '2005$/Mcal', x = '', title = 'Annual Regional beef-dairy prices (fixed scales)') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2))
ggsave(pl_ag_meet_dairy_prices_regional, file = paste0(figures_path,dir_name,"/",'pl3_ag_meet_dairy_prices_regional_fixedScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)


## -- production (beef mixed vs pastoral)
## WORLD (trend)
pl_ag_beef_production_world = ggplot(data = ag_meet_dairy_production_world %>% filter(sector == 'Beef') %>%
                                       rename_scen() %>%
                                       dplyr::group_by(sector,subsector,year,scenario_type) %>%
                                       dplyr::mutate(median_value = median(value)) %>%
                                       dplyr::mutate(min_value = min(value)) %>%
                                       dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,subsector,scenario), color = interaction(scenario_type,subsector)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,subsector)), linewidth = 1.5, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,subsector)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = beef_type_scenario_palette, name = 'Scenario',
  #                    breaks = beef_type_order_palette) +
  # scale_fill_manual(values = beef_type_scenario_palette, name = 'Scenario',
  #                   breaks = beef_type_order_palette) +
  # labs
  labs(y = 'Mt', x = '', title = 'Annual World Beef production') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2))
ggsave(pl_ag_beef_production_world, file = paste0(figures_path,dir_name,"/",'pl3_ag_beef_production_world.pdf'),
       width = 550, height = 500, units = 'mm', limitsize = FALSE)

## WORLD (per diff)
ag_beef_production_diffPer_world = tidyr::pivot_wider(ag_meet_dairy_production_world %>% filter(sector == 'Beef'), names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(Units,sector,subsector,year) %>%
  dplyr::mutate(median_value = median(value),
                min_value = quantile(value, probs= 0.05, na.rm = TRUE),
                max_value = quantile(value, probs= 0.95, na.rm = TRUE))


pl_ag_beef_production_diffPer_world = ggplot(data = ag_beef_production_diffPer_world) +
  geom_line(aes(x = year, y = value, group = interaction(subsector,scenario), color = interaction(subsector)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(subsector)), linewidth = 1.5, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(subsector)), alpha = 0.15) +  # Shadow
  geom_hline(aes(yintercept = 0), linetype = 'dotted', color = 'gray') +
  scale_color_manual(values = beef_type_diff_scenario_palette, name = 'Land Type',
                     breaks = beef_type_diff_order_palette) +
  scale_fill_manual(values = beef_type_diff_scenario_palette, name = 'Land Type',
                    breaks = beef_type_diff_order_palette) +
  # labs
  labs(y = expression(paste('Percentual change compared to Reference')), x = '', title = 'Global median beef type % change between FVV and Reference') +
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
ggsave(pl_ag_beef_production_diffPer_world, file = paste0(figures_path,dir_name,"/",'pl3_ag_beef_production_diffPer_world.pdf'),
       width = 550, height = 500, units = 'mm', limitsize = FALSE)

## REGIONAL (per diff)
ag_beef_production_diffPer_regional = tidyr::pivot_wider(ag_meet_dairy_production_regional %>% filter(sector == 'Beef'), names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(Units,sector,subsector,year,region) %>%
  dplyr::mutate(median_value = median(value),
                min_value = quantile(value, probs= 0.05, na.rm = TRUE),
                max_value = quantile(value, probs= 0.95, na.rm = TRUE))


pl_ag_beef_production_diffPer_regional = ggplot(data = ag_beef_production_diffPer_regional) +
  geom_line(aes(x = year, y = value, group = interaction(subsector,scenario), color = interaction(subsector)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(subsector)), linewidth = 1.5, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(subsector)), alpha = 0.15) +  # Shadow
  geom_hline(aes(yintercept = 0), linetype = 'dotted', color = 'gray') +
  scale_color_manual(values = beef_type_diff_scenario_palette, name = 'Land Type',
                     breaks = beef_type_diff_order_palette) +
  scale_fill_manual(values = beef_type_diff_scenario_palette, name = 'Land Type',
                    breaks = beef_type_diff_order_palette) +
  # facet
  facet_wrap(. ~ region, scales = 'fixed') +
  # labs
  labs(y = expression(paste('Percentual change compared to Reference')), x = '', title = 'Global median beef type % change between FVV and Reference (fixed scales)') +
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
ggsave(pl_ag_beef_production_diffPer_regional, file = paste0(figures_path,dir_name,"/",'pl3_ag_beef_production_diffPer_regional_fixedScales.pdf'),
       width = 750, height = 750, units = 'mm', limitsize = FALSE)

pl_ag_beef_production_diffPer_regional = ggplot(data = ag_beef_production_diffPer_regional) +
  geom_line(aes(x = year, y = value, group = interaction(subsector,scenario), color = interaction(subsector)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(subsector)), linewidth = 1.5, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(subsector)), alpha = 0.15) +  # Shadow
  geom_hline(aes(yintercept = 0), linetype = 'dotted', color = 'gray') +
  scale_color_manual(values = beef_type_diff_scenario_palette, name = 'Land Type',
                     breaks = beef_type_diff_order_palette) +
  scale_fill_manual(values = beef_type_diff_scenario_palette, name = 'Land Type',
                    breaks = beef_type_diff_order_palette) +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(y = expression(paste('Percentual change compared to Reference')), x = '', title = 'Global median beef type % change between FVV and Reference (free scales)') +
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
ggsave(pl_ag_beef_production_diffPer_regional, file = paste0(figures_path,dir_name,"/",'pl3_ag_beef_production_diffPer_regional_freeScales.pdf'),
       width = 750, height = 750, units = 'mm', limitsize = FALSE)



#####

#### Fig: CH4 ======================================
# =============================
## -- CH4 emissions
### WORLD
pl_ch4_world = ggplot(data = nonco2_luc %>% filter(ghg == 'CH4') %>%
                        group_by(Units,scenario,ghg,year) %>%
                        summarise(value = sum(value)) %>%
                        ungroup() %>%
                        rename_scen() %>%
                        dplyr::group_by(year,scenario_type) %>%
                        dplyr::mutate(median_value = median(value)) %>%
                        dplyr::mutate(min_value = min(value)) %>%
                        dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,scenario), color = interaction(scenario_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = mypal_scen, name = 'Scenario') +
  # scale_fill_manual(values = mypal_scen, name = 'Scenario') +
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
ggsave(pl_ch4_world, file = paste0(figures_path,dir_name,"/",'pl2_ch4_world.pdf'),
       width = 550, height = 500, units = 'mm', limitsize = FALSE)

### REGIONAL
## (free scales)
pl_ch4_regional = ggplot(data = nonco2_luc %>% filter(ghg == 'CH4') %>%
                           group_by(region,Units,scenario,ghg,year) %>%
                           summarise(value = sum(value)) %>%
                           ungroup() %>%
                           rename_scen() %>%
                           dplyr::group_by(region,year,scenario_type) %>%
                           dplyr::mutate(median_value = median(value)) %>%
                           dplyr::mutate(min_value = min(value)) %>%
                           dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,scenario), color = interaction(scenario_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = mypal_scen, name = 'Scenario') +
  # scale_fill_manual(values = mypal_scen, name = 'Scenario') +
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
ggsave(pl_ch4_regional, file = paste0(figures_path,dir_name,"/",'pl2_ch4_regional_freeScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)

# (fixed scales)
pl_ch4_regional = ggplot(data = nonco2_luc %>% filter(ghg == 'CH4') %>%
                           group_by(region,Units,scenario,ghg,year) %>%
                           summarise(value = sum(value)) %>%
                           ungroup() %>%
                           rename_scen() %>%
                           dplyr::group_by(region,year,scenario_type) %>%
                           dplyr::mutate(median_value = median(value)) %>%
                           dplyr::mutate(min_value = min(value)) %>%
                           dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,scenario), color = interaction(scenario_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = mypal_scen, name = 'Scenario') +
  # scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # facet
  facet_wrap(. ~ region) +
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
ggsave(pl_ch4_regional, file = paste0(figures_path,dir_name,"/",'pl2_ch4_regional_fixedScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)

#####

#### Fig: N2O ======================================
# =============================
## -- N2O emissions
### WORLD
pl_n2o_world = ggplot(data = nonco2_luc %>% filter(ghg == 'N2O') %>%
                        group_by(Units,scenario,ghg,year) %>%
                        summarise(value = sum(value)) %>%
                        ungroup() %>%
                        rename_scen() %>%
                        dplyr::group_by(year,scenario_type) %>%
                        dplyr::mutate(median_value = median(value)) %>%
                        dplyr::mutate(min_value = min(value)) %>%
                        dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,scenario), color = interaction(scenario_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = mypal_scen, name = 'Scenario') +
  # scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = expression(paste(MtN[2],'O')), x = '', title = expression(paste('Annual World ',N[2],'O emissions'))) +
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
ggsave(pl_n2o_world, file = paste0(figures_path,dir_name,"/",'pl2_n2o_world.pdf'),
       width = 550, height = 500, units = 'mm', limitsize = FALSE)

### REGIONAL
## (free scales)
pl_n2o_regional = ggplot(data = nonco2_luc %>% filter(ghg == 'N2O') %>%
                           group_by(region,Units,scenario,ghg,year) %>%
                           summarise(value = sum(value)) %>%
                           ungroup() %>%
                           rename_scen() %>%
                           dplyr::group_by(region,year,scenario_type) %>%
                           dplyr::mutate(median_value = median(value)) %>%
                           dplyr::mutate(min_value = min(value)) %>%
                           dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,scenario), color = interaction(scenario_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = mypal_scen, name = 'Scenario') +
  # scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(y = expression(paste(MtN[2],'O')), x = '', title = expression(paste('Annual World ',N[2],'O emissions (free scales)'))) +
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
ggsave(pl_n2o_regional, file = paste0(figures_path,dir_name,"/",'pl2_n2o_regional_freeScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)

# (fixed scales)
pl_n2o_regional = ggplot(data = nonco2_luc %>% filter(ghg == 'N2O') %>%
                           group_by(region,Units,scenario,ghg,year) %>%
                           summarise(value = sum(value)) %>%
                           ungroup() %>%
                           rename_scen() %>%
                           dplyr::group_by(region,year,scenario_type) %>%
                           dplyr::mutate(median_value = median(value)) %>%
                           dplyr::mutate(min_value = min(value)) %>%
                           dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,scenario), color = interaction(scenario_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = mypal_scen, name = 'Scenario') +
  # scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # facet
  facet_wrap(. ~ region) +
  # labs
  labs(y = expression(paste(MtN[2],'O')), x = '', title = expression(paste('Annual World ',N[2],'O emissions (fixed scales)'))) +
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
ggsave(pl_n2o_regional, file = paste0(figures_path,dir_name,"/",'pl2_n2o_regional_fixedScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)



# extra checks ===
n2o_luc_diffAbs_regional_sectorial = nonco2_luc %>% filter(ghg == 'N2O') %>%
  # compute difference between Reference and runs
  tidyr::pivot_wider(names_from = 'scenario', values_from = 'value') %>%
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ . - Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(Units,region,year,sector) %>%
  dplyr::summarise(median_value = median(value)) %>%
  # filter desired year
  dplyr::filter(year == selected_year)


pl_n2o_regional_sectorial_bars <- ggplot(data = n2o_luc_diffAbs_regional_sectorial) +
  # barchart
  geom_bar(aes(x = region, y = median_value, fill = as.factor(sector)),
           stat = "identity", color = NA, width = 0.5,
           position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c25, name = '') +
  # horizontal line at y = 0
  geom_hline(yintercept = 0, linewidth = 1.2) +
  labs(x = '', y = expression(paste("Annual N2O emissions difference (Mt ",N[2],"O)","\n"))) +
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
        axis.text.x = element_text(size=30, angle = 90),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(nrow = 3)) +
  # title
  labs(title = paste0('Abs diff of regional sectorial N2O emissions in ',selected_year))
ggsave(pl_n2o_regional_sectorial_bars, file = paste0(figures_path,dir_name, '/pl2_n2o_emissions_diffAbs_regional_sectorial_bars.pdf'),
       width = 700, height = 500, units = 'mm')


#####

#### Fig: LUC CO2 ==================================
# =============================
## -- LUC CO2 emissions
### WORLD
pl_luc_co2_world = ggplot(data = luc %>%
                            group_by(Units,scenario,ghg,year) %>%
                            summarise(value = sum(value)) %>%
                            ungroup() %>%
                            rename_scen() %>%
                            dplyr::group_by(year,scenario_type) %>%
                            dplyr::mutate(median_value = median(value)) %>%
                            dplyr::mutate(min_value = min(value)) %>%
                            dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,scenario), color = interaction(scenario_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = mypal_scen, name = 'Scenario') +
  # scale_fill_manual(values = mypal_scen, name = 'Scenario') +
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
ggsave(pl_luc_co2_world, file = paste0(figures_path,dir_name,"/",'pl2_luc_co2_world.pdf'),
       width = 550, height = 500, units = 'mm', limitsize = FALSE)

### REGIONAL
## (free scales)
pl_luc_co2_regional = ggplot(data = luc %>%
                               group_by(region,Units,scenario,ghg,year) %>%
                               summarise(value = sum(value)) %>%
                               ungroup() %>%
                               rename_scen() %>%
                               dplyr::group_by(region,year,scenario_type) %>%
                               dplyr::mutate(median_value = median(value)) %>%
                               dplyr::mutate(min_value = min(value)) %>%
                               dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,scenario), color = interaction(scenario_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = mypal_scen, name = 'Scenario') +
  # scale_fill_manual(values = mypal_scen, name = 'Scenario') +
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
ggsave(pl_luc_co2_regional, file = paste0(figures_path,dir_name,"/",'pl2_luc_co2_regional_freeScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)

# (fixed scales)
pl_luc_co2_regional = ggplot(data = luc %>%
                               group_by(region,Units,scenario,ghg,year) %>%
                               summarise(value = sum(value)) %>%
                               ungroup() %>%
                               rename_scen() %>%
                               dplyr::group_by(region,year,scenario_type) %>%
                               dplyr::mutate(median_value = median(value)) %>%
                               dplyr::mutate(min_value = min(value)) %>%
                               dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,scenario), color = interaction(scenario_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type)), alpha = 0.15) +  # Shadow
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
ggsave(pl_luc_co2_regional, file = paste0(figures_path,dir_name,"/",'pl2_luc_co2_regional_fixedScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)

#####

#### Fig: land use =================================
# =============================
### WORLD
# (abs diff)
land_use_diffAbs_world = tidyr::pivot_wider(land_use_world, names_from = 'scenario', values_from = 'value') %>%
  rename_scen() %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ . - Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(Units,land_use_type,year,scenario) %>%
  dplyr::summarise(median_value = median(value))


pl_land_use_diffAbs_world = ggplot(data = land_use_diffAbs_world) +
  geom_area(aes(x = year, y = median_value, fill = land_use_type), alpha = 1) +  # Median area
  geom_hline(aes(yintercept = 0)) +
  scale_fill_manual(values = land_use_scenario_palette, name = 'Land Type',
                    breaks = land_use_order_palette) +
  facet_wrap(. ~scenario) +
  # labs
  labs(y = expression(paste('Change in thous. ', km^2, ' compared to Reference')), x = '', title = 'Global median land-use abs change between FVV and Reference') +
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
ggsave(pl_land_use_diffAbs_world, file = paste0(figures_path,dir_name,"/",'pl2_land_use_diffAbs_world.pdf'),
       width = 750, height = 500, units = 'mm', limitsize = FALSE)


# (per diff)
land_use_diffPer_world = tidyr::pivot_wider(land_use_world, names_from = 'scenario', values_from = 'value') %>%
  rename_scen() %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ (. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(Units,land_use_type,year,scenario) %>%
  dplyr::summarise(median_value = median(value))


pl_land_use_diffPer_world = ggplot(data = land_use_diffPer_world) +
  geom_area(aes(x = year, y = median_value, fill = land_use_type), alpha = 1) +  # Median area
  geom_hline(aes(yintercept = 0)) +
  scale_fill_manual(values = land_use_scenario_palette, name = 'Land Type',
                    breaks = land_use_order_palette) +
  facet_wrap(. ~scenario) +
  # labs
  labs(y = expression(paste('Percentual change compared to Reference')), x = '', title = 'Global median land-use % change between FVV and Reference') +
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
ggsave(pl_land_use_diffPer_world, file = paste0(figures_path,dir_name,"/",'pl2_land_use_diffPer_world.pdf'),
       width = 750, height = 500, units = 'mm', limitsize = FALSE)

### REGIONAL
## (free scales)
land_use_diffAbs_regional = tidyr::pivot_wider(land_use_regional, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ . - Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,Units,land_use_type,year) %>%
  dplyr::summarise(median_value = median(value))


pl_land_use_diffAbs_regional = ggplot(data = land_use_diffAbs_regional) +
  geom_area(aes(x = year, y = median_value, fill = land_use_type), alpha = 1) +  # Median area
  geom_hline(aes(yintercept = 0)) +
  scale_fill_manual(values = land_use_scenario_palette, name = 'Land Type',
                    breaks = land_use_order_palette) +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(y = expression(paste('Change in thous. ', km^2, ' compared to Reference')), x = '', title = 'Regional median land-use abs change between FVV and Reference (free scales)') +
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
ggsave(pl_land_use_diffAbs_regional, file = paste0(figures_path,dir_name,"/",'pl2_land_use_diffAbs_regional_freeScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)

# (fixed scales)
pl_land_use_diffAbs_regional = ggplot(data = land_use_diffAbs_regional) +
  geom_area(aes(x = year, y = median_value, fill = land_use_type), alpha = 1) +  # Median area
  geom_hline(aes(yintercept = 0)) +
  scale_fill_manual(values = land_use_scenario_palette, name = 'Land Type',
                    breaks = land_use_order_palette) +
  # facet
  facet_wrap(. ~ region) +
  # labs
  labs(y = expression(paste('Change in thous. ', km^2, ' compared to Reference')), x = '', title = 'Regional median land-use abs change between FVV and Reference (fixed scales)') +
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
ggsave(pl_land_use_diffAbs_regional, file = paste0(figures_path,dir_name,"/",'pl2_land_use_diffAbs_regional_fixedScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)

#####

#### Fig: cropland =================================
# =============================
### WORLD

# waterfall (abs)
pld_cropland_world = tidyr::pivot_wider(land_crop_world,
                                        names_from = 'scenario', values_from = 'value') %>%
  rename_scen() %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ (. - Reference))) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select('year','landleaf',matches("_diff$")) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median by region and crop_name
  group_by(year, landleaf, scenario) %>%
  summarise(median_value = median(value),
            min_value = quantile(value, probs= 0.05, na.rm = TRUE),
            max_value = quantile(value, probs= 0.95, na.rm = TRUE)) %>%
  ungroup() %>%
  # filter desired year
  dplyr::filter(year == selected_year, scenario == 'Rred_RandomIncr_diff') %>%
  # prepare dataset for waterfall
  select(x = landleaf, y = median_value) %>%
  mutate(x = tolower(x)) %>%
  data.table::as.data.table()
pld_cropland_world <- pld_cropland_world[order(pld_cropland_world$x), ]
pld_cropland_world = pld_cropland_world %>%
  mutate(cum_sum = cumsum(y),
         cum_sum = ifelse(abs(y) > 100, cum_sum/2 + (cum_sum-y)/2, cum_sum + abs(y) + 10*nchar(x)))

pl_cropland_world = waterfall(.data = pld_cropland_world,
                              rect_text_labels = rep('',length(pld_cropland_world$x)),
                              calc_total = FALSE,
                              fill_by_sign = FALSE,
                              fill_colours = color_by_sign(pld_cropland_world$y),
                              put_rect_text_outside_when_value_below = 10) +
  # text
  geom_text(data = pld_cropland_world, aes(x = x, y = cum_sum, label = x),
            angle = 90, color = "black", size = 9) +
  # labs
  labs(x = '', y = expression(paste('thous.',km^2)), x = '', title = paste('Annual World cropland abs difference (beh - ref)')) +
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

ggsave(pl_cropland_world, file = paste0(figures_path,dir_name,"/",'pl2_cropland_diffAbs_world.pdf'),
       width = 550, height = 500, units = 'mm', limitsize = FALSE)


# waterfall (per)
pld_cropland_world = tidyr::pivot_wider(land_crop_world,
                                        names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select('year','landleaf',matches("_diff$")) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median by region and crop_name
  group_by(year, landleaf) %>%
  summarise(median_value = median(value),
            min_value = quantile(value, probs= 0.05, na.rm = TRUE),
            max_value = quantile(value, probs= 0.95, na.rm = TRUE)) %>%
  ungroup() %>%
  # filter desired year
  dplyr::filter(year == selected_year) %>%
  # prepare dataset for waterfall
  select(x = landleaf, y = median_value) %>%
  mutate(x = tolower(x)) %>%
  data.table::as.data.table()
pld_cropland_world <- pld_cropland_world[order(pld_cropland_world$x), ]
pld_cropland_world = pld_cropland_world %>%
  mutate(cum_sum = cumsum(y),
         cum_sum = ifelse(abs(y) > 10, cum_sum/2 + (cum_sum-y)/2, cum_sum + abs(y) + nchar(x)/2))

pl_cropland_world = waterfall(pld_cropland_world,
                              rect_text_labels = rep('',length(pld_cropland_world$x)),
                              calc_total = FALSE,
                              fill_by_sign = FALSE,
                              fill_colours = color_by_sign(pld_cropland_world$y),
                              put_rect_text_outside_when_value_below = 10) +
  # text
  geom_text(data = pld_cropland_world, aes(x = x, y = cum_sum, label = x),
            angle = 90, color = "black", size = 9) +
  # labs
  labs(x = '', y = expression(paste('thous.',km^2)), x = '', title = paste('Annual World cropland abs difference (beh - ref)')) +
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

ggsave(pl_cropland_world, file = paste0(figures_path,dir_name,"/",'pl2_cropland_diffPer_world.pdf'),
       width = 550, height = 500, units = 'mm', limitsize = FALSE)

#####

#### Fig: re/de-forestation ========================
# =============================
### WORLD
# (abs diff)
forestation_diffAbs_world = tidyr::pivot_wider(land_use_world %>%
                                                 filter(landleaf %in% c('forest (managed)', 'forest (unmanaged)')),
                                               names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  rename_scen() %>%
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ . - Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # sum all forest types
  dplyr::group_by(Units,land_use_type,year,scenario) %>%
  dplyr::summarise(value = sum(value)) %>%
  # compute median
  dplyr::group_by(Units,land_use_type,year,scenario) %>%
  dplyr::mutate(median_value = median(value),
                min_value = min(value),
                max_value = max(value))

pl_forestation_diffAbs_world = ggplot(data = forestation_diffAbs_world) +
  geom_line(aes(x = year, y = value, group = scenario, color = interaction(scenario,land_use_type)), alpha = 0.3) +  # All runs lines
  # geom_line(aes(x = year, y = median_value, color = land_use_type), linewidth = 1, alpha = 1) +  # Median line
  # geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = land_use_type), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = land_use_scenario_palette, name = 'Land Type',
  #                   breaks = land_use_order_palette) +
  # scale_fill_manual(values = land_use_scenario_palette, name = 'Land Type',
  #                   breaks = land_use_order_palette) +
  # labs
  labs(y = expression(paste('Change in thous. ', km^2, ' compared to Reference')), x = '', title = 'World re-forestation (abs change between FVV and Reference)') +
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
ggsave(pl_forestation_diffAbs_world, file = paste0(figures_path,dir_name,"/",'pl2_forestation_diffAbs_world.pdf'),
       width = 550, height = 500, units = 'mm', limitsize = FALSE)


# (per diff)
forestation_diffPer_world = tidyr::pivot_wider(land_use_world %>%
                                                 filter(landleaf %in% c('forest (managed)', 'forest (unmanaged)')),
                                               names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ ifelse(. - Reference != 0, 100*(. - Reference)/Reference, 0))) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # sum all forest types
  dplyr::group_by(Units,land_use_type,year,scenario) %>%
  dplyr::summarise(value = sum(value)) %>%
  # compute median
  dplyr::group_by(Units,land_use_type,year) %>%
  dplyr::mutate(median_value = median(value),
                min_value = min(value),
                max_value = max(value))


pl_forestation_diffPer_world = ggplot(data = forestation_diffPer_world) +
  geom_line(aes(x = year, y = value, group = scenario, color = land_use_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = land_use_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = land_use_type), alpha = 0.15) +  # Shadow
  scale_color_manual(values = land_use_scenario_palette, name = 'Land Type',
                     breaks = land_use_order_palette) +
  scale_fill_manual(values = land_use_scenario_palette, name = 'Land Type',
                    breaks = land_use_order_palette) +
  # labs
  labs(y = expression(paste('Percentual change compared to Reference')), x = '', title = 'World re-forestation (% change between FVV and Reference)') +
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
ggsave(pl_forestation_diffPer_world, file = paste0(figures_path,dir_name,"/",'pl2_forestation_diffPer_world.pdf'),
       width = 550, height = 500, units = 'mm', limitsize = FALSE)

### REGIONAL
## (free scales)
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
  dplyr::mutate(median_value = median(value),
                min_value = min(value),
                max_value = max(value))


pl_forestation_diffAbs_regional = ggplot(data = forestation_diffAbs_regional) +
  geom_line(aes(x = year, y = value, group = scenario, color = land_use_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = land_use_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = land_use_type), alpha = 0.15) +  # Shadow
  scale_color_manual(values = land_use_scenario_palette, name = 'Land Type',
                     breaks = land_use_order_palette) +
  scale_fill_manual(values = land_use_scenario_palette, name = 'Land Type',
                    breaks = land_use_order_palette) +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(y = expression(paste('Change in thous. ', km^2, ' compared to Reference')), x = '', title = 'Regional re-forestation (abs change between FVV and Reference) (free scales)') +
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
ggsave(pl_forestation_diffAbs_regional, file = paste0(figures_path,dir_name,"/",'pl2_forestation_diffAbs_regional_freeScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)

# (fixed scales)
pl_forestation_diffAbs_regional = ggplot(data = forestation_diffAbs_regional) +
  geom_line(aes(x = year, y = value, group = scenario, color = land_use_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = land_use_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = land_use_type), alpha = 0.15) +  # Shadow
  scale_color_manual(values = land_use_scenario_palette, name = 'Land Type',
                     breaks = land_use_order_palette) +
  scale_fill_manual(values = land_use_scenario_palette, name = 'Land Type',
                    breaks = land_use_order_palette) +
  # facet
  facet_wrap(. ~ region) +
  # labs
  labs(y = expression(paste('Change in thous. ', km^2, ' compared to Reference')), x = '', title = 'Regional re-forestation (abs change between FVV and Reference) (fixed scales)') +
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
ggsave(pl_forestation_diffAbs_regional, file = paste0(figures_path,dir_name,"/",'pl2_forestation_diffAbs_regional_fixedScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)





## MAPS
## -- map (abs difference)
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

#### Fig: feed consumption =========================
# =============================
## WORLD
plt_feed_consumption_world = ggplot(data = feed_consumption_world %>%
                                      rename_scen() %>%
                                      group_by(input,year,scenario_type) %>%
                                      dplyr::mutate(median_value = median(value)) %>%
                                      dplyr::mutate(min_value = min(value)) %>%
                                      dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  # geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  # geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
  facet_wrap(. ~ input, nrow = 1, scales = 'free') +
  # scale_color_manual(values = mypal_scen, name = 'Scenario') +
  # scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = 'Mt', x = '') +
  ggtitle('World feed consumption') +
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
ggsave(plt_feed_consumption_world, file = paste0(figures_path,dir_name,"/",'pl2_feed_consumption_world.pdf'),
       width = 1000, height = 300, units = 'mm')

## REGIONAL
plt_feed_consumption_regional = ggplot(data = feed_consumption_regional %>%
                                         rename_scen() %>%
                                         group_by(region,input,year,scenario_type) %>%
                                         dplyr::mutate(median_value = median(value)) %>%
                                         dplyr::mutate(min_value = min(value)) %>%
                                         dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
  facet_grid(region ~ input) +
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = 'Mt', x = '') +
  ggtitle('Regional feed consumption (fixed scales)') +
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
ggsave(plt_feed_consumption_regional, file = paste0(figures_path,dir_name,"/",'pl2_feed_consumption_regional_fixedScales.pdf'),
       width = 2000, height = 2500, units = 'mm', limitsize = FALSE)

plt_feed_consumption_regional = ggplot(data = feed_consumption_regional %>%
                                         rename_scen() %>%
                                         group_by(region,input,year,scenario_type) %>%
                                         dplyr::mutate(median_value = median(value)) %>%
                                         dplyr::mutate(min_value = min(value)) %>%
                                         dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
  facet_grid(region ~ input, scales = 'free') +
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = 'Mt', x = '') +
  ggtitle('Regional feed consumption (free scales)') +
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
ggsave(plt_feed_consumption_regional, file = paste0(figures_path,dir_name,"/",'pl2_feed_consumption_regional_freeScales.pdf'),
       width = 2000, height = 2500, units = 'mm', limitsize = FALSE)

#####

#### Fig: fertilizer consumption ===================
# =============================
## WORLD
plt_fertilizer_consumption_world = ggplot(data = fertilizer_consumption_world %>%
                                            rename_scen() %>%
                                            group_by(sector,year,scenario_type) %>%
                                            dplyr::mutate(median_value = median(value)) %>%
                                            dplyr::mutate(min_value = min(value)) %>%
                                            dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  # geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  # geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
  facet_wrap(. ~ sector, nrow = 4, scales = 'free') +
  # scale_color_manual(values = mypal_scen, name = 'Scenario') +
  # scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = 'Mt N', x = '') +
  ggtitle('World fertilizer consumption') +
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
ggsave(plt_fertilizer_consumption_world, file = paste0(figures_path,dir_name,"/",'pl2_fertilizer_consumption_world.pdf'),
       width = 1000, height = 700, units = 'mm')

## REGIONAL
plt_fertilizer_consumption_regional =
  +  # Shadow
  facet_grid(region ~ sector) +
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = 'Mt N', x = '') +
  ggtitle('Regional fertilizer consumption (fixed scales)') +
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
ggsave(plt_fertilizer_consumption_regional, file = paste0(figures_path,dir_name,"/",'pl2_fertilizer_consumption_regional_fixedScales.pdf'),
       width = 2000, height = 2500, units = 'mm', limitsize = FALSE)

plt_fertilizer_consumption_regional = ggplot(data = fertilizer_consumption_regional %>%
                                               rename_scen() %>%
                                               group_by(region,sector,year,scenario_type) %>%
                                               dplyr::mutate(median_value = median(value)) %>%
                                               dplyr::mutate(min_value = min(value)) %>%
                                               dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
  facet_grid(region ~ sector, scales = 'free') +
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = 'Mt N', x = '') +
  ggtitle('Regional fertilizer consumption (free scales)') +
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
ggsave(plt_fertilizer_consumption_regional, file = paste0(figures_path,dir_name,"/",'pl2_fertilizer_consumption_regional_freeScales.pdf'),
       width = 2000, height = 2500, units = 'mm', limitsize = FALSE)



## WORLD
# global annual trend
plt_fertilizer_consumption_world = ggplot(data = fertilizer_consumption_world %>%
                                            group_by(year,scenario) %>%
                                            summarise(value = sum(value)) %>%
                                            ungroup() %>%
                                            rename_scen() %>%
                                            group_by(year,scenario_type) %>%
                                            dplyr::mutate(median_value = median(value)) %>%
                                            dplyr::mutate(min_value = min(value)) %>%
                                            dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = 'Mt N', x = '') +
  ggtitle('World fertilizer consumption') +
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
ggsave(plt_fertilizer_consumption_world, file = paste0(figures_path,dir_name,"/",'pl2_fertilizer_consumption_trend_world.pdf'),
       width = 1000, height = 300, units = 'mm')



# extra checks ===
fertilizer_diffAbs_regional_sectorial = fertilizer_consumption_regional %>%
  filter(sector %in% food_sector) %>%
  # compute difference between Reference and runs
  tidyr::pivot_wider(names_from = 'scenario', values_from = 'value') %>%
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ . - Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(Units,region,year,sector) %>%
  dplyr::summarise(median_value = median(value)) %>%
  # filter desired year
  dplyr::filter(year == selected_year)


pl_fertilizer_regional_sectorial_bars <- ggplot(data = fertilizer_diffAbs_regional_sectorial) +
  # barchart
  geom_bar(aes(x = region, y = median_value, fill = as.factor(sector)),
           stat = "identity", color = NA, width = 0.5,
           position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c25, name = '') +
  # horizontal line at y = 0
  geom_hline(yintercept = 0, linewidth = 1.2) +
  labs(x = '', y = paste("Annual fertilizer consumption difference (Mt N)","\n")) +
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
        axis.text.x = element_text(size=30, angle = 90),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(nrow = 3)) +
  # title
  labs(title = paste0('Abs diff of regional sectorial N fertilizer consumption in ',selected_year))
ggsave(pl_fertilizer_regional_sectorial_bars, file = paste0(figures_path,dir_name, '/pl2_fertilizer_emissions_diffAbs_regional_sectorial_bars.pdf'),
       width = 700, height = 500, units = 'mm')

#####

#### Fig: irr vs rfd water =========================
# =============================
### WORLD
# global trend
pl_water_irr_rfd_world = ggplot(data = water_irr_rfd_world %>%
                                  group_by(Units, scenario, year, water) %>%
                                  summarise(value = sum(value)) %>%
                                  ungroup() %>%
                                  rename_scen() %>%
                                  dplyr::group_by(year, scenario_type, water) %>%
                                  dplyr::mutate(median_value = median(value)) %>%
                                  dplyr::mutate(min_value = min(value)) %>%
                                  dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,scenario,water), color = interaction(scenario_type, water)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,water)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type, water)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = irr_rfd_scenario_palette, name = 'Scenario', breaks = irr_rfd_order_palette) +
  scale_fill_manual(values = irr_rfd_scenario_palette, name = 'Scenario', breaks = irr_rfd_order_palette) +
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
ggsave(pl_water_irr_rfd_world, file = paste0(figures_path,dir_name,"/",'pl2_water_irr_rfd_world.pdf'),
       width = 550, height = 500, units = 'mm', limitsize = FALSE)

# faceted (free scales)
pl_water_irr_rfd_world = ggplot(data = water_irr_rfd_world %>%
                                  rename_scen() %>%
                                  dplyr::group_by(year,scenario_type,water,crop) %>%
                                  dplyr::mutate(median_value = median(value)) %>%
                                  dplyr::mutate(min_value = min(value)) %>%
                                  dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,scenario,water), color = interaction(scenario_type, water)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,water)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type, water)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = irr_rfd_scenario_palette, name = 'Scenario', breaks = irr_rfd_order_palette) +
  scale_fill_manual(values = irr_rfd_scenario_palette, name = 'Scenario', breaks = irr_rfd_order_palette) +
  # facet
  facet_wrap(. ~ crop, scales = 'free') +
  # labs
  labs(y = expression(paste('thous.',km^2)), x = '', title = paste('Annual World IRR and RFD water consumption by crop (free scales)')) +
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
ggsave(pl_water_irr_rfd_world, file = paste0(figures_path,dir_name,"/",'pl2_water_irr_rfd_facetedFreeScales_world.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)

# faceted (fixed scales)
pl_water_irr_rfd_world = ggplot(data = water_irr_rfd_world %>%
                                  rename_scen() %>%
                                  dplyr::group_by(year,scenario_type,water,crop) %>%
                                  dplyr::mutate(median_value = median(value)) %>%
                                  dplyr::mutate(min_value = min(value)) %>%
                                  dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,scenario,water), color = interaction(scenario_type, water)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,water)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type, water)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = irr_rfd_scenario_palette, name = 'Scenario', breaks = irr_rfd_order_palette) +
  scale_fill_manual(values = irr_rfd_scenario_palette, name = 'Scenario', breaks = irr_rfd_order_palette) +
  # facet
  facet_wrap(. ~ crop) +
  # labs
  labs(y = expression(paste('thous.',km^2)), x = '', title = paste('Annual World IRR and RFD water consumption by crop (fixed scales)')) +
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
ggsave(pl_water_irr_rfd_world, file = paste0(figures_path,dir_name,"/",'pl2_water_irr_rfd_facetedFixedScales_world.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)


# waterfall
pld_water_irr_rfd_world = tidyr::pivot_wider(water_irr_rfd_world,
                                             names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ (. - Reference))) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select('water','year','crop',matches("_diff$")) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median by region and crop_name
  group_by(year, water, crop) %>%
  summarise(median_value = median(value),
            min_value = quantile(value, probs= 0.05, na.rm = TRUE),
            max_value = quantile(value, probs= 0.95, na.rm = TRUE)) %>%
  ungroup() %>%
  # filter desired year
  dplyr::filter(year == selected_year) %>%
  mutate(crop = tolower(crop)) %>%
  data.table::as.data.table()


#irr
dat = pld_water_irr_rfd_world %>%
  filter(water == 'IRR') %>%
  select(x = crop, y = median_value) %>%
  data.table::as.data.table()
dat <- dat[order(dat$x), ]
dat = dat %>%
  mutate(cum_sum = cumsum(y),
         cum_sum = ifelse(abs(y) > 1, cum_sum/2 + (cum_sum-y)/2, cum_sum + abs(y) + nchar(x)/2))


pl_water_irr_world = waterfall(.data = dat, rect_text_labels = rep('',length(dat$x)), total_rect_text = 'Abs',
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
  select(x = crop, y = median_value) %>%
  data.table::as.data.table()
dat <- dat[order(dat$x), ]
dat = dat %>%
  mutate(cum_sum = cumsum(y),
         cum_sum = ifelse(abs(y) > 10, cum_sum/2 + (cum_sum-y)/2, cum_sum + abs(y) + nchar(x)/2))

pl_water_rfd_world = waterfall(.data = na.omit(dat), rect_text_labels = rep('',length(dat$x)), total_rect_text = 'Abs',
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

pl_water_irr_rfd_waterfall = cowplot::ggdraw() +
  cowplot::draw_plot(pl_water_irr_world + labs(title = 'IRR'),
                     x = 0, y = 0, width = 0.49, height = 0.90) +
  cowplot::draw_plot(pl_water_rfd_world + labs(title = 'RFD', y = ''),
                     x = 0.5, y = 0, width = 0.49, height = 0.90) +
  cowplot::draw_plot_label(label = "Annual World IRR and RFD abs difference (beh.change - ref)", size = 45,
                           x = -0.36, y = 0.99)

ggsave(pl_water_irr_rfd_waterfall, file = paste0(figures_path,dir_name,"/",'pl2_water_irr_rfd_waterfall.pdf'),
       width = 600, height = 300, units = 'mm', limitsize = FALSE)




### REGIONAL
# global trend (free scales)
pl_water_irr_rfd_regional = ggplot(data = water_irr_rfd_regional %>%
                                     group_by(Units, scenario, year, water, region) %>%
                                     summarise(value = sum(value)) %>%
                                     ungroup() %>%
                                     rename_scen() %>%
                                     dplyr::group_by(year, scenario_type, water, region) %>%
                                     dplyr::mutate(median_value = median(value)) %>%
                                     dplyr::mutate(min_value = min(value)) %>%
                                     dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,scenario,water), color = interaction(scenario_type, water)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,water)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type, water)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = irr_rfd_scenario_palette, name = 'Scenario', breaks = irr_rfd_order_palette) +
  scale_fill_manual(values = irr_rfd_scenario_palette, name = 'Scenario', breaks = irr_rfd_order_palette) +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(y = expression(paste('thous.',km^2)), x = '', title = paste('Annual Regional IRR and RFD water consumption (free scales)')) +
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
ggsave(pl_water_irr_rfd_regional, file = paste0(figures_path,dir_name,"/",'pl2_water_irr_rfd_regional_freeScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)

# global trend (fixed scales)
pl_water_irr_rfd_regional = ggplot(data = water_irr_rfd_regional %>%
                                     group_by(Units, scenario, year, water, region) %>%
                                     summarise(value = sum(value)) %>%
                                     ungroup() %>%
                                     rename_scen() %>%
                                     dplyr::group_by(year, scenario_type, water, region) %>%
                                     dplyr::mutate(median_value = median(value)) %>%
                                     dplyr::mutate(min_value = min(value)) %>%
                                     dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,scenario,water), color = interaction(scenario_type, water)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,water)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type, water)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = irr_rfd_scenario_palette, name = 'Scenario', breaks = irr_rfd_order_palette) +
  scale_fill_manual(values = irr_rfd_scenario_palette, name = 'Scenario', breaks = irr_rfd_order_palette) +
  # facet
  facet_wrap(. ~ region) +
  # labs
  labs(y = expression(paste('thous.',km^2)), x = '', title = paste('Annual Regional IRR and RFD water consumption (fixed scales)')) +
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
ggsave(pl_water_irr_rfd_regional, file = paste0(figures_path,dir_name,"/",'pl2_water_irr_rfd_regional_fixedScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)

# faceted (free scales)
pl_water_irr_rfd_regional = ggplot(data = water_irr_rfd_regional %>%
                                     rename_scen() %>%
                                     dplyr::group_by(region,year,scenario_type,water,crop) %>%
                                     dplyr::mutate(median_value = median(value)) %>%
                                     dplyr::mutate(min_value = min(value)) %>%
                                     dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,scenario,water), color = interaction(scenario_type, water)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,water)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type, water)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = irr_rfd_scenario_palette, name = 'Scenario', breaks = irr_rfd_order_palette) +
  scale_fill_manual(values = irr_rfd_scenario_palette, name = 'Scenario', breaks = irr_rfd_order_palette) +
  # facet
  facet_grid(region ~ crop, scales = 'free') +
  # labs
  labs(y = expression(paste('thous.',km^2)), x = '', title = paste('Annual Regional IRR and RFD water consumption by crop (free scales)')) +
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
ggsave(pl_water_irr_rfd_regional, file = paste0(figures_path,dir_name,"/",'pl2_water_irr_rfd_facetedFreeScales_regional.pdf'),
       width = 2000, height = 2000, units = 'mm', limitsize = FALSE)

# faceted (fixed scales)
pl_water_irr_rfd_regional = ggplot(data = water_irr_rfd_regional %>%
                                     rename_scen() %>%
                                     dplyr::group_by(region,year,scenario_type,water,crop) %>%
                                     dplyr::mutate(median_value = median(value)) %>%
                                     dplyr::mutate(min_value = min(value)) %>%
                                     dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,scenario,water), color = interaction(scenario_type, water)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,water)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type, water)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = irr_rfd_scenario_palette, name = 'Scenario', breaks = irr_rfd_order_palette) +
  scale_fill_manual(values = irr_rfd_scenario_palette, name = 'Scenario', breaks = irr_rfd_order_palette) +
  # facet
  facet_grid(region ~ crop) +
  # labs
  labs(y = expression(paste('thous.',km^2)), x = '', title = paste('Annual Regional IRR and RFD water consumption by crop (fixed scales)')) +
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
ggsave(pl_water_irr_rfd_regional, file = paste0(figures_path,dir_name,"/",'pl2_water_irr_rfd_facetedFixedScales_regional.pdf'),
       width = 2500, height = 2500, units = 'mm', limitsize = FALSE)

#####

#### Fig: crop loss ===========================
# =============================
### MAPS
## -- map (ryl.mi difference)
crop_loss_ryl_mi_diffAbs_map = tidyr::pivot_wider(crop_loss$ryl.mi, names_from = 'scenario', values_from = 'value') %>% #crop_loss$ryl.mi
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ . - Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select('fasst_region','year','crop_name',matches("_diff$")) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median by region and crop_name
  group_by(year, fasst_region, crop_name) %>%
  summarise(median_value = median(value),
            min_value = quantile(value, probs= 0.05, na.rm = TRUE),
            max_value = quantile(value, probs= 0.95, na.rm = TRUE)) %>%
  ungroup() %>%
  # compute the total crop loss by region (sum all crop types)
  group_by(year, fasst_region) %>%
  summarise(median_value = sum(median_value),
            min_value = sum(min_value),
            max_value = sum(max_value)) %>%
  ungroup() %>%
  # filter desired year
  dplyr::filter(year == selected_year) %>%
  # merge with GCAM regions
  left_join(rfasst::fasst_reg %>%
              dplyr::rename('ISO3' = 'subRegionAlt'), by = 'fasst_region',
            multiple = 'all') %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO3')


crop_loss_ryl_mi_diffAbs_map = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                       dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                       dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                       dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                     crop_loss_ryl_mi_diffAbs_map, by = 'adm0_a3')

# plot
pl_crop_loss_ryl_mi_diffAbs_map <- ggplot() +
  # color map by regions
  geom_sf(data = crop_loss_ryl_mi_diffAbs_map, aes(fill = median_value)) +
  scale_fill_gradient2(low = "#0DA800", high = "#C60000",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Crop losses %","\n"))) +
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
  labs(title = paste0("Relative yield losses (beh - ref) in ", selected_year))
ggsave(pl_crop_loss_ryl_mi_diffAbs_map, file = paste0(figures_path,dir_name, '/pl2_pl_crop_loss_ryl_mi_diffAbs_map.pdf'), width = 500, height = 300, units = 'mm')


## -- map (ryl.mi by crop_name difference)
crop_loss_ryl_mi_diffAbs_faceted_map = tidyr::pivot_wider(crop_loss$ryl.mi, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ . - Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select('fasst_region','year','crop_name',matches("_diff$")) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median by region and crop_name
  group_by(year, fasst_region, crop_name) %>%
  summarise(median_value = -median(value),
            min_value = -quantile(value, probs= 0.05, na.rm = TRUE),
            max_value = -quantile(value, probs= 0.95, na.rm = TRUE)) %>%
  ungroup() %>%
  # filter desired year
  dplyr::filter(year == selected_year) %>%
  # merge with GCAM regions
  left_join(rfasst::fasst_reg %>%
              dplyr::rename('ISO3' = 'subRegionAlt'), by = 'fasst_region',
            multiple = 'all') %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO3')


crop_loss_ryl_mi_diffAbs_faceted_map = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                               dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                               dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                               dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                             crop_loss_ryl_mi_diffAbs_faceted_map, by = 'adm0_a3')

# plot
pl_crop_loss_ryl_mi_diffAbs_faceted_map <- ggplot() +
  # color map by regions
  geom_sf(data = crop_loss_ryl_mi_diffAbs_faceted_map, aes(fill = median_value)) +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Avoided crop losses %","\n"))) +
  # facet
  facet_wrap(. ~ crop_name) +
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
  labs(title = paste0("Avoided relative yield losses in ", selected_year))
ggsave(pl_crop_loss_ryl_mi_diffAbs_faceted_map, file = paste0(figures_path,dir_name, '/pl2_crop_loss_ryl_mi_diffAbs_faceted_map.pdf'), width = 500, height = 300, units = 'mm')




## WORLD
plt_crop_loss_world = ggplot(data = crop_loss$ryl.mi %>%
                               # statistics by region
                               group_by(fasst_region, year, scenario, crop_name) %>%
                               summarise(min_value = quantile(value, probs= 0.05, na.rm = TRUE),
                                         max_value = quantile(value, probs= 0.95, na.rm = TRUE),
                                         value = median(value)) %>%
                               ungroup() %>%
                               # sum all regions
                               group_by(year, scenario, crop_name) %>%
                               summarise(min_value = sum(min_value),
                                         max_value = sum(max_value),
                                         value = sum(value)) %>%
                               ungroup() %>% dplyr::distinct(., .keep_all = TRUE) %>%
                               # compute statistics by scenario_type
                               rename_scen() %>%
                               group_by(year, scenario_type, crop_name) %>%
                               mutate(min_value = min(min_value),
                                      max_value = max(max_value),
                                      median_value = median(value)) %>%
                               ungroup()) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
  facet_wrap(. ~ crop_name, scales = 'free') +
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = 'Relative crop loss', x = '', title = 'Annual World crop loss') +
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
ggsave(plt_crop_loss_world, file = paste0(figures_path,dir_name,"/",'pl2_crop_loss_world.pdf'),
       width = 1000, height = 1000, units = 'mm')

## REGIONAL
# (free scales)
plt_crop_loss_regional = ggplot(data = crop_loss$ryl.mi %>%
                                  # statistics by region
                                  group_by(fasst_region, year, scenario, crop_name) %>%
                                  summarise(min_value = quantile(value, probs= 0.05, na.rm = TRUE),
                                            max_value = quantile(value, probs= 0.95, na.rm = TRUE),
                                            value = median(value)) %>%
                                  ungroup() %>%
                                  # compute statistics by scenario_type
                                  rename_scen() %>%
                                  group_by(fasst_region, year, scenario_type, crop_name) %>%
                                  mutate(min_value = min(min_value),
                                         max_value = max(max_value),
                                         median_value = median(value)) %>%
                                  ungroup()) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # facet
  facet_wrap(fasst_region ~ crop_name, scales = 'free', ncol = 4) +
  # labs
  labs(y = 'Relative crop loss', x = '', title = 'Annual Regional crop loss (free scales)') +
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
ggsave(plt_crop_loss_regional, file = paste0(figures_path,dir_name,"/",'pl2_crop_loss_regional_freeScales.pdf'),
       width = 1000, height = 4500, units = 'mm', limitsize = FALSE)

# (fixed scales)
plt_crop_loss_regional = ggplot(data = crop_loss$ryl.mi %>%
                                  # statistics by region
                                  group_by(fasst_region, year, scenario, crop_name) %>%
                                  summarise(min_value = quantile(value, probs= 0.05, na.rm = TRUE),
                                            max_value = quantile(value, probs= 0.95, na.rm = TRUE),
                                            value = median(value)) %>%
                                  ungroup() %>%
                                  # compute statistics by scenario_type
                                  rename_scen() %>%
                                  group_by(fasst_region, year, scenario_type, crop_name) %>%
                                  mutate(min_value = min(min_value),
                                         max_value = max(max_value),
                                         median_value = median(value)) %>%
                                  ungroup()) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # facet
  facet_wrap(fasst_region ~ crop_name, ncol = 4) +
  # labs
  labs(y = 'Relative crop loss', x = '', title = 'Annual Regional crop loss (fixed scales)') +
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
ggsave(plt_crop_loss_regional, file = paste0(figures_path,dir_name,"/",'pl2_crop_loss_regional_fixedScales.pdf'),
       width = 1000, height = 4500, units = 'mm', limitsize = FALSE)

#####

# #### Fig: trade ===========================
# # =============================
# ag_import_vs_domestic
#
# pld_ag_import_vs_domestic = tidyr::pivot_wider(ag_import_vs_domestic_regional, names_from = 'scenario', values_from = 'value') %>%
#   # compute difference between Reference and runs
#   dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ . - Reference)) %>%
#   # clean the dataset and keep only the "difference" columns
#   dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
#   # reshape dataset
#   tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
#   # compute median
#   dplyr::group_by(region,Units,sector,subsector,year) %>%
#   dplyr::summarise(median_value = median(value))
# #####


### Fig: summary fig ==============================
# =============================

## SDG 15 ==================
# forestation
data_summary_forestation = tidyr::pivot_wider(land_use_regional %>%
                                                filter(land_use_type == 'Forest'),
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
  dplyr::group_by(region,Units,year) %>%
  dplyr::summarise(median_value = median(value),
                   min_value = min(value),
                   max_value = max(value)) %>%
  # add column indicating the impact
  mutate(impact = 're-forestation',
         year = as.numeric(year))

data_summary_cropland = tidyr::pivot_wider(land_use_regional %>%
                                             filter(land_use_type == 'Cropland'),
                                           names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,Units,year) %>%
  dplyr::summarise(median_value = -median(value),
                   min_value = -min(value),
                   max_value = -max(value)) %>%
  # add column indicating the impact
  mutate(impact = 'avoided cropland area',
         year = as.numeric(year))

data_summary_crop_loss = unique(crop_loss$ryl.mi) %>%
  # merge with GCAM regions
  left_join(rfasst::fasst_reg %>%
              dplyr::rename('ISO3' = 'subRegionAlt'), by = 'fasst_region',
            multiple = 'all') %>%
  right_join(iso_gcam_regions %>%
               dplyr::mutate(iso = toupper(iso)) %>%
               dplyr::rename('ISO3' = 'iso'),
             by = 'ISO3') %>%
  left_join(id_gcam_regions, by = 'GCAM_region_ID') %>%
  # median by GCAM region
  group_by(region, scenario, unit, crop_name, year) %>%
  summarise(value = median(value)) %>%
  ungroup() %>%
  # keep only meaningful columns
  select(unit, year, scenario, value, crop_name, region) %>% distinct(., .keep_all = TRUE) %>%
  # reshape dataset
  tidyr::pivot_wider(names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ ifelse(. - Reference != 0, 100*(. - Reference)/Reference, 0))) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select('unit', 'region','year','crop_name',matches("_diff$")) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median by region and pollutant
  group_by(unit, region, year, crop_name) %>%
  summarise(median_value = median(value),
            min_value = quantile(value, probs= 0.05, na.rm = TRUE),
            max_value = quantile(value, probs= 0.95, na.rm = TRUE)) %>%
  ungroup() %>%
  # compute the total deaths by region (pm25 + o3)
  group_by(unit, region, year) %>%
  summarise(median_value = -sum(median_value),
            min_value = -sum(min_value),
            max_value = -sum(max_value)) %>%
  ungroup() %>%
  # add column indicating that's "mort"
  mutate(impact = 'avoided crop loss',
         Units = 'People',
         year = as.numeric(as.character(year)))


data_summary_fertilizer = tidyr::pivot_wider(fertilizer_consumption_regional %>%
                                               group_by(Units, scenario, year, region) %>%
                                               summarise(value = sum(value)) %>%
                                               ungroup(),
                                             names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ ifelse(. - Reference != 0, 100*(. - Reference)/Reference, 0))) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,Units,year) %>%
  dplyr::summarise(median_value = -median(value),
                   min_value = -min(value),
                   max_value = -max(value)) %>%
  # add column indicating the impact
  mutate(impact = 'avoided fertilizer usage',
         year = as.numeric(year))


## SDG 6 ==================
# total water consumption
data_summary_water = tidyr::pivot_wider(water_consumption_regional, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,Units,year) %>%
  dplyr::summarise(median_value = -median(value),
                   min_value = -quantile(value, probs= 0.05, na.rm = TRUE),
                   max_value = -quantile(value, probs= 0.95, na.rm = TRUE)) %>%
  ungroup() %>%
  # add column indicating the impact
  mutate(impact = 'avoided water consumption',
         year = as.numeric(year))

# irr water consumption
data_summary_irr_water = tidyr::pivot_wider(water_irr_rfd_regional %>%
                                              filter(water == 'IRR') %>%
                                              group_by(Units,scenario,region,water,year) %>%
                                              summarise(value = sum(value)) %>%
                                              ungroup(),
                                            names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,Units,year) %>%
  dplyr::summarise(median_value = -median(value),
                   min_value = -quantile(value, probs= 0.05, na.rm = TRUE),
                   max_value = -quantile(value, probs= 0.95, na.rm = TRUE)) %>%
  ungroup() %>%
  # add column indicating the impact
  mutate(impact = 'avoided irrigated water consumption',
         year = as.numeric(year))

## SDG 13 ==================
# total GHG
data_summary_ghg = tidyr::pivot_wider(ghg_regional, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,Units,year) %>%
  dplyr::summarise(median_value = -median(value),
                   min_value = -quantile(value, probs= 0.05, na.rm = TRUE),
                   max_value = -quantile(value, probs= 0.95, na.rm = TRUE)) %>%
  ungroup() %>%
  # add column indicating that's "ghg"
  mutate(impact = 'avoided GHG emissions',
         year = as.numeric(year))

# agricultural CH4
data_summary_ch4 = tidyr::pivot_wider(nonco2_luc %>% filter(ghg == 'CH4') %>%
                                        group_by(Units,scenario,ghg,year,region) %>%
                                        summarise(value = sum(value)) %>%
                                        ungroup(),
                                      names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,Units,year) %>%
  dplyr::summarise(median_value = -median(value),
                   min_value = -quantile(value, probs= 0.05, na.rm = TRUE),
                   max_value = -quantile(value, probs= 0.95, na.rm = TRUE)) %>%
  ungroup() %>%
  # add column indicating that's "ghg"
  mutate(impact = 'avoided CH4 emissions',
         year = as.numeric(year))


# agricultural N2O
data_summary_n2o = tidyr::pivot_wider(nonco2_luc %>% filter(ghg == 'N2O') %>%
                                        group_by(Units,scenario,ghg,year,region) %>%
                                        summarise(value = sum(value)) %>%
                                        ungroup(),
                                      names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,Units,year) %>%
  dplyr::summarise(median_value = -median(value),
                   min_value = -quantile(value, probs= 0.05, na.rm = TRUE),
                   max_value = -quantile(value, probs= 0.95, na.rm = TRUE)) %>%
  ungroup() %>%
  # add column indicating that's "ghg"
  mutate(impact = 'avoided N2O emissions',
         year = as.numeric(year))

# agricultural LUC CO2
data_summary_luc_co2 = tidyr::pivot_wider(luc %>%
                                            group_by(Units,scenario,ghg,year,region) %>%
                                            summarise(value = sum(value)) %>%
                                            ungroup(),
                                          names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,Units,year) %>%
  dplyr::summarise(median_value = -median(value),
                   min_value = -quantile(value, probs= 0.05, na.rm = TRUE),
                   max_value = -quantile(value, probs= 0.95, na.rm = TRUE)) %>%
  ungroup() %>%
  # add column indicating that's "ghg"
  mutate(impact = 'avoided LUC CO2 emissions',
         year = as.numeric(year))


## SDG 3 ==================
data_summary_mort = mort %>%
  # merge with GCAM regions
  left_join(rfasst::fasst_reg %>%
              dplyr::rename('ISO3' = 'subRegionAlt'), by = 'fasst_region',
            multiple = 'all') %>%
  right_join(iso_gcam_regions %>%
               dplyr::mutate(iso = toupper(iso)) %>%
               dplyr::rename('ISO3' = 'iso'),
             by = 'ISO3') %>%
  left_join(id_gcam_regions, by = 'GCAM_region_ID') %>%
  # keep only meaningful columns
  select(year, scenario, method, value, pollutant, region) %>% distinct(., .keep_all = TRUE) %>%
  # reshape dataset
  tidyr::pivot_wider(names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ ifelse(. - Reference != 0, 100*(. - Reference)/Reference, 0))) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select('region','year','method','pollutant',matches("_diff$")) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median by region and pollutant
  group_by(region, year, pollutant) %>%
  summarise(median_value = median(value),
            min_value = quantile(value, probs= 0.05, na.rm = TRUE),
            max_value = quantile(value, probs= 0.95, na.rm = TRUE)) %>%
  ungroup() %>%
  # compute the total deaths by region (pm25 + o3)
  group_by(region, year) %>%
  summarise(median_value = -sum(median_value),
            min_value = -sum(min_value),
            max_value = -sum(max_value)) %>%
  ungroup() %>%
  # add column indicating that's "mort"
  mutate(impact = 'avoided premautre deaths',
         Units = 'People',
         year = as.numeric(year))

## plot ===================
data_summary = bind_rows(remove_attributes(data_summary_mort),
                         remove_attributes(data_summary_ghg),
                         remove_attributes(data_summary_ch4),
                         remove_attributes(data_summary_n2o),
                         # remove_attributes(data_summary_luc_co2),
                         remove_attributes(data_summary_irr_water),
                         remove_attributes(data_summary_water),
                         remove_attributes(data_summary_forestation),
                         # remove_attributes(data_summary_cropland),
                         remove_attributes(data_summary_crop_loss),
                         remove_attributes(data_summary_fertilizer)
) %>%
  filter(year == selected_year)
data_summary$impact = factor(data_summary$impact,
                             levels = c('avoided crop loss',
                                        're-forestation',
                                        'avoided water consumption',
                                        'avoided irrigated water consumption',
                                        'avoided GHG emissions',
                                        # 'avoided LUC CO2 emissions',
                                        'avoided CH4 emissions',
                                        'avoided N2O emissions',
                                        # 'avoided cropland area',
                                        'avoided fertilizer usage',
                                        'avoided premautre deaths'))
data_summary$region = forcats::fct_rev(data_summary$region)

pl_summary = ggplot(data_summary, aes(x = impact, y = region, fill = median_value)) +
  geom_tile(width = 1, height = 1) +
  coord_equal() +
  scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                       mid = '#f7f7f7', midpoint = 0,
                       name = '% difference') +
  guides(fill = guide_colorbar(title.position = "top")) +
  scale_y_discrete(position = 'right') +
  # labs
  labs(y = '', x = '', title = 'Percentual regional\ndifference of different\nsystem-wide effects') +
  # theme
  theme_light() +
  theme(legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=20, angle = -45, hjust = 0),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40),
        legend.key.height = unit(3, "cm"),
        legend.key.width = unit(1.5, "cm"))
ggsave(pl_summary, file = paste0(figures_path,dir_name,"/",'pl4_summary.pdf'),
       width = 300, height = 500, units = 'mm', limitsize = FALSE)


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
# nutrients (obtained through preprocess_nutrients_data.R)
macronutrients_data = readr::read_csv(paste0(folder_analysis_path,"data/macronutrients_data.csv"))
micronutrients_data = readr::read_csv(paste0(folder_analysis_path,"data/micronutrients_data_RvsM.csv")) %>%
  select(-1) %>%

  # read in MDER (calculated exogenously, FAO data)
  mder <- read.csv(paste0(folder_analysis_path,"data/MDER.csv")) %>%
  rename(mder_units = unit) %>%
  mutate(mder_units = 'kcal/capita/day')



## =========== Macronutrients (Protein and Fat) =================

## macronutrient by kcal of food consumption
macronutrients_basic = food_consumption_regional %>%
  # rename columns
  rename('GCAM_commodity' = 'technology') %>%
  rename('consumption' = 'value') %>%
  # aggregate population data
  left_join(pop_all_regions, by = c("year", "scenario", "region"),
            multiple = "all") %>%
  # convert from Pcal to kcal/capita/day
  mutate(consumptionPerCapita = (consumption * 1e12) / (population * 365),
         Units = "kcal/capita/day") %>%
  left_join(macronutrients_data %>%
              select(-year) %>%
              # match regions' id with regions' name
              left_join(regions_key %>% select(-1), by = 'GCAM_region_ID') %>%
              distinct(.),
            by = c('region','GCAM_commodity'), multiple = "all") %>%
  # compute total Protein and Fat [g/capita/day]
  mutate(gProteinPerCapita = consumptionPerCapita * gProteinPerKcal,
         gFatPerCapita = consumptionPerCapita * gFatPerKcal)


## plot
# WORLD trend
pl_macronutrients_world = ggplot(data = macronutrients_basic %>%
                                   rename_scen() %>%
                                   group_by(scenario, scenario_type, year) %>%
                                   summarise(gProteinPerCapita = median(gProteinPerCapita),
                                             gFatPerCapita = median(gFatPerCapita)) %>%
                                   tidyr::pivot_longer(cols = gProteinPerCapita:gFatPerCapita, names_to = 'macronutrient') %>%
                                   group_by(scenario_type, year, macronutrient) %>%
                                   mutate(min_value = min(value),
                                          max_value = max(value),
                                          median_value = median(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,macronutrient,scenario), color = interaction(scenario_type,macronutrient)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,macronutrient)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,macronutrient)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = macronutrients_scenario_palette, name = 'Scenario') +
  # scale_fill_manual(values = macronutrients_scenario_palette, name = 'Scenario') +
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
ggsave(pl_macronutrients_world, file = paste0(figures_path,dir_name,"/",'pl4_macronutrients_world.pdf'),
       width = 750, height = 500, units = 'mm')

# REGIONAL tren
pl_macronutrients_regional = ggplot(data = macronutrients_basic %>%
                                      rename_scen() %>%
                                      group_by(region, scenario, scenario_type, year) %>%
                                      summarise(gProteinPerCapita = median(gProteinPerCapita),
                                                gFatPerCapita = median(gFatPerCapita)) %>%
                                      tidyr::pivot_longer(cols = gProteinPerCapita:gFatPerCapita, names_to = 'macronutrient') %>%
                                      group_by(region, scenario_type, year, macronutrient) %>%
                                      mutate(min_value = min(value),
                                             max_value = max(value),
                                             median_value = median(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,macronutrient,scenario), color = interaction(scenario_type,macronutrient)), alpha = 0.3) +  # All runs lines
  # geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,macronutrient)), linewidth = 1, alpha = 1) +  # Median line
  # geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,macronutrient)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = macronutrients_scenario_palette, name = 'Scenario') +
  # scale_fill_manual(values = macronutrients_scenario_palette, name = 'Scenario') +
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
ggsave(pl_macronutrients_regional, file = paste0(figures_path,dir_name,"/",'pl4_macronutrients_regional_fixedScale.pdf'),
       width = 1000, height = 1000, units = 'mm')


pl_macronutrients_regional = ggplot(data = macronutrients_basic %>%
                                      rename_scen() %>%
                                      group_by(region, scenario, scenario_type, year) %>%
                                      summarise(gProteinPerCapita = median(gProteinPerCapita),
                                                gFatPerCapita = median(gFatPerCapita)) %>%
                                      tidyr::pivot_longer(cols = gProteinPerCapita:gFatPerCapita, names_to = 'macronutrient') %>%
                                      group_by(region, scenario_type, year, macronutrient) %>%
                                      mutate(min_value = min(value),
                                             max_value = max(value),
                                             median_value = median(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,macronutrient,scenario), color = interaction(scenario_type,macronutrient)), alpha = 0.3) +  # All runs lines
  # geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,macronutrient)), linewidth = 1, alpha = 1) +  # Median line
  # geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,macronutrient)), alpha = 0.15) +  # Shadow
  # scale_color_manual(values = macronutrients_scenario_palette, name = 'Scenario') +
  # scale_fill_manual(values = macronutrients_scenario_palette, name = 'Scenario') +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(y = 'g/capita/day', x = '', title = 'Regional macronutrients intake (free scale)') +
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
ggsave(pl_macronutrients_regional, file = paste0(figures_path,dir_name,"/",'pl4_macronutrients_regional_freeScale.pdf'),
       width = 1000, height = 1000, units = 'mm')



## =========== Micronutrients (Minerals & Vitamins) =================

###### plot
## -- bars (per difference)
micronutrients_diffPer_regional = micronutrients_data %>%
  # compute diff between intake and RNI
  mutate(diff = 100*(total_micronutrient_intake - byRegC_rni)/byRegC_rni) %>%
  # compute median by scenario type
  mutate(scenario_type = ifelse(scenario == 'Reference','Reference','Behavior change')) %>%
  dplyr::group_by(scenario_type,region,year,nutrient_name,nutrient_units) %>%
  dplyr::summarise(median_value = median(diff),
                   min_value = min(diff),
                   max_value = max(diff)) %>%
  # filter desired year
  dplyr::filter(year == selected_year)


pl_micronutrients_diffPer_regional_bars <- ggplot() +
  # barchart
  geom_bar(data = micronutrients_diffPer_regional |> filter(scenario_type != 'Reference'),
           aes(x = as.factor(nutrient_name), y = median_value, fill = as.factor(nutrient_name)),
           stat = "identity", color = NA, width = 0.5) +
  scale_fill_manual(values = c25, name = '') +
  # facet
  facet_grid(scenario_type ~ region, scales = 'fixed') +
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
ggsave(pl_micronutrients_diffPer_regional_bars, file = paste0(figures_path,dir_name, '/pl4_micronutrients_diffPer_BC_regional_bars.pdf'),
       width = 2000, height = 1000, units = 'mm', limitsize = FALSE)

pl_micronutrients_diffPer_regional_bars <- ggplot() +
  # barchart
  geom_bar(data = micronutrients_diffPer_regional |> filter(scenario_type == 'Reference'),
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
ggsave(pl_micronutrients_diffPer_regional_bars, file = paste0(figures_path,dir_name, '/pl4_micronutrients_diffPer_Ref_regional_bars.pdf'),
       width = 1000, height = 1000, units = 'mm')



## total diff regional
micronutrients_diffPer_regional = micronutrients_data %>%
  # compute difference between Reference and runs
  tidyr::pivot_wider(names_from = 'scenario', values_from = 'total_micronutrient_intake') %>%
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median by scenario type
  dplyr::group_by(region,year,nutrient_name,nutrient_units) %>%
  dplyr::summarise(median_value = median(value),
                   min_value = min(value),
                   max_value = max(value)) %>%
  # filter desired year
  dplyr::filter(year == selected_year)


pl_micronutrients_diffPer_regional <- ggplot() +
  # barchart
  geom_bar(data = micronutrients_diffPer_regional,
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
        legend.key.size = unit(1,'cm'), legend.position = c(0.5,0.05), legend.direction = 'horizontal',
        strip.text = element_text(size = 20, color = 'black'),
        strip.background =element_rect(fill="transparent"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(nrow = 3)) +
  # title
  labs(title = paste('Percentual difference between intake in BC and Ref in', selected_year))
ggsave(pl_micronutrients_diffPer_regional, file = paste0(figures_path,dir_name, '/pl4_micronutrients_diffPer_betweenScen_regional_bars.pdf'),
       width = 800, height = 550, units = 'mm')


## total diff world
micronutrients_diffPer_world = micronutrients_data %>%
  # compute difference between Reference and runs
  tidyr::pivot_wider(names_from = 'scenario', values_from = 'total_micronutrient_intake') %>%
  dplyr::mutate_at(vars(starts_with(prefix)), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference',other_cols)) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with(prefix), names_to = 'scenario') %>%
  # compute median by scenario type
  dplyr::group_by(year,nutrient_name,nutrient_units) %>%
  dplyr::summarise(median_value = median(value),
                   min_value = min(value),
                   max_value = max(value)) %>%
  # filter desired year
  dplyr::filter(year == selected_year)


pl_micronutrients_diffPer_world <- ggplot() +
  # barchart
  geom_bar(data = micronutrients_diffPer_world,
           aes(x = as.factor(nutrient_name), y = median_value, fill = as.factor(nutrient_name)),
           stat = "identity", color = NA, width = 0.5) +
  scale_fill_manual(values = c25, name = '') +
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
  labs(title = paste('Percentual difference between intake in BC and Ref in', selected_year))
ggsave(pl_micronutrients_diffPer_world, file = paste0(figures_path,dir_name, '/pl4_micronutrients_diffPer_betweenScen_world_bars.pdf'),
       width = 550, height = 500, units = 'mm')





## ======================= Food security metrics =============================

kcal_intake = food_consumption_regional %>%
  # TODO: find data of nutritional values of FiberCrop (introduce it in the average_data)
  filter(technology != 'FiberCrop') %>%
  left_join(pop_all_regions, by = c("year", "scenario", "region")) %>%
  # convert from Pcal to kcal/day
  mutate(value = (value * 1e12) / (population * 365),
         Units = "kcal/capita/day") %>%
  # rename columns
  rename('GCAM_commodity' = 'technology',
         'consumption' = 'value') %>%
  # total kcal intake
  group_by(Units,region,scenario,year,population) %>%
  summarise(consumption = sum(consumption))

kcal_mder = left_join(mder, weighted_pop_sex_age) %>%
  mutate(mder = mder * pop_sex_age) %>%
  group_by(mder_units,year,region) %>%
  summarise(mder = sum(mder),
            population = sum(pop_sex_age)) %>%
  mutate(mder = mder / population)


# comparison between DER and DES
des_der = merge(kcal_intake %>%
                  select(-population),
                kcal_mder %>%
                  select(-c(population, mder_units))) %>%
  mutate(diff = consumption - mder) %>%
  rename_scen() %>%
  pivot_longer(cols = c(consumption, mder), names_to = 'type') %>%
  filter(!(type == "mder")) # TODO: consider DER and compute some food security indicator


plt_kcal_intake = ggplot(des_der %>%
                           group_by(year, region, scenario_type, type) %>%
                           mutate(median_value = median(value),
                                  min_value = min(value),
                                  max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario,type), color = interaction(scenario_type,type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, group = interaction(scenario,type), color = interaction(scenario_type,type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,type)), alpha = 0.15) +  # Shadow
  facet_wrap(. ~ region, scales = 'free') +
  scale_color_manual(values = kcal_mder_scenario_palette,
                     breaks = kcal_mder_order_palette,
                     labels = kcal_mder_labels_palette,
                     name = 'Variable') +
  scale_fill_manual(values = kcal_mder_scenario_palette,
                    breaks = kcal_mder_order_palette,
                    labels = kcal_mder_labels_palette,
                    name = 'Variable') +
  # labs
  labs(y = 'kcal/capita/day', x = '', labs = 'Regional daily per capita kcal consumption (free scales)') +
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
ggsave(plt_kcal_intake, file = paste0(figures_path,dir_name,"/",'pl4_kcal_intake_regional_freeScales.pdf'),
       width = 1000, height = 1000, units = 'mm')

plt_kcal_intake = ggplot(des_der %>%
                           group_by(year, region, scenario_type, type) %>%
                           mutate(median_value = median(value),
                                  min_value = min(value),
                                  max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario,type), color = interaction(scenario_type,type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, group = interaction(scenario,type), color = interaction(scenario_type,type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,type)), alpha = 0.15) +  # Shadow
  facet_wrap(. ~ region, scales = 'fixed') +
  scale_color_manual(values = kcal_mder_scenario_palette,
                     breaks = kcal_mder_order_palette,
                     labels = kcal_mder_labels_palette,
                     name = 'Variable') +
  scale_fill_manual(values = kcal_mder_scenario_palette,
                    breaks = kcal_mder_order_palette,
                    labels = kcal_mder_labels_palette,
                    name = 'Variable') +
  # labs
  labs(y = 'kcal/capita/day', x = '', labs = 'Regional daily per capita kcal consumption (fixed scales)') +
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
ggsave(plt_kcal_intake, file = paste0(figures_path,dir_name,"/",'pl4_kcal_intake_regional_fixedScales.pdf'),
       width = 1000, height = 1000, units = 'mm')


