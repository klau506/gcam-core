#### prj_analysis.
####
#### Main script to analyse the results

#### INPUTS
# RData

## Set the working directory and load libraries
# setwd('/scratch/bc3lc/gcam-core-iamcompact-xin')
# libP <- .libPaths()
# .libPaths(c(libP,"/scratch/bc3lc/R-libs/4.1"))
#
# library(dplyr)
# library(tidyr)
# library(rgcam)

setwd('C:/GCAM/GCAM_7.0_Claudia/gcam-core-iamcompact/diets_analysis')
# setwd('C:\\Users\\claudia.rodes\\Documents\\IAM_COMPACT\\gcam-iamcompact-xin\\diets_analysis')

.libPaths('C:/Users/claudia.rodes/Documents/R/win-library/4.1-gcamdata_no_CP/')
library(dplyr)
library(magrittr)
library(ggplot2)

source('module_style.R')

##### Load food consumption data ---------------------------------------------------
assign('queries_all', get(load('outputs/queries_all_2.RData')))
assign('queries_ref', get(load('outputs/queries_ref_2.RData')))
assign('mort_queries_all', get(load('outputs/mort_queries_all_2.RData')))
assign('mort_queries_ref', get(load('outputs/mort_queries_ref_2.RData')))

figures_path <- 'figures'
selected_year <- 2050

#####################################################################################
#####################################################################################
# PROTEIN PERCENTAGE
#####################################################################################

##### PLANT  ===============================================================================
plant_percentage <- rbind(queries_all$food_consumption_regional,
                          queries_ref$food_consumption_regional) %>%
  dplyr::filter(nestingSector1 == 'Protein') %>%
  dplyr::mutate(is_plant = ifelse(nestingSector2 == 'Plant',TRUE,FALSE)) %>%
  # compute the total and plant Pcal by region
  dplyr::group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, region, is_plant, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  # compute the plant % (plant/total food consumption)
  tidyr::pivot_wider(names_from = 'is_plant', values_from = 'value') %>%
  dplyr::mutate(value = `TRUE` / (`TRUE` + `FALSE`)) %>%
  dplyr::mutate(Units = 'Percentage') %>%
  select(-`TRUE`) %>%
  select(-`FALSE`)


# plot
plot_data <- plant_percentage %>% dplyr::filter(scen_type %in% c('spp', 'ref'))
palette <- create_palette(unique(plot_data$scenario))

spp_protein <- ggplot(data = plot_data,
                      aes(x = year, y = value, group = scenario, color = scenario)) +
  geom_line() +
  scale_y_continuous(breaks = seq(0.1, 1, by = 0.1)) +
  scale_color_manual(values = palette) +
  facet_wrap(. ~ region, nrow = 8)

ggsave(spp_protein, file = file.path(figures_path, paste0('spp_protein_',selected_year,'.png')), width = 400, height = 600, units = 'mm')


##### RUMINANT ===============================================================================
rumin_percentage <- rbind(queries_all$food_consumption_regional,
                          queries_ref$food_consumption_regional) %>%
  dplyr::filter(nestingSector2 == 'Animal') %>%
  dplyr::mutate(is_rumin = ifelse(nestingSector3 == 'Ruminant',TRUE,FALSE)) %>%
  # compute the total and plant Pcal by region
  dplyr::group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, region, is_rumin, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  # compute the plant % (plant/total food consumption)
  tidyr::pivot_wider(names_from = 'is_rumin', values_from = 'value') %>%
  dplyr::mutate(value = `TRUE` / (`TRUE` + `FALSE`)) %>%
  dplyr::mutate(Units = 'Percentage') %>%
  select(-`TRUE`) %>%
  select(-`FALSE`)

# plot
plot_data <- rumin_percentage %>% dplyr::filter(scen_type %in% c('snr', 'ref'))
palette <- create_palette(unique(plot_data$scenario))

snr_protein <- ggplot(data = plot_data,
                      aes(x = year, y = value, group = scenario, color = scenario)) +
  geom_line() +
  scale_y_continuous(breaks = seq(0.1, 1, by = 0.1)) +
  scale_color_manual(values = palette) +
  facet_wrap(. ~ region, nrow = 8)

ggsave(snr_protein, file = file.path(figures_path, paste0('snr_protein_',selected_year,'.png')), width = 400, height = 600, units = 'mm')




#####################################################################################
#####################################################################################
# DIET COMPOSITION
#####################################################################################

food_subsector <- read.csv('inputs/nutrition/food_subsector.csv', skip = 3) %>%
  rename(technology = subsector) %>%
  mutate(beautiful_name = paste0(category, '|', beautiful_name))

food_consumption <- rbind(queries_all$food_consumption_regional,
                          queries_ref$food_consumption_regional) %>%
  dplyr::left_join(food_subsector, by = 'technology') %>%
  group_by(Units, region, scen_type, technology, year, beautiful_name) %>%
  summarise(value = median(value)) %>%
  ungroup()

food_consumption$beautiful_name <- factor(food_consumption$beautiful_name,
                                          levels = c(  'Crops|Corn','Crops|Fiber crops','Crops|Other grain crops','Crops|Soy bean','Crops|Wheat',
                                                       'Crops|Fruits','Crops|Vegetables',
                                                       'Crops|Oil crops','Crops|Palm oil crops',
                                                       'Crops|Rice','Crops|Root Tubers','Crops|Specialty crops and species','Crops|Sugar crops',
                                                       'Crops|Legumes','Crops|Nuts and Seeds',
                                                       'Livestock products|Beef meat','Livestock products|Dairy','Livestock products|Pork meat','Livestock products|Poultry meat','Livestock products|Sheep and Goat meat',
                                                       'Seafood|Fish'  ))

####### BY REGION
pl_food_consumption <- ggplot(data = food_consumption %>%
                                filter(year == 2050) %>%
                                mutate(scen_type = toupper(scen_type)),
                              aes(x = scen_type, y = value, group = beautiful_name, color = beautiful_name, fill = beautiful_name)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_color_manual(values = food_scenario_palette, name = 'Calorie Source') +
  scale_fill_manual(values = food_scenario_palette, name = 'Calorie Source') +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # style
  labs(y = 'Pcal', x = 'Scenario Type') +
  theme_light() +
  theme(panel.grid.major.y = element_line(color = 'grey20'),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white",
                                       colour = 'grey',linewidth = 2),
        panel.background = element_rect(fill = "white"),
        legend.position = 'bottom', legend.direction = 'horizontal',
        strip.text = element_text(size = 20, color = 'black'),
        strip.background =element_rect(fill="transparent"),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_food_consumption, file = file.path(figures_path, paste0('sdg0_diet_composition_freeS_',selected_year,'.png')),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)

pl_food_consumption <- ggplot(data = food_consumption %>%
                                filter(year == 2050) %>%
                                mutate(scen_type = toupper(scen_type)),
                              aes(x = scen_type, y = value, group = beautiful_name, color = beautiful_name, fill = beautiful_name)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_color_manual(values = food_scenario_palette, name = 'Calorie Source') +
  scale_fill_manual(values = food_scenario_palette, name = 'Calorie Source') +
  # facet
  facet_wrap(. ~ region, scales = 'fixed') +
  # style
  labs(y = 'Pcal', x = 'Scenario Type') +
  theme_light() +
  theme(panel.grid.major.y = element_line(color = 'grey20'),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white",
                                       colour = 'grey',linewidth = 2),
        panel.background = element_rect(fill = "white"),
        legend.position = 'bottom', legend.direction = 'horizontal',
        strip.text = element_text(size = 20, color = 'black'),
        strip.background =element_rect(fill="transparent"),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_food_consumption, file = file.path(figures_path, paste0('sdg0_diet_composition_fixedS_',selected_year,'.png')),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)


####### BY REGION & PER CAPITA
food_consumption_percap <- food_consumption %>%
  # convert from Pcal to kcal/day
  dplyr::left_join(rbind(queries_all$pop_all_regions,
                         queries_ref$pop_all_regions) %>%
                     select(scen_type, region, year, population) %>%
                     unique(), by = c("year", "scen_type", "region")) %>%
  dplyr::mutate(value = (value * 1e12) / (population * 365),
                Units = "kcal/capita/day") %>%
  # convert population to million population
  dplyr::mutate(population = population / 1e6)

food_consumption_percap_toplot <- food_consumption_percap
food_consumption_percap_toplot$region <- factor(food_consumption_percap_toplot$region)
food_consumption_percap_toplot <- food_consumption_percap_toplot %>%
  # complete cases
  select(-technology) %>% unique() %>%
  complete(beautiful_name, nesting(year, scen_type, region, Units, population), fill = list(value = 0)) %>%
  # column to order
  group_by(region, year, scen_type) %>%
  mutate(to_order = sum(value)) %>%
  ungroup() %>%
  arrange(to_order)

# order dataset by beautiful name & region's order (given by the total kcal/cap consumption)
custom_order = c(  'Crops|Corn','Crops|Fiber crops','Crops|Other grain crops','Crops|Soy bean','Crops|Wheat',
                   'Crops|Fruits','Crops|Vegetables',
                   'Crops|Oil crops','Crops|Palm oil crops',
                   'Crops|Rice','Crops|Root Tubers','Crops|Specialty crops and species','Crops|Sugar crops',
                   'Crops|Legumes','Crops|Nuts and Seeds',
                   'Livestock products|Beef meat','Livestock products|Dairy','Livestock products|Pork meat','Livestock products|Poultry meat','Livestock products|Sheep and Goat meat',
                   'Seafood|Fish'  )
food_consumption_percap_toplot <- food_consumption_percap_toplot[order(-food_consumption_percap_toplot$to_order,
                                                                       match(food_consumption_percap_toplot$beautiful_name, custom_order)), ]

food_consumption_percap_toplot <- food_consumption_percap_toplot %>%
  # compute rectangles positions
  filter(year == selected_year, scen_type == 'ref') %>%
  mutate(ymax = ave(value, region, FUN=cumsum)) %>%
  mutate(ymin = ymax - value) %>%
  mutate(right = ave(population, beautiful_name, FUN=cumsum)) %>%
  mutate(left = right - population) %>%
  mutate(mean = left + (right - left) / 2)

p <- ggplot(data = food_consumption_percap_toplot, aes(ymin = 0)) +
  geom_rect(data = food_consumption_percap_toplot,
            aes(xmin=left, xmax = right, ymax = ymax, ymin = ymin, fill = beautiful_name),
            color="white") +
  # region names
  ggrepel::geom_label_repel(data = food_consumption_percap_toplot %>%
                              filter(beautiful_name == 'Crops|Wheat'),
                            aes(x = mean, y = to_order, label = region),
                            size = 7,
                            direction = "y",
                            max.overlaps = 15,
                            nudge_y = 10) +
  # colors
  scale_color_manual(values = food_scenario_palette, name = 'Calorie Source') +
  scale_fill_manual(values = food_scenario_palette, name = 'Calorie Source') +
  # labels
  labs(x = "Populationn (million)", y = "kcal per capita per day") +
  # theme
  theme_light() +
  theme(panel.grid.major.y = element_line(color = 'grey20'),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white",
                                       colour = 'grey',linewidth = 2),
        panel.background = element_rect(fill = "white"),
        legend.position = 'bottom', legend.direction = 'horizontal',
        strip.text = element_text(size = 20, color = 'black'),
        strip.background =element_rect(fill="transparent"),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40, angle = 90),
        title = element_text(size = 40)) +
  guides(color = guide_legend(ncol = 3),
         fill = guide_legend(ncol = 3))
ggsave(p, file = file.path(figures_path,paste0('sdg0_kcal_cap_day_by_pop.png')),
       width = 1000, height = 700, units = 'mm', limitsize = F)



#####################################################################################
#####################################################################################
# SDG15 - LAND USE management
#####################################################################################

############## INDICATOR 1: % of re-/aff-forestation ================================
# compute the Land Indicator (Percent of Re/Afforestation)
land_indicator_forestLand = merge(queries_all$land_use_regional %>%
                                    mutate(value = value * 100) %>% # convert Thous km2 to Mha
                                    mutate(forest = ifelse(land_use_type == 'Forest', 'Forest', 'NoForest')) %>%
                                    group_by(region, scenario, scen_type, scen_path, final_share, peak_year, slope, year, forest) %>%
                                    summarize(value = sum(value)) %>%
                                    ungroup() %>%
                                    group_by(region, scenario, scen_type, scen_path, final_share, peak_year, slope, year) %>%
                                    summarize(percent_forest = 100 * sum(value[forest == "NoForest"]) / sum(value[forest %in% c("NoForest", "Forest")]),
                                              total_land = sum(value[forest %in% c("NoForest", "Forest")])) %>%
                                    ungroup(),
                                  queries_ref$land_use_regional %>%
                                    mutate(value = value * 100) %>% # convert Thous km2 to Mha
                                    mutate(forest = ifelse(land_use_type == 'Forest', 'Forest', 'NoForest')) %>%
                                    group_by(region, scenario, year, forest) %>%
                                    summarize(value = sum(value)) %>%
                                    ungroup() %>%
                                    group_by(region, scenario, year) %>%
                                    summarize(percent_forest_ref = 100 * sum(value[forest == "NoForest"]) / sum(value[forest %in% c("NoForest", "Forest")]),
                                              total_land_ref = sum(value[forest %in% c("NoForest", "Forest")])) %>%
                                    ungroup() %>%
                                    select(-scenario),
                                  by = c('year','region'))

# aggregate Global Value with Weighted Average
land_indicator_global_forestLand = land_indicator_forestLand %>%
  mutate(weight = percent_forest * total_land,
         weight_ref = percent_forest_ref * total_land_ref) %>%
  group_by(scen_type, scen_path, year) %>%
  summarize(percent_forest = sum(weight) / sum(total_land),
            percent_forest_ref = sum(weight_ref) / sum(total_land_ref)) %>%
  ungroup() %>%
  # compute Abs difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = ref_value - value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(region,year,scen_type,scen_path,scen_group) %>%
  dplyr::summarise(median_diff = median(diff))


ag_water_consumption_regional_diffAbs <- merge(queries_all$water_irr_rfd_regional %>%
                                                 dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                                 dplyr::group_by(region, year, scenario, scen_type, scen_path, final_share, peak_year, slope) %>%
                                                 dplyr::summarise(value = sum(value)) %>%
                                                 dplyr::ungroup(),
                                               queries_ref$water_irr_rfd_regional %>%
                                                 dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                                 dplyr::group_by(region, year, scenario, scen_type) %>%
                                                 dplyr::summarise(ref_value = sum(value)) %>%
                                                 dplyr::ungroup() %>%
                                                 dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                               by = c('region','year')) %>%
  # compute Abs difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = ref_value - value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(region,year,scen_type,scen_path,scen_group) %>%
  dplyr::summarise(median_diff = median(diff))

############## INDICATOR 2: % of unmanaged land =====================================
# compute the Land Indicator (Percent of Unmanaged Land)
land_indicator_unmanagedLand = rbind(queries_all$detailed_land_allocation,
                                     queries_ref$detailed_land_allocation) %>%
  mutate(value = value * 0.1) %>% # Convert to Mha
  mutate(management = case_when(
    grepl("ProtectedUnmanagedForest|UnmanagedForest|Shrubland|ProtectedShrubland|Grassland|ProtectedGrassland|UnmanagedPasture|ProtectedUnmanagedPasture|Tundra|RockIceDesert", land_indicator_pre$landleaf) ~ "Unmanaged",
    TRUE ~ "Managed")) %>%
  group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, region, year, management) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, region, year) %>%
  summarize(percent_unmanaged = 100 * sum(value[management == "Unmanaged"]) / sum(value[management %in% c("Unmanaged", "Managed")]),
            total_land = sum(value[management %in% c("Unmanaged", "Managed")])) %>%
  ungroup()

# aggregate Global Value with Weighted Average
land_indicator_global_unmanagedLand = land_indicator_unmanagedLand %>%
  mutate(weight = percent_unmanaged * total_land) %>%
  group_by(scenario, year) %>%
  summarize(percent_unmanaged = sum(weight) / sum(total_land)) %>%
  ungroup()


############## AREA ===============================================================================


#### ABSOLUTE
land_use_diffAbs_world = merge(queries_all$land_use_world,
                               queries_ref$land_use_world %>%
                                 dplyr::select(landleaf, land_use_type, year, Units, ref_value = value),
                               by = c('landleaf','land_use_type','year','Units')) %>%
  # select scenarios to have a matching plot
  dplyr::filter(scenario %in% c('spp_all_90_x0_2045_k_0.805', 'snr_all_10_x0_2045_k_0.805')) %>%
  # compute median by land_use_type
  dplyr::group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, Units, land_use_type, year) %>%
  dplyr::summarise(value = sum(value),
                   ref_value = sum(ref_value)) %>%
  dplyr::ungroup() %>%
  # compute difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = value - ref_value) %>%
  dplyr::select(-value) %>% dplyr::select(-ref_value) %>%
  dplyr::mutate(scen_type = toupper(scen_type))


pl_land_use_diffAbs_world = ggplot(data = land_use_diffAbs_world) +
  geom_area(aes(x = year, y = diff, fill = land_use_type), alpha = 1) +  # Median area
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(. ~ scen_type) +
  scale_fill_manual(values = land_use_scenario_palette, name = 'Land Type',
                    breaks = land_use_order_palette) +
  # labs
  labs(y = expression(paste('Change in thous. ', km^2, ' compared to Reference')), x = '') +
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
ggsave(pl_land_use_diffAbs_world, file = file.path(figures_path,paste0('sdg15_land_use_diffAbs_world.png')),
       width = 575, height = 400, units = 'mm')


#### PERCENTAGE
land_use_diffPer_world = merge(queries_all$land_use_world,
                               queries_ref$land_use_world %>%
                                 dplyr::select(landleaf, land_use_type, year, Units, ref_value = value),
                               by = c('landleaf','land_use_type','year','Units')) %>%
  # select scenarios to have a matching plot
  dplyr::filter(scenario %in% c('spp_all_90_x0_2045_k_0.805', 'snr_all_10_x0_2045_k_0.805')) %>%
  # compute median by land_use_type
  dplyr::group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, Units, land_use_type, year) %>%
  dplyr::summarise(value = sum(value),
                   ref_value = sum(ref_value)) %>%
  dplyr::ungroup() %>%
  # compute difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = ifelse(ref_value != 0, 100*(value - ref_value)/ref_value, 0)) %>%
  dplyr::select(-value) %>% dplyr::select(-ref_value) %>%
  dplyr::mutate(scen_type = toupper(scen_type))


pl_land_use_diffPer_world = ggplot(data = land_use_diffPer_world) +
  geom_area(aes(x = year, y = diff, fill = land_use_type), alpha = 1) +  # Median area
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(. ~ scen_type) +
  scale_fill_manual(values = land_use_scenario_palette, name = 'Land Type',
                    breaks = land_use_order_palette) +
  # labs
  labs(y = expression(paste('Change in thous. ', km^2, ' compared to Reference')), x = '') +
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
ggsave(pl_land_use_diffPer_world, file = file.path(figures_path,paste0('sdg15_land_use_diffPer_world.png')), width = 575, height = 400, units = 'mm')




############## BARS ===============================================================================


#### ABSOLUTE

land_use_diffAbs_world = merge(queries_all$land_use_world %>%
                                 dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                 dplyr::group_by(year, scenario, scen_type, scen_path, final_share, peak_year, slope, land_use_type) %>%
                                 dplyr::summarise(value = sum(value)) %>%
                                 dplyr::ungroup(),
                               queries_ref$land_use_world %>%
                                 dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                 dplyr::group_by(year, scenario, scen_type, land_use_type) %>%
                                 dplyr::summarise(ref_value = sum(value)) %>%
                                 dplyr::ungroup() %>%
                                 dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                               by = c('land_use_type','year')) %>%
  # compute Abs difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = value - ref_value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(land_use_type,year,scen_type,scen_path,scen_group) %>%
  dplyr::summarise(median_diff = median(diff)) %>%
  dplyr::ungroup()


pl_land_use_diffAbs_world_bars <- ggplot(data = land_use_diffAbs_world %>% dplyr::filter(year == selected_year), aes(x = median_diff, y = scen_path, fill = land_use_type)) +
  geom_bar(stat = "identity", position = "identity") +
  facet_grid(scen_group ~ scen_type, scales = "free") +
  scale_fill_manual(values = land_use_scenario_palette, name = 'Land Type',
                    breaks = land_use_order_palette) +
  geom_vline(xintercept = 0, linewidth = 1.2) +
  labs(y = '', x = expression(paste('thous ', km^2, ' difference'))) +
  theme_light() +
  theme(panel.grid.major.y = element_line(color = 'grey20'),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white",
                                       colour = 'grey',linewidth = 2),
        panel.background = element_rect(fill = "white"),
        legend.position = 'bottom', legend.direction = 'horizontal',
        strip.text = element_text(size = 20, color = 'black'),
        strip.background =element_rect(fill="transparent"),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_land_use_diffAbs_world_bars, file = file.path(figures_path, paste0('sdg13_landType_abs_',selected_year,'.png')), width = 475, height = 500, units = 'mm')



#### PERCENT
land_use_diffPer_world = merge(queries_all$land_use_world %>%
                                 dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                 dplyr::group_by(year, scenario, scen_type, scen_path, final_share, peak_year, slope, land_use_type) %>%
                                 dplyr::summarise(value = sum(value)) %>%
                                 dplyr::ungroup(),
                               queries_ref$land_use_world %>%
                                 dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                 dplyr::group_by(year, scenario, scen_type, land_use_type) %>%
                                 dplyr::summarise(ref_value = sum(value)) %>%
                                 dplyr::ungroup() %>%
                                 dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                               by = c('land_use_type','year')) %>%
  # compute Per difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = 100*(value - ref_value)/ref_value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(land_use_type,year,scen_type,scen_path,scen_group) %>%
  dplyr::summarise(median_diff = median(diff)) %>%
  dplyr::ungroup()


pl_land_use_diffPer_world_bars <- ggplot(data = land_use_diffPer_world %>% dplyr::filter(year == selected_year), aes(x = median_diff, y = scen_path, fill = land_use_type)) +
  geom_bar(stat = "identity", position = "identity") +
  facet_grid(scen_group ~ scen_type, scales = "free") +
  scale_fill_manual(values = land_use_scenario_palette, name = 'Land Type',
                    breaks = land_use_order_palette) +
  geom_vline(xintercept = 0, linewidth = 1.2) +
  labs(y = '', x = expression(paste('% difference'))) +
  theme_light() +
  theme(panel.grid.major.y = element_line(color = 'grey20'),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white",
                                       colour = 'grey',linewidth = 2),
        panel.background = element_rect(fill = "white"),
        legend.position = 'bottom', legend.direction = 'horizontal',
        strip.text = element_text(size = 20, color = 'black'),
        strip.background =element_rect(fill="transparent"),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_land_use_diffPer_world_bars, file = file.path(figures_path, paste0('sdg13_landType_per_',selected_year,'.png')), width = 475, height = 500, units = 'mm')




#### HEATMAPS ===============================================================================


#### ABSOLUTE WITH LAND TYPE
landType_regional_diffAbs <- merge(queries_all$land_use_regional %>%
                                     dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                     dplyr::group_by(region, year, scenario, scen_type, scen_path, final_share, peak_year, slope, land_use_type) %>%
                                     dplyr::summarise(value = sum(value)) %>%
                                     dplyr::ungroup(),
                                   queries_ref$land_use_regional %>%
                                     dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                     dplyr::group_by(region, year, scenario, scen_type, land_use_type) %>%
                                     dplyr::summarise(ref_value = sum(value)) %>%
                                     dplyr::ungroup() %>%
                                     dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                   by = c('region','year','land_use_type')) %>%
  # compute Abs difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = value - ref_value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(region,year,scen_type,scen_path,scen_group,land_use_type) %>%
  dplyr::summarise(median_diff = median(diff))

pl_landType_regional_diffAbs_heatmap <- ggplot(landType_regional_diffAbs %>%
                                                 dplyr::filter(year == selected_year),
                                               aes(x = scen_group, y = region, fill = median_diff)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen",
                       name = expression(paste('thous ', km^2, " difference")), ) +
  facet_grid(scen_type ~ land_use_type) +
  # labs
  labs(y = '', x = 'Scenario type') + #, title = 'Absolute regional difference\nin water consumption'
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 35),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=25, angle = 90, hjust = 1, vjust = 0.25),
        axis.text.y = element_text(size=25),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 30))
ggsave(pl_landType_regional_diffAbs_heatmap, file = file.path(figures_path, paste0('sdg15_landType_abs_heatmap_',selected_year,'.png')),
       width = 750, height = 600, units = 'mm', limitsize = FALSE)


#### PERCENT WITH LAND TYPE
landType_regional_diffPer <- merge(queries_all$land_use_regional %>%
                                     dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                     dplyr::group_by(region, year, scenario, scen_type, scen_path, final_share, peak_year, slope, land_use_type) %>%
                                     dplyr::summarise(value = sum(value)) %>%
                                     dplyr::ungroup(),
                                   queries_ref$land_use_regional %>%
                                     dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                     dplyr::group_by(region, year, scenario, scen_type, land_use_type) %>%
                                     dplyr::summarise(ref_value = sum(value)) %>%
                                     dplyr::ungroup() %>%
                                     dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                   by = c('region','year','land_use_type')) %>%
  # compute Per difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = ifelse(ref_value == 0, 0, 100*(value - ref_value)/ref_value)) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(region,year,scen_type,scen_path,scen_group,land_use_type) %>%
  dplyr::summarise(median_diff = median(diff))

pl_landType_regional_diffPer_heatmap <- ggplot(landType_regional_diffPer %>%
                                                 dplyr::filter(year == selected_year),
                                               aes(x = scen_group, y = region, fill = median_diff)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen",
                       name = expression(paste("% difference")), ) +
  facet_grid(scen_type ~ land_use_type) +
  # labs
  labs(y = '', x = 'Scenario type') + #, title = 'Perolute regional difference\nin water consumption'
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 35),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=25, angle = 90, hjust = 1, vjust = 0.25),
        axis.text.y = element_text(size=25),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 30))
ggsave(pl_landType_regional_diffPer_heatmap, file = file.path(figures_path, paste0('sdg15_landType_per_heatmap_',selected_year,'.png')),
       width = 750, height = 600, units = 'mm', limitsize = FALSE)



#####################################################################################
#####################################################################################
# SDG6 - WATER consumption
#####################################################################################


pl_water_consumption_world <- ggplot(data = rbind(queries_all$water_consumption_world,
                                                  queries_ref$water_consumption_world) %>%
                                       dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                       dplyr::group_by(year, scen_type) %>%
                                       mutate(median_value = median(value)) %>%
                                       mutate(min_value = min(value)) %>%
                                       mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow

  geom_line(data = queries_ref$water_consumption_world %>%
              dplyr::mutate(scen_type = toupper(scen_type)), aes(x = year, y = value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line REF

  scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr, name = 'Scenario') +
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
        title = element_text(size = 40))
ggsave(pl_water_consumption_world, file = file.path(figures_path,'sdg6_annual_water_consumption_line.png'), width = 500, height = 400, units = 'mm')


ag_water_consumption_world <- rbind(queries_all$water_irr_rfd_world,
                                    queries_ref$water_irr_rfd_world) %>%
  dplyr::mutate(scen_type = toupper(scen_type)) %>%
  dplyr::group_by(year, scenario, scen_type) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year, scen_type) %>%
  dplyr::mutate(median_value = median(value),
                min_value = min(value),
                max_value = max(value)) %>%
  dplyr::ungroup()

pl_water_consumption_agriculture_world <- ggplot(data = ag_water_consumption_world) +
  geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr, name = 'Scenario') +
  # labs
  labs(y = expression(paste("Annual Water flows (thous ",km^2,")")), x = '') +
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
        title = element_text(size = 40))
ggsave(pl_water_consumption_agriculture_world, file = file.path(figures_path,'sdg6_annual_water_agriculture_consumption_line.png'), width = 500, height = 400, units = 'mm')





#### ABSOLUTE

water_irr_rfd_diffAbs_world = merge(queries_all$water_irr_rfd_world %>%
                                      dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                      dplyr::group_by(year, scenario, scen_type, scen_path, final_share, peak_year, slope, water) %>%
                                      dplyr::summarise(value = sum(value)) %>%
                                      dplyr::ungroup(),
                                    queries_ref$water_irr_rfd_world %>%
                                      dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                      dplyr::group_by(year, scenario, scen_type, water) %>%
                                      dplyr::summarise(ref_value = sum(value)) %>%
                                      dplyr::ungroup() %>%
                                      dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                    by = c('water','year')) %>%
  # compute Abs difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = ref_value - value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(water,year,scen_type,scen_path,scen_group) %>%
  dplyr::summarise(median_diff = median(diff))


pl_water_irr_rfd_diffAbs_world_bars <- ggplot(data = water_irr_rfd_diffAbs_world %>% dplyr::filter(year == selected_year), aes(x = median_diff, y = scen_path, fill = water)) +
  geom_bar(stat = "identity", position = "identity") +
  facet_grid(scen_group ~ scen_type, scales = "fixed") +
  scale_fill_brewer(palette = 'Paired', name = '') +
  geom_vline(xintercept = 0, linewidth = 1.2) +
  labs(y = '', x = expression(paste('thous ', km^2))) +
  theme_light() +
  theme(panel.grid.major.y = element_line(color = 'grey20'),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white",
                                       colour = 'grey',linewidth = 2),
        panel.background = element_rect(fill = "white"),
        legend.position = 'bottom', legend.direction = 'horizontal',
        strip.text = element_text(size = 20, color = 'black'),
        strip.background =element_rect(fill="transparent"),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_water_irr_rfd_diffAbs_world_bars, file = file.path(figures_path, paste0('sdg6_waterType_abs_',selected_year,'.png')), width = 400, height = 500, units = 'mm')



#### PERCENT
water_irr_rfd_diffPer_world = merge(queries_all$water_irr_rfd_world %>%
                                      dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                      dplyr::group_by(year, scenario, scen_type, scen_path, final_share, peak_year, slope, water) %>%
                                      dplyr::summarise(value = sum(value)) %>%
                                      dplyr::ungroup(),
                                    queries_ref$water_irr_rfd_world %>%
                                      dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                      dplyr::group_by(year, scenario, scen_type, water) %>%
                                      dplyr::summarise(ref_value = sum(value)) %>%
                                      dplyr::ungroup() %>%
                                      dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                    by = c('water','year')) %>%
  # compute Per difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = 100*(ref_value - value)/ref_value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(water,year,scen_type,scen_path,scen_group) %>%
  dplyr::summarise(median_diff = median(diff))


pl_water_irr_rfd_diffPer_world_bars <- ggplot(data = water_irr_rfd_diffPer_world %>% dplyr::filter(year == selected_year), aes(x = median_diff, y = scen_path, fill = water)) +
  geom_bar(stat = "identity", position = "identity") +
  facet_grid(scen_group ~ scen_type, scales = "fixed") +
  scale_fill_brewer(palette = 'Paired', name = '') +
  geom_vline(xintercept = 0, linewidth = 1.2) +
  labs(y = '', x = expression(paste('% difference'))) +
  theme_light() +
  theme(panel.grid.major.y = element_line(color = 'grey20'),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white",
                                       colour = 'grey',linewidth = 2),
        panel.background = element_rect(fill = "white"),
        legend.position = 'bottom', legend.direction = 'horizontal',
        strip.text = element_text(size = 20, color = 'black'),
        strip.background =element_rect(fill="transparent"),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_water_irr_rfd_diffPer_world_bars, file = file.path(figures_path, paste0('sdg6_waterType_per_',selected_year,'.png')), width = 400, height = 500, units = 'mm')



##### MAPS ==============================================================================


#### ABSOLUTE
ag_water_consumption_regional_diffAbs <- merge(queries_all$water_irr_rfd_regional %>%
                                                 dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                                 dplyr::group_by(region, year, scenario, scen_type, scen_path, final_share, peak_year, slope) %>%
                                                 dplyr::summarise(value = sum(value)) %>%
                                                 dplyr::ungroup(),
                                               queries_ref$water_irr_rfd_regional %>%
                                                 dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                                 dplyr::group_by(region, year, scenario, scen_type) %>%
                                                 dplyr::summarise(ref_value = sum(value)) %>%
                                                 dplyr::ungroup() %>%
                                                 dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                               by = c('region','year')) %>%
  # compute Abs difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = ref_value - value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(region,year,scen_type,scen_path,scen_group) %>%
  dplyr::summarise(median_diff = median(diff))

ag_water_consumption_regional_diffAbs_map <- ag_water_consumption_regional_diffAbs %>%
  # filter desired year
  dplyr::filter(year == selected_year) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  dplyr::inner_join(rfasst::GCAM_reg, by = 'GCAM Region', multiple = "all", relationship = "many-to-many") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3') %>%
  # subset scen
  dplyr::mutate(scen_to_subset = paste0(scen_type, '_', scen_group)) %>%
  dplyr::filter(scen_to_subset %in% c('SPP_all_90', 'SNR_all_10')) %>%
  dplyr::select(-scen_to_subset)

ag_water_consumption_regional_diffAbs_map = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                                    dplyr::mutate('adm0_a3' = dplyr::if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                                    dplyr::mutate('adm0_a3' = dplyr::if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                                    dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                                  ag_water_consumption_regional_diffAbs_map, by = 'adm0_a3')

# plot
pl_ag_water_consumption_regional_diffAbs_map <- ggplot() +
  # color map by regions
  geom_sf(data = ag_water_consumption_regional_diffAbs_map, aes(fill = median_diff)) +
  facet_wrap(. ~ scen_type, scales = 'fixed') +
  scale_fill_gradient2(low = "darkred", high = "darkgreen",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Annual Water flows difference (",km^3,")","\n"))) +
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
        strip.background =element_rect(fill="white"), title = element_text(size = 30))
ggsave(pl_ag_water_consumption_regional_diffAbs_map, file = file.path(figures_path, paste0('sdg6_waterAg_abs_map_',selected_year,'.png')), width = 500, height = 300, units = 'mm')



#### PERCENT
ag_water_consumption_regional_diffPer <- merge(queries_all$water_irr_rfd_regional %>%
                                                 dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                                 dplyr::group_by(region, year, scenario, scen_type, scen_path, final_share, peak_year, slope) %>%
                                                 dplyr::summarise(value = sum(value)) %>%
                                                 dplyr::ungroup(),
                                               queries_ref$water_irr_rfd_regional %>%
                                                 dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                                 dplyr::group_by(region, year, scenario, scen_type) %>%
                                                 dplyr::summarise(ref_value = sum(value)) %>%
                                                 dplyr::ungroup() %>%
                                                 dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                               by = c('region','year')) %>%
  # compute Per difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = 100*(ref_value - value)/ref_value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(region,year,scen_type,scen_path,scen_group) %>%
  dplyr::summarise(median_diff = median(diff))

ag_water_consumption_regional_diffPer_map <- ag_water_consumption_regional_diffPer %>%
  # filter desired year
  dplyr::filter(year == selected_year) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  dplyr::inner_join(rfasst::GCAM_reg, by = 'GCAM Region', multiple = "all", relationship = "many-to-many") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3') %>%
  # subset scen
  dplyr::mutate(scen_to_subset = paste0(scen_type, '_', scen_group)) %>%
  dplyr::filter(scen_to_subset %in% c('SPP_all_90', 'SNR_all_10')) %>%
  dplyr::select(-scen_to_subset)

ag_water_consumption_regional_diffPer_map = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                                    dplyr::mutate('adm0_a3' = dplyr::if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                                    dplyr::mutate('adm0_a3' = dplyr::if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                                    dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                                  ag_water_consumption_regional_diffPer_map, by = 'adm0_a3')

# plot
pl_ag_water_consumption_regional_diffPer_map <- ggplot() +
  # color map by regions
  geom_sf(data = ag_water_consumption_regional_diffPer_map, aes(fill = median_diff)) +
  facet_wrap(. ~ scen_type, scales = 'fixed') +
  scale_fill_gradient2(low = "darkred", high = "darkgreen",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Annual Water flows % difference","\n"))) +
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
        strip.background =element_rect(fill="white"), title = element_text(size = 30))
ggsave(pl_ag_water_consumption_regional_diffPer_map, file = file.path(figures_path, paste0('sdg6_waterAg_per_map_',selected_year,'.png')), width = 500, height = 300, units = 'mm')



#### HEATMAPS ===============================================================================


#### ABSOLUTE
pl_ag_water_consumption_regional_diffAbs_heatmap <- ggplot(ag_water_consumption_regional_diffAbs %>%
                                                             dplyr::filter(year == selected_year),
                                                           aes(x = scen_group, y = region, fill = median_diff)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen",
                       name = expression(paste(km^3, " difference")), ) +
  facet_wrap(. ~ scen_type) +
  # labs
  labs(y = '', x = 'Scenario type') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 35),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=25, angle = 90, hjust = 1, vjust = 0.25),
        axis.text.y = element_text(size=25),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 30))
ggsave(pl_ag_water_consumption_regional_diffAbs_heatmap, file = file.path(figures_path, paste0('sdg6_waterAg_abs_heatmap_',selected_year,'.png')),
       width = 400, height = 450, units = 'mm', limitsize = FALSE)



#### PERCENT
pl_ag_water_consumption_regional_diffPer_heatmap <- ggplot(ag_water_consumption_regional_diffPer %>%
                                                             dplyr::filter(year == selected_year),
                                                           aes(x = scen_group, y = region, fill = median_diff)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen",
                       name = '% difference') +
  facet_wrap(. ~ scen_type) +
  # labs
  labs(y = '', x = 'Scenario type') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 35),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=25, angle = 90, hjust = 1, vjust = 0.25),
        axis.text.y = element_text(size=25),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 30))
ggsave(pl_ag_water_consumption_regional_diffPer_heatmap, file = file.path(figures_path, paste0('sdg6_waterAg_per_heatmap_',selected_year,'.png')),
       width = 400, height = 450, units = 'mm', limitsize = FALSE)



#### ABSOLUTE WITH WATER TYPE
ag_waterType_consumption_regional_diffAbs <- merge(queries_all$water_irr_rfd_regional %>%
                                                     dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                                     dplyr::group_by(region, year, scenario, scen_type, scen_path, final_share, peak_year, slope, water) %>%
                                                     dplyr::summarise(value = sum(value)) %>%
                                                     dplyr::ungroup(),
                                                   queries_ref$water_irr_rfd_regional %>%
                                                     dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                                     dplyr::group_by(region, year, scenario, scen_type, water) %>%
                                                     dplyr::summarise(ref_value = sum(value)) %>%
                                                     dplyr::ungroup() %>%
                                                     dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                                   by = c('region','year','water')) %>%
  # compute Abs difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = ref_value - value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(region,year,scen_type,scen_path,scen_group,water) %>%
  dplyr::summarise(median_diff = median(diff))

pl_ag_waterType_consumption_regional_diffAbs_heatmap <- ggplot(ag_waterType_consumption_regional_diffAbs %>%
                                                                 dplyr::filter(year == selected_year),
                                                               aes(x = scen_group, y = region, fill = median_diff)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen",
                       name = expression(paste(km^3, " difference")), ) +
  facet_grid(scen_type ~ water) +
  # labs
  labs(y = '', x = 'Scenario type') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 25),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=25, angle = 90, hjust = 1, vjust = 0.25),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        title = element_text(size = 30))
ggsave(pl_ag_waterType_consumption_regional_diffAbs_heatmap, file = file.path(figures_path, paste0('sdg6_waterType_abs_heatmap_',selected_year,'.png')),
       width = 500, height = 450, units = 'mm', limitsize = FALSE)


#### PERCENT WITH WATER TYPE
ag_waterType_consumption_regional_diffPer <- merge(queries_all$water_irr_rfd_regional %>%
                                                     dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                                     dplyr::group_by(region, year, scenario, scen_type, scen_path, final_share, peak_year, slope, water) %>%
                                                     dplyr::summarise(value = sum(value)) %>%
                                                     dplyr::ungroup(),
                                                   queries_ref$water_irr_rfd_regional %>%
                                                     dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                                     dplyr::group_by(region, year, scenario, scen_type, water) %>%
                                                     dplyr::summarise(ref_value = sum(value)) %>%
                                                     dplyr::ungroup() %>%
                                                     dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                                   by = c('region','year','water')) %>%
  # compute Per difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = 100*(ref_value - value)/ref_value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(region,year,scen_type,scen_path,scen_group,water) %>%
  dplyr::summarise(median_diff = median(diff))

pl_ag_waterType_consumption_regional_diffPer_heatmap <- ggplot(ag_waterType_consumption_regional_diffPer %>%
                                                                 dplyr::filter(year == selected_year),
                                                               aes(x = scen_group, y = region, fill = median_diff)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen",
                       name = expression(paste("% difference")), ) +
  facet_grid(scen_type ~ water) +
  # labs
  labs(y = '', x = 'Scenario type') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 25),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=25, angle = 90, hjust = 1, vjust = 0.25),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        title = element_text(size = 30))
ggsave(pl_ag_waterType_consumption_regional_diffPer_heatmap, file = file.path(figures_path, paste0('sdg6_waterType_per_heatmap_',selected_year,'.png')),
       width = 500, height = 500, units = 'mm', limitsize = FALSE)


#####################################################################################
#####################################################################################
# SDG13 - EMISSIONS
#####################################################################################



pl_ghg_emissions_world <- ggplot(data = rbind(queries_all$ghg_world,
                                              queries_ref$ghg_world) %>%
                                   dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                   dplyr::group_by(year, scen_type) %>%
                                   dplyr::mutate(median_value = median(value)) %>%
                                   dplyr::mutate(min_value = min(value)) %>%
                                   dplyr::mutate(max_value = max(value)) %>%
                                   dplyr::ungroup()) +
  geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr, name = 'Scenario') +
  # labs
  labs(y = expression(paste("Mt",CO[2])), x = '') +
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
  labs(title = 'Annual World GHG emissions')
ggsave(pl_ghg_emissions_world, file = file.path(figures_path,'sdg13_ghg_emissions_line.png'), width = 500, height = 400, units = 'mm')




#### ABSOLUTE - REGIONAL
ghg_by_ghg_diffAbs_world = merge(queries_all$ghg_by_ghg_world %>%
                                   dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                   dplyr::group_by(year, scenario, scen_type, scen_path, final_share, peak_year, slope, group) %>%
                                   dplyr::summarise(value = sum(value)) %>%
                                   dplyr::ungroup(),
                                 queries_ref$ghg_by_ghg_world %>%
                                   dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                   dplyr::group_by(year, scenario, scen_type, group) %>%
                                   dplyr::summarise(ref_value = sum(value)) %>%
                                   dplyr::ungroup() %>%
                                   dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                 by = c('group','year')) %>%
  # compute Abs difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = ref_value - value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(group,year,scen_type,scen_path,scen_group) %>%
  dplyr::summarise(median_diff = median(diff))


pl_ghg_by_ghg_diffAbs_world_bars <- ggplot(data = ghg_by_ghg_diffAbs_world %>% filter(year == selected_year), aes(x = median_diff, y = scen_path, fill = group)) +
  geom_bar(stat = "identity", position = "identity") +
  facet_grid(scen_group ~ scen_type, scales = "fixed") +
  scale_fill_brewer(palette = 'Paired', name = '') +
  geom_vline(xintercept = 0, linewidth = 1.2) +
  labs(y = '', x = expression(paste(MtCO[2], ' difference'))) +
  theme_light() +
  theme(panel.grid.major.y = element_line(color = 'grey20'),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white",
                                       colour = 'grey',linewidth = 2),
        panel.background = element_rect(fill = "white"),
        legend.position = 'bottom', legend.direction = 'horizontal',
        strip.text = element_text(size = 20, color = 'black'),
        strip.background =element_rect(fill="transparent"),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_ghg_by_ghg_diffAbs_world_bars, file = file.path(figures_path, paste0('sdg13_ghg_by_ghg_abs_',selected_year,'.png')), width = 400, height = 500, units = 'mm')



#### PERCENT
ghg_by_ghg_diffPer_world = merge(queries_all$ghg_by_ghg_world %>%
                                   dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                   dplyr::group_by(year, scenario, scen_type, scen_path, final_share, peak_year, slope, group) %>%
                                   dplyr::summarise(value = sum(value)) %>%
                                   dplyr::ungroup(),
                                 queries_ref$ghg_by_ghg_world %>%
                                   dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                   dplyr::group_by(year, scenario, scen_type, group) %>%
                                   dplyr::summarise(ref_value = sum(value)) %>%
                                   dplyr::ungroup() %>%
                                   select(-scenario) %>%  select(-scen_type),
                                 by = c('group','year')) %>%
  # compute Per difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = 100*(ref_value - value)/ref_value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(group,year,scen_type,scen_path,scen_group) %>%
  dplyr::summarise(median_diff = median(diff))


pl_ghg_by_ghg_diffPer_world_bars <- ggplot(data = ghg_by_ghg_diffPer_world %>% filter(year == selected_year), aes(x = median_diff, y = scen_path, fill = group)) +
  geom_bar(stat = "identity", position = "identity") +
  facet_grid(scen_group ~ scen_type, scales = "fixed") +
  scale_fill_brewer(palette = 'Paired', name = '') +
  geom_vline(xintercept = 0, linewidth = 1.2) +
  labs(y = '', x = expression(paste('% difference'))) +
  theme_light() +
  theme(panel.grid.major.y = element_line(color = 'grey20'),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white",
                                       colour = 'grey',linewidth = 2),
        panel.background = element_rect(fill = "white"),
        legend.position = 'bottom', legend.direction = 'horizontal',
        strip.text = element_text(size = 20, color = 'black'),
        strip.background =element_rect(fill="transparent"),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_ghg_by_ghg_diffPer_world_bars, file = file.path(figures_path, paste0('sdg13_ghg_by_ghg_per_',selected_year,'.png')), width = 400, height = 500, units = 'mm')




##### MAPS ==============================================================================


#### ABSOLUTE
ghg_world_regional_diffAbs <- merge(queries_all$ghg_regional %>%
                                      dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                      dplyr::group_by(region, year, scenario, scen_type, scen_path, final_share, peak_year, slope) %>%
                                      dplyr::summarise(value = sum(value)) %>%
                                      dplyr::ungroup(),
                                    queries_ref$ghg_regional %>%
                                      dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                      dplyr::group_by(region, year, scenario, scen_type) %>%
                                      dplyr::summarise(ref_value = sum(value)) %>%
                                      dplyr::ungroup() %>%
                                      dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                    by = c('region','year')) %>%
  # compute Abs difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = ref_value - value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(region,year,scen_type,scen_path,scen_group) %>%
  dplyr::summarise(median_diff = median(diff))

ghg_world_regional_diffAbs_map <- ghg_world_regional_diffAbs %>%
  # filter desired year
  dplyr::filter(year == selected_year) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  dplyr::inner_join(rfasst::GCAM_reg, by = 'GCAM Region', multiple = "all", relationship = "many-to-many") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3') %>%
  # subset scen
  dplyr::mutate(scen_to_subset = paste0(scen_type, '_', scen_group)) %>%
  dplyr::filter(scen_to_subset %in% c('SPP_all_90', 'SNR_all_10')) %>%
  dplyr::select(-scen_to_subset)

ghg_world_regional_diffAbs_map = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                         dplyr::mutate('adm0_a3' = dplyr::if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                         dplyr::mutate('adm0_a3' = dplyr::if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                         dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                       ghg_world_regional_diffAbs_map, by = 'adm0_a3')

# plot
pl_ghg_world_regional_diffAbs_map <- ggplot() +
  # color map by regions
  geom_sf(data = ag_water_consumption_regional_diffAbs_map, aes(fill = median_diff)) +
  facet_wrap(. ~ scen_type, scales = 'fixed') +
  scale_fill_gradient2(low = "darkred", high = "darkgreen",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste(MtCO[2], " difference","\n"))) +
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
        strip.background =element_rect(fill="white"), title = element_text(size = 30))
# # title
# labs(title = paste0("Annual water consumption abs difference in ", selected_year))
ggsave(pl_ghg_world_regional_diffAbs_map, file = file.path(figures_path, paste0('sdg13_ghg_abs_map_',selected_year,'.png')), width = 500, height = 300, units = 'mm')



#### PERCENT
ghg_world_regional_diffPer <- merge(queries_all$ghg_regional %>%
                                      dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                      dplyr::group_by(region, year, scenario, scen_type, scen_path, final_share, peak_year, slope) %>%
                                      dplyr::summarise(value = sum(value)) %>%
                                      dplyr::ungroup(),
                                    queries_ref$ghg_regional %>%
                                      dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                      dplyr::group_by(region, year, scenario, scen_type) %>%
                                      dplyr::summarise(ref_value = sum(value)) %>%
                                      dplyr::ungroup() %>%
                                      dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                    by = c('region','year')) %>%
  # compute Per difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = 100*(ref_value - value)/ref_value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(region,year,scen_type,scen_path,scen_group) %>%
  dplyr::summarise(median_diff = median(diff))

ghg_world_regional_diffPer_map <- ghg_world_regional_diffPer %>%
  # filter desired year
  dplyr::filter(year == selected_year) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  dplyr::inner_join(rfasst::GCAM_reg, by = 'GCAM Region', multiple = "all", relationship = "many-to-many") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3') %>%
  # subset scen
  dplyr::mutate(scen_to_subset = paste0(scen_type, '_', scen_group)) %>%
  dplyr::filter(scen_to_subset %in% c('SPP_all_90', 'SNR_all_10')) %>%
  dplyr::select(-scen_to_subset)

ghg_world_regional_diffPer_map = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                         dplyr::mutate('adm0_a3' = dplyr::if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                         dplyr::mutate('adm0_a3' = dplyr::if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                         dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                       ghg_world_regional_diffPer_map, by = 'adm0_a3')

# plot
pl_ghg_world_regional_diffPer_map <- ggplot() +
  # color map by regions
  geom_sf(data = ghg_world_regional_diffPer_map, aes(fill = median_diff)) +
  facet_wrap(. ~ scen_type, scales = 'fixed') +
  scale_fill_gradient2(low = "darkred", high = "darkgreen",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("% difference","\n"))) +
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
        strip.background =element_rect(fill="white"), title = element_text(size = 30))
# title
# labs(title = paste0("Annual water consumption per difference in ", selected_year))
ggsave(pl_ghg_world_regional_diffPer_map, file = file.path(figures_path, paste0('sdg13_ghg_per_map_',selected_year,'.png')), width = 500, height = 300, units = 'mm')



#### HEATMAPS ===============================================================================


#### ABSOLUTE
pl_ghg_world_regional_diffAbs_heatmap <- ggplot(ghg_world_regional_diffAbs %>%
                                                  dplyr::filter(year == selected_year),
                                                aes(x = scen_group, y = region, fill = median_diff)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen",
                       name = expression(paste(MtCO[2], " difference")), ) +
  facet_wrap(. ~ scen_type) +
  # labs
  labs(y = '', x = 'Scenario type') + #, title = 'Absolute regional difference\nin water consumption'
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 35),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=25, angle = 90, hjust = 1, vjust = 0.25),
        axis.text.y = element_text(size=25),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 30))
ggsave(pl_ghg_world_regional_diffAbs_heatmap, file = file.path(figures_path, paste0('sdg13_ghg_abs_heatmap_',selected_year,'.png')),
       width = 400, height = 600, units = 'mm', limitsize = FALSE)



#### PERCENT
pl_ghg_world_regional_diffPer_heatmap <- ggplot(ghg_world_regional_diffAbs %>%
                                                  dplyr::filter(year == selected_year),
                                                aes(x = scen_group, y = region, fill = median_diff)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen",
                       name = '% difference') +
  facet_wrap(. ~ scen_type) +
  # labs
  labs(y = '', x = 'Scenario type') + # , title = 'Percentual regional difference\nin water consumption'
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 35),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=25, angle = 90, hjust = 1, vjust = 0.25),
        axis.text.y = element_text(size=25),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 30))
ggsave(pl_ghg_world_regional_diffPer_heatmap, file = file.path(figures_path, paste0('sdg13_ghg_per_heatmap_',selected_year,'.png')),
       width = 400, height = 600, units = 'mm', limitsize = FALSE)



#### ABSOLUTE WITH GHG TYPE
ghgType_regional_diffAbs <- merge(queries_all$ghg_by_ghg_regional %>%
                                    dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                    dplyr::group_by(region, year, scenario, scen_type, scen_path, final_share, peak_year, slope, group) %>%
                                    dplyr::summarise(value = sum(value)) %>%
                                    dplyr::ungroup(),
                                  queries_ref$ghg_by_ghg_regional %>%
                                    dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                    dplyr::group_by(region, year, scenario, scen_type, group) %>%
                                    dplyr::summarise(ref_value = sum(value)) %>%
                                    dplyr::ungroup() %>%
                                    dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                  by = c('region','year','group')) %>%
  # compute Abs difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = ref_value - value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(region,year,scen_type,scen_path,scen_group,group) %>%
  dplyr::summarise(median_diff = median(diff))

pl_ghgType_regional_diffAbs_heatmap <- ggplot(ghgType_regional_diffAbs %>%
                                                dplyr::filter(year == selected_year),
                                              aes(x = scen_group, y = region, fill = median_diff)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen",
                       name = expression(paste(MtCO[2], " difference")), ) +
  facet_grid(scen_type ~ group) +
  # labs
  labs(y = '', x = 'Scenario type') + #, title = 'Absolute regional difference\nin water consumption'
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 35),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=25, angle = 90, hjust = 1, vjust = 0.25),
        axis.text.y = element_text(size=25),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 30))
ggsave(pl_ghgType_regional_diffAbs_heatmap, file = file.path(figures_path, paste0('sdg13_ghgType_abs_heatmap_',selected_year,'.png')),
       width = 750, height = 600, units = 'mm', limitsize = FALSE)


#### PERCENT WITH GHG TYPE
ghgType_regional_diffPer <- merge(queries_all$ghg_by_ghg_regional %>%
                                    dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                    dplyr::group_by(region, year, scenario, scen_type, scen_path, final_share, peak_year, slope, group) %>%
                                    dplyr::summarise(value = sum(value)) %>%
                                    dplyr::ungroup(),
                                  queries_ref$ghg_by_ghg_regional %>%
                                    dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                    dplyr::group_by(region, year, scenario, scen_type, group) %>%
                                    dplyr::summarise(ref_value = sum(value)) %>%
                                    dplyr::ungroup() %>%
                                    dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                  by = c('region','year','group')) %>%
  # compute Per difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = 100*(ref_value - value)/ref_value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(region,year,scen_type,scen_path,scen_group,group) %>%
  dplyr::summarise(median_diff = median(diff))

pl_ghgType_regional_diffPer_heatmap <- ggplot(ghgType_regional_diffPer %>%
                                                dplyr::filter(year == selected_year),
                                              aes(x = scen_group, y = region, fill = median_diff)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen",
                       name = expression(paste("% difference")), ) +
  facet_grid(scen_type ~ group) +
  # labs
  labs(y = '', x = 'Scenario type') + #, title = 'Perolute regional difference\nin water consumption'
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 35),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=25, angle = 90, hjust = 1, vjust = 0.25),
        axis.text.y = element_text(size=25),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 30))
ggsave(pl_ghgType_regional_diffPer_heatmap, file = file.path(figures_path, paste0('sdg13_ghgType_per_heatmap_',selected_year,'.png')),
       width = 750, height = 600, units = 'mm', limitsize = FALSE)



#####################################################################################
# SDG3 - HEALTH
#####################################################################################



pl_deaths_world <- ggplot(data = rbind(mort_queries_all,
                                       mort_queries_ref) %>%
                            dplyr::mutate(scen_type = toupper(scen_type)) %>%
                            # subset one pm25 and one o3 method
                            dplyr::filter(method %in% c('GBD', 'GBD2016')) %>%
                            # world deaths
                            dplyr::group_by(year, scenario, scen_type, scen_path, final_share, peak_year, slope) %>%
                            dplyr::summarise(value = sum(mort)) %>%
                            dplyr::ungroup() %>%
                            # statistics
                            dplyr::group_by(year, scen_type) %>%
                            dplyr::mutate(median_value = median(value)) %>%
                            dplyr::mutate(min_value = min(value)) %>%
                            dplyr::mutate(max_value = max(value)) %>%
                            dplyr::ungroup()) +
  geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr, name = 'Scenario') +
  # labs
  labs(y = expression(paste("Premature deaths")), x = '') +
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
        title = element_text(size = 40))
ggsave(pl_deaths_world, file = file.path(figures_path,'sdg3_deaths_line.png'), width = 500, height = 400, units = 'mm')




#### ABSOLUTE - REGIONAL
deathsType_diffAbs_world = merge(mort_queries_all %>%
                                   dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                   dplyr::mutate(pollutant = toupper(pollutant)) %>%
                                   dplyr::filter(method %in% c('GBD', 'GBD2016')) %>%
                                   dplyr::group_by(year, scenario, scen_type, scen_path, final_share, peak_year, slope, pollutant) %>%
                                   dplyr::summarise(value = sum(mort)) %>%
                                   dplyr::ungroup(),
                                 mort_queries_ref %>%
                                   dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                   dplyr::mutate(pollutant = toupper(pollutant)) %>%
                                   dplyr::group_by(year, scenario, scen_type, pollutant) %>%
                                   dplyr::summarise(ref_value = sum(mort)) %>%
                                   dplyr::ungroup() %>%
                                   dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                 by = c('pollutant','year')) %>%
  # compute Abs difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = ref_value - value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(pollutant,year,scen_type,scen_path,scen_group) %>%
  dplyr::summarise(median_diff = median(diff))


pl_deathsType_diffAbs_world_bars <- ggplot(data = deathsType_diffAbs_world %>% filter(year == selected_year),
                                           aes(x = median_diff, y = scen_path, fill = pollutant)) +
  geom_bar(stat = "identity", position = "identity") +
  facet_grid(scen_group ~ scen_type, scales = "fixed") +
  scale_fill_manual(values = deaths_scenario_palette, name = 'Scenario') +
  geom_vline(xintercept = 0, linewidth = 1.2) +
  labs(y = '', x = expression(paste('Avoided premature deaths\n'))) +
  theme_light() +
  theme(panel.grid.major.y = element_line(color = 'grey20'),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white",
                                       colour = 'grey',linewidth = 2),
        panel.background = element_rect(fill = "white"),
        legend.position = 'bottom', legend.direction = 'horizontal',
        strip.text = element_text(size = 20, color = 'black'),
        strip.background =element_rect(fill="transparent"),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_deathsType_diffAbs_world_bars, file = file.path(figures_path, paste0('sdg3_deathsType_abs_',selected_year,'.png')), width = 400, height = 500, units = 'mm')



#### PERCENT
deathsType_diffPer_world = merge(mort_queries_all %>%
                                   dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                   dplyr::mutate(pollutant = toupper(pollutant)) %>%
                                   dplyr::filter(method %in% c('GBD', 'GBD2016')) %>%
                                   dplyr::group_by(year, scenario, scen_type, scen_path, final_share, peak_year, slope, pollutant) %>%
                                   dplyr::summarise(value = sum(mort)) %>%
                                   dplyr::ungroup(),
                                 mort_queries_ref %>%
                                   dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                   dplyr::mutate(pollutant = toupper(pollutant)) %>%
                                   dplyr::group_by(year, scenario, scen_type, pollutant) %>%
                                   dplyr::summarise(ref_value = sum(mort)) %>%
                                   dplyr::ungroup() %>%
                                   dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                 by = c('pollutant','year')) %>%
  # compute Per difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = 100*(ref_value - value)/ref_value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(pollutant,year,scen_type,scen_path,scen_group) %>%
  dplyr::summarise(median_diff = median(diff))


pl_deathsType_diffPer_world_bars <- ggplot(data = deathsType_diffPer_world %>% filter(year == selected_year),
                                           aes(x = median_diff, y = scen_path, fill = pollutant)) +
  geom_bar(stat = "identity", position = "identity") +
  facet_grid(scen_group ~ scen_type, scales = "fixed") +
  scale_fill_brewer(palette = 'Paired', name = '') +
  geom_vline(xintercept = 0, linewidth = 1.2) +
  labs(y = '', x = expression(paste('% avoided premature deaths'))) +
  theme_light() +
  theme(panel.grid.major.y = element_line(color = 'grey20'),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white",
                                       colour = 'grey',linewidth = 2),
        panel.background = element_rect(fill = "white"),
        legend.position = 'bottom', legend.direction = 'horizontal',
        strip.text = element_text(size = 20, color = 'black'),
        strip.background =element_rect(fill="transparent"),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_deathsType_diffPer_world_bars, file = file.path(figures_path, paste0('sdg3_deathsType_per_',selected_year,'.png')), width = 400, height = 500, units = 'mm')




##### MAPS ==============================================================================


#### ABSOLUTE
deaths_world_regional_diffAbs <- merge(mort_queries_all %>%
                                         dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                         dplyr::filter(method %in% c('GBD', 'GBD2016')) %>%
                                         dplyr::group_by(year, region, scenario, scen_type, scen_path, final_share, peak_year, slope) %>%
                                         dplyr::summarise(value = sum(mort)) %>%
                                         dplyr::ungroup(),
                                       mort_queries_ref %>%
                                         dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                         dplyr::group_by(year, region, scenario, scen_type) %>%
                                         dplyr::summarise(ref_value = sum(mort)) %>%
                                         dplyr::ungroup() %>%
                                         dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                       by = c('region', 'year')) %>%
  # compute Abs difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = ref_value - value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(fasst_region = region,year,scen_type,scen_path,scen_group) %>%
  dplyr::summarise(median_diff = median(diff))

deaths_world_regional_diffAbs_map <- deaths_world_regional_diffAbs %>%
  # filter desired year
  dplyr::filter(year == selected_year) %>%
  # merge with GCAM regions
  left_join(rfasst::fasst_reg %>%
              dplyr::rename('ISO3' = 'subRegionAlt'), by = 'fasst_region',
            multiple = 'all') %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO3') %>%
  # subset scen
  dplyr::mutate(scen_to_subset = paste0(scen_type, '_', scen_group)) %>%
  dplyr::filter(scen_to_subset %in% c('SPP_all_90', 'SNR_all_10')) %>%
  dplyr::select(-scen_to_subset)

deaths_world_regional_diffAbs_map = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                            dplyr::mutate('adm0_a3' = dplyr::if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                            dplyr::mutate('adm0_a3' = dplyr::if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                            dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                          deaths_world_regional_diffAbs_map, by = 'adm0_a3')

# plot
pl_deaths_world_regional_diffAbs_map <- ggplot() +
  # color map by regions
  geom_sf(data = deaths_world_regional_diffAbs_map, aes(fill = median_diff)) +
  facet_wrap(. ~ scen_type, scales = 'fixed') +
  scale_fill_gradient2(low = "darkred", high = "darkgreen",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Avoided premature deaths","\n"))) +
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
        legend.text = element_text(size = 25, angle = 45, hjust = 1), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 30, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 30))
ggsave(pl_deaths_world_regional_diffAbs_map, file = file.path(figures_path, paste0('sdg3_deaths_abs_map_',selected_year,'.png')), width = 500, height = 200, units = 'mm')



#### PERCENT
deaths_world_regional_diffPer <- merge(mort_queries_all %>%
                                         dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                         dplyr::filter(method %in% c('GBD', 'GBD2016')) %>%
                                         dplyr::group_by(year, region, scenario, scen_type, scen_path, final_share, peak_year, slope) %>%
                                         dplyr::summarise(value = sum(mort)) %>%
                                         dplyr::ungroup(),
                                       mort_queries_ref %>%
                                         dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                         dplyr::group_by(year, region, scenario, scen_type) %>%
                                         dplyr::summarise(ref_value = sum(mort)) %>%
                                         dplyr::ungroup() %>%
                                         dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                       by = c('region', 'year')) %>%
  # compute Per difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = ifelse(ref_value == 0, 0, 100*(ref_value - value)/ref_value)) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(fasst_region = region,year,scen_type,scen_path,scen_group) %>%
  dplyr::summarise(median_diff = median(diff))

deaths_world_regional_diffPer_map <- deaths_world_regional_diffPer %>%
  # filter desired year
  dplyr::filter(year == selected_year) %>%
  # merge with GCAM regions
  left_join(rfasst::fasst_reg %>%
              dplyr::rename('ISO3' = 'subRegionAlt'), by = 'fasst_region',
            multiple = 'all') %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO3') %>%
  # subset scen
  dplyr::mutate(scen_to_subset = paste0(scen_type, '_', scen_group)) %>%
  dplyr::filter(scen_to_subset %in% c('SPP_all_90', 'SNR_all_10')) %>%
  dplyr::select(-scen_to_subset)

deaths_world_regional_diffPer_map = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                            dplyr::mutate('adm0_a3' = dplyr::if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                            dplyr::mutate('adm0_a3' = dplyr::if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                            dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                          deaths_world_regional_diffPer_map, by = 'adm0_a3')

# plot
pl_deaths_world_regional_diffPer_map <- ggplot() +
  # color map by regions
  geom_sf(data = deaths_world_regional_diffPer_map, aes(fill = median_diff)) +
  facet_wrap(. ~ scen_type, scales = 'fixed') +
  scale_fill_gradient2(low = "darkred", high = "darkgreen",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Avoided premature deaths","\n"))) +
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
        legend.text = element_text(size = 25, angle = 45, hjust = 1), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 30, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 30))
ggsave(pl_deaths_world_regional_diffPer_map, file = file.path(figures_path, paste0('sdg3_deaths_per_map_',selected_year,'.png')), width = 500, height = 200, units = 'mm')



#### HEATMAPS ===============================================================================


#### ABSOLUTE
pl_ghg_world_regional_diffAbs_heatmap <- ggplot(ghg_world_regional_diffAbs %>%
                                                  dplyr::filter(year == selected_year),
                                                aes(x = scen_group, y = region, fill = median_diff)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen",
                       name = expression(paste(MtCO[2], " difference")), ) +
  facet_wrap(. ~ scen_type) +
  # labs
  labs(y = '', x = 'Scenario type') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 35),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=25, angle = 90, hjust = 1, vjust = 0.25),
        axis.text.y = element_text(size=25),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 30))
ggsave(pl_ghg_world_regional_diffAbs_heatmap, file = file.path(figures_path, paste0('sdg13_ghg_abs_heatmap_',selected_year,'.png')),
       width = 400, height = 600, units = 'mm', limitsize = FALSE)



#### PERCENT
pl_ghg_world_regional_diffPer_heatmap <- ggplot(ghg_world_regional_diffAbs %>%
                                                  dplyr::filter(year == selected_year),
                                                aes(x = scen_group, y = region, fill = median_diff)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen",
                       name = '% difference') +
  facet_wrap(. ~ scen_type) +
  # labs
  labs(y = '', x = 'Scenario type') + # , title = 'Percentual regional difference\nin water consumption'
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 35),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=25, angle = 90, hjust = 1, vjust = 0.25),
        axis.text.y = element_text(size=25),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 30))
ggsave(pl_ghg_world_regional_diffPer_heatmap, file = file.path(figures_path, paste0('sdg13_ghg_per_heatmap_',selected_year,'.png')),
       width = 400, height = 600, units = 'mm', limitsize = FALSE)



#### ABSOLUTE WITH GHG TYPE
ghgType_regional_diffAbs <- merge(queries_all$ghg_by_ghg_regional %>%
                                    dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                    dplyr::group_by(region, year, scenario, scen_type, scen_path, final_share, peak_year, slope, group) %>%
                                    dplyr::summarise(value = sum(value)) %>%
                                    dplyr::ungroup(),
                                  queries_ref$ghg_by_ghg_regional %>%
                                    dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                    dplyr::group_by(region, year, scenario, scen_type, group) %>%
                                    dplyr::summarise(ref_value = sum(value)) %>%
                                    dplyr::ungroup() %>%
                                    dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                  by = c('region','year','group')) %>%
  # compute Abs difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = ref_value - value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(region,year,scen_type,scen_path,scen_group,group) %>%
  dplyr::summarise(median_diff = median(diff))

pl_ghgType_regional_diffAbs_heatmap <- ggplot(ghgType_regional_diffAbs %>%
                                                dplyr::filter(year == selected_year),
                                              aes(x = scen_group, y = region, fill = median_diff)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen",
                       name = expression(paste(MtCO[2], " difference")), ) +
  facet_grid(scen_type ~ group) +
  # labs
  labs(y = '', x = 'Scenario type') + #, title = 'Absolute regional difference\nin water consumption'
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 35),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=25, angle = 90, hjust = 1, vjust = 0.25),
        axis.text.y = element_text(size=25),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 30))
ggsave(pl_ghgType_regional_diffAbs_heatmap, file = file.path(figures_path, paste0('sdg13_ghgType_abs_heatmap_',selected_year,'.png')),
       width = 750, height = 600, units = 'mm', limitsize = FALSE)


#### PERCENT WITH GHG TYPE
ghgType_regional_diffPer <- merge(queries_all$ghg_by_ghg_regional %>%
                                    dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                    dplyr::group_by(region, year, scenario, scen_type, scen_path, final_share, peak_year, slope, group) %>%
                                    dplyr::summarise(value = sum(value)) %>%
                                    dplyr::ungroup(),
                                  queries_ref$ghg_by_ghg_regional %>%
                                    dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                    dplyr::group_by(region, year, scenario, scen_type, group) %>%
                                    dplyr::summarise(ref_value = sum(value)) %>%
                                    dplyr::ungroup() %>%
                                    dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                  by = c('region','year','group')) %>%
  # compute Per difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = 100*(ref_value - value)/ref_value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(region,year,scen_type,scen_path,scen_group,group) %>%
  dplyr::summarise(median_diff = median(diff))

pl_ghgType_regional_diffPer_heatmap <- ggplot(ghgType_regional_diffPer %>%
                                                dplyr::filter(year == selected_year),
                                              aes(x = scen_group, y = region, fill = median_diff)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen",
                       name = expression(paste("% difference")), ) +
  facet_grid(scen_type ~ group) +
  # labs
  labs(y = '', x = 'Scenario type') + #, title = 'Perolute regional difference\nin water consumption'
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 35),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=25, angle = 90, hjust = 1, vjust = 0.25),
        axis.text.y = element_text(size=25),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 30))
ggsave(pl_ghgType_regional_diffPer_heatmap, file = file.path(figures_path, paste0('sdg13_ghgType_per_heatmap_',selected_year,'.png')),
       width = 750, height = 600, units = 'mm', limitsize = FALSE)




#####################################################################################
#####################################################################################
# SDG 2 - FOOD EXPENDITURE
#####################################################################################

food_subsector <- read.csv('inputs/nutrition/food_subsector.csv', skip = 3)

#### econ. basket bill ---

food_econ_basket_bill_regional = rbind(queries_all$food_consumption_regional,
                                       queries_ref$food_consumption_regional) %>%
  dplyr::left_join(food_subsector %>%
                     dplyr::rename('technology' = 'subsector')) %>%
  # Pcal to kcal/capita/day
  dplyr::left_join(rbind(queries_all$pop_all_regions,
                         queries_ref$pop_all_regions), by = c("year", "scenario", "scen_type", "scen_path", "final_share", "peak_year", "slope", "region")) %>%
  # convert from Pcal to kcal/day
  dplyr::mutate(value = (value * 1e12) / (population * 365),
                Units = "kcal/capita/day") %>%
  # total staples and nonstaples kcal consumption
  dplyr::group_by(Units,region,scenario,scen_type,scen_path,final_share,peak_year,slope,year,supplysector) %>%
  dplyr::summarise(consumption = sum(value)) %>%
  # compute the expenditure by supplysector
  dplyr::left_join(rbind(queries_all$food_demand_prices_regional,
                         queries_ref$food_demand_prices_regional) %>%
                     dplyr::mutate(price = value * 1e3,
                                   units_price = '2005$/kcal/day') %>%
                     dplyr::select(-c(Units,value)) %>%
                     dplyr::rename('supplysector' = 'input'),
                   by = c('region','year','supplysector','scenario', "scen_type", "scen_path", "final_share", "peak_year", 'slope')) %>%
  dplyr::mutate(expenditure = consumption * price,
                units_expenditure = '2005$/capita/day') %>%
  # total expenditure (staples + nonstaples)
  dplyr::group_by(units_expenditure,region,scenario,scen_type,scen_path,final_share,peak_year,slope,year) %>%
  dplyr::summarise(expenditure = sum(expenditure)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scen_type = toupper(scen_type))


#### WORLD
pl_food_econ_basket_bill_world = ggplot(data = food_econ_basket_bill_regional %>%
                                          dplyr::filter(year == selected_year) %>%
                                          dplyr::group_by(year,scen_type) %>%
                                          dplyr::summarise(median_value = median(expenditure),
                                                           min_value = min(expenditure),
                                                           max_value = max(expenditure))) +
  geom_bar(aes(x = scen_type, y = median_value, fill = scen_type), stat = 'identity', alpha = 0.3) +
  geom_errorbar(aes(x = scen_type, ymin = min_value, ymax = max_value), width=0.3, colour="#757575", alpha=1, linewidth=1.2) +
  scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr, name = 'Scenario') +
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
        title = element_text(size = 40))
ggsave(pl_food_econ_basket_bill_world, file = file.path(figures_path, paste0('sgd2_food_econ_basket_bill_world_',selected_year,'.png')), width = 400, height = 600, units = 'mm')

#### REGIONAL
pl_food_econ_basket_bill_regional = ggplot(data = food_econ_basket_bill_regional %>%
                                             dplyr::filter(year == selected_year) %>%
                                             dplyr::group_by(year,scen_type, region) %>%
                                             dplyr::summarise(median_value = median(expenditure),
                                                              min_value = min(expenditure),
                                                              max_value = max(expenditure))) +
  geom_bar(aes(x = scen_type, y = median_value, fill = scen_type), stat = 'identity', alpha = 0.3) +
  geom_errorbar(aes(x = scen_type, ymin = min_value, ymax = max_value), width=0.3, colour="#757575", alpha=1, linewidth=1.2) +
  # facet
  facet_wrap(. ~ region, scales = 'fixed') +
  scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr, name = 'Scenario') +
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
        title = element_text(size = 40))
ggsave(pl_food_econ_basket_bill_regional, file = file.path(figures_path, paste0('sgd2_food_econ_basket_bill_regional_',selected_year,'.png')),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)


#### PERCENT REGIONAL diff
food_econ_basket_bill_regional_ref = food_econ_basket_bill_regional %>%
  dplyr::filter(scenario == 'ref') %>%
  dplyr::select(units_expenditure, region, year, ref_expenditure = expenditure)
food_econ_basket_bill_regional_diff = merge(
  food_econ_basket_bill_regional %>%
    dplyr::filter(scenario != 'ref'),
  food_econ_basket_bill_regional_ref,
  by = c('units_expenditure', 'region', 'year')
) %>%
  dplyr::mutate(diff = 100*(ref_expenditure - expenditure)/ref_expenditure) %>%
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  dplyr::select(region, year, scen_group, scen_type, diff) %>%
  dplyr::arrange(desc(region))


pl_food_econ_basket_bill_regional_diff <- ggplot(food_econ_basket_bill_regional_diff %>% dplyr::filter(year == selected_year), aes(x = scen_group, y = region, fill = diff)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen",
                       name = '% difference') +
  facet_wrap(. ~ scen_type) +
  # labs
  labs(y = '', x = 'Scenario type') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30, angle = 90, hjust = 1, vjust = 0.25),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 30))
ggsave(pl_food_econ_basket_bill_regional_diff, file = file.path(figures_path, paste0('sgd2_food_econ_basket_bill_regional_diff_',selected_year,'.png')),
       width = 400, height = 600, units = 'mm', limitsize = FALSE)


#####################################################################################
# SDG 3 - NUTRITIONAL VALUES
#####################################################################################

food_subsector <- read.csv('inputs/nutrition/food_subsector.csv', skip = 3)
data_macronutrient <- read.csv('inputs/nutrition/gcam_macronutrient.csv', skip = 5)
data_micronutrient <- read.csv('inputs/nutrition/USDA_data_final.csv')
colnames(data_micronutrient) <- c("Food", "GCAM_commodity", "Calories (kcal)", "Protein (g)",
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
mder <- read.csv(paste0("inputs/nutrition/MDER.csv")) %>%
  rename(mder_units = unit) %>%
  mutate(mder_units = 'kcal/capita/day')
colnames(mder) = c('variable','mder_units','mder','std','min','max')
GramProteinFatPerKcal <- read.csv("inputs/nutrition/GramProteinFatPerKcal.csv", skip = 3)
micronutrients <- read.csv('inputs/nutrition/rni.csv', skip = 3)

# TODO: check with FAO data

####################### ADESA computation ###########################
#### dietary energy supply (DES) ---
# units: kcal/cap/day
# by region
# get total consumption in calories
dietary_energy_supply <- rbind(queries_all$food_consumption_regional,
                               queries_ref$food_consumption_regional) %>%
  group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, region, year) %>%
  # aggregate staple and non-staple calories
  summarize(value = sum(value)) %>%
  left_join(rbind(queries_all$pop_all_regions,
                  queries_ref$pop_all_regions),
            by = c("year", "scenario", "scen_type", "scen_path", "final_share", "peak_year", "slope", "region")) %>%
  # convert from Pcal to kcal/cap/day
  mutate(value = (value * 1e12) / (population * 365),
         units = "kcal/cap/day") %>%
  filter(year <= MODEL_HALF_CENTURY_YEAR)

## share of dietary energy supply from staples ---
# find consumption of staple and non-staple Pcal
staples_vs_nonstaples_Pcal <- rbind(queries_all$food_consumption_regional,
                                    queries_ref$food_consumption_regional) %>%
  select(-nestingSector1) %>% select(-nestingSector2) %>% select(-nestingSector3) %>%
  rename(subsector = technology) %>%
  left_join_error_no_match(food_subsector, by = 'subsector') %>%
  group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, region, year, supplysector) %>%
  summarise(value = sum(value)) %>%
  mutate(Units = 'Pcal') %>%
  ungroup()

## SSP2 population data ---
ssp_data <- read.csv(paste0("inputs/nutrition/SSP2_population_by_demographic.csv"), skip = 1)
iso_gcam_regions <- read.csv(paste0("inputs/mappings/iso_GCAM_regID.csv"), skip = 6)
id_gcam_regions <- read.csv(paste0("inputs/mappings/gcam_id_to_region.csv"))
colnames(id_gcam_regions) = c('GCAM_region_ID', 'region')
country_gcam_regions <- read.csv(paste0("inputs/mappings/country_to_gcam_id.csv"))
regions_key <- dplyr::left_join(country_gcam_regions, id_gcam_regions, by = "GCAM_region_ID") %>%
  dplyr::select(-1)

ssp_data_clean <- iso_gcam_regions %>%
  select(-region_GCAM3, -GCAM_region_ID) %>%
  left_join(ssp_data %>%
              filter(SCENARIO == 'SSP2_v9_130115'),
            by = "iso", multiple = 'all') %>%
  select(-MODEL, -REGION) %>%
  rename(scenario = SCENARIO,
         variable = VARIABLE,
         unit = UNIT)
# Remove X from year columns
colnames(ssp_data_clean) <- gsub("X", "", colnames(ssp_data_clean))
# Pivot longer
ssp_data_long <- ssp_data_clean %>%
  tidyr::pivot_longer(cols = 6:24, names_to = "year", values_to = "value") %>%
  mutate(value = value * 1e6,
         unit = "total population") %>%
  mutate(year = as.integer(year))
# Isolate reference (total) population
reference_pop <- ssp_data_long %>%
  filter(variable == "Population") %>%
  rename(total_pop = value) %>%
  select(iso, year, total_pop)
# Join and calculate demographic shares of population
ssp_data_final <- ssp_data_long %>%
  # Remove total male and total female pop, we want by age/sex
  filter(!variable %in% c("Population|Male", "Population|Female", "Population", NA)) %>%
  left_join(reference_pop, by = c("iso", "year")) %>%
  mutate(demo_share = value / total_pop) %>%
  rename(sub_pop = value)  %>%
  rename(pop_units = unit)

# Get population by sex and age
# Population weighting
total_regional_pop <- ssp_data_final %>%
  select(-scenario,-iso) %>%
  # get GCAM regions instead of country names
  left_join(regions_key, by = "country_name") %>%
  # get total regional population
  group_by(year, GCAM_region_ID, country_name, region) %>%
  # isolate total population by country
  distinct(total_pop) %>%
  group_by(year, GCAM_region_ID, region) %>%
  # sum for total regional population
  mutate(total_regional_pop = sum(total_pop)) %>%
  ungroup()

weighted_pop <- ssp_data_final %>%
  select(-scenario,-iso) %>%
  # get GCAM regions instead of country names
  left_join(regions_key, by = "country_name") %>%
  # get total regional population
  left_join(total_regional_pop) %>%
  # weight each country by its population over total regional pop
  group_by(country_name, year) %>%
  mutate(weight = total_pop / total_regional_pop) %>%
  # get GCAM population
  left_join(rbind(queries_all$pop_all_regions,
                  queries_ref$pop_all_regions),
            by = c("region", "year"), relationship = "many-to-many") %>%
  # compute GCAM population by sex and age for each country
  mutate(weighted_demographics = demo_share * weight * population)

weighted_pop_sex_age <- weighted_pop %>%
  select(-pop_units, -sub_pop, -total_pop, -demo_share, -weight) %>%
  group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, variable, year, region) %>%
  # sum the weighted averages for each country into GCAM regions
  summarize(pop_sex_age = sum(weighted_demographics))

# join with MDER data, calculate caloric requirements by sex and age
adesa_denominator <- weighted_pop_sex_age %>%
  left_join(mder, by = "variable") %>%
  select(-std) %>%
  group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, variable, year, region) %>%
  # compute a range because of differing physical activity levels
  summarize(cal_req_x_pop = mder * pop_sex_age,
            min_cal_req_x_pop = min * pop_sex_age,
            max_cal_req_x_pop = max * pop_sex_age) %>%
  # aggregate caloric requirements to get total regional values
  group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, region, year) %>%
  summarize(denominator_sum = sum(cal_req_x_pop),
            min_denominator_sum = sum(min_cal_req_x_pop),
            max_denominator_sum = sum(max_cal_req_x_pop)) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year <= MODEL_HALF_CENTURY_YEAR)

# add in regional calorie info, calculate ADESA
adesa <- left_join(adesa_denominator, dietary_energy_supply) %>%
  group_by(year, region, scenario, scen_type, scen_path, final_share, peak_year, slope) %>%
  reframe(adesa = (value / denominator_sum) * population * 100, # convert to unitless and percentage
          min_adesa = (value / min_denominator_sum) * population * 100,
          max_adesa = (value / max_denominator_sum) * population * 100,
          .groups = "keep")


########################### ADESA plot ###########################

pl = ggplot(adesa %>%
              dplyr::mutate(scen_type = toupper(scen_type)) %>%
              dplyr::group_by(scen_type, scenario, year) %>%
              dplyr::mutate(adesa = mean(adesa)) %>%
              dplyr::ungroup() %>%
              dplyr::group_by(scen_type, year) %>%
              dplyr::mutate(mean_adesa = mean(adesa),
                            min_value = min(adesa),
                            max_value = max(adesa)) %>%
              dplyr::ungroup()) +
  geom_line(aes(x = year, y = mean_adesa, color = scen_type), linewidth = 2, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr, name = 'Scenario') +
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
ggsave(pl, filename = file.path(figures_path, 'sdg3_adesa_world.png'),
       width = 750, height = 450, units = 'mm', limitsize = F)

# check for ADESA trends and get rid of the scenarios with a down-going ADESA
pl = ggplot(adesa %>%
              dplyr::mutate(scen_type = toupper(scen_type)) %>%
              dplyr::group_by(sce