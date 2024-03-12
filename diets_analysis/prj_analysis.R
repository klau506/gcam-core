#### prj_analysis.
####
#### Main script to analyse the results

#### INPUTS
# RData

## Set the working directory and load libraries
# setwd('/scratch/bc3LC/gcam-core-iamcompact-xin')
# libP <- .libPaths()
# .libPaths(c(libP,"/scratch/bc3LC/R-libs/4.1"))
#
# library(dplyr)
# library(tidyr)
# library(rgcam)

# setwd('C:/GCAM/GCAM_7.0_Claudia/gcam-core-iamcompact/diets_analysis')
setwd('C:\\Users\\claudia.rodes\\Documents\\IAM_COMPACT\\gcam-iamcompact-xin')

# .libPaths('C:/Users/claudia.rodes/Documents/R/win-library/4.1-gcamdata_CP/')
library(dplyr)
library(magrittr)

##### Load food consumption data ---------------------------------------------------
assign('queries_all', get(load('outputs/queries_all_2.RData')))
assign('queries_ref', get(load('outputs/queries_ref_2.RData')))

figures_path <- 'figures'
selected_year <- 2050

#####################################################################################
#####################################################################################
# PROTEIN PERCENTAGE
#####################################################################################

##### PLANT
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


##### RUMINANT
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
# SDG15 - LAND USE management
#####################################################################################


#### ABSOLUTE
land_use_diffAbs_world = merge(queries_all$land_use_world,
                               queries_ref$land_use_world %>%
                                 select(landleaf, land_use_type, year, Units, ref_value = value),
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
ggsave(pl_land_use_diffAbs_world, file = file.path(figures_path,paste0('sdg15_land_use_diffAbs_world.png')), width = 500, height = 400, units = 'mm')


#### PERCENTAGE
land_use_diffPer_world = merge(queries_all$land_use_world,
                               queries_ref$land_use_world %>%
                                 select(landleaf, land_use_type, year, Units, ref_value = value),
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
  dplyr::mutate(diff = ifelse(ref_value != 0, (value - ref_value)/ref_value, 0)) %>%
  dplyr::select(-value) %>% dplyr::select(-ref_value) %>%
  dplyr::mutate(scen_type = toupper(scen_type))


pl_land_use_diffPer_world = ggplot(data = land_use_diffPer_world) +
  geom_area(aes(x = year, y = diff, fill = land_use_type), alpha = 1) +  # Median area
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(. ~ scen_type) +
  scale_fill_manual(values = land_use_scenario_palette, name = 'Land Type',
                    breaks = land_use_order_palette) +
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
ggsave(pl_land_use_diffPer_world, file = file.path(figures_path,paste0('sdg15_land_use_diffPer_world.png')), width = 500, height = 400, units = 'mm')



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
        title = element_text(size = 40)) +
  # title
  labs(title = 'Annual World Water consumption')
ggsave(pl_water_consumption_world, file = file.path(figures_path,'sdg6_annual_water_consumption_line.png'), width = 500, height = 400, units = 'mm')



pl_water_consumption_agriculture_world <- ggplot(data = rbind(queries_all$water_irr_rfd_world,
                                                              queries_ref$water_irr_rfd_world) %>%
                                                   dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                                   dplyr::group_by(year, scenario, scen_type) %>%
                                                   dplyr::summarise(value = sum(value)) %>%
                                                   dplyr::ungroup() %>%
                                                   dplyr::group_by(year, scen_type) %>%
                                                   dplyr::mutate(median_value = median(value),
                                                                 min_value = min(value),
                                                                 max_value = max(value)) %>%
                                                   dplyr::ungroup()) +
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
        title = element_text(size = 40)) +
  # title
  labs(title = 'Annual World Water (agriculture) consumption')
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
                                      select(-scenario) %>%  select(-scen_type),
                                    by = c('water','year')) %>%
  # compute Abs difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = ref_value - value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(water,year,scen_type,scen_path,scen_group) %>%
  dplyr::summarise(median_diff = median(diff))


pl_water_irr_rfd_diffAbs_world_bars <- ggplot(data = water_irr_rfd_diffAbs_world %>% filter(year == selected_year), aes(x = median_diff, y = scen_path, fill = water)) +
  geom_bar(stat = "identity", position = "identity") +
  facet_grid(scen_group ~ scen_type, scales = "free") +
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
                                      select(-scenario) %>%  select(-scen_type),
                                    by = c('water','year')) %>%
  # compute Per difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = (ref_value - value)/ref_value) %>%
  # create scen_group
  dplyr::mutate(scen_group = paste0(scen_path, '_', final_share)) %>%
  # compute median by scen
  dplyr::group_by(water,year,scen_type,scen_path,scen_group) %>%
  dplyr::summarise(median_diff = median(diff))


pl_water_irr_rfd_diffPer_world_bars <- ggplot(data = water_irr_rfd_diffPer_world %>% filter(year == selected_year), aes(x = median_diff, y = scen_path, fill = water)) +
  geom_bar(stat = "identity", position = "identity") +
  facet_grid(scen_group ~ scen_type, scales = "free") +
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
ggsave(pl_water_irr_rfd_diffPer_world_bars, file = file.path(figures_path, paste0('sdg6_waterType_abs_',selected_year,'.png')), width = 400, height = 500, units = 'mm')






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
                                      select(-scenario) %>%  select(-scen_type),
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
  dplyr::mutate(diff = (ref_value - value)/ref_value) %>%
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




