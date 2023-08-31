## script to produce all the outputs (figures and tables) to do a system-wide
## analysis of the FVV scenarios

#### Libraries =================================================================
# ==============================================================================
library(rgcam)
library(dplyr)
library(ggplot2)
library(rfasst)


#### Paths =====================================================================
# ==============================================================================
gcam_path <<- substr(getwd(), start = 1, stop = regexpr("gcam-core/", getwd()) + 9)
tmp_output_data_path <<- paste0(gcam_path, "/input/gcamdata/outputs_binomial/")
figures_path <<- paste0(gcam_path, "/input/gcamdata/figures_binomial/")
folder_analysis_path <<- paste0(gcam_path, "input/gcamdata/study7_analysis/")

db_path <<- paste0(gcam_path, "output")
db_name_base <<- 'behaviour_basexdb'
prj_name <<- 'behavioral_change_x5.dat'
query_path <<- paste0(gcam_path, "input/gcamdata/study7_analysis/data/")
queries <<- 'queries_beh.xml'
desired_scen <<- c('Reference', paste0("Flex.ds.beh", 1:25))

source(paste0(folder_analysis_path,'zzz.R'))
source(paste0(folder_analysis_path,'fun_cobenefits.R'))

#### Create prj ================================================================
# ==============================================================================

# if prj does not exist, create it. Load it otherwise
if (!file.exists(prj_name)) {

  print('create prj')
  ## select db according to the scenario
  for (sc in desired_scen) {
    print(sc)

    db_name = find_db_name(sc)

    ## create prj
    conn <- localDBConn(db_path, db_name)
    prj <<- addScenario(conn, prj_name, sc,
                        paste0(query_path, queries),
                        clobber = FALSE)

    # add 'nonCO2' large query
    fill_queries(db_path, db_name, prj_name, sc)

  }

  saveProject(prj, file = prj_name)

} else {
  ## load prj
  print('load prj')
  prj <<- loadProject(prj_name)
  listQueries(prj)
  listScenarios(prj)
}


#### Data preprocess ===========================================================
# ==============================================================================

year_s = 2000
year_e = 2100
final_db_year <<- 2100
selected_scen = desired_scen

# load queries
load_queries()

# # compute premature mortalities
# mort = load_premature_mortalities()
# mort = compute_premature_mortalities_diff(mort)

#### System-wide effects =======================================================
# ==============================================================================

if (!dir.exists(paste0(figures_path,"tmp_figs"))) dir.create(paste0(figures_path,"tmp_figs"))

selected_year = 2030

#### Fig: food consumption, production & demand ====
# =============================
## == A: start food consumption
## WORLD
# subset relevant items
pltD_food_consumption = food_consumption_world %>%
  dplyr::filter(technology %in% c('Beef','Pork','Poultry','OtherMeat_Fish','Legumes','NutsSeeds')) %>%
  dplyr::mutate(technology = ifelse(technology == 'OtherMeat_Fish', 'Other Meat and Fish',
                                    ifelse(technology == 'NutsSeeds', 'Nuts and Seeds', technology))) %>%
  dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change'))

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
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = 'Pcal', x = '') +
  ggtitle('World food consumption') +
  # theme
  theme_light() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(plt_food_consumption_world, file = paste0(figures_path,"tmp_figs/",'pl1_food_consumption_6elem_world.pdf'),
       width = 1000, height = 300, units = 'mm')

## REGIONAL
# subset relevant items
pltD_food_consumption = food_consumption_regional %>%
  dplyr::filter(technology %in% c('Beef','Pork','Poultry','OtherMeat_Fish','Legumes','NutsSeeds')) %>%
  dplyr::mutate(technology = ifelse(technology == 'OtherMeat_Fish', 'Other Meat and Fish',
                                    ifelse(technology == 'NutsSeeds', 'Nuts and Seeds', technology))) %>%
  dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change'))

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
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = 'Pcal', x = '') +
  ggtitle('Regional food consumption (fixed scales)') +
  # theme
  theme_light() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(plt_food_consumption_regional, file = paste0(figures_path,"tmp_figs/",'pl1_food_consumption_6elem_regional_fixedScales.pdf'),
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
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = 'Pcal', x = '') +
  ggtitle('Regional food consumption (free scales)') +
  # theme
  theme_light() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(plt_food_consumption_regional, file = paste0(figures_path,"tmp_figs/",'pl1_food_consumption_6elem_regional_freeScales.pdf'),
       width = 2000, height = 2500, units = 'mm', limitsize = FALSE)




## WORLD
# plot all food elem
plt_food_consumption_world = ggplot(data = food_consumption_world %>%
                                      dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
                                      group_by(technology,year,scenario_type) %>%
                                      dplyr::mutate(median_value = median(value)) %>%
                                      dplyr::mutate(min_value = min(value)) %>%
                                      dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
  facet_wrap(. ~ technology, scales = 'free') +
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = 'Pcal', x = '') +
  ggtitle('World food consumption') +
  # theme
  theme_light() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(plt_food_consumption_world, file = paste0(figures_path,"tmp_figs/",'pl1_food_consumption_allElem_world.pdf'),
       width = 1000, height = 1000, units = 'mm')

## REGIONAL
# plot all food elem
plt_food_consumption_regional = ggplot(data = food_consumption_regional %>%
                                         dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
                                         group_by(region,technology,year,scenario_type) %>%
                                         dplyr::mutate(median_value = median(value)) %>%
                                         dplyr::mutate(min_value = min(value)) %>%
                                         dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
  facet_grid(region ~ technology) +
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = 'Pcal', x = '') +
  ggtitle('Regional food consumption (fixed scales)') +
  # theme
  theme_light() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(plt_food_consumption_regional, file = paste0(figures_path,"tmp_figs/",'pl1_food_consumption_allElem_regional_fixedScales.pdf'),
       width = 2500, height = 2500, units = 'mm', limitsize = FALSE)

plt_food_consumption_regional = ggplot(data = food_consumption_regional %>%
                                         dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
                                         group_by(region,technology,year,scenario_type) %>%
                                         dplyr::mutate(median_value = median(value)) %>%
                                         dplyr::mutate(min_value = min(value)) %>%
                                         dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
  facet_grid(region ~ technology, scales = 'free') +
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = 'Pcal', x = '') +
  ggtitle('Regional food consumption (free scales)') +
  # theme
  theme_light() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(plt_food_consumption_regional, file = paste0(figures_path,"tmp_figs/",'pl1_food_consumption_allElem_regional_freeScales.pdf'),
       width = 2500, height = 2500, units = 'mm', limitsize = FALSE)



### WORLD
pl_food_demand_staplesVsNonStapes_world = ggplot(data = food_demand_world %>%
                                                      dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
                                                      group_by(input,year,scenario_type) %>%
                                                      dplyr::mutate(median_value = median(value)) %>%
                                                      dplyr::mutate(min_value = min(value)) %>%
                                                      dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,input,scenario), color = interaction(scenario_type,input)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,input)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,input)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
                     breaks = staples_vs_nonstaples_order_palette) +
  scale_fill_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
                    breaks = staples_vs_nonstaples_order_palette) +
  # labs
  labs(y = 'Pcal', x = '', title = 'Annual World food demand') +
  # theme
  theme_light() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2))
ggsave(pl_food_demand_staplesVsNonStapes_world, file = paste0(figures_path,"tmp_figs/",'pl1_food_demand_staplesVsNonStapes_world.pdf'),
       width = 550, height = 500, units = 'mm', limitsize = FALSE)

### REGIONAL
## (free scales)
pl_food_demand_staplesVsNonStapes_regional = ggplot(data = food_demand_regional %>%
                                                      dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
                                                      group_by(region,input,year,scenario_type) %>%
                                                      dplyr::mutate(median_value = median(value)) %>%
                                                      dplyr::mutate(min_value = min(value)) %>%
                                                      dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,input,scenario), color = interaction(scenario_type,input)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,input)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,input)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
                     breaks = staples_vs_nonstaples_order_palette) +
  scale_fill_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
                    breaks = staples_vs_nonstaples_order_palette) +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(y = 'Pcal', x = '', title = 'Annual Regional food demand (free scales)') +
  # theme
  theme_light() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2))
ggsave(pl_food_demand_staplesVsNonStapes_regional, file = paste0(figures_path,"tmp_figs/",'pl1_food_demand_staplesVsNonStapes_regional_freeScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)


## (fixed scales)
pl_food_demand_staplesVsNonStapes_regional = ggplot(data = food_demand_regional %>%
                                                      dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
                                                      group_by(region,input,year,scenario_type) %>%
                                                      dplyr::mutate(median_value = median(value)) %>%
                                                      dplyr::mutate(min_value = min(value)) %>%
                                                      dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,input,scenario), color = interaction(scenario_type,input)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,input)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,input)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
                     breaks = staples_vs_nonstaples_order_palette) +
  scale_fill_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
                    breaks = staples_vs_nonstaples_order_palette) +
  # facet
  facet_wrap(. ~ region) +
  # labs
  labs(y = 'Pcal', x = '', title = 'Annual Regional food demand (fixed scales)') +
  # theme
  theme_light() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2))
ggsave(pl_food_demand_staplesVsNonStapes_regional, file = paste0(figures_path,"tmp_figs/",'pl1_food_demand_staplesVsNonStapes_regional_fixedScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)


### WORLD
pl_ag_production_world = ggplot(data = ag_production_world %>%
                                  dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
                                  group_by(sector,year,scenario_type) %>%
                                  dplyr::mutate(median_value = median(value)) %>%
                                  dplyr::mutate(min_value = min(value)) %>%
                                  dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,sector,scenario), color = interaction(scenario_type,sector)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,sector)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,sector)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = food_items_scenario_palette, name = 'Scenario',
                     breaks = food_items_order_palette) +
  scale_fill_manual(values = food_items_scenario_palette, name = 'Scenario',
                    breaks = food_items_order_palette) +
  # labs
  labs(y = 'Pcal', x = '', title = 'Annual World ag production') +
  # theme
  theme_light() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(ncol = 4), color = guide_legend(ncol = 4))
ggsave(pl_ag_production_world, file = paste0(figures_path,"tmp_figs/",'pl1_ag_production_world.pdf'),
       width = 750, height = 500, units = 'mm', limitsize = FALSE)

### REGIONAL
## (free scales)
pl_ag_production_regional = ggplot(data = ag_production_regional %>%
                                     dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
                                     group_by(region,sector,year,scenario_type) %>%
                                     dplyr::mutate(median_value = median(value)) %>%
                                     dplyr::mutate(min_value = min(value)) %>%
                                     dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,sector,scenario), color = interaction(scenario_type,sector)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,sector)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,sector)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = food_items_scenario_palette, name = 'Scenario',
                     breaks = food_items_order_palette) +
  scale_fill_manual(values = food_items_scenario_palette, name = 'Scenario',
                    breaks = food_items_order_palette) +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(y = 'Pcal', x = '', title = 'Annual Regional ag production (free scales)') +
  # theme
  theme_light() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(ncol = 4), color = guide_legend(ncol = 4))
ggsave(pl_ag_production_regional, file = paste0(figures_path,"tmp_figs/",'pl1_ag_production_regional_freeScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)


## (fixed scales)
pl_ag_production_regional = ggplot(data = ag_production_regional %>%
                                     dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
                                     group_by(region,sector,year,scenario_type) %>%
                                     dplyr::mutate(median_value = median(value)) %>%
                                     dplyr::mutate(min_value = min(value)) %>%
                                     dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,sector,scenario), color = interaction(scenario_type,sector)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,sector)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,sector)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = food_items_scenario_palette, name = 'Scenario',
                     breaks = food_items_order_palette) +
  scale_fill_manual(values = food_items_scenario_palette, name = 'Scenario',
                    breaks = food_items_order_palette) +
  # facet
  facet_wrap(. ~ region) +
  # labs
  labs(y = 'Pcal', x = '', title = 'Annual Regional ag production (fixed scales)') +
  # theme
  theme_light() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(ncol = 4), color = guide_legend(ncol = 4))
ggsave(pl_ag_production_regional, file = paste0(figures_path,"tmp_figs/",'pl1_ag_production_regional_fixedScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)

## == A: end food consumption
#####

#### Fig: food price ===============================
# =============================
## == C: start food price
## REGIONAL
# by abs value
ag_prices_diffAbs_regional = tidyr::pivot_wider(ag_prices_regional, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with("Flex.ds.beh")), list(diff = ~ . - Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference')) %>%
  # select desired sectors
  dplyr::filter(year == selected_year, sector %in% c('regional beef', 'regional pork', 'regional poultry', 'regional legumes',
                                                   'regional fruits', 'regional vegetables', 'regional oilcrop', 'regional root_tuber',
                                                   'regional corn', 'regional wheat', 'regional rice')) %>%
  # rename sectors
  dplyr::mutate(sector = gsub("_", " ", sector, fixed=TRUE)) %>%
  dplyr::mutate(sector = stringr::str_to_title(sub(".*regional ", "", sector))) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with("Flex.ds.beh"), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,sector,year) %>%
  dplyr::summarise(median_value = median(value))

pl_ag_prices_diffAbs_regional <- ggplot(data = ag_prices_diffAbs_regional) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
  geom_bar(aes(x=reorder(region,median_value), y=median_value, fill=sector),stat="identity",col='black',linewidth=0.2) +
  scale_fill_manual(name="",
                    values=c('#2C39FC','#3EA1DA','#1FDDED','#B800AC','#0E8600','#00BB19','#85E892','#B00000','#D05959','#D1D400','#CF8400'),
                    breaks=c("Beef","Pork","Poultry","Legumes","Rice","Corn","Wheat","Fruits","Vegetables","Oilcrop","Root Tuber")) +
  guides(fill = guide_legend(nrow = 2)) +
  geom_hline(yintercept = 0, linewidth = 1.5) +
  labs(y="$/Mt",x="",title = paste("Abs difference Behavior-Ref change in", selected_year))+
  theme_bw()+
  theme(
    strip.background = element_blank(),
    axis.text.x = element_text(size=30),
    axis.text.y = element_text(size=30),
    legend.text = element_text(size = 30),
    legend.title = element_text(size = 40),
    legend.position = 'bottom',
    title = element_text(size = 40)
  ) +
  coord_flip()
ggsave(pl_ag_prices_diffAbs_regional, file = paste0(figures_path,'tmp_figs/pl1_ag_prices_diffAbs_regional.pdf'), width = 600, height = 700, units = 'mm')


# by percentage
ag_prices_diffPer_regional = tidyr::pivot_wider(ag_prices_regional, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with("Flex.ds.beh")), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference')) %>%
  # select desired sectors
  dplyr::filter(year == selected_year, sector %in% c('regional beef', 'regional pork', 'regional poultry', 'regional legumes',
                                                     'regional fruits', 'regional vegetables', 'regional oilcrop', 'regional root_tuber',
                                                     'regional corn', 'regional wheat', 'regional rice')) %>%
  # rename sectors
  dplyr::mutate(sector = gsub("_", " ", sector, fixed=TRUE)) %>%
  dplyr::mutate(sector = stringr::str_to_title(sub(".*regional ", "", sector))) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with("Flex.ds.beh"), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,sector,year) %>%
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
  theme_bw()+
  theme(
    strip.background = element_blank(),
    axis.text.x = element_text(size=30),
    axis.text.y = element_text(size=30),
    legend.text = element_text(size = 30),
    legend.title = element_text(size = 40),
    legend.position = 'bottom',
    title = element_text(size = 40)
  ) +
  coord_flip()
ggsave(pl_ag_prices_diffPer_regional, file = paste0(figures_path,'tmp_figs/pl1_ag_prices_diffPer_regional.pdf'), width = 600, height = 700, units = 'mm')




### WORLD
pl_food_demand_staplesVsNonStapes_world = ggplot(data = food_demand_prices_world %>%
                                                   dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
                                                   group_by(input,year,scenario_type) %>%
                                                   dplyr::mutate(median_value = median(value)) %>%
                                                   dplyr::mutate(min_value = min(value)) %>%
                                                   dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,input,scenario), color = interaction(scenario_type,input)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,input)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,input)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
                     breaks = staples_vs_nonstaples_order_palette) +
  scale_fill_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
                    breaks = staples_vs_nonstaples_order_palette) +
  # labs
  labs(y = '2005$/Mcal', x = '', title = 'Annual World food demand prices') +
  # theme
  theme_light() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2))
ggsave(pl_food_demand_staplesVsNonStapes_world, file = paste0(figures_path,"tmp_figs/",'pl1_food_demand_prices_staplesVsNonStapes_world.pdf'),
       width = 550, height = 500, units = 'mm', limitsize = FALSE)

### REGIONAL
pl_food_demand_staplesVsNonStapes_regional = ggplot(data = food_demand_prices_regional %>%
                                                   dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
                                                   group_by(region,input,year,scenario_type) %>%
                                                   dplyr::mutate(median_value = median(value)) %>%
                                                   dplyr::mutate(min_value = min(value)) %>%
                                                   dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,input,scenario), color = interaction(scenario_type,input)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,input)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,input)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
                     breaks = staples_vs_nonstaples_order_palette) +
  scale_fill_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
                    breaks = staples_vs_nonstaples_order_palette) +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(y = '2005$/Mcal', x = '', title = 'Annual Regional food demand prices (free scales)') +
  # theme
  theme_light() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2))
ggsave(pl_food_demand_staplesVsNonStapes_regional, file = paste0(figures_path,"tmp_figs/",'pl1_food_demand_prices_staplesVsNonStapes_regional_freeScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)

pl_food_demand_staplesVsNonStapes_regional = ggplot(data = food_demand_prices_regional %>%
                                                      dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
                                                      group_by(region,input,year,scenario_type) %>%
                                                      dplyr::mutate(median_value = median(value)) %>%
                                                      dplyr::mutate(min_value = min(value)) %>%
                                                      dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,input,scenario), color = interaction(scenario_type,input)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type,input)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type,input)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
                     breaks = staples_vs_nonstaples_order_palette) +
  scale_fill_manual(values = staples_vs_nonstaples_scenario_palette, name = 'Scenario',
                    breaks = staples_vs_nonstaples_order_palette) +
  # facet
  facet_wrap(. ~ region) +
  # labs
  labs(y = '2005$/Mcal', x = '', title = 'Annual Regional food demand prices (fixed scales)') +
  # theme
  theme_light() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2))
ggsave(pl_food_demand_staplesVsNonStapes_regional, file = paste0(figures_path,"tmp_figs/",'pl1_food_demand_prices_staplesVsNonStapes_regional_fixedScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)
## == C: end food price
#####

#### Fig: ghg emissions ============================
# =============================
## == A: start ghg
## WORLD
## -- annual trend
pl_ghg_world <- ggplot(data = ghg_world %>%
                             dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
                             dplyr::group_by(Units,year,scenario_type) %>%
                             dplyr::mutate(median_value = median(value)) %>%
                             dplyr::mutate(min_value = min(value)) %>%
                             dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scenario_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scenario_type), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scenario_type), alpha = 0.15) +  # Shadow
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
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
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_ghg_world, file = paste0(figures_path,'tmp_figs/pl2_ghg_world.pdf'), width = 400, height = 300, units = 'mm')


## -- map (abs difference)
ghg_diffAbs_regional = tidyr::pivot_wider(ghg_regional, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with("Flex.ds.beh")), list(diff = ~ . - Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference')) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with("Flex.ds.beh"), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,Units,year) %>%
  dplyr::summarise(median_value = median(value)) %>%
  # filter desired year
  dplyr::filter(year == selected_year) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3')# %>%
  # inner_join(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
  #              dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
  #              dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
  #              dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
  #            by = "adm0_a3")

ghg_diffAbs_regional = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
              ghg_diffAbs_regional, by = 'adm0_a3')

# plot
pl_ghg_diffAbs_map <- ggplot() +
  # color map by regions
  geom_sf(data = ghg_diffAbs_regional, aes(fill = median_value)) +
  scale_fill_gradient2(low = "#0DA800", high = "#C60000",
                       mid = '#C2DAC1', midpoint = 0,
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
        legend.text = element_text(size = 30), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # title
  labs(title = paste('Abs MTCO2 regional GHG avoided emissions in', selected_year))
ggsave(pl_ghg_diffAbs_map, file = paste0(figures_path,'tmp_figs/pl2_ghg_diffAbs_map.pdf'), width = 500, height = 300, units = 'mm')



## -- map (per difference)
ghg_diffPer_regional = tidyr::pivot_wider(ghg_regional, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with("Flex.ds.beh")), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference')) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with("Flex.ds.beh"), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,Units,year) %>%
  dplyr::summarise(median_value = median(value)) %>%
  # filter desired year
  dplyr::filter(year == selected_year) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  inner_join(GCAM_reg, by = 'GCAM Region', multiple = "all") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3')# %>%

ghg_diffPer_regional = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                               dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                               dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                               dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                             ghg_diffPer_regional, by = 'adm0_a3')

# plot
pl_ghg_diffPer_map <- ggplot() +
  # color map by regions
  geom_sf(data = ghg_diffPer_regional, aes(fill = median_value)) +
  scale_fill_gradient2(low = "#0DA800", high = "#C60000",
                       mid = '#C2DAC1', midpoint = 0,
                       name = expression(paste(MtCO[2],' % difference'))) +
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
  labs(title = paste('Per diff regional GHG emissions in', selected_year))
ggsave(pl_ghg_diffPer_map, file = paste0(figures_path,'tmp_figs/pl2_ghg_diffPer_map.pdf'), width = 500, height = 300, units = 'mm')


## -- ghg emission by type (abs difference)
ghg_by_ghg_diffAbs_world = tidyr::pivot_wider(ghg_by_ghg_world, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with("Flex.ds.beh")), list(diff = ~ . - Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference')) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with("Flex.ds.beh"), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(group,Units,year) %>%
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
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  # title
  labs(title = paste('Abs diff of world ghg emissions in', selected_year))
ggsave(pl_ghg_diffAbs_world_bars, file = paste0(figures_path,'tmp_figs/pl2_ghg_diffAbs_bars_world.pdf'), width = 400, height = 150, units = 'mm')


## -- ghg emission by type (per difference)
ghg_by_ghg_diffPer_world = tidyr::pivot_wider(ghg_by_ghg_world, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with("Flex.ds.beh")), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference')) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with("Flex.ds.beh"), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(group,Units,year) %>%
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
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  # title
  labs(title = paste('Per diff of world ghg emissions in', selected_year))
ggsave(pl_ghg_diffPer_world_bars, file = paste0(figures_path,'tmp_figs/pl2_ghg_diffPer_bars_world.pdf'), width = 400, height = 150, units = 'mm')

## == A: end ghg
#####

#### Fig: avoided deaths ===========================
# =============================
# ## map av deaths
# deaths.labs <- c("d1", "d2")
# names(deaths.labs) <- c("Protein", "Trade")
# # pl2_A_p = do_fig2_plA(mort_diff_percentage,'percentage')
# do_fig2_plA(mort_diff_absNum,'abs_numbers')


#####

#### Fig: water consumption and withdrawals ========
# =============================
## == C: start water
# water consumption
### WORLD
## -- world water consumption
pl_water_consumption_world <- ggplot(data = water_consumption_world %>%
                           dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
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
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  # title
  labs(title = 'Annual World Water consumption')
ggsave(pl_water_consumption_world, file = paste0(figures_path,'tmp_figs/pl2_water_consumption_world.pdf'), width = 500, height = 300, units = 'mm')



### REGIONAL
## -- regional water consumption (free scales)
pl_water_consumption_regional <- ggplot(data = water_consumption_regional %>%
                                       dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
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
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  # title
  labs(title = 'Annual Water consumption (free scales)')
ggsave(pl_water_consumption_regional + theme(strip.text = element_text(size = 30)), file = paste0(figures_path,'tmp_figs/pl2_water_consumption_regional_freeScales.pdf'),
       width = 800, height = 700, units = 'mm')

## -- regional water consumption (fixed scales)
pl_water_consumption_regional <- ggplot(data = water_consumption_regional %>%
                                          dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
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
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  # title
  labs(title = 'Annual Water consumption (fixed scales)')
ggsave(pl_water_consumption_regional + theme(strip.text = element_text(size = 30)), file = paste0(figures_path,'tmp_figs/pl2_water_consumption_regional_fixedScales.pdf'),
       width = 800, height = 700, units = 'mm')


### MAPS
## -- map (abs difference)
water_consumption_diffAbs_regional = tidyr::pivot_wider(water_consumption_regional, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with("Flex.ds.beh")), list(diff = ~ . - Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference')) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with("Flex.ds.beh"), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,year) %>%
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
                       mid = '#C2DAC1', midpoint = 0,
                       name = expression(paste("Annual Water flows difference (billion ",m^3,")","\n"))) +
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
  labs(title = expression(paste("Annual water consumption abs difference (billion ",m^3,") in ", selected_year, "\n")))
ggsave(pl_water_consumption_diffAbs_map, file = paste0(figures_path,'tmp_figs/pl2_water_consumption_diffAbs_map.pdf'), width = 500, height = 300, units = 'mm')


## -- map (per difference)
water_consumption_diffPer_regional = tidyr::pivot_wider(water_consumption_regional, names_from = 'scenario', values_from = 'value') %>%
  # compute difference between Reference and runs
  dplyr::mutate_at(vars(starts_with("Flex.ds.beh")), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference')) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with("Flex.ds.beh"), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,year) %>%
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
                       mid = '#C2DAC1', midpoint = 0,
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
        legend.text = element_text(size = 30), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 40)) +
  # title
  labs(title = paste("Annual water consumption % difference in", selected_year, "\n"))
ggsave(pl_water_consumption_diffPer_map, file = paste0(figures_path,'tmp_figs/pl2_water_consumption_diffPer_map.pdf'), width = 500, height = 300, units = 'mm')



## -- bars (abs difference)
water_consumption_diffAbs_regional_sectorial = water_consumption_regional_sectorial %>%
  # compute regional-sectorial consumption
  dplyr::filter(sector %in% food_sector) %>%
  group_by(year,scenario,sector,region) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  # compute difference between Reference and runs
  tidyr::pivot_wider(names_from = 'scenario', values_from = 'value') %>%
  dplyr::mutate_at(vars(starts_with("Flex.ds.beh")), list(diff = ~ . - Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference')) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with("Flex.ds.beh"), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,year,sector) %>%
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
  labs(x = '', y = expression(paste("Annual Water flows difference (billion ",m^3,")","\n"))) +
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
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  # title
  labs(title = paste0('Abs diff of regional sectorial water consumption in ',selected_year))
ggsave(pl_water_consumption_diffAbs_regional_sectorial_bars, file = paste0(figures_path,'tmp_figs/pl2_water_consumption_diffAbs_regional_sectorial_bars.pdf'),
       width = 700, height = 500, units = 'mm')

## -- bars (per difference)
water_consumption_diffPer_regional_sectorial = water_consumption_regional_sectorial %>%
  # compute regional-sectorial consumption
  dplyr::filter(sector %in% food_sector) %>%
  group_by(year,scenario,sector,region) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  # compute difference between Reference and runs
  tidyr::pivot_wider(names_from = 'scenario', values_from = 'value') %>%
  dplyr::mutate_at(vars(starts_with("Flex.ds.beh")), list(diff = ~ 100*(. - Reference)/Reference)) %>%
  # clean the dataset and keep only the "difference" columns
  dplyr::select(-c(matches("[0-9]$"),'Reference')) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = starts_with("Flex.ds.beh"), names_to = 'scenario') %>%
  # compute median
  dplyr::group_by(region,year,sector) %>%
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
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  # title
  labs(title = paste0('Per diff of regional sectorial water consumption in ',selected_year))
ggsave(pl_water_consumption_diffAbs_regional_sectorial_bars, file = paste0(figures_path,'tmp_figs/pl2_water_consumption_diffPer_regional_sectorial_bars.pdf'),
       width = 700, height = 500, units = 'mm')





### WORLD
## -- world water withdrawals
pl_water_withdrawals_world <- ggplot(data = water_withdrawals_world %>%
                                       dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
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
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  # title
  labs(title = 'Annual World Water withdrawals')
ggsave(pl_water_withdrawals_world, file = paste0(figures_path,'tmp_figs/pl2_water_withdrawals_world.pdf'), width = 500, height = 300, units = 'mm')



### REGIONAL
## -- regional water withdrawals (free scales)
pl_water_withdrawals_regional <- ggplot(data = water_withdrawals_regional %>%
                                          dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
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
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  # title
  labs(title = 'Annual Water withdrawals (free scales)')
ggsave(pl_water_withdrawals_regional + theme(strip.text = element_text(size = 30)), file = paste0(figures_path,'tmp_figs/pl2_water_withdrawals_regional_freeScales.pdf'),
       width = 800, height = 700, units = 'mm')

## -- regional water withdrawals (fixed scales)
pl_water_withdrawals_regional <- ggplot(data = water_withdrawals_regional %>%
                                          dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
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
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  # title
  labs(title = 'Annual Water withdrawals (fixed scales)')
ggsave(pl_water_withdrawals_regional + theme(strip.text = element_text(size = 30)), file = paste0(figures_path,'tmp_figs/pl2_water_withdrawals_regional_fixedScales.pdf'),
       width = 800, height = 700, units = 'mm')

#####

#### Fig: virtual water trade ======================
# =============================
# source('vwt.R')
# compute_vwt()
## == C: end water
#####

#### Fig: beef - dairy =============================
# =============================
## -- prices (meet and dairy)
### WORLD
pl_ag_meet_dairy_prices_world = ggplot(data = ag_meet_dairy_prices_world %>%
                                                   dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
                                                   dplyr::group_by(sector,year,scenario_type) %>%
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
  # labs
  labs(y = '2005$/Mcal', x = '', title = 'Annual World beef-dairy prices') +
  # theme
  theme_light() +
  theme(legend.position = 'bottom', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2))
ggsave(pl_ag_meet_dairy_prices_world, file = paste0(figures_path,"tmp_figs/",'pl3_ag_meet_dairy_prices_world.pdf'),
       width = 550, height = 500, units = 'mm', limitsize = FALSE)

### REGIONAL
## (free scales)
pl_ag_meet_dairy_prices_regional = ggplot(data = ag_meet_dairy_prices_regional %>%
                                                   dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
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
  theme(legend.position = 'bottom', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2))
ggsave(pl_ag_meet_dairy_prices_regional, file = paste0(figures_path,"tmp_figs/",'pl3_ag_meet_dairy_prices_regional_freeScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)

# (fixed scales)
pl_ag_meet_dairy_prices_regional = ggplot(data = ag_meet_dairy_prices_regional %>%
                                            dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
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
  theme(legend.position = 'bottom', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40)) +
  guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2))
ggsave(pl_ag_meet_dairy_prices_regional, file = paste0(figures_path,"tmp_figs/",'pl3_ag_meet_dairy_prices_regional_fixedScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)

#####

#### Fig: CH4 =============================
# =============================
## -- CH4 emissions
### WORLD
pl_ghg_by_ghg_world = ggplot(data = ghg_by_ghg_world %>% filter(group == 'CH4') %>%
                                         dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
                                         dplyr::group_by(year,scenario_type) %>%
                                         dplyr::mutate(median_value = median(value)) %>%
                                         dplyr::mutate(min_value = min(value)) %>%
                                         dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,scenario), color = interaction(scenario_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = expression(MtCO[2]), x = '', title = expression(paste('Annual World ',CH[4],' emissions prices'))) +
  # theme
  theme_light() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_ghg_by_ghg_world, file = paste0(figures_path,"tmp_figs/",'pl3_ghg_by_ghg_world.pdf'),
       width = 550, height = 500, units = 'mm', limitsize = FALSE)

### REGIONAL
## (free scales)
pl_ghg_by_ghg_regional = ggplot(data = ghg_by_ghg_regional %>% filter(group == 'CH4') %>%
                               dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
                               dplyr::group_by(region,year,scenario_type) %>%
                               dplyr::mutate(median_value = median(value)) %>%
                               dplyr::mutate(min_value = min(value)) %>%
                               dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,scenario), color = interaction(scenario_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # facet
  facet_wrap(. ~ region, scales = 'free') +
  # labs
  labs(y = expression(MtCO[2]), x = '', title = expression(paste('Annual World ',CH[4],' emissions prices (free scales)'))) +
  # theme
  theme_light() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_ghg_by_ghg_regional, file = paste0(figures_path,"tmp_figs/",'pl3_ghg_by_ghg_regional_freeScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)

# (fixed scales)
pl_ghg_by_ghg_regional = ggplot(data = ghg_by_ghg_regional %>% filter(group == 'CH4') %>%
                                  dplyr::mutate(scenario_type = ifelse(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
                                  dplyr::group_by(region,year,scenario_type) %>%
                                  dplyr::mutate(median_value = median(value)) %>%
                                  dplyr::mutate(min_value = min(value)) %>%
                                  dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario_type,scenario), color = interaction(scenario_type)), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = interaction(scenario_type)), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(scenario_type)), alpha = 0.15) +  # Shadow
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  # facet
  facet_wrap(. ~ region) +
  # labs
  labs(y = expression(MtCO[2]), x = '', title = expression(paste('Annual World ',CH[4],' emissions prices (fixed scales)'))) +
  # theme
  theme_light() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_ghg_by_ghg_regional, file = paste0(figures_path,"tmp_figs/",'pl3_ghg_by_ghg_regional_fixedScales.pdf'),
       width = 1000, height = 1000, units = 'mm', limitsize = FALSE)

#####


################################################################################
## extra figures
################################################################################

# ## -- premature mortalities
# plt_mort = ggplot(data = mort_simpl %>% group_by(year, scenario) %>% summarise('value' = sum(total_mort)) %>% rename_scen()) +
#   geom_line(aes(x = year, y = value, color = scenario, group = scenario), alpha = 0.5) +
#   scale_color_manual(values = mypal_scen) +
#   ggtitle('premature mortalities')
# plt_mort
# ggsave(plt_mort, file = paste0(figures_path,"extra_figs//plt_mort.png"), width = 200, height = 150, units = 'mm')

