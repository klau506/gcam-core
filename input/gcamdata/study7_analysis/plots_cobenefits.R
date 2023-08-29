# devtools::install_github("klau506/rfasst")

library(rgcam)
library(dplyr)
library(ggplot2)
library(rfasst)

gcam_path <- substr(getwd(), start = 1, stop = regexpr("gcam-core/", getwd()) + 9)
tmp_output_data_path = paste0(gcam_path, "/input/gcamdata/outputs_binomial/")
figures_path = paste0(gcam_path, "/input/gcamdata/figures_binomial/")
folder_analysis_path = paste0(gcam_path, "input/gcamdata/study7_analysis/")

db_path <- paste0(gcam_path, "output")
db_name_base = 'behaviour_basexdb'
prj_name = 'behavioral_change_x5.dat'
query_path = paste0(gcam_path, "input/gcamdata/study7_analysis/data/")
queries = 'queries_beh.xml'
desired_scen = c('Reference', paste0("Flex.ds.beh", 1:25))

final_db_year = 2100

create_prj = 0 # 0 to skip, 1 to create it

if (create_prj) {

  ## select db accordingn to the scenario
  for (sc in desired_scen) {
    if (sc == 'Reference') {
      db_name = paste0(db_name_base,'_ref')
    } else {
      sc_num <- sub(".*beh(.*)$", "\\1", sc)

      if (sc_num <= 5) {
        db_name = paste0(db_name_base,'_1_5')
      } else if (sc_num > 5 & sc_num <= 10) {
        db_name = paste0(db_name_base,'_6_10')
      } else if (sc_num > 10 & sc_num <= 15) {
        db_name = paste0(db_name_base,'_11_15')
      } else if (sc_num > 15 & sc_num <= 20) {
        db_name = paste0(db_name_base,'_16_20')
      } else if (sc_num > 20 & sc_num <= 25) {
        db_name = paste0(db_name_base,'_21_25')
      }
    }

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
  prj <<- loadProject(prj_name)
  listQueries(prj)
  listScenarios(prj)
}


################################################################################
## data preprocess
################################################################################
source(paste0(folder_analysis_path,'zzz.R'))
year_s = 2000
year_e = 2100
selected_scen = scen_name

food_demand <- rgcam::getQuery(prj, "food demand") %>%
  filter(scenario %in% selected_scen) %>%
  group_by(scenario, input, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  filter(year >= year_s, year <= year_e)

food_consumption <- rgcam::getQuery(prj, "food consumption by type (specific)") %>%
  filter(scenario %in% selected_scen) %>%
  group_by(scenario, technology, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  filter(year >= year_s, year <= year_e)

ag_production <- rgcam::getQuery(prj, "ag production by crop type") %>%
  filter(scenario %in% selected_scen) %>%
  group_by(scenario, sector, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  filter(year >= year_s, year <= year_e)

food_demand_prices <- rgcam::getQuery(prj, "food demand prices") %>%
  filter(scenario %in% selected_scen) %>%
  group_by(scenario, input, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  filter(year >= year_s, year <= year_e)

ag_prices <- rgcam::getQuery(prj, "ag import vs. domestic prices") %>%
  filter(scenario %in% selected_scen) %>%
  group_by(scenario, sector, subsector, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  filter(year >= year_s, year <= year_e)

ag_regional_prices <- rgcam::getQuery(prj, "ag regional prices (weighted average b/t domestic and imported prices)") %>%
  filter(scenario %in% selected_scen) %>%
  group_by(scenario, region, sector, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  filter(year >= year_s, year <= year_e)

ag_import_vs_domestic <- rgcam::getQuery(prj, "ag import vs. domestic supply (Regional Armington competition)") %>%
  filter(scenario %in% selected_scen) %>%
  group_by(scenario, sector, subsector, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate('type' = ifelse(substr(subsector, 1, 8) == "domestic", "domestic",
                         ifelse(substr(subsector, 1, 8) == "imported", "imported", NA))) %>%
  filter(scenario %in% selected_scen) %>%
  filter(!is.na(type)) %>%
  filter(year >= year_s, year <= year_e)

water_withdrawals <- rgcam::getQuery(prj, "water withdrawals by region") %>%
  filter(scenario %in% selected_scen) %>%
  group_by(scenario, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  filter(year >= year_s, year <= year_e)

food_sector = c('Beef','Corn','Dairy','FiberCrop','FodderHerb','Fruits','Legumes',
                'MiscCrop','NutsSeeds','OilCrop','Pork','Poultry','Rice','RootTuber',
                'SheepGoat','Soybean','SugarCrop','Vegetables','Wheat','OilPalm',
                'FodderGrass')
water_consumption_by_reg <- rgcam::getQuery(prj, "water consumption by subsector") %>%
  filter(scenario %in% selected_scen, sector %in% food_sector) %>%
  group_by(scenario, region, sector, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  filter(year >= year_s, year <= year_e)
water_consumption <- rgcam::getQuery(prj, "water consumption by subsector") %>%
  filter(scenario %in% selected_scen, sector %in% food_sector) %>%
  group_by(scenario, sector, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  filter(year >= year_s, year <= year_e)
water_consumption_total <- rgcam::getQuery(prj, "water consumption by subsector") %>%
  filter(scenario %in% selected_scen, sector %in% food_sector) %>%
  group_by(scenario, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  filter(year >= year_s, year <= year_e)


## -- rfasst
mort = add_mort_scen('Flex.ds.beh1')
mort = add_mort_scen('Reference')
for (i in unique(selected_scen)[2:length(selected_scen)]) {
  mort = dplyr::bind_rows(mort,add_mort_scen(i))
}
save(mort, file = paste0(tmp_output_data_path,'mort_',prj_name,'.RData'))
# load(paste0('mort_',prj_name,'.RData'))

mort_simpl = mort %>%
  filter(scenario %in% selected_scen) %>%
  filter(year >= year_s, year <= year_e) %>%
  # compute mean mortality accross all imp fun
  dplyr::group_by(region,year,scenario,pollutant) %>%
  dplyr::summarise('mean_mort' = mean(as.numeric(mort), na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  # add o3 and pm mortalities
  dplyr::group_by(region,year,scenario) %>%
  dplyr::summarise('total_mort' = sum(mean_mort))

# mortality difference (in %)
mort_diff_percentage = tidyr::pivot_wider(mort_simpl, names_from = 'scenario', values_from = 'total_mort') %>%
  dplyr::mutate('diff_protein' = 100*(Diets_Ref  - Diets_Protein)/Diets_Ref) %>%
  dplyr::mutate('diff_trade' = 100*(Diets_Ref - Diets_Trade.adj21)/Diets_Ref)
mort_diff_percentage = tidyr::pivot_longer(mort_diff_percentage %>% dplyr::select(region,year,diff_protein,diff_trade),
                                           cols = c('diff_protein','diff_trade')) %>%
  dplyr::rename('scenario' = 'name') %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))

# mortality difference (in absolute numbers)
mort_diff_absNum = tidyr::pivot_wider(mort_simpl, names_from = 'scenario', values_from = 'total_mort') %>%
  dplyr::mutate('diff_protein' = Diets_Ref  - Diets_Protein) %>%
  dplyr::mutate('diff_trade' = Diets_Ref - Diets_Trade.adj21)
mort_diff_absNum = tidyr::pivot_longer(mort_diff_absNum %>% dplyr::select(region,year,diff_protein,diff_trade),
                                       cols = c('diff_protein','diff_trade')) %>%
  dplyr::rename('scenario' = 'name') %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))




###### -- emissions
GWP <- readr::read_csv(paste0(folder_analysis_path,"data/GWP_AR5.csv"))

co2 <- getQuery(prj,"CO2 emissions by region") %>%
  group_by(scenario, region, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(ghg = "CO2") %>%
  filter(year >= year_s, year <= year_e)

luc <- getQuery(prj,"LUC emissions by region") %>%
  group_by(Units, scenario, region, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(ghg = "LUC CO2",
         Units = "MTC") %>%
  filter(year >= year_s, year <= year_e)

nonco2 <- getQuery(prj,"nonCO2 emissions by region") %>%
  filter(year >= year_s, year <= year_e)


# GHG emissions ----
ghg_by_ghg <- bind_rows(luc,co2,nonco2) %>%
  rename_scen() %>%
  left_join(GWP, by = c("Units", "ghg")) %>%
  mutate(value = value * GWP,
         Units = "MtCO2e") %>%
  group_by(group, scenario, year, Units) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  filter(!is.na(group))

ghg_by_reg <- bind_rows(luc,co2,nonco2) %>%
  rename_scen() %>%
  left_join(GWP, by = c("Units", "ghg")) %>%
  mutate(value = value * GWP,
         Units = "MtCO2e") %>%
  group_by(scenario, region, year, Units) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  ungroup()

ghg_total <- bind_rows(luc,co2,nonco2) %>%
  rename_scen() %>%
  left_join(GWP, by = c("Units", "ghg")) %>%
  mutate(value = value * GWP,
         Units = "MtCO2e") %>%
  group_by(scenario, year, Units) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  ungroup()



################################################################################
## abstract figures
################################################################################
if (!dir.exists(paste0(figures_path,"tmp_figs"))) dir.create(paste0(figures_path,"tmp_figs"))


source(paste0(folder_analysis_path,'fun_cobenefits.R'))
selected_year = 2030


#### -- fig consumption trends

## A: food consumption
# subset relevant items
plt_food_consumption = food_consumption %>% #rename_scen() %>%
  dplyr::filter(technology %in% c('Beef','Pork','Poultry','OtherMeat_Fish','Legumes','NutsSeeds')#,scenario %in% c('Reference','Protein')
                ) %>%
  dplyr::mutate(technology = ifelse(technology == 'OtherMeat_Fish', 'Other Meat and Fish',
                                    ifelse(technology == 'NutsSeeds', 'Nuts and Seeds', technology)))

plt_food_consumption$technology = factor(plt_food_consumption$technology, levels = c('Beef','Pork','Poultry','Other Meat and Fish','Legumes','Nuts and Seeds'))

# plot
pl1_A = ggplot(data = plt_food_consumption,
               aes(x = year, y = value, color = scenario), alpha = 0.5) +
  geom_line(alpha = 0.75, linewidth = 2) +
  facet_wrap(. ~ technology, nrow = 1, scales = 'free') +
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  # labs
  labs(y = 'Pcal', x = '') +
  ggtitle('Food consumption') +
  # theme
  theme_light() +
  theme(legend.position = 'none', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl1_A, file = paste0(figures_path,"tmp_figs/",'pl1_A.png'), width = 300, height = 125, units = 'mm')

## B: food security
pl_ag_import_vs_domestic = ag_import_vs_domestic %>% #rename_scen() %>%
  dplyr::mutate(sector = gsub("_", " ", sector, fixed=TRUE)) %>%
  dplyr::mutate(sector = stringr::str_to_title(sub(".*regional ", "", sector))) %>%
  dplyr::filter(sector %in% c('Beef','Pork','Poultry','Fruits','Vegetables','Legumes',
                              'Nuts Seeds','Corn', 'Wheat', 'Rice')#, scenario %in% c('Reference','Trade')
                )

pl_ag_import_vs_domestic$sector = factor(pl_ag_import_vs_domestic$sector,
                                         levels = c('Beef','Pork','Poultry','Fruits',
                                                    'Vegetables','Legumes','Nuts Seeds',
                                                    'Corn', 'Wheat', 'Rice'))


pl1_B = ggplot(data = pl_ag_import_vs_domestic, #%>% rename_scen(),
               aes(x = year, y = value, color = scenario, linetype = type), alpha = 0.5) +
  geom_line(alpha = 0.75, linewidth = 2) +
  facet_wrap(. ~ sector, nrow = 2, scales = 'free') +
  scale_color_manual(values = mypal_scen, name = 'Scenario') +
  labs(y = 'Mt', x = '') +
  ggtitle('Domesic vs Imported goods supply') +
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
ggsave(pl1_B, file = paste0(figures_path,"tmp_figs/",'pl1_B.png'), width = 300, height = 125, units = 'mm')


## C: food price
do_fig_prices()


## FIG 1
blank_p <- patchwork::plot_spacer() + theme_void()
leg_ag_import_vs_domestic = ag_import_vs_domestic %>% rename_scen()
leg_ag_import_vs_domestic$scenario = factor(leg_ag_import_vs_domestic$scenario, levels = c('Reference', 'Protein', 'Trade'))
leg_ag_import_vs_domestic = leg_ag_import_vs_domestic %>%
  dplyr::mutate(type = stringr::str_to_title(type))

legend_scen = ggpubr::get_legend(ggplot(data = leg_ag_import_vs_domestic,
                                        aes(x = year, y = value, color = scenario, linetype = type), alpha = 0.5) +
                                   geom_line(alpha = 0.5) +
                                   scale_color_manual(values = mypal_scen) +
                                   guides(linetype = 'none', color = guide_legend(title = 'Scenario',
                                                                                  keywidth = 4, override.aes = list(linewidth = 3))) +
                                   theme_light() +
                                   theme(legend.position = 'bottom', legend.direction = 'horizontal',
                                         strip.background = element_blank(),
                                         axis.text.x = element_text(size=30),
                                         axis.text.y = element_text(size=30),
                                         legend.text = element_text(size = 30),
                                         legend.title = element_text(size = 40),
                                         title = element_text(size = 40)))
legend_linetype = ggpubr::get_legend(ggplot(data = leg_ag_import_vs_domestic,
                                            aes(x = year, y = value, color = scenario, linetype = type), alpha = 0.5) +
                                       geom_line(alpha = 0.5) +
                                       guides(color = 'none', linetype = guide_legend(title = 'Supply type',
                                                                                      keywidth = 4, override.aes = list(linewidth = 3))) +
                                       theme_light() +
                                       theme(legend.position = 'bottom', legend.direction = 'horizontal',
                                             strip.background = element_blank(),
                                             axis.text.x = element_text(size=30),
                                             axis.text.y = element_text(size=30),
                                             legend.text = element_text(size = 30),
                                             legend.title = element_text(size = 40),
                                             title = element_text(size = 40)))

pl_C = ggpubr::ggarrange(pl_C1 + labs(title = 'c1'), pl_C2 + labs(title = 'c2'),
                         ncol=2, common.legend = T,legend = "bottom")

fig1 = cowplot::ggdraw() +
  theme(plot.background = element_rect(fill="white")) +
  cowplot::draw_plot(pl1_A + labs(title = 'a'),
                     x = 0.005, y = 0.79, width = 0.99, height = 0.18) +
  cowplot::draw_plot(pl1_B + theme(legend.position = 'none') + labs(title = 'b'),
                     x = 0.005, y = 0.40, width = 0.99, height = 0.38) +
  cowplot::draw_plot(cowplot::plot_grid(legend_scen,blank_p,nrow=1), x = 0.2, y = 0.22, width = 0.3, height = 0.35) +
  cowplot::draw_plot(cowplot::plot_grid(legend_linetype,blank_p,nrow=1), x = 0.67, y = 0.22, width = 0.3, height = 0.35) +
  cowplot::draw_plot(pl_C, x = 0.005, y = 0, width = 0.99, height = 0.38) +
  cowplot::draw_plot_label(label = "Fig1: Food consumption and supply", size = 45,
                           x = -0.125, y = 1)

# fig1
ggsave(fig1, file = paste0(figures_path,'fig1.png'), width = 1050, height = 1485, units = 'mm',
       limitsize = FALSE)

################################################################################
source('plotting_fun.R')
selected_year = 2030

#### -- fig cobenefits

## ghg
ghg.labs <- c("b1", "b2")
names(ghg.labs) <- c("Protein", "Trade")
do_ghg_figs()

## map av deaths
deaths.labs <- c("d1", "d2")
names(deaths.labs) <- c("Protein", "Trade")
# pl2_A_p = do_fig2_plA(mort_diff_percentage,'percentage')
do_fig2_plA(mort_diff_absNum,'abs_numbers')

## water
# water consumption
water.labs <- c("f1", "f2")
names(water.labs) <- c("Protein", "Trade")
do_water_figs()
# vwt
# source('vwt.R')
# compute_vwt()

## FIG 2
fig2 = cowplot::ggdraw() +
  theme(plot.background = element_rect(fill="white")) +
  cowplot::draw_plot(pl_ghg_emissions + labs(title = 'a'),
                     x = 0, y = 0.66, width = 0.27, height = 0.27) +
  cowplot::draw_plot(pl_ghg_bars + labs(title = ''),
                     x = 0.1, y = 0.71, width = 0.18, height = 0.125) +
  cowplot::draw_plot(pl_ghg_map + facet_wrap(.~scenario,
                                             labeller = labeller(scenario = ghg.labs)) +
                       theme(strip.text = element_text(angle = 0, hjust = 0.075)),
                     x = 0.27, y = 0.3, width = 0.73, height = 1) +
  cowplot::draw_plot(pl_mort_global + labs(title = 'c'),
                     x = 0, y = 0.35, width = 0.27, height = 0.27) +
  cowplot::draw_plot(pl_mort_map + facet_wrap(.~scenario,
                                              labeller = labeller(scenario = deaths.labs)) +
                       theme(strip.text = element_text(angle = 0, hjust = 0.075)),
                     x = 0.27, y = 0, width = 0.73, height = 1) +
  cowplot::draw_plot(pl_water_global + labs(title = 'e'),
                     x = 0, y = 0.04, width = 0.27, height = 0.27) +
  cowplot::draw_plot(pl_w_map + facet_wrap(.~scenario,
                                           labeller = labeller(scenario = water.labs)) +
                       theme(strip.text = element_text(angle = 0, hjust = 0.075)),
                     x = 0.28, y = -0.31, width = 0.73, height = 1) +
  # cowplot::draw_plot(pl_w_bars_Protein + labs(title = ' '),
  #                    x = 0.4, y = 0.0, width = 0.18, height = 0.125) +
  # cowplot::draw_plot(pl_w_bars_Trade + labs(title = ' '),
  #                    x = 0.8, y = 0.0, width = 0.18, height = 0.125) +
  cowplot::draw_plot_label(label = "Fig2: Co-benefits associated with sustainable food consumption patterns", size = 45,
                           x = -0.26, y = 0.99)
# fig2
ggsave(fig2, file = paste0(figures_path,'fig2.png'), width = 1050, height = 1000,
       units = 'mm', limitsize = FALSE)




################################################################################
## extra figures
################################################################################
if (!dir.exists(paste0(figures_path,"extra_figs"))) dir.create(paste0(figures_path,"extra_figs"))

plt_food_demand = ggplot(data = food_demand %>% rename_scen(),
                         aes(x = year, y = value, color = scenario, linetype = input)) +
  geom_line(alpha = 0.5) +
  scale_color_manual(values = mypal_scen) +
  ggtitle('food demand')
plt_food_demand
ggsave(plt_food_demand, file = paste0(figures_path,"extra_figs/plt_food_demand.png"), width = 200, height = 150, units = 'mm')

plt_food_consumption = ggplot(data = food_consumption %>% rename_scen(),
                              aes(x = year, y = value, color = scenario), alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(. ~ technology, nrow = 3) +
  scale_color_manual(values = mypal_scen) +
  ggtitle('food consumption')
plt_food_consumption
ggsave(plt_food_consumption, file = paste0(figures_path,"extra_figs/plt_food_consumption.png"), width = 250, height = 150, units = 'mm')

plt_food_demand_prices = ggplot(data = food_demand_prices %>% rename_scen(),
                                aes(x = year, y = value, color = scenario), alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(. ~ input, nrow = 3) +
  scale_color_manual(values = mypal_scen) +
  ggtitle('food demand prices')
plt_food_demand_prices
ggsave(plt_food_demand_prices, file = paste0(figures_path,"extra_figs/plt_food_demand_prices.png"), width = 250, height = 150, units = 'mm')


plt_ag_import_vs_domestic = ggplot(data = ag_import_vs_domestic %>% filter(scenario %in% selected_scen) %>% rename_scen(),
                                   aes(x = year, y = value, color = scenario, linetype = type), alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(. ~ sector, nrow = 3, scales = 'free') +
  scale_color_manual(values = mypal_scen) +
  ggtitle('ag demmand')
plt_ag_import_vs_domestic
ggsave(plt_ag_import_vs_domestic, file = paste0(figures_path,"extra_figs/plt_ag_import_vs_domestic.png"), width = 350, height = 150, units = 'mm')

plt_ag_production = ggplot(data = ag_production %>% rename_scen(),
                           aes(x = year, y = value, color = scenario), alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(. ~ sector, nrow = 3) +
  scale_color_manual(values = mypal_scen) +
  ggtitle('ag production')
plt_ag_production
ggsave(plt_ag_production, file = paste0(figures_path,"extra_figs//plt_ag_production.png"), width = 300, height = 150, units = 'mm')




## -- premature mortalities
plt_mort = ggplot(data = mort_simpl %>% group_by(year, scenario) %>% summarise('value' = sum(total_mort)) %>% rename_scen()) +
  geom_line(aes(x = year, y = value, color = scenario, group = scenario), alpha = 0.5) +
  scale_color_manual(values = mypal_scen) +
  ggtitle('premature mortalities')
plt_mort
ggsave(plt_mort, file = paste0(figures_path,"extra_figs//plt_mort.png"), width = 200, height = 150, units = 'mm')





## -- extra figures water by region (and sector)
water_consumption_by_reg_diff = water_consumption_by_reg %>%
  dplyr::filter(year == selected_year, sector %in% food_sector) %>%
  group_by(year,scenario,region) %>%
  summarise(value = sum(value)) %>%
  ungroup()
water_consumption_by_reg_diff = tidyr::pivot_wider(water_consumption_by_reg_diff , names_from = 'scenario', values_from = 'value') %>%
  dplyr::mutate('diff_protein' = 100*(Diets_Protein - Diets_Ref)/Diets_Ref) %>%
  dplyr::mutate('diff_trade' = 100*(Diets_Trade.adj21 - Diets_Ref)/Diets_Ref)
water_consumption_by_reg_diff = tidyr::pivot_longer(water_consumption_by_reg_diff %>% dplyr::select(region,year,diff_protein,diff_trade),
                                                    cols = c('diff_protein','diff_trade')) %>%
  dplyr::rename('scenario' = 'name') %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))


water_consumption_by_reg_sector_diff = water_consumption_by_reg %>%
  dplyr::filter(year == selected_year, sector %in% food_sector) %>%
  group_by(year,scenario,sector,region) %>%
  summarise(value = sum(value)) %>%
  ungroup()
water_consumption_by_reg_sector_diff = tidyr::pivot_wider(water_consumption_by_reg_sector_diff , names_from = 'scenario', values_from = 'value') %>%
  dplyr::mutate('diff_protein' = 100*(Diets_Protein - Diets_Ref)/Diets_Ref) %>%
  dplyr::mutate('diff_trade' = 100*(Diets_Trade.adj21 - Diets_Ref)/Diets_Ref)
water_consumption_by_reg_sector_diff = tidyr::pivot_longer(water_consumption_by_reg_sector_diff %>% dplyr::select(region,year,sector,diff_protein,diff_trade),
                                                           cols = c('diff_protein','diff_trade')) %>%
  dplyr::rename('scenario' = 'name') %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))




## -- imports and exports
plt_ag_import_vs_domestic = rgcam::getQuery(prj, "ag import vs. domestic supply (Regional Armington competition)") %>%
  filter(scenario %in% selected_scen, year == selected_year) %>%
  group_by(scenario, sector, subsector, year, region) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate('type' = ifelse(substr(subsector, 1, 8) == "domestic", "domestic",
                         ifelse(substr(subsector, 1, 8) == "imported", "imported", NA))) %>%
  filter(scenario %in% selected_scen) %>%
  filter(!is.na(type)) %>%
  ungroup()
plt_ag_import_vs_domestic_absval = tidyr::pivot_wider(plt_ag_import_vs_domestic , names_from = 'scenario', values_from = 'value') %>%
  dplyr::mutate('diff_protein' = Diets_Protein - Diets_Ref) %>%
  dplyr::mutate('diff_trade' = Diets_Trade.adj21 - Diets_Ref)
plt_ag_import_vs_domestic_absval = tidyr::pivot_longer(plt_ag_import_vs_domestic_absval %>% dplyr::select(region,year,sector,subsector,diff_protein,diff_trade),
                                                       cols = c('diff_protein','diff_trade')) %>%
  dplyr::rename('scenario' = 'name') %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))
plt_ag_import_vs_domestic_absval = plt_ag_import_vs_domestic_absval %>%
  dplyr::mutate('type' = stringr::str_extract(subsector, "\\w+"))

plt_ag_import_vs_domestic_p = tidyr::pivot_wider(plt_ag_import_vs_domestic , names_from = 'scenario', values_from = 'value') %>%
  dplyr::mutate('diff_protein' = Diets_Protein - Diets_Ref) %>%
  dplyr::mutate('diff_trade' = Diets_Trade.adj21 - Diets_Ref)
plt_ag_import_vs_domestic_p = tidyr::pivot_longer(plt_ag_import_vs_domestic_p %>% dplyr::select(region,year,sector,subsector,diff_protein,diff_trade),
                                                  cols = c('diff_protein','diff_trade')) %>%
  dplyr::rename('scenario' = 'name') %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))
plt_ag_import_vs_domestic_p = plt_ag_import_vs_domestic_p %>%
  dplyr::mutate('type' = stringr::str_extract(subsector, "\\w+"))

#####
# bars p
#####
sc = 'Protein'
for (ty in unique(plt_ag_import_vs_domestic_p$type)) {
  pl = plt_ag_import_vs_domestic_p %>% dplyr::filter(scenario == sc, year == selected_year, type == ty) %>%
    ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
    geom_bar(aes(x=reorder(region,value), y=value, fill=subsector),stat="identity",col='black',size=0.2) +
    geom_hline(yintercept = 0, linewidth = 1.5) +
    scale_fill_manual(values = c25) +
    labs(y=expression(paste("% Mt difference")),x="",title=paste(sc,ty))+
    theme_bw()+
    theme(
      strip.background = element_blank(),
      axis.text.x = element_text(size=8),
      axis.text.y = element_text(size=10),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 15),
      title = element_text(size = 15)
    ) +
    coord_flip()
  ggsave(pl, file = paste0(figures_path,"extra_figs/plt_food_supply",ty,'_p_by_reg_sector_bars_',sc,'.png'), width = 500, height = 300, units = 'mm')
}
#####
# bars absval
#####
sc = 'Protein'
for (ty in unique(plt_ag_import_vs_domestic_absval$type)) {
  pl = plt_ag_import_vs_domestic_absval %>% dplyr::filter(scenario == sc, year == selected_year, type == ty, region == 'Middle East') %>%
    ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
    geom_bar(aes(x=reorder(region,value), y=value, fill=subsector),stat="identity",col='black',size=0.2) +
    geom_hline(yintercept = 0, linewidth = 1.5) +
    scale_fill_manual(values = c25) +
    labs(y=expression(paste("Mt difference")),x="",title=paste(sc,ty))+
    theme_bw()+
    theme(
      strip.background = element_blank(),
      axis.text.x = element_text(size=8),
      axis.text.y = element_text(size=10),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 15),
      title = element_text(size = 15)
    ) +
    coord_flip()
  ggsave(pl, file = paste0(figures_path,'extra_figs/plt_food_supply',ty,'_absval_middleEast_sector_bars_',sc,'.png'), width = 500, height = 300, units = 'mm')
}

####
# maps
####
# merge dataset with world regions
plt_ag_import_vs_domestic_absval = plt_ag_import_vs_domestic_absval %>%
  dplyr::mutate('GCAM Region' = region)

world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
  dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM',adm0_a3)) %>%
  dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD',adm0_a3))
world <- subset(world,!adm0_a3 %in% c("ATA","FJI"))

plt_ag_import_vs_domestic_absval = merge(plt_ag_import_vs_domestic_absval, GCAM_reg, by = 'GCAM Region')

world <- merge(world,plt_ag_import_vs_domestic_absval, by.x = "adm0_a3", by.y = "ISO 3")

leg = 'Mt goods difference'

if (!dir.exists(paste0(figures_path,"extra_figs/plots_imports_vs_exports_Trade"))) dir.create(paste0(figures_path,"extra_figs/plots_imports_vs_exports_Trade"))

# plot
for (good in unique(world$sector)) {
  pl_ag_imports_vs_domestic_map <- ggplot() +
    # color map by regions
    geom_sf(data = world |> filter(sector == good, scenario == 'Trade'), aes(fill = value)) +
    scale_fill_gradient2(low = "#0DA800", high = "#C60000",
                         mid = '#C2DAC1', midpoint = 0,
                         name = leg) +
    facet_wrap(.~subsector) +
    # theme
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
          legend.text = element_text(size = 15), legend.title = element_text(size = 20),
          strip.text = element_text(size = 20, color = 'black'),
          strip.background =element_rect(fill="white"))
  ggsave(pl_ag_imports_vs_domestic_map, file = paste0(figures_path,'extra_figs/plots_maps_imports_vs_exports_Trade/pl_ag_imports_vs_domestic_map_',good,'.png'),
         width = 250, height = 150, units = 'mm')
}

world2$subsector = str_extract(world$subsector, "\\w+")

pl_ag_imports_vs_domestic_map <- ggplot() +
  # color map by regions
  geom_sf(data = world2 |> filter(scenario == 'Trade'), aes(fill = value)) +
  scale_fill_gradient2(low = "#0DA800", high = "#C60000",
                       mid = '#C2DAC1', midpoint = 0,
                       name = leg) +
  facet_grid(sector~subsector) +
  # theme
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
        legend.text = element_text(size = 15), legend.title = element_text(size = 20),
        strip.text = element_text(size = 20, color = 'black'),
        strip.background =element_rect(fill="white"))
ggsave(pl_ag_imports_vs_domestic_map, file = paste0(figures_path,'extra_figs/plots_maps_imports_vs_exports_Trade/pl_ag_imports_vs_domestic_map_ALL.pdf'),
       width = 1000, height = 2000, units = 'mm', limitsize = FALSE)

plt_ag_import_vs_domestic_absval = plt_ag_import_vs_domestic_absval %>%
  dplyr::mutate('type' = str_extract(subsector, "\\w+")) %>%
  dplyr::select(region,year,sector,subsector,scenario,value,type) %>%
  distinct(., .keep_all = FALSE)
pl_bars <- plt_ag_import_vs_domestic_absval %>% dplyr::filter(scenario == 'Trade', type == 'imported') %>%
  ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
  geom_bar(aes(x=reorder(region,value), y=value, fill=sector),stat="identity",col='black',size=0.2) +
  scale_fill_manual(values = c25) +
  geom_hline(yintercept = 0, linewidth = 1.5) +
  labs(y="MT",x="",title="Trade")+
  theme_bw()+
  theme(
    strip.background = element_blank(),
    axis.text.x = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 25),
    title = element_text(size = 30)
  ) +
  coord_flip()
ggsave(pl_bars, file = paste0(figures_path,'extra_figs/pl_ag_imports_vs_domestic_bars.png'), width = 600, height = 700, units = 'mm')





## -- food consumption (to understand what's going on with Middle East)
extr_food_consumption <- rgcam::getQuery(prj, "food consumption by type (specific)") %>%
  filter(scenario %in% selected_scen) %>%
  group_by(scenario, region, technology, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  filter(year >= year_s, year <= year_e)

extr_food_consumption_p_diff = tidyr::pivot_wider(extr_food_consumption , names_from = 'scenario', values_from = 'value') %>%
  dplyr::mutate('diff_protein' = 100*(Diets_Protein - Diets_Ref)/Diets_Ref) %>%
  dplyr::mutate('diff_trade' = 100*(Diets_Trade.adj21 - Diets_Ref)/Diets_Ref)
extr_food_consumption_p_diff = tidyr::pivot_longer(extr_food_consumption_p_diff %>% dplyr::select(region,year,technology,diff_protein,diff_trade),
                                                   cols = c('diff_protein','diff_trade')) %>%
  dplyr::rename('scenario' = 'name') %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))

extr_food_consumption_absval_diff = tidyr::pivot_wider(extr_food_consumption , names_from = 'scenario', values_from = 'value') %>%
  dplyr::mutate('diff_protein' = Diets_Protein - Diets_Ref) %>%
  dplyr::mutate('diff_trade' = Diets_Trade.adj21 - Diets_Ref)
extr_food_consumption_absval_diff = tidyr::pivot_longer(extr_food_consumption_absval_diff %>% dplyr::select(region,year,technology,diff_protein,diff_trade),
                                                        cols = c('diff_protein','diff_trade')) %>%
  dplyr::rename('scenario' = 'name') %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))


##############################################################
# time-line val by reg & sector
##############################################################
pl = extr_food_consumption %>% rename_scen() %>% dplyr::filter(scenario != 'Trade', region == 'China', technology != 'Wheat') %>%
  ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
  geom_line(aes(x=year, y=value, color=technology, linetype = scenario),linewidth=2) +
  scale_color_manual(values = c25) +
  scale_linetype_manual(values = c('Reference' = 'solid', 'Protein' = 'dotted')) +
  labs(y=expression(paste("Annual Water flows (billion ",m^3,")")),x="",title=sc)+
  theme_bw()+
  theme(
    strip.background = element_blank(),
    axis.text.x = element_text(size=8),
    axis.text.y = element_text(size=10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 15),
    title = element_text(size = 15)
  )
ggsave(pl, file = paste0(figures_path,'extra_figs/plt_food_consumption_annual_china_sector_',sc,'.png'), width = 500, height = 300, units = 'mm')


#######
# bars p
#######
sc = 'Protein'
pl = extr_food_consumption_p_diff %>% dplyr::filter(scenario == sc, year == selected_year) %>%
  ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
  geom_bar(aes(x=reorder(region,value), y=value, fill=technology),stat="identity",col='black',size=0.2) +
  geom_hline(yintercept = 0, linewidth = 1.5) +
  scale_fill_manual(values = c25) +
  labs(y=expression(paste("% Mt difference")),x="",title=sc)+
  theme_bw()+
  theme(
    strip.background = element_blank(),
    axis.text.x = element_text(size=8),
    axis.text.y = element_text(size=10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 15),
    title = element_text(size = 15)
  ) +
  coord_flip()
ggsave(pl, file = paste0(figures_path,'extra_figs/plt_food_consumption_p_by_reg_sector_bars_',sc,'.png'), width = 500, height = 300, units = 'mm')
#######
# bars absval
#######
sc = 'Protein'
pl = extr_food_consumption_absval_diff %>% dplyr::filter(scenario == sc, year == selected_year, region == 'Middle East') %>%
  ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
  geom_bar(aes(x=reorder(region,value), y=value, fill=technology),stat="identity",col='black',size=0.2) +
  geom_hline(yintercept = 0, linewidth = 1.5) +
  scale_fill_manual(values = c25) +
  labs(y=expression(paste("Mt difference")),x="",title=sc)+
  theme_bw()+
  theme(
    strip.background = element_blank(),
    axis.text.x = element_text(size=8),
    axis.text.y = element_text(size=10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 15),
    title = element_text(size = 15)
  ) +
  coord_flip()
ggsave(pl, file = paste0(figures_path,'extra_figs/plt_food_consumption_absval_middleEast_sector_bars_',sc,'.png'), width = 500, height = 300, units = 'mm')

for (reg in unique(extr_food_consumption$region)) {
  plt_food_consumption = ggplot(data = extr_food_consumption %>% rename_scen() %>% filter(region == reg, scenario != 'Protein'),
                                aes(x = year, y = value, color = scenario), alpha = 0.5) +
    geom_line(alpha = 0.5) +
    facet_wrap(. ~ technology, nrow = 3, scales = 'free') +
    scale_color_manual(values = mypal_scen) +
    ggtitle(paste0('food consumption ',reg))
  ggsave(plt_food_consumption, file = paste0(figures_path,'extra_figs/plots_food_consumption_Trade/plt_food_consumption_',reg,'.png'), width = 250, height = 150, units = 'mm')
}





## -- prices (imports & domestic)
extr_ag_prices <- rgcam::getQuery(prj, "ag import vs. domestic prices") %>%
  filter(scenario %in% selected_scen) %>%
  group_by(scenario, region, sector, subsector, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  filter(year >= year_s, year <= year_e) %>%
  mutate('type' = stringr::str_extract(subsector, "\\w+"))
extr_ag_prices = tidyr::pivot_wider(extr_ag_prices , names_from = 'scenario', values_from = 'value') %>%
  dplyr::mutate('diff_protein' = Diets_Protein - Diets_Ref) %>%
  dplyr::mutate('diff_trade' = Diets_Trade.adj21 - Diets_Ref)
extr_ag_prices = tidyr::pivot_longer(extr_ag_prices %>% dplyr::select(region,year,sector,subsector,type,diff_protein,diff_trade),
                                     cols = c('diff_protein','diff_trade')) %>%
  dplyr::rename('scenario' = 'name') %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))
for (ty in unique(extr_ag_prices$type)) {
  pl = extr_ag_prices %>% dplyr::filter(scenario == sc, year == selected_year, type == ty) %>%
    ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
    geom_bar(aes(x=reorder(region,value), y=value, fill=subsector),stat="identity",col='black',size=0.2) +
    geom_hline(yintercept = 0, linewidth = 1.5) +
    scale_fill_manual(values = c25) +
    labs(y=expression(paste("$/kg difference")),x="",title=paste(sc,ty))+
    theme_bw()+
    theme(
      strip.background = element_blank(),
      axis.text.x = element_text(size=8),
      axis.text.y = element_text(size=10),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 15),
      title = element_text(size = 15)
    ) +
    coord_flip()
  ggsave(pl, file = paste0(figures_path,'extra_figs/plt_ag_prices_',ty,'_absval_by_reg_sector_bars_',sc,'.png'), width = 500, height = 300, units = 'mm')
}





## -- prices (average between imports and domestic)
extr_ag_regional_prices <- rgcam::getQuery(prj, "ag regional prices (weighted average b/t domestic and imported prices)") %>%
  filter(scenario %in% selected_scen) %>%
  group_by(scenario, region, sector, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  filter(year >= year_s, year <= year_e)
extr_ag_regional_prices = tidyr::pivot_wider(extr_ag_regional_prices , names_from = 'scenario', values_from = 'value') %>%
  dplyr::mutate('diff_protein' = Diets_Protein - Diets_Ref) %>%
  dplyr::mutate('diff_trade' = Diets_Trade.adj21 - Diets_Ref)
extr_ag_regional_prices = tidyr::pivot_longer(extr_ag_regional_prices %>% dplyr::select(region,year,sector,diff_protein,diff_trade),
                                              cols = c('diff_protein','diff_trade')) %>%
  dplyr::rename('scenario' = 'name') %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))
pl = extr_ag_regional_prices %>% dplyr::filter(scenario == sc, year == selected_year) %>%
  ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
  geom_bar(aes(x=reorder(region,value), y=value, fill=sector),stat="identity",col='black',size=0.2) +
  geom_hline(yintercept = 0, linewidth = 1.5) +
  scale_fill_manual(values = c25) +
  labs(y=expression(paste("$/kg difference")),x="",title=sc)+
  theme_bw()+
  theme(
    strip.background = element_blank(),
    axis.text.x = element_text(size=8),
    axis.text.y = element_text(size=10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 15),
    title = element_text(size = 15)
  ) +
  coord_flip()
ggsave(pl, file = paste0(figures_path,'extra_figs/plt_ag_regional_prices_absval_by_reg_sector_bars_',sc,'.png'), width = 500, height = 300, units = 'mm')






## -- prices (meet and dairy)
extr_ag_meet_dairy_prices <- rgcam::getQuery(prj, "meat and dairy prices") %>%
  filter(scenario %in% selected_scen) %>%
  group_by(scenario, region, sector, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  filter(year >= year_s, year <= year_e)
extr_ag_meet_dairy_prices = tidyr::pivot_wider(extr_ag_meet_dairy_prices , names_from = 'scenario', values_from = 'value') %>%
  dplyr::mutate('diff_protein' = Diets_Protein - Diets_Ref) %>%
  dplyr::mutate('diff_trade' = Diets_Trade.adj21 - Diets_Ref)
extr_ag_meet_dairy_prices = tidyr::pivot_longer(extr_ag_meet_dairy_prices %>% dplyr::select(region,year,sector,diff_protein,diff_trade),
                                                cols = c('diff_protein','diff_trade')) %>%
  dplyr::rename('scenario' = 'name') %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))
pl = extr_ag_meet_dairy_prices %>% dplyr::filter(scenario == sc, year == selected_year) %>%
  ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
  geom_bar(aes(x=reorder(region,value), y=value, fill=sector),stat="identity",col='black',size=0.2) +
  geom_hline(yintercept = 0, linewidth = 1.5) +
  scale_fill_manual(values = c25) +
  labs(y=expression(paste("$/kg difference")),x="",title=sc)+
  theme_bw()+
  theme(
    strip.background = element_blank(),
    axis.text.x = element_text(size=8),
    axis.text.y = element_text(size=10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 15),
    title = element_text(size = 15)
  ) +
  coord_flip()
ggsave(pl, file = paste0(figures_path,'extra_figs/plt_extr_ag_meet_dairy_prices_absval_by_reg_sector_bars_',sc,'.png'), width = 500, height = 300, units = 'mm')






##### -- extra water plots

plt_water_withdrawals = ggplot(data = water_withdrawals %>% rename_scen(),
                               aes(x = year, y = value, color = scenario), alpha = 0.5) +
  geom_line(alpha = 0.5) +
  scale_color_manual(values = mypal_scen) +
  ggtitle('water withdrawals')
plt_water_withdrawals
ggsave(plt_water_withdrawals, paste0(figures_path,'extra_figs/plt_water_withdrawals.png'), width = 300, height = 150, units = 'mm')

plt_water_consumption = ggplot(data = water_consumption %>% rename_scen(),
                               aes(x = year, y = value, color = scenario), alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(. ~ sector, nrow = 3) +
  scale_color_manual(values = mypal_scen) +
  ggtitle('water consumption')
plt_water_consumption
ggsave(plt_water_consumption, paste0(figures_path,'extra_figs/plt_water_consumption.png'), width = 300, height = 150, units = 'mm')

plt_water_consumption_total = ggplot(data = water_consumption_total  %>% rename_scen(),
                                     aes(x = year, y = value, color = scenario), alpha = 0.5) +
  geom_line(alpha = 0.5) +
  scale_color_manual(values = mypal_scen) +
  ggtitle('water consumption total')
plt_water_consumption_total
ggsave(plt_water_consumption_total, paste0(figures_path,'extra_figs/plt_water_consumption_total.png'), width = 150, height = 150, units = 'mm')



food_sector = c('Beef','Corn','Dairy','FiberCrop','FodderHerb','Fruits','Legumes',
                'MiscCrop','NutsSeeds','OilCrop','Pork','Poultry','Rice','RootTuber',
                'SheepGoat','Soybean','SugarCrop','Vegetables','Wheat','OilPalm',
                'FodderGrass')

water_consumption_by_reg_diff = water_consumption_by_reg %>%
  dplyr::filter(year == selected_year, sector %in% food_sector) %>%
  group_by(year,scenario,region) %>%
  summarise(value = sum(value)) %>%
  ungroup()
water_consumption_by_reg_diff = tidyr::pivot_wider(water_consumption_by_reg_diff , names_from = 'scenario', values_from = 'value') %>%
  dplyr::mutate('diff_protein' = 100*(Diets_Protein - Diets_Ref)/Diets_Ref) %>%
  dplyr::mutate('diff_trade' = 100*(Diets_Trade.adj21 - Diets_Ref)/Diets_Ref)
water_consumption_by_reg_diff = tidyr::pivot_longer(water_consumption_by_reg_diff %>% dplyr::select(region,year,diff_protein,diff_trade),
                                                    cols = c('diff_protein','diff_trade')) %>%
  dplyr::rename('scenario' = 'name') %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))


water_consumption_by_reg_sector_diff = water_consumption_by_reg %>%
  dplyr::filter(year == selected_year, sector %in% food_sector) %>%
  group_by(year,scenario,sector,region) %>%
  summarise(value = sum(value)) %>%
  ungroup()
water_consumption_by_reg_sector_diff = tidyr::pivot_wider(water_consumption_by_reg_sector_diff , names_from = 'scenario', values_from = 'value') %>%
  dplyr::mutate('diff_protein' = 100*(Diets_Protein - Diets_Ref)/Diets_Ref) %>%
  dplyr::mutate('diff_trade' = 100*(Diets_Trade.adj21 - Diets_Ref)/Diets_Ref)
water_consumption_by_reg_sector_diff = tidyr::pivot_longer(water_consumption_by_reg_sector_diff %>% dplyr::select(region,year,sector,diff_protein,diff_trade),
                                                           cols = c('diff_protein','diff_trade')) %>%
  dplyr::rename('scenario' = 'name') %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
  dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))

#########################################################################
# map
#########################################################################
pl_mort_diff = water_consumption_by_reg_diff %>%
  dplyr::filter(year == selected_year) %>%
  dplyr::mutate('GCAM Region' = region)

# merge dataset with world regions
world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
  dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM',adm0_a3)) %>%
  dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD',adm0_a3))
world <- subset(world,!adm0_a3 %in% c("ATA","FJI"))

pl_mort_diff = merge(pl_mort_diff, GCAM_reg, by = 'GCAM Region')

world <- merge(world,pl_mort_diff, by.x = "adm0_a3", by.y = "ISO 3")

leg = 'Water consumption percentual difference'

# plot
pl = ggplot() +
  # color map by regions
  geom_sf(data = world, aes(fill = value)) +
  scale_fill_gradient2(low = "#0DA800", high = "#C60000",
                       mid = '#C2DAC1', midpoint = 0,
                       name = leg) +
  facet_wrap(.~scenario) +
  # theme
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
        legend.text = element_text(size = 15), legend.title = element_text(size = 20),
        strip.text = element_text(size = 20, color = 'black'),
        strip.background =element_rect(fill="white"))
pl
ggsave(pl, file = paste0(figures_path,'extra_figs/plt_water_consumption_by_reg_map.png'), width = 250, height = 150, units = 'mm')


##############################################################
# line
##############################################################


for (sc in c('Trade','Protein')) {
  pl = water_consumption_by_reg_sector_diff %>% dplyr::filter(scenario == sc) %>%
    ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
    geom_bar(aes(x=reorder(region,value), y=value, fill=sector),stat="identity",col='black',size=0.2) +
    geom_hline(yintercept = 0, linewidth = 1.5) +
    labs(y="%km^3",x="",title=sc)+
    theme_bw()+
    theme(
      strip.background = element_blank(),
      axis.text.x = element_text(size=8),
      axis.text.y = element_text(size=10),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 15),
      title = element_text(size = 15)
    ) +
    coord_flip()
  ggsave(pl, file = paste0(figures_path,'extra_figs/plt_water_consumption_by_reg_sector_line',sc,'.png'), width = 250, height = 150, units = 'mm')
}
for (sc in c('Trade','Protein')) {
  pl = water_consumption_by_reg_diff %>% dplyr::filter(scenario == sc) %>%
    ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
    geom_bar(aes(x=reorder(region,value), y=value, fill=scenario),stat="identity",col='black',size=0.2) +
    scale_fill_manual(values = mypal_scen, name = 'Scenario') +
    geom_hline(yintercept = 0, linewidth = 1.5) +
    labs(y="%km^3",x="",title=sc)+
    theme_bw()+
    theme(
      strip.background = element_blank(),
      axis.text.x = element_text(size=8),
      axis.text.y = element_text(size=10),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 15),
      title = element_text(size = 15)
    ) +
    coord_flip()
  ggsave(pl, file = paste0(figures_path,'extra_figs/plt_water_consumption_by_reg_line',sc,'.png'), width = 250, height = 150, units = 'mm')
}
pl = water_consumption_by_reg_diff %>%
  ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
  geom_bar(aes(x=reorder(region,value), y=value, fill=scenario),stat="identity",col='black',size=0.2) +
  scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  geom_hline(yintercept = 0, linewidth = 1.5) +
  labs(y="%km^3",x="",title='')+
  theme_bw()+
  theme(
    strip.background = element_blank(),
    axis.text.x = element_text(size=8),
    axis.text.y = element_text(size=10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 15),
    title = element_text(size = 15)
  ) +
  coord_flip()
ggsave(pl, file = paste0(figures_path,'extra_figs/plt_water_consumption_by_reg_line_all.png'), width = 250, height = 150, units = 'mm')
