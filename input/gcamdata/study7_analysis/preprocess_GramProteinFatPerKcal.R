
################################################################
################################################################
## PRESCRIPT

# Code in this script can be run based on the inputs in the
# module_aglu_L100.FAO_SUA_connection

devtools::load_all()
inputs_of("module_aglu_L100.FAO_SUA_connection") %>% load_from_cache() -> all_data

# We have protein and fat data here but not used in GCAM (dropped here)
DF_Macronutrient_FoodItem2 %>%
  tidyr::gather(macronutrient, value, calperg:proteinperc) %>%
  # Join max regional conversion for adjustments later
  left_join(
    FAO_Food_MacronutrientRate_2010_2019_MaxValue,
    by = c("GCAM_commodity", "macronutrient")
  ) %>%
  # In rare cases, primary equivalent resulted in lower food mass consumption
  # mainly due to a higher-than-one-extraction rate, e.g., beer of barley
  # or small discrepancies (or possibly representing real processing)
  # thus, the cal per g conversion is larger than the max of the conversion
  # of the corresponding SUA items
  # I.e., a few OtherGrain cases (e.g., Indonesia) and a Mexico soybean case;
  # they all have relatively small consumption/impacts
  # But we use the max of the conversion of the corresponding SUA items to limit the value here
  # mainly for avoiding too different macronutrient rates across regions
  mutate(value = pmin(value, max_macronutrient_value)) %>%
  select(-max_macronutrient_value) %>%
  # There are sill NA values e.g., palm oil is not consumed in Canada
  # And fiber crop is not consumed in few regions
  # Fill in NA with world mean
  dplyr::group_by_at(vars(-GCAM_region_ID, -value)) %>%
  mutate(value = if_else(is.na(value), mean(value, na.rm = T), value)) %>%
  ungroup() %>%
  #filter(macronutrient == "calperg") %>%
  spread(macronutrient, value) ->
  DF_Macronutrient_FoodItem3_calperg



DF_Macronutrient_FoodItem4 <-
  L100.FAO_SUA_APE_balance %>% filter(element == "Food") %>%
  filter(year %in% 2015) %>%
  rename(Mt = value) %>%
  left_join_error_no_match(DF_Macronutrient_FoodItem3_calperg,
                           by = c("GCAM_commodity", "GCAM_region_ID")) %>%
  mutate(Kcalperg = calperg / 1000,
         MKcal =  Kcalperg * Mt * 1000) %>%
  select(-calperg) %>%
  mutate(gProteinPerKcal = proteinperc / Kcalperg,
         gFatPerKcal = fatperc / Kcalperg)


DF_Macronutrient_FoodItem4 %>%
  readr::write_csv(paste0(inputs_path,"nutrition/GramProteinFatPerKcal.csv"))





## =========== Micronutrients =================

raw_data <- read.csv(paste0(inputs_path,"nutrition/USDA_data_final.csv"))
rni_data <- read.csv(paste0(inputs_path,"nutrition/rni.csv"))

# Clean up column names
colnames(raw_data) <- c("Food", "GCAM_commodity", "Calories (kcal)", "Protein (g)",
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

# Average over individual food items to get a representative value for each commodity
average_data <- raw_data %>%
  filter(`GCAM_commodity` != "") %>%
  pivot_longer(cols = 3:67, names_to = "Nutrient") %>%
  group_by(`GCAM_commodity`, Nutrient) %>%
  summarize(average = median(value)) %>%
  filter_all(all_vars(!stringr::str_detect(., ":"))) %>%
  pivot_wider(names_from = Nutrient, values_from = average) %>%
  pivot_longer(-c('GCAM_commodity','Calories (kcal)'), names_to = 'nutrient', values_to = 'nutrient_value') %>%
  mutate(nutrient_value = nutrient_value/`Calories (kcal)`) %>%
  mutate(
    nutrient_name = stringr::str_split(nutrient, " \\(") %>%
      sapply(function(x) x[1]),
    nutrient_units = stringr::str_split(nutrient, " \\(") %>%
      sapply(function(x) sub("\\)$", "", x[2])),
    nutrient_units = paste0(nutrient_units,'/kcal')
  ) %>%
  select(-c(`Calories (kcal)`,nutrient))


# Total micronutrients consumption
micronutrients_consumption = left_join(dt$food_consumption_regional %>%
                                         # TODO: find data of nutritional values of FiberCrop (introduce it in the average_data)
                                         filter(technology != 'FiberCrop') %>%
                                         left_join(dt$pop_all_regions, by = c("year", "scenario", "t0", "k", "scen_type", "region")) %>%
                                         # convert from Pcal to kcal/day
                                         mutate(value = (value * 1e12) / (population * 365),
                                                Units = "kcal/capita/day") %>%
                                         # rename columns
                                         rename('GCAM_commodity' = 'technology',
                                                'consumption' = 'value'),
                                       average_data,
                                       by = 'GCAM_commodity') %>%
  mutate('total_micronutrient_intake' = consumption * nutrient_value) %>%
  group_by(region,scenario,year,nutrient_name,nutrient_units) %>%
  summarise(total_micronutrient_intake = sum(total_micronutrient_intake, na.rm = TRUE)) %>%
  mutate(nutrient_units = stringr::str_replace(nutrient_units, "/kcal", "/capita/day")) %>%
  mutate(year = as.numeric(as.character(year)))


micronutrients_RNI = merge(rni_data %>%
                             rename('nutrient_name' = 'micronutrient',
                                    'units_rni' = 'Units') %>%
                             mutate(nutrient_name = tolower(nutrient_name)) %>%
                             select(-data_source),
                           weighted_pop_sex_age,
                           by = 'variable') %>%
  mutate(bySocioGroup_rni = as.numeric(mean_requirement * pop_sex_age)) %>%
  group_by(nutrient_name,units_rni,year,region) %>%
  summarise(byReg_rni = sum(bySocioGroup_rni),
            pop = sum(pop_sex_age),
            byRegC_rni = byReg_rni/pop) %>%
  mutate(units_rni = stringr::str_replace(units_rni, "/day", "/capita/day")) %>%
  mutate(year = as.numeric(as.character(year)))



micronutrients = merge(micronutrients_RNI %>%
                         mutate(nutrient_name = tolower(nutrient_name)),
                       micronutrients_consumption %>%
                         mutate(nutrient_name = tolower(nutrient_name)),
                       by = c('region','year','nutrient_name'))

write.csv(micronutrients, file = paste0(inputs_path,'nutrition/micronutrients_computed.csv'))


##############################################################
##############################################################
### POSTSCRIPT
GramProteinFatPerKcal = readr::read_csv(paste0(inputs_path,"nutrition/GramProteinFatPerKcal.csv"))


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
                                          median_value = median(value))) +
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
ggsave(pl_macronutrients_world, file = paste0(figures_path,dir_name,'/pl4_macronutrients_world.png'),
       width = 750, height = 450, units = 'mm')

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
                                             median_value = median(value))) +
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
ggsave(pl_macronutrients_regional, file = paste0(figures_path,dir_name,'/pl4_macronutrients_regional_fixedScale.png'),
       width = 1000, height = 1000, units = 'mm', limitsize = F)


pl_macronutrients_regional = ggplot(data = macronutrients_basic %>%
                                      mutate(scen_type = substr(scen_type, 1, 3)) %>%
                                      group_by(scenario, scen_type, year, region) %>%
                                      summarise(gProteinPerCapita = median(gProteinPerCapita),
                                                gFatPerCapita = median(gFatPerCapita)) %>%
                                      tidyr::pivot_longer(cols = gProteinPerCapita:gFatPerCapita, names_to = 'macronutrient') %>%
                                      group_by(scen_type, year, macronutrient, region) %>%
                                      mutate(min_value = min(value),
                                             max_value = max(value),
                                             median_value = median(value))) +
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
ggsave(pl_macronutrients_regional, file = paste0(figures_path,dir_name,'/pl4_macronutrients_regional_freeScale.png'),
       width = 1000, height = 1000, units = 'mm', limitsize = F)




###### micronutrients ===================

micronutrients = read.csv(file = paste0(inputs_path,'nutrition/micronutrients_computed.csv')) %>%
  select(-1)

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
  geom_bar(data = micronutrients_diffPer_regional |> filter(scen_type == 'spp'),
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
ggsave(pl_micronutrients_diffPer_regional_bars, file = paste0(figures_path,dir_name,'/pl4_micronutrients_diffPer_SPP_regional_bars.png'),
       width = 1000, height = 1000, units = 'mm')

pl_micronutrients_diffPer_regional_bars <- ggplot() +
  # barchart
  geom_bar(data = micronutrients_diffPer_regional |> filter(scen_type == 'snr'),
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
ggsave(pl_micronutrients_diffPer_regional_bars, file = paste0(figures_path,'tmp_figs/pl4_micronutrients_diffPer_SNR_regional_bars.png'),
       width = 1000, height = 1000, units = 'mm')


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
  geom_bar(data = micronutrients_diffPer_world,
           aes(x = as.factor(nutrient_name), y = median_value, fill = as.factor(nutrient_name)),
           stat = "identity", color = NA, width = 0.5) +
  scale_fill_manual(values = c25, name = '') +
  facet_wrap(. ~ toupper(scen_type)) +
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
ggsave(pl_micronutrients_diffPer_world, file = paste0(figures_path,dir_name,'/pl4_micronutrients_diffPer_betweenScen_world_bars.png'),
       width = 750, height = 450, units = 'mm')


## =========== Average dietary energy supply adequacy (ADESA) =================

total_regional_calories <- food_consumption_regional %>%
  group_by(scenario, region, year) %>%
  # aggregate all commodities
  summarize(value = sum(value)) %>%
  left_join(pop_all_regions, by = c("year", "scenario", "region")) %>%
  # convert from Pcal to kcal/day
  mutate(value = (value * 1e12) / (population * 365),
         Units = "kcal/capita/day") %>%
  group_by(Units,region,scenario,year,population) %>%
  summarise(value = sum(value))

# Get population by sex and age
# Population weighting
total_regional_pop <- ssp_data_final %>%
  filter(year >= year_s, year <= year_e) %>%
  select(-scenario, -iso) %>%
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
  filter(year >= year_s, year <= year_e) %>%
  select(-scenario, -iso) %>%
  # get GCAM regions instead of country names
  left_join(regions_key, by = "country_name") %>%
  # get total regional population
  left_join(total_regional_pop) %>%
  # weight each country by its population over total regional pop
  group_by(country_name, year) %>%
  mutate(weight = total_pop / total_regional_pop) %>%
  # get GCAM population
  left_join(mutate(pop_all_regions, year = as.character(year)), by = c("region", "year"),
            multiple = "all") %>%
  # compute GCAM population by sex and age for each country
  mutate(weighted_demographics = demo_share * weight * population)

weighted_pop_sex_age <- weighted_pop %>%
  select(-pop_units, -sub_pop, -total_pop, -demo_share, -weight) %>%
  group_by(variable, year, region) %>%
  # sum the weighted averages for each country into GCAM regions
  summarize(pop_sex_age = sum(weighted_demographics))

# join with MDER data, calculate caloric requirements by sex and age
adesa_denominator <- weighted_pop_sex_age %>%
  left_join(mder, by = "variable") %>%
  select(-std) %>%
  group_by(variable, year, region, mder_units) %>%
  # compute a range because of differing physical activity levels
  summarize(cal_req_x_pop = mder * pop_sex_age,
            min_cal_req_x_pop = min * pop_sex_age,
            max_cal_req_x_pop = max * pop_sex_age) %>%
  # aggregate caloric requirements to get total regional values
  group_by(region, year, mder_units) %>%
  summarize(denominator_sum = sum(cal_req_x_pop),
            min_denominator_sum = sum(min_cal_req_x_pop),
            max_denominator_sum = sum(max_cal_req_x_pop)) %>%
  mutate(year = as.numeric(year))

# add in regional calorie info, calculate ADESA
adesa <- left_join(adesa_denominator, total_regional_calories) %>%
  # select(-population) %>%
  group_by(year, region, scenario) %>%
  reframe(adesa = (value / denominator_sum) * population * 100, # convert to unitless and percentage
          min_adesa = (value / min_denominator_sum) * population * 100,
          max_adesa = (value / max_denominator_sum) * population * 100)
