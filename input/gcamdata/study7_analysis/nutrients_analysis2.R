#### NUTRITIONAL VALUES ========================================================
#### Basic data ================================================================
# ==============================================================================

if(!dir.exists(paste0(figures_path,dir_name))) dir.create(paste0(figures_path,dir_name))
if(!dir.exists(paste0(figures_path,dir_name,"/macronutrients/"))) dir.create(paste0(figures_path,dir_name,"/macronutrients/"))


# Read in MDER (calculated exogenously, FAO data)
mder <- read.csv(paste0(inputs_path,"nutrition/MDER.csv")) %>%
  rename(mder_units = unit) %>%
  mutate(mder_units = 'kcal/capita/day')
# Macronutrients: this comes from module_aglu_L100.FAO_SUA_connection
gcamdata_macro <- read.csv(paste0(inputs_path,"nutrition/gcamdata_macronutrient.csv"))
gcamdata_macro[is.na(gcamdata_macro)] <- 0

#####
#### Dietary energy supply =====================================================
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

## Average dietary supply adequacy =============================================

# We need to isolate population by sex and age for each region
# First, we need to take the SSP data and process
# Read in SSP data, remove unneeded columns, join with country identifiers
ssp_data_clean <- iso_gcam_regions %>%
  select(-region_GCAM3, -GCAM_region_ID) %>%
  left_join(ssp_data, by = "iso", multiple = 'all') %>%
  select(-MODEL, -REGION) %>%
  rename(scenario = SCENARIO,
         variable = VARIABLE,
         unit = UNIT)
# Remove X from year columns
colnames(ssp_data_clean) <- gsub("X", "", colnames(ssp_data_clean))
# Pivot longer
ssp_data_long <- ssp_data_clean %>%
  pivot_longer(cols = 6:24, names_to = "year", values_to = "value") %>%
  mutate(value = value * 1e6,
         unit = "total population")
# Isolate reference (total) population
reference_pop <- ssp_data_long %>%
  filter(variable == "Population") %>%
  rename(total_pop = value) %>%
  select(iso, year, total_pop)
# Join and calculate demographic shares of population
ssp_data_final <- ssp_data_long %>%
  filter(variable != "Population") %>%
  left_join(reference_pop, by = c("iso", "year")) %>%
  mutate(demo_share = value / total_pop) %>%
  rename(sub_pop = value)  %>%
  # Remove total male and total female pop, we want by age/sex
  filter(!(variable %in% c("Population|Male", "Population|Female"))) %>%
  rename(pop_units = unit)



### Calculate average dietary supply adequacy ==================================
# total regional calories / SUM_age,sex(calreq_a,s * pop_a,s)
total_regional_calories <- dietary_energy_supply

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
  left_join(mutate(dt$pop_all_regions, year = as.character(year)), by = c("region", "year"),
            multiple = "all") %>%
  # compute GCAM population by sex and age for each country
  mutate(weighted_demographics = demo_share * weight * population)

weighted_pop_sex_age <- weighted_pop %>%
  select(-pop_units, -sub_pop, -total_pop, -demo_share, -weight) %>%
  group_by(scenario, t0, k, scen_type, variable, year, region) %>%
  # sum the weighted averages for each country into GCAM regions
  summarize(pop_sex_age = sum(weighted_demographics))

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
              ungroup())+
  geom_line(aes(x = year, y = mean_adesa, color = scen_type), linewidth = 2, alpha = 1) +  # Median line
  geom_line(aes(x = year, y = adesa, group = scenario, color = scen_type), linewidth = 1, alpha = 0.8) +  # Median line
  scale_color_manual(values = scen_palette_refVsSppVsSnr, labels = scen_palette_refVsSppVsSnr.labs, name = 'Scenario') +
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
ggsave(pl, filename = paste0(figures_path,dir_name,"/",'pl_adesa_world2.pdf'),
       width = 750, height = 450, units = 'mm', limitsize = F)

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
              ungroup())+
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
ggsave(pl, file = paste0(figures_path,dir_name,"/",'pl_adesa_reg_freeS.pdf'),
       width = 2000, height = 2000, units = 'mm', limitsize = F)
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
              ungroup())+
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
ggsave(pl, file = paste0(figures_path,dir_name,"/",'pl_adesa_reg_fixedS.pdf'),
       width = 2000, height = 2000, units = 'mm', limitsize = F)


## Macronutrients ==============================================================
# Figures from preprocess_GramProteinFatPerKcal unitl line 487

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









# TODO 3/11/2023: order the code from here. Check with preprocess_GramProteinFatPerKcal and preprocess_nutrients.R
## Macronutrients ==============================================================

# Note from Xin
# E.g., beef has a protein of 14% which is 14 g protein per 100 g of beef;
# the Kcal per g is 1.47 Kcal per g of beef, that is 147 Kcal per 100 g
# If you calculate the ratio, you get 14 g / 147 Kcal for beef.
# That is, 0.095 g protein per Kcal of beef.

macronutrients <- gcamdata_macro %>%
  select(-1) %>%
  mutate(calperg = calperg / 1000) %>% # convert to kcal
  rename(kcalperg = calperg,
         protein_per_100g = proteinperc,
         fat_per_100g = fatperc,
         technology = GCAM_commodity) %>%
  mutate(kcal_per_100g = kcalperg * 100,
         grams_fat_per_kcal_comm = fat_per_100g / kcal_per_100g, # units of g (fat) per kcal (comm)
         grams_protein_per_kcal_comm = protein_per_100g / kcal_per_100g) %>% # units of g (protein) per kcal (comm)
  format(scientific = F)

macronutrients$GCAM_region_ID <- as.numeric(macronutrients$GCAM_region_ID)

# Break out consumption by commodity by GCAM consumer
# 1. Get share of individual commodities over staple or non-staple cals
# 2. Multiply shares of commodities by staple or non-staple calories by consumer

consumption_cals <- dt$food_consumption_regional %>%
  select(-starts_with('nestingSector')) %>%
  left_join(dt$staples_nonstaples_regional,
            by = c("scenario", "t0", "k", "scen_type", "region", "year"))

# Share of staple commodities in staple calories (and total)
consumption_shares_staples <- consumption_cals %>%
  filter(technology %in% staples) %>%
  select(-NonStaples) %>%
  mutate(share_of_comm = value / Staples, # Share of staple commodity in staple calories
         share_of_comm_total = value / Total) %>% # Share of staple commodity in total calories
  select(-Total, -Staples, -value, -Units.x) %>%
  rename(Units = Units.y)

# Share of non-staple commodities in non-staple calories (and total)
consumption_shares_nonstaples <- consumption_cals %>%
  filter(!(technology %in% staples)) %>%
  select(-Staples) %>%
  mutate(share_of_comm = value / NonStaples,
         share_of_comm_total = value / Total) %>%
  select(-Total, -NonStaples, -value, -Units.x) %>%
  rename(Units = Units.y)

# Get calorie information
consumption_by_comm_deciles_staples <- share_diet_staples_region %>%
  left_join(consumption_shares_staples, by = c("region", "year", "scenario", "t0", "k", "scen_type", "Units"), multiple = 'all') %>%
  mutate(commodity_cals = Staples * share_of_comm)

consumption_by_comm_deciles_nonstaples <- share_diet_staples_region %>%
  left_join(consumption_shares_nonstaples, by = c("region", "year", "scenario", "t0", "k", "scen_type", "Units"), multiple = 'all') %>%
  mutate(commodity_cals = NonStaples * share_of_comm)

# Combine staples and non-staples information
consumption_by_commodity_decile <- bind_rows(consumption_by_comm_deciles_staples,
                                             consumption_by_comm_deciles_nonstaples)

# Calculate macronutrient information
consum_macronutrient <- consumption_by_commodity_decile %>%
  # Add in GCAM region ID
  left_join(id_gcam_regions, by = "region") %>%
  # Add population by region
  left_join(dt$pop_all_regions, by = c("scenario", "t0", "k", "scen_type", "region", "year")) %>%
  # Add regional macronutrient data
  left_join(macronutrients %>%
              mutate(GCAM_region_ID = as.integer(GCAM_region_ID)),
            by = c("GCAM_region_ID", "technology")) %>%
  rename(commodity = technology)

consum_macronutrient$fat_per_100g <- as.numeric(consum_macronutrient$fat_per_100g)
consum_macronutrient$protein_per_100g <- as.numeric(consum_macronutrient$protein_per_100g)

### Avg protein supply =========================================================
# SUM_commodity(protein/kg_c * consumption_c) / population
protein_supply_by_commodity <- consum_macronutrient %>%
  select(1:7, 14:19, 21, 22, grams_protein_per_kcal_comm) %>%
  filter(protein_per_100g != 0) %>%
  mutate(commodity_cals = commodity_cals * 1e12, # convert Pcal to kcal
         Units = "kcal") %>%
  group_by(scenario, region, year, commodity) %>%
  reframe(numerator = (as.numeric(grams_protein_per_kcal_comm) * commodity_cals), # protein (g per kcal) * calories
          denominator = population,
          protein_supply = numerator / (denominator * 365), # convert year to day
          units = "g/capita/day",
          data = "FAO") %>%
  select(-numerator, -denominator)

# Aggregate each commodity to get total regional protein supply
protein_supply <- protein_supply_by_commodity %>%
  group_by(scenario, region, year) %>%
  # Sum protein supply by commodity to get total protein supply for each region
  reframe(protein_agg = sum(protein_supply),
          units = units) %>%
  distinct() %>%
  mutate(year = as.character(year))


# Get SingleConsumer info - same process as above, but for one consumer
protein_oneconsumer <- consumption_by_commodity_decile %>%
  # Add in GCAM region ID
  left_join(id_gcam_regions, by = "region") %>%
  # Add population by region
  left_join(dt$pop_all_regions, by = c("scenario", "t0", "k", "scen_type", "region", "year")) %>%
  # Add regional macronutrient data
  left_join(macronutrients %>%
              mutate(GCAM_region_ID = as.integer(GCAM_region_ID)),
            by = c("GCAM_region_ID", "technology")) %>%
  rename(commodity = technology) %>%
  select(1:7, 10, 14:19, 21, 22, grams_protein_per_kcal_comm) %>%
  mutate(protein_per_100g = as.numeric(protein_per_100g)) %>%
  filter(protein_per_100g != 0) %>%
  mutate(commodity_cals = commodity_cals * 1e12, # convert Pcal to kcal
         Units = "kcal") %>%
  group_by(scenario, t0, k, scen_type, region, year, commodity) %>%
  reframe(numerator = (as.numeric(grams_protein_per_kcal_comm) * commodity_cals), # protein (g per kcal) * calories
          denominator = population,
          protein_supply = numerator / (denominator * 365), # convert year to day
          units = "g/capita/day") %>%
  select(-numerator, -denominator) %>% distinct() %>%
  group_by(scenario, t0, k, scen_type, region, year) %>%
  # Sum protein supply by commodity to get total protein supply for each region
  reframe(value = sum(protein_supply),
          units = units,
          data = "FAO") %>% distinct()



pl = ggplot(protein_oneconsumer %>% filter(scenario %in% selected_scen)) +
  geom_line(aes(year, value, group = scenario, color = scen_type)) +
  scale_color_manual(values = scen_palette_refVsAllSpp) +
  facet_wrap(. ~ region) +
  labs(title = 'Total protein', y = '[g/capita/day]', x = 'year') +
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
ggsave(pl, file = paste0(figures_path,dir_name,"/macronutrients/",'protein_oneconsumer.png'), width = 1000, height = 1000, units = 'mm')


pl = ggplot(protein_oneconsumer %>% filter(scenario %in% selected_scen) %>%
              group_by(scen_type, year, units, data) %>%
              mutate(mean_value = median(value)) %>%
              group_by(scenario, year, units, data) %>%
              mutate(value = median(value))) +
  geom_line(aes(year, mean_value, color = scen_type), linewidth = 2) +
  geom_line(aes(year, value, group = scenario, color = scen_type)) +
  scale_color_manual(values = scen_palette_refVsAllSpp) +
  labs(title = 'World total median protein', y = '[g/capita/day]', x = 'year') +
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
ggsave(pl, file = paste0(figures_path,dir_name,"/macronutrients/",'protein_median_oneconsumer_world.png'),
       width = 1000, height = 500, units = 'mm', limitsize = F)


### Protein supply from animals ================================================
# Same process as above but filtered for animal commodities
animal_protein_supply_by_commodity <- consum_macronutrient %>%
  select(1:7, 10, 14:19, 21, 22, grams_protein_per_kcal_comm) %>%
  filter(protein_per_100g != 0,
         commodity %in% animal_commodities) %>%
  mutate(commodity_cals = commodity_cals * 1e12, # convert Pcal to kcal
         Units = "kcal") %>%
  group_by(scenario, t0, k, scen_type, region, year, commodity) %>%
  reframe(numerator = (as.numeric(grams_protein_per_kcal_comm) * commodity_cals), # protein (g per kcal) * calories
          denominator = population,
          animal_protein_supply = numerator / (denominator * 365), # convert year to day
          units = "g/capita/day",
          data = "FAO") %>%
  select(-numerator, -denominator) %>%
  distinct(., .keep_all = FALSE)

# Total regional supply
animal_protein_supply <- animal_protein_supply_by_commodity %>%
  group_by(scenario, t0, k, scen_type, region, year) %>%
  # Sum protein supply by commodity to get total protein supply for each region
  reframe(value = sum(animal_protein_supply),
          units = units) %>%
  distinct() %>%
  mutate(year = as.character(year))

# Get SingleConsumer info
animal_protein_oneconsumer <- consumption_by_commodity_decile %>%
  # Add in GCAM region ID
  left_join(id_gcam_regions, by = "region") %>%
  # Add population by region
  left_join(dt$pop_all_regions, by = c("scenario", "t0", "k", "scen_type", "region", "year")) %>%
  # Add regional macronutrient data
  left_join(macronutrients %>%
              mutate(GCAM_region_ID = as.integer(GCAM_region_ID)),
            by = c("GCAM_region_ID", "technology")) %>%
  rename(commodity = technology) %>%
  select(1:7, 10, 14:19, 21, 22, grams_protein_per_kcal_comm) %>%
  mutate(protein_per_100g = as.numeric(protein_per_100g)) %>%
  filter(protein_per_100g != 0,
         commodity %in% animal_commodities) %>%
  mutate(commodity_cals = commodity_cals * 1e12, # convert Pcal to kcal
         Units = "kcal") %>%
  group_by(scenario, region, year, commodity) %>%
  reframe(numerator = (as.numeric(grams_protein_per_kcal_comm) * commodity_cals), # protein (g per kcal) * calories
          denominator = population,
          protein_supply = numerator / (denominator * 365), # convert year to day
          units = "g/capita/day") %>%
  select(-numerator, -denominator) %>%
  group_by(scenario, region, year) %>%
  # Sum protein supply by commodity to get total protein supply for each region
  reframe(value = sum(protein_supply),
          units = units,
          data = "FAO") %>%
  distinct()


pl = ggplot(protein_oneconsumer %>%
              mutate(scen_type = substr(scen_type, 1, 3)) %>%
              # filter(scenario %in% selected_scen) %>%
              group_by(scen_type, region, year, units, data) %>%
              mutate(mean_value = median(value)) %>%
              group_by(scenario, region, year, units, data) %>%
              mutate(value = median(value))) +
  geom_line(aes(year, mean_value, color = scen_type), linewidth = 2) +
  geom_line(aes(year, value, group = scenario, color = scen_type)) +
  # scale_color_manual(values = scen_palette_refVsAllSpp) +
  facet_wrap(. ~ region) +
  labs(title = 'Animal protein', y = '[g/capita/day]', x = 'year') +
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
ggsave(pl, file = paste0(figures_path,dir_name,"/macronutrients/",'animal_protein.png'), width = 1000, height = 1000, units = 'mm')

### Avg fat supply =============================================================
# SUM_commodity(fat/kg_c * consumption_c) / population
fat_supply_by_commodity <- consum_macronutrient %>%
  select(1:7, 10, 14:19, 21, 22, grams_fat_per_kcal_comm, grams_protein_per_kcal_comm) %>%
  filter(fat_per_100g != 0) %>%
  mutate(commodity_cals = commodity_cals * 1e12, # convert Pcal to kcal
         Units = "kcal") %>%
  group_by(scenario, t0, k, scen_type, region, year, commodity) %>%
  reframe(numerator = (as.numeric(grams_fat_per_kcal_comm) * commodity_cals), # protein (g per kcal) * calories
          denominator = population,
          fat_supply = numerator / (denominator * 365), # convert year to day
          units = "g/capita/day",
          data = "FAO") %>%
  select(-numerator, -denominator)

# Total regional values
fat_supply <- fat_supply_by_commodity %>%
  group_by(scenario, t0, k, scen_type, region, year) %>%
  # Sum protein supply by commodity to get total protein supply for each region
  reframe(fat_agg = sum(fat_supply),
          units = units) %>%
  distinct() %>%
  mutate(year = as.character(year),
         data = "FAO")

# Get SingleConsumer info
fat_oneconsumer <- consumption_by_commodity_decile %>%
  # Add in GCAM region ID
  left_join(id_gcam_regions, by = "region") %>%
  # Add population by region
  left_join(dt$pop_all_regions, by = c("scenario", "t0", "k", "scen_type", "region", "year")) %>%
  # Add regional macronutrient data
  left_join(macronutrients %>%
              mutate(GCAM_region_ID = as.integer(GCAM_region_ID)), by = c("GCAM_region_ID", "technology")) %>%
  rename(commodity = technology) %>%
  select(1:7, 10, 14:19, 21, 22, grams_fat_per_kcal_comm, grams_protein_per_kcal_comm) %>%
  mutate(fat_per_100g = as.numeric(fat_per_100g)) %>%
  filter(fat_per_100g != 0) %>%
  mutate(commodity_cals = commodity_cals * 1e12, # convert Pcal to kcal
         Units = "kcal") %>%
  group_by(scenario, t0, k, scen_type, region, year, commodity) %>%
  reframe(numerator = (as.numeric(grams_fat_per_kcal_comm) * commodity_cals), # protein (g per kcal) * calories
          denominator = population,
          fat_supply = numerator / (denominator * 365), # convert year to day
          units = "g/capita/day") %>%
  select(-numerator, -denominator) %>%
  group_by(scenario, t0, k, scen_type, region, year) %>%
  # Sum fat supply by commodity to get total fat supply for each region
  reframe(value = sum(fat_supply),
          units = units,
          data = "FAO") %>%
  distinct()


pl = ggplot(fat_oneconsumer%>%
              mutate(scen_type = substr(scen_type, 1, 3))) +
  geom_line(aes(year, as.numeric(value), group = scenario, color = scen_type)) +
  facet_wrap(. ~ region) +
  labs(title = 'Fat', y = '[g/capita/day]', x = 'year') +
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
ggsave(pl, file = paste0(figures_path,dir_name,"/macronutrients/",'fat_oneconsumer.png'), width = 1000, height = 1000, units = 'mm')

pl = ggplot(fat_oneconsumer %>%
              mutate(scen_type = substr(scen_type, 1, 3)) %>%
              group_by(scen_type, year, units, data) %>%
              mutate(mean_value = median(value))) +
  geom_line(aes(year, as.numeric(value), group = scenario, color = scen_type)) +
  geom_line(aes(year, as.numeric(mean_value), color = scen_type), linewidth = 2) +
  labs(title = 'World Fat', y = '[g/capita/day]', x = 'year') +
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
ggsave(pl, file = paste0(figures_path,dir_name,"/macronutrients/",'fat_median_oneconsumer_world.png'), width = 1000, height = 1000, units = 'mm')


## TODO: from here
## USDA data -------------------------------------------------------------------
# Read in USDA data
raw_data <- read.csv(paste0(inputs_path, "nutrition/USDA_data_final.csv"))

# Clean up column names
colnames(raw_data) <- c("Food", "GCAM commodity", "Calories (kcal)", "Protein (g)",
                        "Carbohydrate (g)", "Sugars (g)", "Fiber (g)", "Total fat (g)",
                        "Fatty acids (saturated, g)", "Fatty acids (monounsaturated, g)",
                        "Fatty acids (polyunsaturated, g)", "Cholesterol (mg)",
                        "Retinol (mcg)", "Vitamin A (mcg)", "Alpha carotene (mcg)",
                        "Beta carotene (mcg)", "Cryptoxanthin, beta (mcg)",
                        "Lycopene (mcg)", "Lutein and zeaxanthin (mcg)", "Thiamin (mg)",
                        "Riboflavin (mg)", "Niacin (mg)", "Vitamin B6 (mg)",
                        "Folic acid (mcg)", "Folate (food, mcg)", "Folate (DFE, mcg)",
                        "Folate (total, mcg)", "Choline (mg)", "Vitamin B12 (mcg)",
                        "Added vitamin B12 (mcg)", "Vitamin C (mg)",
                        "Vitamin D2 and D3 (mcg)", "Vitamin E (alpha-tocopherol, mg)",
                        "Added vitamin E", "Vitamin K (phylloquinone, mcg)", "Calcium (mg)",
                        "Phosphorus (mg)", "Magnesium (mg)", "Iron (mg)", "Zinc (mg)",
                        "Copper (mg)", "Selenium (mcg)", "Potassium (mg)", "Sodium (mg)",
                        "Caffeine (mg)", "Theobromine (mg)", "Alcohol (g)", "4:0 (g)",
                        "6:0 (g)", "8:0 (g)", "10:0 (g)", "12:0 (g)", "14:0 (g)",
                        "16:0 (g)", "18:0 (g)", "16:1 (g)", "18:1 (g)", "20:1 (g)",
                        "22:1 (g)", "18:2 (g)", "18:3 (g)", "18:4 (g)", "20:4 (g)",
                        "20:5 n3 (g)", "22:5 n3 (g)", "22:6 n3 (g)", "Water (g)")

# Pivot longer
long_data <- raw_data %>%
  filter(`GCAM commodity` != "") %>%
  pivot_longer(cols = 3:67, names_to = "Nutrient")

# Average over individual food items to get a representative value for each commodity
average_data <- long_data %>%
  group_by(`GCAM commodity`, Nutrient) %>%
  summarize(average = mean(value))

average_data_wide <- average_data %>%
  pivot_wider(names_from = "Nutrient", values_from = "average")

# Create a dataframe of calories, protein, and fat from USDA data
usda_macronutrients <- average_data %>% ## TODO: NOTE from this file different macronutrients can step from here
  filter(Nutrient %in% c("Calories (kcal)", "Protein (g)", "Total fat (g)")) %>%
  pivot_wider(names_from = "Nutrient", values_from = "average") %>%
  mutate(g_protein_per_kcal_comm = `Protein (g)` / `Calories (kcal)`,
         g_fat_per_kcal_comm = `Total fat (g)` / `Calories (kcal)`) %>%
  rename(technology = `GCAM commodity`)

# Now, repeat the above process to calculate protein and fat supply using the
# USDA conversion values
# Get consumption data in calories, add USDA macronutrients
consum_macronutrient_usda <- consumption_by_commodity_decile %>%
  # Add in GCAM region ID
  left_join(id_gcam_regions, by = "region") %>%
  # Add population by region
  left_join(dt$pop_all_regions, by = c("scenario", "t0", "k", "scen_type", "region", "year")) %>%
  # Add regional macronutrient data
  left_join(usda_macronutrients, by = "technology") %>%
  rename(commodity = technology) %>%
  filter(commodity != "FiberCrop") %>%
  mutate(population = population / 10) # account for deciles = 1/10 pop

### Avg protein supply =========================================================
# SUM_commodity(protein/kg_c * consumption_c) / population
protein_supply_by_commodity_usda <- consum_macronutrient_usda %>%
  select(1:7, 10, 14:19, 21, 22, g_protein_per_kcal_comm) %>%
  mutate(commodity_cals = commodity_cals * 1e12, # convert Pcal to kcal
         Units = "kcal") %>%
  group_by(scenario, t0, k, scen_type, region, year, commodity) %>%
  reframe(numerator = (g_protein_per_kcal_comm * commodity_cals), # protein (g per kcal) * calories
          denominator = population,
          protein_supply = numerator / (denominator * 365), # convert year to day
          units = "g/capita/day",
          data = "USDA") %>%
  select(-numerator, -denominator)

protein_supply_usda <- protein_supply_by_commodity_usda %>%
  group_by(scenario, t0, k, scen_type, region, year) %>%
  # Sum protein supply by commodity to get total protein supply for each region
  reframe(protein_agg = sum(protein_supply),
          units = units) %>%
  distinct() %>%
  mutate(year = as.character(year),
         data = "USDA")

# Get SingleConsumer info
protein_oneconsumer_usda <- consumption_by_commodity_decile %>%
  # Add in GCAM region ID
  left_join(id_gcam_regions, by = "region") %>%
  # Add population by region
  left_join(dt$pop_all_regions, by = c("scenario", "t0", "k", "scen_type", "region", "year")) %>%
  # Add regional macronutrient data
  left_join(usda_macronutrients, by = "technology") %>%
  rename(commodity = technology) %>%
  filter(commodity != "FiberCrop") %>%
  select(1:7, 10, 14:19, 21, 22, g_protein_per_kcal_comm) %>%
  mutate(commodity_cals = commodity_cals * 1e12, # convert Pcal to kcal
         Units = "kcal") %>%
  group_by(scenario, t0, k, scen_type, region, year, commodity) %>%
  reframe(numerator = (g_protein_per_kcal_comm * commodity_cals), # protein (g per kcal) * calories
          denominator = population,
          protein_supply = numerator / (denominator * 365), # convert year to day
          units = "g/capita/day") %>%
  select(-numerator, -denominator) %>%
  group_by(scenario, t0, k, scen_type, region, year) %>%
  # Sum protein supply by commodity to get total protein supply for each region
  reframe(value = sum(protein_supply),
          units = units,
          data = "USDA") %>%
  distinct()

# compare USDA macro data to FAO, add source column, can compare commodity to commodity
protein_compare <- protein_supply_agg_usda


# TODO: from here, but protein_supply_agg missing
#### Protein plots =============================================================
# Compare FAO and USDA total regional protein supply for multiple consumers
protein_compare_plot <- protein_compare %>%
  filter(scenario == "MultipleConsumers") %>%
  ggplot(aes(x = year, y = value, fill = data)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~region, scales = "free_y") +
  scale_fill_viridis_d(end = 0.5) +
  theme_bw() +
  labs(x = "Year",
       y = "g/capita/day",
       title = "Protein supply in the MultipleConsumers scenario") +
  theme(axis.text.x = element_text(angle = 90)); protein_compare_plot

ggsave(plot = protein_compare_plot, "./figures/usda/ref_protein_comparison.jpg", width = 15, height = 6, units = "in")

# Compare FAO and USDA total regional protein supply for a single consumer
protein_compare_single_consumer <- bind_rows(protein_oneconsumer, protein_oneconsumer_usda)

protein_compare_one_plot <- protein_compare_single_consumer %>%
  ggplot(aes(x = year, y = value, fill = data)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~region, scales = "free_y") +
  scale_fill_viridis_d(end = 0.5) +
  theme_bw() +
  labs(x = "Year",
       y = "g/capita/day",
       title = "Protein supply in the MultipleConsumers scenario") +
  theme(axis.text.x = element_text(angle = 90)); protein_compare_one_plot

ggsave(plot = protein_compare_one_plot, "./figures/usda/oneconsumer_protein_comparison.jpg", width = 15, height = 6, units = "in")

# Plot protein values by commodity
protein_by_comm <- bind_rows(protein_supply_by_commodity, protein_supply_by_commodity_usda) %>%
  filter(scenario == "MultipleConsumers") %>%
  group_by(region, year, commodity, data) %>%
  summarize(value = mean(protein_supply)) %>%
  mutate(commodity = as.character(commodity))

# Staples
protein_staples_plot <- protein_by_comm %>%
  filter(commodity %in% staples) %>%
  ggplot(aes(x = year, y = value, linetype = data, color = commodity)) +
  geom_line(linewidth = 1) +
  facet_wrap(~region, scales = "free") +
  theme_bw() +
  labs(x = "Year",
       y = "g/capita/day",
       title = "Protein supply: staples") +
  theme(axis.text.x = element_text(angle = 90)); protein_staples_plot

# Animal protein
protein_animal_plot <- protein_by_comm %>%
  filter(commodity %in% animal_commodities) %>%
  ggplot(aes(x = year, y = value, linetype = data, color = commodity)) +
  geom_line(linewidth = 1) +
  facet_wrap(~region, scales = "free") +
  theme_bw() +
  labs(x = "Year",
       y = "g/capita/day",
       title = "Protein supply: animal commodities") +
  theme(axis.text.x = element_text(angle = 90)); protein_animal_plot

# Other commodities
protein_other_plot <- protein_by_comm %>%
  filter(!c(commodity %in% c(staples, animal_commodities))) %>%
  ggplot(aes(x = year, y = value, linetype = data, color = commodity)) +
  geom_line(linewidth = 1) +
  facet_wrap(~region, scales = "free") +
  theme_bw() +
  labs(x = "Year",
       y = "g/capita/day",
       title = "Protein supply: other commodities") +
  theme(axis.text.x = element_text(angle = 90)) ; protein_other_plot

ggsave(plot = protein_staples_plot, "./figures/usda/ref_protein_staples_comparison.jpg", width = 18, height = 12, units = "in")
ggsave(plot = protein_animal_plot, "./figures/usda/ref_protein_animal_comparison.jpg", width = 18, height = 12, units = "in")
ggsave(plot = protein_other_plot, "./figures/usda/ref_protein_other_comparison.jpg", width = 18, height = 12, units = "in")

### Animal protein supply ===== ================================================
# Same as above, calculate animal protein supply using USDA conversions
animal_protein_supply_by_commodity_usda <- consum_macronutrient_usda %>%
  select(1:5, 9:16, g_protein_per_kcal_comm) %>%
  filter(commodity %in% animal_commodities) %>%
  mutate(commodity_cals = commodity_cals * 1e12, # convert Pcal to kcal
         Units = "kcal") %>%
  group_by(scenario, region, year, commodity, gcam.consumer) %>%
  reframe(numerator = (g_protein_per_kcal_comm * commodity_cals), # protein (g per kcal) * calories
          denominator = population,
          animal_protein_supply = numerator / (denominator * 365), # convert year to day
          units = "g/capita/day") %>%
  select(-numerator, -denominator)

# Total regional values
animal_protein_supply_usda <- animal_protein_supply_by_commodity_usda %>%
  group_by(scenario, region, year) %>%
  # Sum protein supply by commodity to get total protein supply for each region
  reframe(value = sum(animal_protein_supply),
          units = units) %>%
  distinct() %>%
  mutate(year = as.character(year),
         data = "USDA")


# Get SingleConsumer info
animal_protein_oneconsumer_usda <- consumption_by_commodity_decile %>%
  # Add in GCAM region ID
  left_join(id_gcam_regions, by = "region") %>%
  # Add population by region
  left_join(pop_all_regions, by = c("scenario", "region", "year")) %>%
  # Add regional macronutrient data
  left_join(usda_macronutrients, by = "technology") %>%
  rename(commodity = technology) %>%
  filter(commodity %in% animal_commodities) %>%
  select(1:5, 9:16, g_protein_per_kcal_comm) %>%
  mutate(commodity_cals = commodity_cals * 1e12, # convert Pcal to kcal
         Units = "kcal") %>%
  group_by(scenario, region, year, commodity) %>%
  reframe(numerator = (g_protein_per_kcal_comm * commodity_cals), # protein (g per kcal) * calories
          denominator = population,
          animal_protein_supply = numerator / (denominator * 365), # convert year to day
          units = "g/capita/day") %>%
  select(-numerator, -denominator) %>%
  group_by(scenario, region, year) %>%
  # Sum protein supply by commodity to get total protein supply for each region
  reframe(value = sum(animal_protein_supply),
          units = units,
          data = "USDA") %>%
  distinct()

# Write outputs
write.csv(animal_protein_supply_usda, "./fs_metrics/animal_protein_supply_region_usda.csv")
write.csv(animal_protein_supply_agg_usda, "./aggregation/animal_protein_usda.csv")

#### Animal protein plots ======================================================
# Compare animal protein supply from FAO vs USDA data for multiple consumers...
animal_protein_compare <- bind_rows(animal_protein_supply_agg, animal_protein_supply_agg_usda)

animal_protein_compare_plot <- animal_protein_compare %>%
  filter(scenario == "MultipleConsumers") %>%
  ggplot(aes(x = year, y = value, fill = data)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~region, scales = "free_y") +
  scale_fill_viridis_d(end = 0.5) +
  theme_bw() +
  labs(x = "Year",
       y = "g/capita/day",
       title = "Animal protein supply in the MultipleConsumers scenario") ; animal_protein_compare_plot

ggsave(plot = animal_protein_compare_plot, "./figures/usda/ref_animal_protein_comparison.jpg", width = 15, height = 6, units = "in")

# ... and for a single consumer
animal_protein_compare_single_consumer <- bind_rows(animal_protein_oneconsumer, animal_protein_oneconsumer_usda)

animal_protein_compare_one_plot <- animal_protein_compare_single_consumer %>%
  ggplot(aes(x = year, y = value, fill = data)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~region, scales = "free_y") +
  scale_fill_viridis_d(end = 0.5) +
  theme_bw() +
  labs(x = "Year",
       y = "g/capita/day",
       title = "Animal protein supply in the MultipleConsumers scenario") ; animal_protein_compare_one_plot

ggsave(plot = animal_protein_compare_one_plot, "./figures/usda/oneconsumer_animal_protein_comparison.jpg", width = 15, height = 6, units = "in")

### Avg fat supply =============================================================
# For USDA values
# SUM_commodity(fat/kg_c * consumption_c) / population
fat_supply_by_commodity_usda <- consum_macronutrient_usda %>%
  # filter(scenario == "MultipleConsumers") %>%
  select(1:5, 9:15, 17, 19) %>%
  filter(commodity != "FiberCrop") %>%
  mutate(commodity_cals = commodity_cals * 1e12, # convert Pcal to kcal
         Units = "kcal") %>%
  group_by(scenario, region, year, commodity, gcam.consumer) %>%
  reframe(numerator = (as.numeric(g_fat_per_kcal_comm) * commodity_cals), # protein (g per kcal) * calories
          denominator = population,
          fat_supply = numerator / (denominator * 365), # convert year to day
          units = "g/capita/day",
          data = "USDA") %>%
  select(-numerator, -denominator)

# Total regional values
fat_supply_usda <- fat_supply_by_commodity_usda %>%
  group_by(scenario, region, year, gcam.consumer) %>%
  # Sum protein supply by commodity to get total protein supply for each region
  reframe(fat_agg = sum(fat_supply),
          units = units,
          data = "USDA") %>%
  distinct() %>%
  mutate(year = as.character(year))

# Adjust labels for plotting
fat_supply_by_commodity_usda$gcam.consumer <- factor(fat_supply_by_commodity_usda$gcam.consumer,
                                                     levels = c("FoodDemand_Group1", "FoodDemand_Group2", "FoodDemand_Group3",
                                                                "FoodDemand_Group4", "FoodDemand_Group5", "FoodDemand_Group6",
                                                                "FoodDemand_Group7", "FoodDemand_Group8", "FoodDemand_Group9",
                                                                "FoodDemand_Group10"))
fat_supply_usda$gcam.consumer <- factor(fat_supply_usda$gcam.consumer,
                                        levels = c("FoodDemand_Group1", "FoodDemand_Group2", "FoodDemand_Group3",
                                                   "FoodDemand_Group4", "FoodDemand_Group5", "FoodDemand_Group6",
                                                   "FoodDemand_Group7", "FoodDemand_Group8", "FoodDemand_Group9",
                                                   "FoodDemand_Group10"))

# Aggregate ten consumers to one
fat_supply_agg_usda <- fat_supply_usda %>%
  filter(scenario != "SingleConsumer") %>%
  group_by(scenario, region, year) %>%
  summarize(value = sum(fat_agg) / 10,
            data = "USDA")

# Get SingleConsumer info
fat_oneconsumer_usda <- consumption_by_commodity_decile %>%
  filter(scenario == "SingleConsumer") %>%
  # Add in GCAM region ID
  left_join(id_to_region, by = "region") %>%
  # Add population by region
  left_join(pop_all_regions, by = c("scenario", "region", "year")) %>%
  # Add regional macronutrient data
  left_join(usda_macronutrients, by = "technology") %>%
  rename(commodity = technology) %>%
  select(1:5, 9:15, 17, 19) %>%
  filter(commodity != "FiberCrop") %>%
  mutate(commodity_cals = commodity_cals * 1e12, # convert Pcal to kcal
         Units = "kcal") %>%
  group_by(scenario, region, year, commodity, gcam.consumer) %>%
  reframe(numerator = (as.numeric(g_fat_per_kcal_comm) * commodity_cals), # protein (g per kcal) * calories
          denominator = population,
          fat_supply = numerator / (denominator * 365), # convert year to day
          units = "g/capita/day") %>%
  select(-numerator, -denominator) %>%
  group_by(scenario, region, year, gcam.consumer) %>%
  # Sum protein supply by commodity to get total protein supply for each region
  reframe(value = sum(fat_supply),
          units = units,
          data = "USDA") %>%
  distinct()

# Write outputs
write.csv(fat_supply_usda, "./fs_metrics/fat_supply_region_usda.csv")
write.csv(fat_supply_agg_usda, "./aggregation/fat_usda.csv")

#### Fat plots =================================================================
# Compare fat supply from FAO vs USDA data for multiple consumers...
fat_compare <- bind_rows(fat_supply_agg, fat_supply_agg_usda)

fat_compare_plot <- fat_compare %>%
  filter(scenario == "MultipleConsumers") %>%
  ggplot(aes(x = year, y = value, fill = data)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~region, scales = "free_y") +
  scale_fill_viridis_d(end = 0.5) +
  theme_bw() +
  labs(x = "Year",
       y = "g/capita/day",
       title = "Fat supply in the MultipleConsumers scenario") ; fat_compare_plot

ggsave(plot = fat_compare_plot, "./figures/usda/oneconsumer_fat_comparison.jpg", width = 15, height = 6, units = "in")

# ...and for a single consumer
fat_compare_single_consumer <- bind_rows(fat_oneconsumer, fat_oneconsumer_usda)

fat_compare_one_plot <- fat_compare_single_consumer %>%
  ggplot(aes(x = year, y = value, fill = data)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~region, scales = "free_y") +
  scale_fill_viridis_d(end = 0.5) +
  theme_bw() +
  labs(x = "Year",
       y = "g/capita/day",
       title = "Fat supply in the MultipleConsumers scenario") ; fat_compare_one_plot

ggsave(plot = fat_compare_one_plot, "./figures/usda/oneconsumer_fat_comparison.jpg", width = 15, height = 6, units = "in")

# Plot fat supply by commodity
fat_by_comm <- bind_rows(fat_supply_by_commodity, fat_supply_by_commodity_usda) %>%
  filter(scenario == "MultipleConsumers") %>%
  group_by(region, year, commodity, data) %>%
  summarize(value = mean(fat_supply)) %>%
  mutate(commodity = as.character(commodity))

# Staples
fat_staples_plot <- fat_by_comm %>%
  filter(commodity %in% staples) %>%
  ggplot(aes(x = year, y = value, linetype = data, color = commodity)) +
  geom_line(linewidth = 1) +
  facet_wrap(~region, scales = "free") +
  theme_bw() +
  labs(x = "Year",
       y = "g/capita/day",
       title = "Fat supply: staples") +
  theme(axis.text.x = element_text(angle = 90)); fat_staples_plot

# Animal commodities
fat_animal_plot <- fat_by_comm %>%
  filter(commodity %in% animal_commodities) %>%
  ggplot(aes(x = year, y = value, linetype = data, color = commodity)) +
  geom_line(linewidth = 1) +
  facet_wrap(~region, scales = "free") +
  theme_bw() +
  labs(x = "Year",
       y = "g/capita/day",
       title = "Fat supply: animal commodities") +
  theme(axis.text.x = element_text(angle = 90)); fat_animal_plot

# Other commodities
fat_other_plot <- fat_by_comm %>%
  filter(!c(commodity %in% c(staples, animal_commodities))) %>%
  ggplot(aes(x = year, y = value, linetype = data, color = commodity)) +
  geom_line(linewidth = 1) +
  facet_wrap(~region, scales = "free") +
  theme_bw() +
  labs(x = "Year",
       y = "g/capita/day",
       title = "Fat supply: other commodities") +
  theme(axis.text.x = element_text(angle = 90)) ; fat_other_plot

ggsave(plot = fat_staples_plot, "./figures/usda/ref_fat_staples_comparison.jpg", width = 18, height = 12, units = "in")
ggsave(plot = fat_animal_plot, "./figures/usda/ref_fat_animal_comparison.jpg", width = 18, height = 12, units = "in")
ggsave(plot = fat_other_plot, "./figures/usda/ref_fat_other_comparison.jpg", width = 18, height = 12, units = "in")

### Iron =======================================================================
# Use iron consumption data from USDA to estimate anemia
# Get iron nutritional data
iron <- average_data %>%
  filter(Nutrient %in% c("Calories (kcal)", "Iron (mg)")) %>%
  pivot_wider(names_from = "Nutrient", values_from = "average") %>%
  mutate(mg_iron_per_kcal_comm = `Iron (mg)` / `Calories (kcal)`) %>%
  rename(technology = `GCAM commodity`)

# Add GCAM calories, region, population
consum_iron <- consumption_by_commodity_decile %>%
  # Add in GCAM region ID
  left_join(id_to_region, by = "region") %>%
  # Add population by region
  left_join(pop_all_regions, by = c("scenario", "region", "year")) %>%
  # Add regional macronutrient data
  left_join(iron, by = "technology") %>%
  rename(commodity = technology) %>%
  filter(scenario != "SingleConsumer",
         commodity != "FiberCrop") %>%
  mutate(population = population / 10) # account for deciles = 1/10 pop

# Calculate iron supply
# SUM_commodity(iron/kg_c * consumption_c) / population
iron_supply_by_commodity <- consum_iron %>%
  select(1:5, 9:17, mg_iron_per_kcal_comm) %>%
  mutate(commodity_cals = commodity_cals * 1e12, # convert Pcal to kcal
         Units = "kcal") %>%
  group_by(scenario, region, year, commodity, gcam.consumer) %>%
  reframe(numerator = (mg_iron_per_kcal_comm * commodity_cals), # iron (g per kcal) * calories
          denominator = population,
          iron_supply = numerator / (denominator * 365), # convert year to day
          units = "mg/capita/day") %>%
  select(-numerator, -denominator)

# Total regional values
iron_supply <- iron_supply_by_commodity %>%
  group_by(scenario, region, year, gcam.consumer) %>%
  # Sum iron supply by commodity to get total iron supply for each region
  reframe(iron_agg = sum(iron_supply),
          units = units) %>%
  distinct() %>%
  mutate(year = as.character(year))

# Adjust levels for plotting
iron_supply$gcam.consumer <- factor(iron_supply$gcam.consumer,
                                    levels = c("FoodDemand_Group1", "FoodDemand_Group2", "FoodDemand_Group3",
                                               "FoodDemand_Group4", "FoodDemand_Group5", "FoodDemand_Group6",
                                               "FoodDemand_Group7", "FoodDemand_Group8", "FoodDemand_Group9",
                                               "FoodDemand_Group10"))
# Aggregate ten consumers to one
iron_supply_agg <- iron_supply %>%
  filter(scenario != "SingleConsumer") %>%
  group_by(scenario, region, year) %>%
  summarize(value = sum(iron_agg) / 10)

# Write outputs
write.csv(iron_supply, "./fs_metrics/iron_supply_region.csv")
write.csv(iron_supply_agg, "./aggregation/iron_usda.csv")

#### Iron plots ================================================================
# Plot total regional values for iron
# Blue line represents the minimum iron requirements for males, pink for females
iron_fig <- iron_supply_agg %>%
  mutate(year = as.factor(year)) %>%
  filter(scenario == "MultipleConsumers") %>%
  ggplot(aes(x = year, y = value, fill = scenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 8, color = "blue") +
  geom_hline(yintercept = 18, color = "pink") +
  facet_wrap(~region, scales = "free_y") +
  theme_bw() +
  scale_fill_manual(values = "grey50") +
  labs(y = "Iron supply (mg/capita/day)",
       x = "Year",
       title = "Iron supply",
       fill = "") ; iron_fig

ggsave(plot = iron_fig, "./figures/usda/iron/agg_iron.jpg", height = 6, width = 9, units = "in")

# Plot iron supply by decile
iron_deciles_fig <- iron_supply %>%
  filter(scenario == "MultipleConsumers") %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x = year, y = iron_agg, color = gcam.consumer)) +
  geom_line() +
  scale_color_manual(values = colors_10) +
  geom_hline(yintercept = 8, color = "blue", linewidth = 1) +
  geom_hline(yintercept = 18, color = "pink", linewidth = 1) +
  facet_wrap(~region, scales = "free") +
  labs(x = "Year", y = "mg/capita/day", title = "Iron supply") +
  theme_bw() ; iron_deciles_fig

ggsave(plot = iron_deciles_fig, "./figures/usda/iron/all_regions_iron.jpg", height = 9 , width = 18, units = "in")

# Plot iron by decile individually for each region
for(region_name in unique(iron_supply$region)) {
  g <- iron_supply %>%
    filter(scenario == "MultipleConsumers", region == region_name) %>%
    mutate(year = as.numeric(year)) %>%
    ggplot(aes(x = year, y = iron_agg, color = gcam.consumer)) +
    geom_line() +
    scale_color_manual(values = colors_10) +
    geom_hline(yintercept = 8, color = "blue", linewidth = 1) +
    geom_hline(yintercept = 18, color = "pink", linewidth = 1) +
    labs(x = "Year",
         y = "mg/capita/day",
         title = paste0("Iron supply in ", region_name)) +
    theme_bw()

  ggsave(plot = g, filename = paste0("./figures/usda/iron/iron_supply_", region_name, ".jpg"), height = 6, width = 9, units = "in")
}

#### Population under iron requirements ========================================
# Female
# Get iron supply and minimum iron requirements
iron_female_req <- iron_supply %>%
  filter(scenario == "MultipleConsumers", year > 2005) %>%
  mutate(min_iron = 18,
         year = as.numeric(year)) %>%
  select(-scenario)

# Access demographic information - how many females are in each region?
# Check if the iron supply is greater than the minimum requirements for females
female_pop_under_iron <- weighted_pop_sex_age %>%
  mutate(year = as.numeric(year)) %>%
  filter(grepl("Female", variable)) %>%
  group_by(region, year) %>%
  summarize(total_female_pop = sum(pop_sex_age)) %>%
  left_join(iron_female_req, by = c("region", "year")) %>%
  mutate(below_req = iron_agg - min_iron)

female_pop_under_iron$check <- female_pop_under_iron$below_req < 0

# Find how many deciles are below the requirement, use this to calculate number
# of females below iron requirement
female_iron <- female_pop_under_iron %>%
  group_by(region, year) %>%
  reframe(number_of_deciles_below_iron = sum(check),
          total_female_pop) %>%
  distinct() %>%
  mutate(percent_of_female_pop_below = number_of_deciles_below_iron * 10,
         number_of_females_below = (percent_of_female_pop_below / 100) * total_female_pop)

# The FSI looks specifically at "reproductive age (15-49)"
# Isolate this age range, check how many people are below the requirement
female_rep_pop_under_iron <- weighted_pop_sex_age %>%
  mutate(year = as.numeric(year)) %>%
  filter(grepl("Female", variable)) %>%
  filter(grepl("15-19|20-24|25-29|30-34|35-39|40-44|45-49", variable)) %>%
  group_by(region, year) %>%
  summarize(total_female_pop = sum(pop_sex_age)) %>%
  left_join(iron_female_req, by = c("region", "year")) %>%
  mutate(below_req = iron_agg - min_iron)

female_rep_pop_under_iron$check <- female_rep_pop_under_iron$below_req < 0

female_rep_iron <- female_rep_pop_under_iron %>%
  group_by(region, year) %>%
  reframe(number_of_deciles_below_iron = sum(check),
          total_female_pop) %>%
  distinct() %>%
  mutate(percent_of_female_pop_below = number_of_deciles_below_iron * 10,
         number_of_females_below = (percent_of_female_pop_below / 100) * total_female_pop)

# Write outputs
write.csv(female_rep_iron, "./fs_metrics/females_under_iron.csv")

# Male
# Same process, but for men
iron_male_req <- iron_supply %>%
  filter(scenario == "MultipleConsumers", year > 2005) %>%
  mutate(min_iron = 8,
         year = as.numeric(year)) %>%
  select(-scenario)

# Access demographic data
male_pop_under_iron <- weighted_pop_sex_age %>%
  mutate(year = as.numeric(year)) %>%
  filter(grepl("Male", variable)) %>%
  group_by(region, year) %>%
  summarize(total_male_pop = sum(pop_sex_age)) %>%
  left_join(iron_male_req, by = c("region", "year")) %>%
  mutate(below_req = iron_agg - min_iron)

# Check if decile is below requirement
male_pop_under_iron$check <- male_pop_under_iron$below_req < 0

# Calculate number of men below iron requirements
male_iron <- male_pop_under_iron %>%
  group_by(region, year) %>%
  reframe(number_of_deciles_below_iron = sum(check),
          total_male_pop) %>%
  distinct() %>%
  mutate(percent_of_male_pop_below = number_of_deciles_below_iron * 10,
         number_of_males_below =(percent_of_male_pop_below / 100) * total_male_pop)

## Nutrients per gram ----------------------------------------------------------
# Represent conversion as grams of nutrients per 100g of commodity, as opposed
# to grams commodity per calorie
grams <- macronutrients %>%
  select(-kcalperg, -grams_fat_per_kcal_comm, -grams_protein_per_kcal_comm)

grams$kcal_per_100g <- as.numeric(grams$kcal_per_100g)
grams$protein_per_100g <- as.numeric(grams$protein_per_100g)
grams$fat_per_100g <- as.numeric(grams$fat_per_100g)

# Protein
grams %>%
  filter(protein_per_100g != 0) %>%
  # Reorder data in terms of smallest conversions to largest
  mutate(data = reorder_within(GCAM_region_ID, protein_per_100g, technology)) %>%
  group_by(technology) %>%
  # Get average values for each commodity (one value per region)
  mutate(mean_comm = mean(protein_per_100g),
         sd_comm = sd(protein_per_100g)) %>%
  # Plot
  ggplot(aes(x = data, y = protein_per_100g,
             fill = as.factor(GCAM_region_ID))) +
  geom_col(position = "dodge") +
  # Plot +/- 1 sd
  geom_hline(aes(yintercept = mean_comm + sd_comm), color = "black") +
  geom_hline(aes(yintercept = mean_comm - sd_comm), color = "black") +
  # Plot +/- 2 sd
  geom_hline(aes(yintercept = mean_comm + (2 * sd_comm)), color = "red") +
  geom_hline(aes(yintercept = mean_comm - (2 * sd_comm)), color = "red") +
  facet_wrap(~technology, scales = "free") +
  scale_fill_manual(values = pals::glasbey()) +
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  labs(x = "GCAM region",
       y = "Protein (g) per 100 g of commodity",
       title = "Protein per commodity") -> g_protein ; g_protein

ggsave(plot = g_protein, "./figures/conversions/grams_protein.jpg", height = 6, width = 10, units = "in")

# Fat
grams %>%
  filter(fat_per_100g != 0) %>%
  # Reorder data in terms of smallest conversions to largest
  mutate(data = reorder_within(GCAM_region_ID, fat_per_100g, technology)) %>%
  group_by(technology) %>%
  # Get average values for each commodity (one value per region)
  mutate(mean_comm = mean(fat_per_100g),
         sd_comm = sd(fat_per_100g)) %>%
  # Plot
  ggplot(aes(x = data, y = fat_per_100g,
             fill = as.factor(GCAM_region_ID))) +
  geom_col(position = "dodge") +
  # Plot +/- 1 sd
  geom_hline(aes(yintercept = mean_comm + sd_comm), color = "black") +
  geom_hline(aes(yintercept = mean_comm - sd_comm), color = "black") +
  # Plot +/- 2 sd
  geom_hline(aes(yintercept = mean_comm + (2 * sd_comm)), color = "red") +
  geom_hline(aes(yintercept = mean_comm - (2 * sd_comm)), color = "red") +
  facet_wrap(~technology, scales = "free") +
  scale_fill_manual(values = pals::glasbey()) +
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  labs(x = "GCAM region",
       y = "Fat (g) per 100 g of commodity",
       title = "Fat per commodity") -> g_fat ; g_fat

ggsave(plot = g_fat, "./figures/conversions/grams_fat.jpg", height = 6, width = 10, units = "in")

# Calories
grams %>%
  filter(kcal_per_100g != 0) %>%
  # Reorder data in terms of smallest conversions to largest
  mutate(data = reorder_within(GCAM_region_ID, kcal_per_100g, technology)) %>%
  group_by(technology) %>%
  # Get average values for each commodity (one value per region)
  mutate(mean_comm = mean(kcal_per_100g),
         sd_comm = sd(kcal_per_100g)) %>%
  # Plot
  ggplot(aes(x = data, y = kcal_per_100g,
             fill = as.factor(GCAM_region_ID))) +
  geom_col(position = "dodge") +
  # Plot +/- 1 sd
  geom_hline(aes(yintercept = mean_comm + sd_comm), color = "black") +
  geom_hline(aes(yintercept = mean_comm - sd_comm), color = "black") +
  # Plot +/- 2 sd
  geom_hline(aes(yintercept = mean_comm + (2 * sd_comm)), color = "red") +
  geom_hline(aes(yintercept = mean_comm - (2 * sd_comm)), color = "red") +
  facet_wrap(~technology, scales = "free") +
  scale_fill_manual(values = pals::glasbey()) +
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  labs(x = "GCAM region",
       y = "Calories (kcal) per 100 g of commodity",
       title = "Calories per commodity") -> g_kcal ; g_kcal

ggsave(plot = g_kcal, "./figures/conversions/grams_kcal.jpg", height = 6, width = 10, units = "in")

# We see that there are outliers in the conversion factors that may be affecting results
# Values that are higher than 1 sd are replaced with the mean + sd value, and
# the opposite for values lower than 1 sd. Values equal to 0 are replaced with the
# mean value (excluding zero values)

# Recalculate g commodity per kcal using the new conversion values
new_conversions <- grams %>%
  group_by(technology) %>%
  filter(protein_per_100g != 0, fat_per_100g != 0) %>%
  mutate(avg_protein = mean(protein_per_100g), # get average of non-zero values
         avg_fat = mean(fat_per_100g),
         avg_kcal = mean(kcal_per_100g),
         protein_cutoff_max = avg_protein + sd(protein_per_100g), # define cutoff as mean +/- 1 sd
         protein_cutoff_min = avg_protein - sd(protein_per_100g),
         fat_cutoff_max = avg_fat + sd(fat_per_100g),
         fat_cutoff_min = avg_fat - sd(protein_per_100g),
         kcal_cutoff_max = avg_kcal + sd(kcal_per_100g),
         kcal_cutoff_min = avg_kcal - sd(protein_per_100g)) %>%
  # Check if conversions are outside of the sd range, tag if so
  mutate(new_protein = if_else(protein_per_100g < protein_cutoff_min, "below", "no"),
         new_protein = if_else(protein_per_100g > protein_cutoff_max, "above", new_protein),
         new_fat = if_else(fat_per_100g < fat_cutoff_min, "below", "no"),
         new_fat = if_else(fat_per_100g > fat_cutoff_max, "above", new_fat),
         new_kcal = if_else(kcal_per_100g < kcal_cutoff_min, "below", "no"),
         new_kcal = if_else(kcal_per_100g > kcal_cutoff_max, "above", new_kcal)) %>%
  # If a value is outside of the acceptable range, replace it with the appropriate value
  # (+/- 1 sd, depending on if the value was too high or low)
  mutate(protein_per_100g = if_else(new_protein == "above", protein_cutoff_max, protein_per_100g),
         protein_per_100g = if_else(new_protein == "below", protein_cutoff_min, protein_per_100g),
         fat_per_100g = if_else(new_fat == "above", fat_cutoff_max, fat_per_100g),
         fat_per_100g = if_else(new_fat == "below", fat_cutoff_min, fat_per_100g),
         kcal_per_100g = if_else(new_kcal == "above", kcal_cutoff_max, kcal_per_100g),
         kcal_per_100g = if_else(new_kcal == "below", kcal_cutoff_min, kcal_per_100g))

# Replace values equal to 0 with the mean protein and fat conversions from above
zeros <- grams %>%
  filter(protein_per_100g == 0 | fat_per_100g == 0) %>%
  left_join(select(new_conversions, avg_protein, avg_fat, avg_kcal), relationship = "many-to-many") %>%
  mutate(protein_per_100g = if_else(protein_per_100g == 0, avg_protein, protein_per_100g),
         fat_per_100g = if_else(fat_per_100g == 0, avg_fat, fat_per_100g),
         kcal_per_100g = if_else(kcal_per_100g == 0, avg_kcal, kcal_per_100g)) %>%
  unique()

# Combine zero and non-zero values to get a complete set of new conversions
final_new_conversions <- new_conversions %>%
  select(1:5) %>%
  bind_rows(select(zeros, 1:5)) %>%
  # Recalculate grams commodity per kcal
  mutate(grams_fat_per_kcal_comm = fat_per_100g / kcal_per_100g,
         grams_protein_per_kcal_comm = protein_per_100g / kcal_per_100g)

final_new_conversions$GCAM_region_ID <- as.numeric(final_new_conversions$GCAM_region_ID)

# Recalculate macronutrient data
consum_macronutrient_new <- consumption_by_commodity_decile %>%
  # Add in GCAM region ID
  left_join(id_to_region, by = "region") %>%
  # Add population by region
  left_join(pop_all_regions, by = c("scenario", "region", "year")) %>%
  # Add regional macronutrient data
  left_join(final_new_conversions, by = c("GCAM_region_ID", "technology")) %>%
  rename(commodity = technology) %>%
  filter(scenario != "SingleConsumer") %>%
  mutate(population = population / 10) # account for deciles = 1/10 pop

consum_macronutrient_new$fat_per_100g <- as.numeric(consum_macronutrient_new$fat_per_100g)
consum_macronutrient_new$protein_per_100g <- as.numeric(consum_macronutrient_new$protein_per_100g)

## Plot new conversions --------------------------------------------------------
# Pull out average protein and fat
avg_protein_new <- final_new_conversions %>%
  group_by(technology) %>%
  summarize(avg_protein = mean(grams_protein_per_kcal_comm))

avg_fat_new <- final_new_conversions %>%
  group_by(technology) %>%
  summarize(avg_fat = mean(grams_fat_per_kcal_comm))

# Plot new protein conversions
protein_conv_plot_new <- ggplot(
  data = filter(final_new_conversions, technology != "FiberCrop"),
  aes(x = as.factor(GCAM_region_ID),
      y = grams_protein_per_kcal_comm,
      fill = as.factor(GCAM_region_ID))) +
  geom_col(position = "dodge") +
  facet_wrap(~technology, scales = "free") +
  scale_fill_manual(values = pals::glasbey()) +
  theme_bw() +
  theme(legend.key.size = unit(2, "cm"), legend.position = "none") +
  geom_hline(data = avg_protein, aes(yintercept = avg_protein, color = "Average regional value")) +
  labs(x = "ID",
       y = "Grams protein per calorie of commodity",
       title = "Protein conversions",
       color = "") ; protein_conv_plot_new

ggsave(plot = protein_conv_plot_new, "./figures/conversions/protein_conversion_comparison_new.jpg", height = 9, width = 27, units = "in")

# Plot new fat conversions
fat_conv_plot_new <- final_new_conversions %>%
  filter(technology != "FiberCrop") %>%
  ggplot(aes(x = GCAM_region_ID, y = grams_fat_per_kcal_comm, fill = as.factor(GCAM_region_ID))) +
  geom_col(position = "dodge") +
  facet_wrap(~technology, scales = "free") +
  scale_fill_manual(values = pals::glasbey()) +
  theme_bw() +
  theme(legend.key.size = unit(2, "cm"), legend.position = "none") +
  geom_hline(data = avg_fat, aes(yintercept = avg_fat, color = "Average regional value")) +
  labs(x = "ID",
       y = "Grams fat per calorie of commodity",
       title = "Fat conversions",
       color = "") ; fat_conv_plot_new

ggsave(plot = fat_conv_plot_new, "./figures/conversions/fat_conversion_comparison_new.jpg", height = 9, width = 27, units = "in")

# Plot calories
kcal_perg_plot <- final_new_conversions %>%
  filter(technology != "FiberCrop") %>%
  ggplot(aes(x = as.factor(GCAM_region_ID),
             y = kcal_per_100g,
             fill = as.factor(GCAM_region_ID))) +
  geom_col(position = "dodge") +
  facet_wrap(~technology, scales = "free") +
  scale_fill_manual(values = pals::glasbey()) +
  theme_bw() +
  theme(legend.key.size = unit(2, "cm"), legend.position = "none") +
  labs(x = "ID",
       y = "Calories in 100g",
       title = "Regional calories by commodity",
       color = "") ; kcal_perg_plot

ggsave(plot = kcal_perg_plot, "./figures/conversions/kcal_per_100g.jpg", height = 9, width = 27, units = "in")

## Cereal import dependency ratio ==============================================
# Wheat, rice, corn, OtherGrain
# SUM_cerealgrains((imports - exports)/(production+imports-exports))
# TODO check why this is so off from FAO

# We likely can't calculate this accurately since production is not split up
# by food vs non-food production

# Consumption (MT)
consumption_mt <- getQuery(prj, "demand balances by crop commodity") %>%
  filter(year >= figure_start_year,
         sector %in% c("FoodDemand_Staples", "FoodDemand_NonStaples"),
         input %in% c("regional wheat", "regional rice", "regional corn", "regional othergrain")) %>%
  select(-4) %>%
  rename(sector = input) %>%
  group_by(scenario, region, year) %>%
  summarize(consumption = sum(value))

# Production (MT)
production <- getQuery(prj, "ag production by crop type") %>%
  filter(year >= figure_start_year,
         sector %in% c("Corn", "Wheat", "Rice", "OtherGrain")) %>%
  select(-5) %>%
  group_by(scenario, region, year) %>%
  summarize(production = sum(value))

# Imports (MT)
imports <- getQuery(prj, "outputs by tech") %>%
  filter(sector %in% c("regional corn", "regional othergrain",
                       "regional rice", "regional wheat"),
         grepl("imported", technology)) %>%
  select(1, 2, 3, 4, 8, 9) %>%
  group_by(scenario, region, year) %>%
  summarize(imports = sum(value))

imports[is.na(imports)] <- 0

# Exports (MT) and all data
exports <- left_join(consumption_mt, production, by = c("scenario", "region", "year")) %>%
  left_join(imports, by = c("scenario", "region", "year")) %>%
  filter(imports != "NA") %>%
  mutate(exports = production + imports - consumption)

cereal_import_dep_ratio <- exports %>%
  mutate(cereal_ratio = ((imports  - exports) * 100) / (production + imports))

# Method 2: regional production over regional consumption
cereal <- left_join(production, consumption_mt) %>%
  mutate(ratio = 1 - (production / consumption),
         percent = ratio * 100)

# Write outputs
write.csv(cereal_import_dep_ratio, "./fs_metrics/cereal_import_dep_ratio.csv")

# Validation ===================================================================
# Get population data for each country
pop_by_country <- ssp_data_long %>%
  filter(variable == "Population") %>%
  rename(total_pop = value) %>%
  select(country_name, year, total_pop)

pop_by_country$year <- as.numeric(pop_by_country$year)

# Function to read and format FAO data
read_fao_data <- function(params) {
  x <- read.csv(paste0("./FAO comparison data/", params, ".csv")) %>%
    select(4, 10, 11, 12) %>%
    rename(region = Area,
           year = Year,
           Units = Unit,
           fao_value = Value) %>%
    filter(region != "China") %>%
    mutate(region = case_when(region == "Antigua and Barbuda" ~ "Antigua & Barbuda",
                              region == "Bolivia (Plurinational State of)" ~ "Bolivia",
                              region == "Cabo Verde" ~ "Cape Verde",
                              region == "China, mainland" ~ "China",
                              region == "China, Hong Kong SAR" ~ "Hong Kong",
                              region == "China, Macao SAR" ~ "Macau",
                              region == "China, Taiwan Province of" ~ "Taiwan",
                              region == "CÃ´te d'Ivoire" ~ "Cote dIvoire",
                              region == "Czechia" ~ "Czech Republic",
                              region == "Democratic People's Republic of Korea" ~ "Korea, Democratic Peoples Republic of",
                              region == "Democratic Republic of the Congo" ~ "Congo, the Democratic Republic of the",
                              region == "Eswatini" ~ "Swaziland",
                              region == "Iran (Islamic Republic of)" ~ "Iran, Islamic Republic of",
                              region == "Lao People's Democratic Republic" ~ "Lao Peoples Democratic Republic",
                              region == "Libya" ~ "Libyan Arab Jamahiriya",
                              region == "Micronesia (Federated States of)" ~ "Micronesia, Federated States of",
                              region == "Netherlands (Kingdom of the)" ~ "Netherlands Antilles",
                              region == "North Macedonia" ~ "Macedonia, the former Yugoslav Republic of",
                              region == "Palestine" ~ "Palestinian Territory, Occupied",
                              region == "Republic of Korea" ~ "Korea, Republic of",
                              region == "Republic of Moldova" ~ "Moldova, Republic of",
                              region == "Timor-Leste" ~ "Timor Leste",
                              region == "TÃ¼rkiye" ~ "Turkey",
                              region == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
                              region == "United Republic of Tanzania" ~ "Tanzania, United Republic of",
                              region == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                              T ~ region)) %>%
    filter(year != "2019-2021") %>%
    left_join(country_to_id, by = "region") %>%
    rename(country_name = region) %>%
    left_join(id_to_region, by = "GCAM_region_ID")

  return(x)
}

# Function to weight FAO data by country and aggregate into GCAM regions
filter_weight_fao <- function(data) {
  x <- data %>%
    # Remove empty cells
    filter(fao_value != 0, region != 0) %>%
    group_by(year, country_name) %>%
    # Aggregate all the tiny country values to get one value per country
    mutate(fao_value = sum(fao_value)) %>%
    distinct() %>%
    # Add population data and get rid of empty cells
    left_join(pop_by_country, by = c("country_name", "year")) %>%
    filter(total_pop != "NA") %>%
    group_by(year, GCAM_region_ID) %>% # , region) %>%
    # Get population for each GCAM region
    mutate(total_regional_pop = sum(total_pop),
           # Compute individual country weights
           weight = total_pop / total_regional_pop,
           # Computed weighted protein supply values
           weighted_fao_value = fao_value * weight) %>%
    group_by(year, GCAM_region_ID, region) %>%
    # Compute weighted regional values
    summarize(regional_fao_value = sum(weighted_fao_value)) %>%
    mutate(year = as.numeric(year))

  return(x)
}


# Write out csvs, do comparison in Excel
### ADESA ======================================================================
# Read in data
ADESA_fao <- read_fao_data("ADESA") %>%
  mutate(year = case_when(year == "2004-2006" ~ "2005",
                          year == "2009-2011" ~ "2010",
                          year == "2014-2016" ~ "2015",
                          year == "2019-2021" ~ "2020",
                          T ~ year)) %>%
  select(-Units) %>%
  filter(fao_value != "NA")

ADESA_fao$year <- as.numeric(ADESA_fao$year)

# Filter and weight data
ADESA_fao_filter <- filter_weight_fao(ADESA_fao)

# Join with GCAM results, compute absolute and percent differences
ADESA_valid <- adesa %>%
  filter(year %in% c(2010, 2015),
         scenario == "MultipleConsumers") %>%
  mutate(year = as.numeric(year)) %>%
  left_join(ADESA_fao_filter, by = c("region", "year")) %>%
  # Compute absolute and percent differences between GCAM and FAOSTAT values
  mutate(diff = regional_fao_value - adesa,
         diff_min = regional_fao_value - min_adesa,
         diff_max = regional_fao_value - max_adesa,
         per_diff = ((regional_fao_value - adesa) / regional_fao_value) * 100,
         per_diff_min = ((regional_fao_value - min_adesa) / regional_fao_value) * 100,
         per_diff_max = ((regional_fao_value - max_adesa) / regional_fao_value) * 100) %>%
  filter(GCAM_region_ID != "NA")

# Check second method (compare % to FAO)
ADESA_valid_2 <- adesa_method2 %>%
  filter(year %in% c(2010, 2015),
         scenario == "MultipleConsumers") %>%
  mutate(year = as.numeric(year)) %>%
  left_join(ADESA_fao_filter, by = c("region", "year")) %>%
  mutate(diff = regional_fao_value - adesa_per,
         diff_min = regional_fao_value - adesa_per_min,
         diff_max = regional_fao_value - adesa_per_max,
         per_diff = ((regional_fao_value - adesa_per) / regional_fao_value) * 100,
         per_diff_min = ((regional_fao_value - adesa_per_min) / regional_fao_value) * 100,
         per_diff_max = ((regional_fao_value - adesa_per_max) / regional_fao_value) * 100) %>%
  filter(GCAM_region_ID != "NA")

write.csv(ADESA_valid, "./Validation/adesa_validation.csv")
write.csv(ADESA_valid_2, "./Validation/adesa_validation_2.csv")

### Animal protein =============================================================
# Read in FAO data for comparison - three sources
animal_protein_supply_fao_sua <- read_fao_data("Animal protein SUA") %>%
  filter(fao_value != 0)

animal_protein_supply_fao_fbs <- read_fao_data("Animal protein FBS") %>%
  filter(fao_value != 0)

animal_protein_supply_fao_fsi <- read_fao_data("Animal protein supply") %>%
  filter(fao_value != 0) %>%
  mutate(year = case_when(year == "2004-2006" ~ "2005",
                          year == "2009-2011" ~ "2010",
                          year == "2014-2016" ~ "2015",
                          year == "2019-2021" ~ "2020",
                          T ~ year))

animal_protein_supply_fao_sua$year <- as.numeric(animal_protein_supply_fao_sua$year)
animal_protein_supply_fao_fbs$year <- as.numeric(animal_protein_supply_fao_fbs$year)
animal_protein_supply_fao_fsi$year <- as.numeric(animal_protein_supply_fao_fsi$year)

# Make NAs = 0
animal_protein_supply_fao_sua[is.na(animal_protein_supply_fao_sua)] <- 0
animal_protein_supply_fao_fbs[is.na(animal_protein_supply_fao_fbs)] <- 0
animal_protein_supply_fao_fsi[is.na(animal_protein_supply_fao_fsi)] <- 0

# Compute weighted values
animal_protein_fao_filter_sua <- filter_weight_fao(animal_protein_supply_fao_sua) %>%
  mutate(source = "SUA")
animal_protein_fao_filter_fbs <- filter_weight_fao(animal_protein_supply_fao_fbs) %>%
  mutate(source = "FBS")
animal_protein_fao_filter_fsi <- filter_weight_fao(animal_protein_supply_fao_fsi) %>%
  mutate(source = "FSI")

# Combine data
animal_protein_fao_all <- bind_rows(animal_protein_fao_filter_sua, animal_protein_fao_filter_fbs) %>%
  bind_rows(animal_protein_fao_filter_fsi)

# Join with GCAM output and compute diff/percent diff
animal_protein_valid <- left_join(mutate(animal_protein_supply_agg, year = as.numeric(year)),
                                  animal_protein_fao_all, by = c("region", "year"), relationship = "many-to-many") %>%
  filter(year %in% c(2010, 2015),
         GCAM_region_ID != "NA") %>%
  pivot_wider(names_from = "source", values_from = "regional_fao_value") %>%
  mutate(SUA_diff = SUA - value,
         SUA_per_diff = ((SUA - value) / SUA) * 100,
         FBS_diff = FBS - value,
         FBS_per_diff = ((FBS - value) / FBS) * 100,
         FSI_diff = FSI - value,
         FSI_per_diff = ((FSI - value) / FSI) * 100)

write.csv(animal_protein_valid,"./Validation/animal_protein_validation_all_sources.csv")

### Fat supply =================================================================
# Read in FAO data for comparison - three sources
fat_supply_fao_sua <- read_fao_data("Fat SUA") %>%
  filter(fao_value != 0)

fat_supply_fao_fbs <- read_fao_data("Fat FBS") %>%
  filter(fao_value != 0)

fat_supply_fao_fsi <- read_fao_data("Fat supply") %>%
  filter(fao_value != 0) %>%
  mutate(year = case_when(year == "2004-2006" ~ "2005",
                          year == "2009-2011" ~ "2010",
                          year == "2014-2016" ~ "2015",
                          year == "2019-2021" ~ "2020",
                          T ~ year))

fat_supply_fao_sua$year <- as.numeric(fat_supply_fao_sua$year)
fat_supply_fao_fbs$year <- as.numeric(fat_supply_fao_fbs$year)
fat_supply_fao_fsi$year <- as.numeric(fat_supply_fao_fsi$year)

# Make NAs = 0
fat_supply_fao_sua[is.na(fat_supply_fao_sua)] <- 0
fat_supply_fao_fbs[is.na(fat_supply_fao_fbs)] <- 0
fat_supply_fao_fsi[is.na(fat_supply_fao_fsi)] <- 0

# Compute weighted values
fat_fao_filter_sua <- filter_weight_fao(fat_supply_fao_sua) %>%
  mutate(source = "SUA")
fat_fao_filter_fbs <- filter_weight_fao(fat_supply_fao_fbs) %>%
  mutate(source = "FBS")
fat_fao_filter_fsi <- filter_weight_fao(fat_supply_fao_fsi) %>%
  mutate(source = "FSI")

# Combine data
fat_fao_all <- bind_rows(fat_fao_filter_sua, fat_fao_filter_fbs) %>%
  bind_rows(fat_fao_filter_fsi)

# Join with GCAM output and compute diff/percent diff
fat_valid <- left_join(mutate(fat_supply_agg, year = as.numeric(year)),
                       fat_fao_all, by = c("region", "year"), relationship = "many-to-many") %>%
  filter(year %in% c(2010, 2015),
         GCAM_region_ID != "NA") %>%
  pivot_wider(names_from = "source", values_from = "regional_fao_value") %>%
  mutate(SUA_diff = SUA - value,
         SUA_per_diff = ((SUA - value) / SUA) * 100,
         FBS_diff = FBS - value,
         FBS_per_diff = ((FBS - value) / FBS) * 100,
         FSI_diff = FSI - value,
         FSI_per_diff = ((FSI - value) / FSI) * 100)

write.csv(fat_valid,"./Validation/fat_validation_agg.csv")

### Protein supply =============================================================
# Read in FAO data for comparison - three sources
protein_supply_fao_sua <- read_fao_data("Protein SUA") %>%
  filter(fao_value != 0)

protein_supply_fao_fbs <- read_fao_data("Protein FBS") %>%
  filter(fao_value != 0)

protein_supply_fao_fsi <- read_fao_data("Protein supply") %>%
  filter(fao_value != 0) %>%
  mutate(year = case_when(year == "2004-2006" ~ "2005",
                          year == "2009-2011" ~ "2010",
                          year == "2014-2016" ~ "2015",
                          year == "2019-2021" ~ "2020",
                          T ~ year))

protein_supply_fao_sua$year <- as.numeric(protein_supply_fao_sua$year)
protein_supply_fao_fbs$year <- as.numeric(protein_supply_fao_fbs$year)
protein_supply_fao_fsi$year <- as.numeric(protein_supply_fao_fsi$year)

# Make NAs = 0
protein_supply_fao_sua[is.na(protein_supply_fao_sua)] <- 0
protein_supply_fao_fbs[is.na(protein_supply_fao_fbs)] <- 0
protein_supply_fao_fsi[is.na(protein_supply_fao_fsi)] <- 0

# Compute weighted values
protein_fao_filter_sua <- filter_weight_fao(protein_supply_fao_sua) %>%
  mutate(source = "SUA")
protein_fao_filter_fbs <- filter_weight_fao(protein_supply_fao_fbs) %>%
  mutate(source = "FBS")
protein_fao_filter_fsi <- filter_weight_fao(protein_supply_fao_fsi) %>%
  mutate(source = "FSI")

# Combine data
protein_fao_all <- bind_rows(protein_fao_filter_sua, protein_fao_filter_fbs) %>%
  bind_rows(protein_fao_filter_fsi)

# Join with GCAM output and compute diff/percent diff
protein_valid <- left_join(mutate(protein_supply_agg, year = as.numeric(year)),
                           protein_fao_all, by = c("region", "year"), relationship = "many-to-many") %>%
  filter(year %in% c(2010, 2015),
         GCAM_region_ID != "NA") %>%
  pivot_wider(names_from = "source", values_from = "regional_fao_value") %>%
  mutate(SUA_diff = SUA - value,
         SUA_per_diff = ((SUA - value) / SUA) * 100,
         FBS_diff = FBS - value,
         FBS_per_diff = ((FBS - value) / FBS) * 100,
         FSI_diff = FSI - value,
         FSI_per_diff = ((FSI - value) / FSI) * 100)

write.csv(protein_valid,"./Validation/protein_validation_all_sources.csv")

### Iron - percent anemia ======================================================
# Check against FAO anemia values
percent_anemic_fao <- read_fao_data("Percent of anemic women") %>%
  filter(fao_value != "NA")

percent_anemic_fao$year <- as.numeric(percent_anemic_fao$year)
percent_anemic_fao$fao_value <- as.numeric(percent_anemic_fao$fao_value)

percent_anemic_fao_filter <- filter_weight_fao(percent_anemic_fao)

percent_anemic_valid <- female_rep_iron %>%
  filter(year %in% c(2010, 2015)) %>%
  mutate(year = as.numeric(year)) %>%
  left_join(percent_anemic_fao_filter, by = c("region", "year")) %>%
  mutate(diff = regional_fao_value - percent_of_female_pop_below,
         per_diff = ((regional_fao_value - percent_of_female_pop_below) / regional_fao_value) * 100) %>%
  filter(GCAM_region_ID != "NA") %>%
  select(-number_of_deciles_below_iron, -number_of_females_below)

write.csv(percent_anemic_valid, "./Validation/anemia.csv")

### Cereal import dependency ratio =============================================
# Read in FAO data for comparison
cereal_fao <- read_fao_data("Cereal import dependency ratio") %>%
  mutate(year = case_when(year == "2004-2006" ~ "2005",
                          year == "2009-2011" ~ "2010",
                          year == "2014-2016" ~ "2015",
                          T ~ year)) %>%
  filter(fao_value != "NA")

cereal_fao$year <- as.numeric(cereal_fao$year)

# Filter and weight data
cereal_fao_filter <- filter_weight_fao(cereal_fao)

# Join with GCAM results
cereal_valid <- left_join(cereal,
                          cereal_fao_filter, by = c("region", "year")) %>%
  filter(year %in% c(2010, 2015))  %>%
  mutate(diff = regional_fao_value - percent,
         per_diff = ((regional_fao_value - percent) / regional_fao_value) * 100)

write.csv(cereal_valid, "./Validation/cereal_import_dep_ratio_validation.csv")

### Dietary energy supply ======================================================
# Read in FAO data for comparison - three sources
des_fao_sua <- read_fao_data("DES SUA") %>%
  filter(fao_value != 0)

des_fao_fbs <- read_fao_data("DES FBS") %>%
  filter(fao_value != 0)

des_fao_fsi <- read_fao_data("DES") %>%
  filter(fao_value != 0) %>%
  mutate(year = case_when(year == "2004-2006" ~ "2005",
                          year == "2009-2011" ~ "2010",
                          year == "2014-2016" ~ "2015",
                          year == "2019-2021" ~ "2020",
                          T ~ year))

des_fao_sua$year <- as.numeric(des_fao_sua$year)
des_fao_fbs$year <- as.numeric(des_fao_fbs$year)
des_fao_fsi$year <- as.numeric(des_fao_fsi$year)

# Make NAs = 0
des_fao_sua[is.na(des_fao_sua)] <- 0
des_fao_fbs[is.na(des_fao_fbs)] <- 0
des_fao_fsi[is.na(des_fao_fsi)] <- 0

# Compute weighted values
des_fao_filter_sua <- filter_weight_fao(des_fao_sua) %>%
  mutate(source = "SUA")
des_fao_filter_fbs <- filter_weight_fao(des_fao_fbs) %>%
  mutate(source = "FBS")
des_fao_filter_fsi <- filter_weight_fao(des_fao_fsi) %>%
  mutate(source = "FSI")

# Combine data
des_fao_all <- bind_rows(des_fao_filter_sua, des_fao_filter_fbs) %>%
  bind_rows(des_fao_filter_fsi)

# Join with GCAM output and compute diff/percent diff
des_valid <- left_join(mutate(dietary_energy_supply, year = as.numeric(year)),
                       des_fao_all, by = c("region", "year"), relationship = "many-to-many") %>%
  filter(year %in% c(2010, 2015),
         GCAM_region_ID != "NA") %>%
  pivot_wider(names_from = "source", values_from = "regional_fao_value") %>%
  mutate(SUA_diff = SUA - value,
         SUA_per_diff = ((SUA - value) / SUA) * 100,
         FBS_diff = FBS - value,
         FBS_per_diff = ((FBS - value) / FBS) * 100,
         FSI_diff = FSI - value,
         FSI_per_diff = ((FSI - value) / FSI) * 100)

write.csv(des_valid,"./Validation/des_validation_agg.csv")

### Share of staples in total ==================================================
# Read in FAO data for comparison
staples_fao <- read_fao_data("Share of staples in total") %>%
  mutate(year = case_when(year == "2004-2006" ~ "2005",
                          year == "2009-2011" ~ "2010",
                          year == "2014-2016" ~ "2015",
                          T ~ year)) %>%
  filter(fao_value != "NA")

staples_fao$year <- as.numeric(staples_fao$year)

# Filter and weight values
staples_fao_filter <- filter_weight_fao(staples_fao)

# MultipleConsumers
staples_agg_valid <- left_join(share_staples_agg, staples_fao_filter, by = c("region", "year")) %>%
  filter(year %in% c(2010, 2015))  %>%
  mutate(agg_staples_in_total = agg_staples_in_total * 100,
         diff = regional_fao_value - agg_staples_in_total,
         per_diff = ((regional_fao_value - agg_staples_in_total) / regional_fao_value) * 100)

# SingleConsumer
staples_valid <- left_join(share_diet_staples_region, staples_fao_filter, by = c("region", "year")) %>%
  filter(year %in% c(2010, 2015))  %>%
  mutate(staples_in_total = staples_in_total * 100,
         diff = regional_fao_value - staples_in_total,
         per_diff = ((regional_fao_value - staples_in_total) / regional_fao_value) * 100)

write.csv(staples_agg_valid, "./Validation/staples_agg_validation.csv")
write.csv(staples_valid, "./Validation/staples_validation.csv")

# Figures ======================================================================
## ADESA =======================================================================
adesa_plot <- ADESA_valid %>%
  rename(`GCAM minimum` = max_adesa,
         `GCAM maximum` = min_adesa,
         `GCAM mean` = adesa,
         `FSI` = regional_fao_value) %>%
  pivot_longer(cols = c(`GCAM minimum`, `GCAM maximum`, `GCAM mean`, `FSI`),
               names_to = "key", values_to = "values")

adesa_plot$key <- factor(adesa_plot$key, levels = c("GCAM minimum", "GCAM mean", "FSI", "GCAM maximum"))

# Plot range of ADESA values with FAO
adesa_fig_with_mean <- adesa_plot %>%
  mutate(year = as.factor(year)) %>%
  filter(scenario == "MultipleConsumers") %>%
  ggplot(aes(x = year, y = values, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~region, scales = "free_y") +
  scale_fill_discrete(type = c("aquamarine3", "black", "grey70", "aquamarine4")) +
  theme_bw() +
  labs(y = "ADESA",
       x = "Year",
       title = "ADESA validation") ; adesa_fig_with_mean

ggsave(plot = adesa_fig_with_mean, filename = "./Validation/figures/adesa_valid_bar_with_mean.jpg", height = 6, width = 12, units= "in")

# Same plot but without the GCAM mean, just min/max
adesa_fig <- adesa_plot %>%
  mutate(year = as.factor(year)) %>%
  filter(scenario == "MultipleConsumers",
         key != "GCAM mean") %>%
  ggplot(aes(x = year, y = values, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~region, scales = "free_y") +
  scale_fill_discrete(type = c("navy", "black", "aquamarine4")) +
  theme_bw() +
  labs(y = "ADESA",
       x = "Year",
       title = "ADESA validation") ; adesa_fig

ggsave(plot = adesa_fig, filename = "./Validation/figures/adesa_valid_bar.jpg", height = 6, width = 12, units= "in")


## Protein =====================================================================
# Isolate single consumer
p_onecons <- protein_oneconsumer %>%
  select(-gcam.consumer, -units) %>%
  filter(year %in% c(2010, 2015)) %>%
  rename(key = scenario, values = value)

# Plot GCAM protein against FAO
# Prep data
protein_plot <- protein_valid %>%
  select(-c(10:15)) %>%
  filter(scenario != "MultipleConsumers_2p6") %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  pivot_longer(cols = c("SUA", "FBS", "FSI", "MultipleConsumers"),
               names_to = "key", values_to = "values") %>%
  select(-GCAM_region_ID) %>%
  bind_rows(p_onecons)

protein_plot$key <- factor(protein_plot$key, levels = c("SUA", "FBS", "FSI", "SingleConsumer", "MultipleConsumers"),
                           labels = c("SUA", "FBS", "FSI", "SingleConsumer", "MultipleConsumers (aggregated)"))

protein_fig <- protein_plot %>%
  filter(region != "Taiwan") %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(x = year, y = values, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~region, scales = "free_y") +
  scale_fill_discrete(type = c("grey30", "grey50", "grey70", "navy", "aquamarine4")) +
  theme_bw() +
  labs(y = "Protein supply (g/capita/day)",
       x = "Year",
       title = "Protein validation") ; protein_fig

ggsave(plot = protein_fig, filename = "./Validation/figures/protein_valid_all_bar.jpg", height = 6, width = 12, units= "in")

#### All protein validation ====================================================
# Prep USDA data
protein_usda <- protein_oneconsumer_usda %>%
  select(scenario, region, year, value, data) %>%
  mutate(scenario = "SingleConsumer_USDA") %>%
  bind_rows(mutate(protein_supply_agg_usda,
                   year = as.numeric(year))) %>%
  select(-data) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  rename(`MultipleConsumers_USDA (agg)` = MultipleConsumers) %>%
  select(-MultipleConsumers_2p6) %>%
  pivot_longer(cols = c("SingleConsumer_USDA", "MultipleConsumers_USDA (agg)"), names_to = "key", values_to = "values")

all_protein_valid <- protein_plot %>%
  select(-data) %>%
  bind_rows(protein_usda) %>%
  filter(region != "Taiwan",
         year %in% c(2010, 2015)) %>%
  mutate(year = as.factor(year))

all_protein_valid$key <- factor(all_protein_valid$key,
                                levels = c("SUA", "FBS", "FSI", "SingleConsumer", "MultipleConsumers (aggregated)", "SingleConsumer_USDA", "MultipleConsumers_USDA (agg)"),
                                labels = c("SUA", "FBS", "FSI", "SingleConsumer", "MultipleConsumers (aggregated)", "SingleConsumer (USDA)", "MultipleConsumers (USDA, aggregated)"))
# Plot all sources of protein
all_protein_valid_plot <- all_protein_valid %>%
  ggplot(aes(x = year, y = values, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~region, scales = "free_y") +
  scale_fill_discrete(type = c("grey30", "grey50", "grey70", "navy", "aquamarine4", "firebrick4", "darkmagenta")) +
  theme_bw() +
  labs(y = "Protein supply (g/capita/day)",
       x = "Year",
       title = "Protein validation") ; all_protein_valid_plot

ggsave(plot = all_protein_valid_plot, "./figures/usda/protein_comparison.jpg", height = 9, width = 18, units = "in")

### NEW protein supply =========================================================
# Recompute protein supply with new conversion factors
# SUM_commodity(protein/kg_c * consumption_c) / population
protein_supply_by_commodity_new <- consum_macronutrient_new %>%
  select(1:5, 9:14, 16, 17, grams_protein_per_kcal_comm) %>%
  filter(protein_per_100g != 0) %>%
  mutate(commodity_cals = commodity_cals * 1e12, # convert Pcal to kcal
         Units = "kcal") %>%
  group_by(scenario, region, year, commodity, gcam.consumer) %>%
  reframe(numerator = (as.numeric(grams_protein_per_kcal_comm) * commodity_cals), # protein (g per kcal) * calories
          denominator = population,
          protein_supply = numerator / (denominator * 365), # convert year to day
          units = "g/capita/day",
          data = "FAO") %>%
  select(-numerator, -denominator)

# Total regional values
protein_supply_new <- protein_supply_by_commodity_new %>%
  group_by(scenario, region, year, gcam.consumer) %>%
  # Sum protein supply by commodity to get total protein supply for each region
  reframe(protein_agg = sum(protein_supply),
          units = units) %>%
  distinct() %>%
  mutate(year = as.character(year))

# Adjust labels for plotting
protein_supply_by_commodity_new$gcam.consumer <- factor(protein_supply_by_commodity_new$gcam.consumer,
                                                        levels = c("FoodDemand_Group1", "FoodDemand_Group2", "FoodDemand_Group3",
                                                                   "FoodDemand_Group4", "FoodDemand_Group5", "FoodDemand_Group6",
                                                                   "FoodDemand_Group7", "FoodDemand_Group8", "FoodDemand_Group9",
                                                                   "FoodDemand_Group10", "Average Regional MDER"))

protein_supply_new$gcam.consumer <- factor(protein_supply_new$gcam.consumer,
                                           levels = c("FoodDemand_Group1", "FoodDemand_Group2", "FoodDemand_Group3",
                                                      "FoodDemand_Group4", "FoodDemand_Group5", "FoodDemand_Group6",
                                                      "FoodDemand_Group7", "FoodDemand_Group8", "FoodDemand_Group9",
                                                      "FoodDemand_Group10", "Average Regional MDER"))

# Aggregate ten consumers to one
protein_supply_agg_new <- protein_supply_new %>%
  filter(scenario == "MultipleConsumers") %>%
  group_by(scenario, region, year) %>%
  summarize(value = sum(protein_agg) / 10,
            data = "FAO") %>%
  mutate(scenario = "MultipleConsumers (new values)")

protein <- mutate(protein_supply_agg_new, year = as.numeric(year)) %>%
  bind_rows(mutate(filter(protein_supply_agg, scenario == "MultipleConsumers"), year = as.numeric(year)))

# Join with GCAM output and compute diff/percent diff
protein_valid_new <- left_join(protein, protein_fao_all, by = c("region", "year"), relationship = "many-to-many") %>%
  filter(year %in% c(2010, 2015),
         GCAM_region_ID != "NA") %>%
  pivot_wider(names_from = "source", values_from = "regional_fao_value") %>%
  mutate(SUA_diff = SUA - value,
         SUA_per_diff = ((SUA - value) / SUA) * 100,
         FBS_diff = FBS - value,
         FBS_per_diff = ((FBS - value) / FBS) * 100,
         FSI_diff = FSI - value,
         FSI_per_diff = ((FSI - value) / FSI) * 100)

# Join with FAO
protein_plot_new <- protein_valid_new %>%
  select(-c(10:15)) %>%
  filter(scenario != "MultipleConsumers_2p6") %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  pivot_longer(cols = c("SUA", "FBS", "FSI", `MultipleConsumers (new values)`, "MultipleConsumers"),
               names_to = "key", values_to = "values") %>%
  select(-GCAM_region_ID)

protein_plot_new$key <- factor(protein_plot_new$key, levels = c("SUA", "FBS", "FSI", "MultipleConsumers (new values)", "MultipleConsumers"),
                               labels = c("SUA", "FBS", "FSI", "MultipleConsumers (new values)", "MultipleConsumers (old values)"))

# Plot
protein_fig_new <- protein_plot_new %>%
  filter(region != "Taiwan") %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(x = year, y = values, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~region, scales = "free_y") +
  scale_fill_discrete(type = c("grey30", "grey50", "grey70", "navy", "aquamarine4")) +
  theme_bw() +
  labs(y = "Protein supply (g/capita/day)",
       x = "Year",
       title = "Protein validation") ; protein_fig_new

ggsave(plot = protein_fig_new, filename = "./figures/conversions/protein_valid_all_bar.jpg", height = 6, width = 12, units= "in")

## Animal protein ==============================================================
# Repeat for animal protein
# Isolate single consumer
ap_onecons <- animal_protein_oneconsumer %>%
  select(-gcam.consumer, -units) %>%
  filter(year %in% c(2010, 2015)) %>%
  rename(key = scenario, values = value)

# Plot GCAM animal protein against FAO
animal_protein_plot <- animal_protein_valid %>%
  select(-c(10:15)) %>%
  filter(scenario == "MultipleConsumers") %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  pivot_longer(cols = c("SUA", "FBS", "FSI", "MultipleConsumers"),
               names_to = "key", values_to = "values") %>%
  select(-GCAM_region_ID) %>%
  bind_rows(ap_onecons)

animal_protein_plot$key <- factor(animal_protein_plot$key, levels = c("SUA", "FBS", "FSI", "SingleConsumer", "MultipleConsumers"),
                                  labels = c("SUA", "FBS", "FSI", "SingleConsumer", "MultipleConsumers (aggregated)"))

animal_protein_fig <- animal_protein_plot %>%
  filter(region != "Taiwan") %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(x = year, y = values, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~region, scales = "free_y") +
  scale_fill_discrete(type = c("grey30", "grey50", "grey70", "navy", "aquamarine4")) +
  theme_bw() +
  labs(y = "Animal protein supply (g/capita/day)",
       x = "Year",
       title = "Animal protein validation") ; animal_protein_fig

ggsave(plot = animal_protein_fig, filename = "./Validation/figures/animal_protein_valid_all_bar.jpg", height = 6, width = 12, units= "in")

#### All animal protein validation =============================================
# Get USDA values
animal_protein_usda <- animal_protein_oneconsumer_usda %>%
  select(scenario, region, year, value, data) %>%
  mutate(scenario = "SingleConsumer_USDA") %>%
  bind_rows(mutate(animal_protein_supply_agg_usda,
                   year = as.numeric(year))) %>%
  select(-data) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  rename(`MultipleConsumers_USDA (agg)` = MultipleConsumers) %>%
  select(-MultipleConsumers_2p6) %>%
  pivot_longer(cols = c("SingleConsumer_USDA", "MultipleConsumers_USDA (agg)"), names_to = "key", values_to = "values")

# Plot GCAM, FAO, and USDA values
all_animal_protein_valid <- animal_protein_plot %>%
  select(-data) %>%
  bind_rows(animal_protein_usda) %>%
  filter(region != "Taiwan",
         year %in% c(2010, 2015)) %>%
  mutate(year = as.factor(year))

all_animal_protein_valid$key <- factor(all_animal_protein_valid$key,
                                       levels = c("SUA", "FBS", "FSI", "SingleConsumer", "MultipleConsumers (aggregated)", "SingleConsumer_USDA", "MultipleConsumers_USDA (agg)"),
                                       labels = c("SUA", "FBS", "FSI", "SingleConsumer", "MultipleConsumers (aggregated)", "SingleConsumer (USDA)", "MultipleConsumers (USDA, aggregated)"))

all_animal_protein_valid_plot <- all_animal_protein_valid %>%
  ggplot(aes(x = year, y = values, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~region, scales = "free_y") +
  scale_fill_discrete(type = c("grey30", "grey50", "grey70", "navy", "aquamarine4", "firebrick4", "darkmagenta")) +
  theme_bw() +
  labs(y = "Protein supply (g/capita/day)",
       x = "Year",
       title = "Animal Protein validation") ; all_animal_protein_valid_plot

ggsave(plot = all_animal_protein_valid_plot, "./figures/usda/animal_protein_comparison.jpg", height = 9, width = 18, units = "in")

### NEW Animal protein supply ==================================================
animal_protein_supply_by_commodity_new <- consum_macronutrient_new %>%
  select(1:5, 9:14, 16, 17, grams_protein_per_kcal_comm) %>%
  filter(protein_per_100g != 0,
         commodity %in% animal_commodities) %>%
  mutate(commodity_cals = commodity_cals * 1e12, # convert Pcal to kcal
         Units = "kcal") %>%
  group_by(scenario, region, year, commodity, gcam.consumer) %>%
  reframe(numerator = (as.numeric(grams_protein_per_kcal_comm) * commodity_cals), # protein (g per kcal) * calories
          denominator = population,
          animal_protein_supply = numerator / (denominator * 365), # convert year to day
          units = "g/capita/day",
          data = "FAO") %>%
  select(-numerator, -denominator)

# Total regional values
animal_protein_supply_new <- animal_protein_supply_by_commodity_new %>%
  group_by(scenario, region, year, gcam.consumer) %>%
  # Sum protein supply by commodity to get total protein supply for each region
  reframe(value = sum(animal_protein_supply),
          units = units) %>%
  distinct() %>%
  mutate(year = as.character(year))

# Adjust labels for plotting
animal_protein_supply_by_commodity_new$gcam.consumer <- factor(animal_protein_supply_by_commodity_new$gcam.consumer,
                                                               levels = c("FoodDemand_Group1", "FoodDemand_Group2", "FoodDemand_Group3",
                                                                          "FoodDemand_Group4", "FoodDemand_Group5", "FoodDemand_Group6",
                                                                          "FoodDemand_Group7", "FoodDemand_Group8", "FoodDemand_Group9",
                                                                          "FoodDemand_Group10", "Average Regional MDER"))

animal_protein_supply_new$gcam.consumer <- factor(animal_protein_supply_new$gcam.consumer,
                                                  levels = c("FoodDemand_Group1", "FoodDemand_Group2", "FoodDemand_Group3",
                                                             "FoodDemand_Group4", "FoodDemand_Group5", "FoodDemand_Group6",
                                                             "FoodDemand_Group7", "FoodDemand_Group8", "FoodDemand_Group9",
                                                             "FoodDemand_Group10", "Average Regional MDER"))
# Aggregate ten consumers to one
animal_protein_supply_agg_new <- animal_protein_supply_new %>%
  filter(scenario == "MultipleConsumers") %>%
  group_by(scenario, region, year) %>%
  summarize(value = sum(value) / 10,
            data = "FAO") %>%
  mutate(scenario = "MultipleConsumers (new values)")

animal <- mutate(animal_protein_supply_agg_new, year = as.numeric(year)) %>%
  bind_rows(mutate(filter(animal_protein_supply_agg, scenario == "MultipleConsumers"), year = as.numeric(year)))

# Join with GCAM output and compute diff/percent diff
animal_protein_valid_new <- left_join(animal,
                                      animal_protein_fao_all, by = c("region", "year"), relationship = "many-to-many") %>%
  filter(year %in% c(2010, 2015),
         GCAM_region_ID != "NA") %>%
  pivot_wider(names_from = "source", values_from = "regional_fao_value") %>%
  mutate(SUA_diff = SUA - value,
         SUA_per_diff = ((SUA - value) / SUA) * 100,
         FBS_diff = FBS - value,
         FBS_per_diff = ((FBS - value) / FBS) * 100,
         FSI_diff = FSI - value,
         FSI_per_diff = ((FSI - value) / FSI) * 100)

# Plot old vs new GCAM values against FAO
animal_protein_plot_new <- animal_protein_valid_new %>%
  select(-c(10:15)) %>%
  filter(scenario != "MultipleConsumers_2p6") %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  pivot_longer(cols = c("SUA", "FBS", "FSI", `MultipleConsumers (new values)`, "MultipleConsumers"),
               names_to = "key", values_to = "values") %>%
  select(-GCAM_region_ID)


animal_protein_plot_new$key <- factor(animal_protein_plot_new$key, levels = c("SUA", "FBS", "FSI", "MultipleConsumers (new values)", "MultipleConsumers"),
                                      labels = c("SUA", "FBS", "FSI", "MultipleConsumers (new values)", "MultipleConsumers (old values)"))

animal_protein_fig_new <- animal_protein_plot_new %>%
  filter(region != "Taiwan") %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(x = year, y = values, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~region, scales = "free_y") +
  scale_fill_discrete(type = c("grey30", "grey50", "grey70", "navy", "aquamarine4")) +
  theme_bw() +
  labs(y = "Animal protein supply (g/capita/day)",
       x = "Year",
       title = "Animal protein validation") ; animal_protein_fig_new

ggsave(plot = animal_protein_fig_new, filename = "./figures/conversions/animal_protein_valid_all_bar.jpg", height = 6, width = 12, units= "in")

## Fat supply ==================================================================
# Compare GCAM to FAO for fat supply
fat_onecons <- fat_oneconsumer %>%
  select(-gcam.consumer, -units) %>%
  filter(year %in% c(2010, 2015)) %>%
  rename(key = scenario, values = value)

fat_plot <- fat_valid %>%
  select(-c(10:15)) %>%
  filter(scenario != "MultipleConsumers_2p6") %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  pivot_longer(cols = c("SUA", "FBS", "FSI", "MultipleConsumers"),
               names_to = "key", values_to = "values") %>%
  select(-GCAM_region_ID) %>%
  bind_rows(fat_onecons)

fat_plot$key <- factor(fat_plot$key, levels = c("SUA", "FBS", "FSI", "SingleConsumer", "MultipleConsumers"),
                       labels = c("SUA", "FBS", "FSI", "SingleConsumer", "MultipleConsumers (aggregated)"))

fat_fig <- fat_plot %>%
  filter(region != "Taiwan") %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(x = year, y = values, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~region, scales = "free_y") +
  scale_fill_discrete(type = c("grey30", "grey50", "grey70", "navy", "aquamarine4")) +
  theme_bw() +
  labs(y = "Fat supply (g/capita/day)",
       x = "Year",
       title = "Fat validation") ; animal_protein_fig

ggsave(plot = fat_fig, filename = "./Validation/figures/fat_valid_all_bar.jpg", height = 6, width = 12, units= "in")

#### All fat validation ========================================================
# Bring in USDA data
fat_usda <- fat_oneconsumer_usda %>%
  select(scenario, region, year, value, data) %>%
  mutate(scenario = "SingleConsumer_USDA") %>%
  bind_rows(mutate(fat_supply_agg_usda,
                   year = as.numeric(year))) %>%
  select(-data) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  rename(`MultipleConsumers_USDA (agg)` = MultipleConsumers) %>%
  select(-MultipleConsumers_2p6) %>%
  pivot_longer(cols = c("SingleConsumer_USDA", "MultipleConsumers_USDA (agg)"), names_to = "key", values_to = "values")

all_fat_valid <- fat_plot %>%
  select(-data) %>%
  bind_rows(fat_usda) %>%
  filter(region != "Taiwan",
         year %in% c(2010, 2015)) %>%
  mutate(year = as.factor(year))

all_fat_valid$key <- factor(all_fat_valid$key,
                            levels = c("SUA", "FBS", "FSI", "SingleConsumer", "MultipleConsumers (aggregated)", "SingleConsumer_USDA", "MultipleConsumers_USDA (agg)"),
                            labels = c("SUA", "FBS", "FSI", "SingleConsumer", "MultipleConsumers (aggregated)", "SingleConsumer (USDA)", "MultipleConsumers (USDA, aggregated)"))

all_fat_valid_plot <- all_fat_valid %>%
  ggplot(aes(x = year, y = values, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~region, scales = "free_y") +
  scale_fill_discrete(type = c("grey30", "grey50", "grey70", "navy", "aquamarine4", "firebrick4", "darkmagenta")) +
  theme_bw() +
  labs(y = "Fat supply (g/capita/day)",
       x = "Year",
       title = "Fat validation") ; all_fat_valid_plot

ggsave(plot = all_fat_valid_plot, "./figures/usda/fat_comparison.jpg", height = 9, width = 18, units = "in")

### NEW fat supply =============================================================
# SUM_commodity(fat/kg_c * consumption_c) / population
fat_supply_by_commodity_new <- consum_macronutrient_new %>%
  filter(scenario == "MultipleConsumers") %>%
  select(1:5, 9:15, 17:18) %>%
  filter(fat_per_100g != 0) %>%
  mutate(commodity_cals = commodity_cals * 1e12, # convert Pcal to kcal
         Units = "kcal") %>%
  group_by(scenario, region, year, commodity, gcam.consumer) %>%
  reframe(numerator = (as.numeric(grams_fat_per_kcal_comm) * commodity_cals), # protein (g per kcal) * calories
          denominator = population,
          fat_supply = numerator / (denominator * 365), # convert year to day
          units = "g/capita/day",
          data = "FAO") %>%
  select(-numerator, -denominator)

# Total regional values
fat_supply_new <- fat_supply_by_commodity_new %>%
  group_by(scenario, region, year, gcam.consumer) %>%
  # Sum protein supply by commodity to get total protein supply for each region
  reframe(fat_agg = sum(fat_supply),
          units = units) %>%
  distinct() %>%
  mutate(year = as.character(year),
         data = "FAO")

# Adjust levels for plotting
fat_supply_by_commodity_new$gcam.consumer <- factor(fat_supply_by_commodity_new$gcam.consumer,
                                                    levels = c("FoodDemand_Group1", "FoodDemand_Group2", "FoodDemand_Group3",
                                                               "FoodDemand_Group4", "FoodDemand_Group5", "FoodDemand_Group6",
                                                               "FoodDemand_Group7", "FoodDemand_Group8", "FoodDemand_Group9",
                                                               "FoodDemand_Group10"))
fat_supply_new$gcam.consumer <- factor(fat_supply_new$gcam.consumer,
                                       levels = c("FoodDemand_Group1", "FoodDemand_Group2", "FoodDemand_Group3",
                                                  "FoodDemand_Group4", "FoodDemand_Group5", "FoodDemand_Group6",
                                                  "FoodDemand_Group7", "FoodDemand_Group8", "FoodDemand_Group9",
                                                  "FoodDemand_Group10"))

# Aggregate ten consumers to one
fat_supply_agg_new <- fat_supply_new %>%
  filter(scenario == "MultipleConsumers") %>%
  group_by(scenario, region, year) %>%
  summarize(value = sum(fat_agg) / 10,
            data = "FAO") %>%
  mutate(scenario = "MultipleConsumers (new values)")

fat <- mutate(fat_supply_agg_new, year = as.numeric(year)) %>%
  bind_rows(mutate(filter(fat_supply_agg, scenario == "MultipleConsumers"), year = as.numeric(year)))

# Join with GCAM output and compute diff/percent diff
fat_valid_new <- left_join(fat, fat_fao_all, by = c("region", "year"), relationship = "many-to-many") %>%
  filter(year %in% c(2010, 2015),
         GCAM_region_ID != "NA") %>%
  pivot_wider(names_from = "source", values_from = "regional_fao_value") %>%
  mutate(SUA_diff = SUA - value,
         SUA_per_diff = ((SUA - value) / SUA) * 100,
         FBS_diff = FBS - value,
         FBS_per_diff = ((FBS - value) / FBS) * 100,
         FSI_diff = FSI - value,
         FSI_per_diff = ((FSI - value) / FSI) * 100)

fat_plot_new <- fat_valid_new %>%
  select(-c(10:15)) %>%
  filter(scenario != "MultipleConsumers_2p6") %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  pivot_longer(cols = c("SUA", "FBS", "FSI", `MultipleConsumers (new values)`, "MultipleConsumers"),
               names_to = "key", values_to = "values") %>%
  select(-GCAM_region_ID)


fat_plot_new$key <- factor(fat_plot_new$key, levels = c("SUA", "FBS", "FSI", "MultipleConsumers (new values)", "MultipleConsumers"),
                           labels = c("SUA", "FBS", "FSI", "MultipleConsumers (new values)", "MultipleConsumers (old values)"))

# Plot old and new GCAM values vs FAO
fat_fig_new <- fat_plot_new %>%
  filter(region != "Taiwan") %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(x = year, y = values, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~region, scales = "free_y") +
  scale_fill_discrete(type = c("grey30", "grey50", "grey70", "navy", "aquamarine4")) +
  theme_bw() +
  labs(y = "Fat supply (g/capita/day)",
       x = "Year",
       title = "Fat validation") ; fat_fig_new

ggsave(plot = fat_fig_new, filename = "./figures/conversions/fat_valid_all_bar.jpg", height = 6, width = 12, units= "in")

## DES =========================================================================
# Calorie validation - GCAM vs FAO
des_plot <- des_valid %>%
  select(-c(11:16)) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  select(-MultipleConsumers_2p6) %>%
  pivot_longer(cols = c("SUA", "FBS", "FSI", "SingleConsumer", "MultipleConsumers"),
               names_to = "key", values_to = "values")

des_plot$key <- factor(des_plot$key, levels = c("SUA", "FBS", "FSI", "SingleConsumer", "MultipleConsumers"),
                       labels = c("SUA", "FBS", "FSI", "SingleConsumer", "MultipleConsumers (aggregated)"))

des_fig <- des_plot %>%
  filter(region != "Taiwan") %>%
  ggplot(aes(x = as.character(year), y = values, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~region, scales = "free_y") +
  scale_fill_discrete(type = c("grey30", "grey50", "grey70", "navy", "aquamarine4")) +
  theme_bw() +
  labs(y = "Dietary energy supply (kcal/capita/day)",
       x = "Year",
       title = "Dietary energy supply validation") +
  theme(axis.text.x = element_text(angle = 90)) ; des_fig

ggsave(plot = des_fig, filename = "./Validation/figures/des_valid_bar.jpg", height = 6, width = 12, units= "in")

# Just show multiple consumers version
des_fig_no_single <- des_plot %>%
  filter(region != "Taiwan",
         key != "SingleConsumer") %>%
  ggplot(aes(x = as.character(year), y = values, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~region, scales = "free_y") +
  scale_fill_discrete(type = c("grey30", "grey50", "grey70", "aquamarine4")) +
  theme_bw() +
  labs(y = "Dietary energy supply (kcal/capita/day)",
       x = "Year",
       title = "Dietary energy supply validation") +
  theme(axis.text.x = element_text(angle = 90)) ; des_fig_no_single

ggsave(plot = des_fig_no_single, filename = "./Validation/figures/des_valid_no_single_bar.jpg", height = 6, width = 12, units= "in")

## Cereal import dep ratio =====================================================
# GCAM values vs FAO
cereal_plot <- cereal_valid %>%
  pivot_wider(names_from = scenario, values_from = percent) %>%
  select(-production, -consumption, -ratio, -MultipleConsumers_2p6) %>%
  rename(`FAO results` = regional_fao_value) %>%
  pivot_longer(cols = c("FAO results", "SingleConsumer", "MultipleConsumers"),
               names_to = "key", values_to = "values")

cereal_fig <- cereal_plot %>%
  filter(region != "Taiwan") %>%
  ggplot(aes(x = as.character(year), y = values, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~region, scales = "free_y") +
  scale_fill_discrete(type = c("black", "navy", "aquamarine4")) +
  theme_bw() +
  labs(y = "Ratio",
       x = "Year",
       title = "Cereal import dependency ratio") +
  theme(axis.text.x = element_text(angle = 90)) ; cereal_fig

ggsave(plot = cereal_fig, filename = "./Validation/figures/cereal_valid_test.jpg", height = 6, width = 12, units= "in")

## Share of staples in total ===================================================
# GCAM vs FAO
shares_fao <- staples_fao_filter %>%
  rename(value = regional_fao_value) %>%
  mutate(scenario = "FAO data")

shares_combined <- staples_valid %>%
  filter(scenario == "SingleConsumer") %>%
  rename(value = staples_in_total) %>%
  bind_rows(rename(staples_agg_valid, value = agg_staples_in_total)) %>%
  select(-nonstaples_in_total, -percent_staples_in_total, -diff, -per_diff, -Units) %>%
  select(-regional_fao_value) %>%
  bind_rows(shares_fao) %>%
  filter(scenario != "MultipleConsumers_2p6")

shares_combined$scenario <- factor(shares_combined$scenario, levels = c("FAO data", "SingleConsumer", "MultipleConsumers"),
                                   labels = c("FSI", "SingleConsumer", "MultipleConsumers (aggregated)"))

staples_fig <- shares_combined %>%
  filter(region != "Taiwan") %>%
  ggplot(aes(x = as.character(year), y = value, fill = scenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~region, scales = "free_y") +
  scale_fill_discrete(type = c("grey70", "navy", "aquamarine4")) +
  theme_bw() +
  labs(y = "Share of staples (%)",
       x = "Year",
       title = "Share of staples in total calories validation") +
  theme(axis.text.x = element_text(angle = 90)) ; staples_fig

ggsave(plot = staples_fig, filename = "./Validation/figures/staples_valid_bar.jpg", height = 6, width = 12, units= "in")

## Share of food expenditure of poor ===========================================
# GCAM vs FAO
share_poor_plot <- food_expenditure_shares %>%
  filter(scenario == "MultipleConsumers", year == 2020) %>%
  mutate(gcam.consumer = case_when(grepl("1", gcam.consumer) ~ "d1",
                                   grepl("2", gcam.consumer) ~ "d2",
                                   grepl("3", gcam.consumer) ~ "d3",
                                   grepl("4", gcam.consumer) ~ "d4",
                                   grepl("5", gcam.consumer) ~ "d5",
                                   T~gcam.consumer))

share_poor_plot$input <- factor(share_poor_plot$input, levels = c("Staples", "NonStaples"))

share_poor_fig <- share_poor_plot %>%
  ggplot(aes(x = gcam.consumer, y = value * 100, fill = input)) +
  geom_bar(stat = "identity") +
  facet_wrap(~region, scales = "free") +
  scale_fill_manual(values = colors_10) +
  theme_bw() +
  labs(x = "Income decile",
       y = "Share of food expenditure (%)",
       title = paste0("Share of food expenditure of the poor in ", unique(share_poor_plot$year))) ; share_poor_fig

ggsave(plot = share_poor_fig, filename = paste0("./figures/share_food_exp_poor_", unique(share_poor_plot$year), ".jpg"), height = 6, width = 12, units= "in")
