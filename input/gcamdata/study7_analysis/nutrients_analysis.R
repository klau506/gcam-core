# Staple commodities
staples <- c("Corn", "OtherGrain", "Rice", "RootTuber", "Wheat")

# Animal commodities
animal_commodities <- c("Beef", "Dairy", "OtherMeat_Fish", "Pork", "Poultry", "SheepGoat")

# Datasets
gcam_regions <- read.csv(paste0(folder_analysis_path,"data/iso_GCAM_regID.csv"), skip = 6)
ssp_data <- read.csv(paste0(folder_analysis_path,"data/SSP2 population by demographic.csv"), skip = 1)


# Food Demand By Group
fd <- getQuery(prj, "food demand") %>%
  filter(year >= year_s, year <= year_e) %>%
  select(-input) %>%
  group_by(scenario, region, Units, year, nodeinput) %>%
  summarize(Total = sum(value))

# Pull out staple and non-staple calories
staples_nonstaples <- getQuery(prj, "food demand") %>%
  filter(year >= year_s, year <= year_e) %>%
  mutate(input = gsub("FoodDemand_", "", input)) %>%
  tidyr::pivot_wider(names_from = input, values_from = value) %>%
  left_join(fd)


# Define constants and commonly used items -------------------------------------
# Map countries to GCAM regions
id_to_region <- read.csv(paste0(folder_analysis_path,"data/gcam_id_to_region.csv")) %>%
  rename(GCAM_region_ID = "ï..GCAM_region_ID")
country_id <- read.csv(paste0(folder_analysis_path,"data/country_to_gcam_id.csv"))

regions_key <- left_join(country_id, id_to_region, by = "GCAM_region_ID") %>%
  select(-1)

# Country to GCAM region ID
country_to_id <- country_id %>%
  select(-1) %>%
  rename(region = country_name)

# Get GCAM regions and SSP population data
gcam_regions <- read.csv(paste0(folder_analysis_path,"data/iso_GCAM_regID.csv"), skip = 6)
ssp_data <- read.csv(paste0(folder_analysis_path,"data/SSP2 population by demographic.csv"), skip = 1)

# Population by region
pop_all_regions <- getQuery(prj, "population by region") %>%
  filter(year >= year_s, year <= year_e) %>%
  mutate(value = value * 1000) %>% # Convert from thous ppl to total ppl
  select(-Units) %>%
  rename(population = value)

# Read in MDER (calculated exogenously, FAO data)
mder <- read.csv(paste0(folder_analysis_path,"data/MDER.csv")) %>%
  rename(variable = ï..variable,
         mder_units = unit)


## Dietary energy supply =======================================================
# Units: kcal/capita/day

# By region
# Get total consumption in calories
dietary_energy_supply <- getQuery(prj, "food consumption by type (general)") %>%
  filter(year >= year_s, year <= year_e) %>%
  group_by(scenario, region, year) %>%
  # Aggregate staple and non-staple calories
  summarize(value = sum(value)) %>%
  left_join(pop_all_regions, by = c("year", "scenario", "region")) %>%
  # Convert from Pcal to kcal/capita/day
  mutate(value = (value * 1e12) / (population * 365),
         units = "kcal/capita/day")

# Write outputs
write.csv(dietary_energy_supply, paste0(tmp_output_data_path,"/dietary_energy_supply.csv"))


## Share of dietary energy supply from staples =================================
# Find share of staple and non-staple calories in total calories
share_dietary_from_staples <- staples_nonstaples %>%
  mutate(staples_in_total = Staples/Total,
         nonstaples_in_total = NonStaples/Total,
         percent_staples_in_total = staples_in_total * 100)

# Average over region
share_diet_staples_region <- share_dietary_from_staples %>%
  filter(scenario == "SingleConsumer")

write.csv(share_diet_staples_region, paste0(tmp_output_data_path,"/share_des_staples_region.csv"))



## Average dietary supply adequacy =============================================

# We need to isolate population by sex and age for each region
# First, we need to take the SSP data and process

## SSP2 demographic data processing ===========================================
# Read in SSP data, remove unneeded columns, join with country identifiers
ssp_data_clean <- gcam_regions %>%
  select(-region_GCAM3, -GCAM_region_ID) %>%
  left_join(ssp_data, by = "iso") %>%
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
  # Get total regional population
  group_by(year, GCAM_region_ID, country_name, region) %>%
  # Isolate total population by country
  distinct(total_pop) %>%
  group_by(year, GCAM_region_ID, region) %>%
  # Sum for total regional population
  mutate(total_regional_pop = sum(total_pop)) %>%
  ungroup()

weighted_pop <- ssp_data_final %>%
  filter(year >= year_s, year <= year_e) %>%
  select(-scenario, -iso) %>%
  # Get GCAM regions instead of country names
  left_join(regions_key, by = "country_name") %>%
  # Get total regional population
  left_join(total_regional_pop) %>%
  # Weight each country by its population over total regional pop
  group_by(country_name, year) %>%
  mutate(weight = total_pop / total_regional_pop) %>%
  # Get GCAM population
  left_join(mutate(pop_all_regions, year = as.character(year)), by = c("region", "year"),
            multiple = "all") %>%
  # Compute GCAM population by sex and age for each country
  mutate(weighted_demographics = demo_share * weight * population)

weighted_pop_sex_age <- weighted_pop %>%
  select(-pop_units, -sub_pop, -total_pop, -demo_share, -weight) %>%
  group_by(variable, year, region) %>%
  # Sum the weighted averages for each country into GCAM regions
  summarize(pop_sex_age = sum(weighted_demographics))

# Join with MDER data, calculate caloric requirements by sex and age
weighted_pop_mder <- weighted_pop_sex_age %>%
  left_join(mder, by = "variable") %>%
  select(-std) %>%
  group_by(variable, year, region) %>%
  # compute a range because of differing physical activity levels
  summarize(cal_req_x_pop = mder * pop_sex_age,
            min_cal_req_x_pop = min * pop_sex_age,
            max_cal_req_x_pop = max * pop_sex_age)

# Aggregate caloric requirements to get total regional values
adesa_denominator <- weighted_pop_mder %>%
  group_by(region, year) %>%
  summarize(denominator_sum = sum(cal_req_x_pop),
            min_denominator_sum = sum(min_cal_req_x_pop),
            max_denominator_sum = sum(max_cal_req_x_pop)) %>%
  mutate(year = as.numeric(year))

# Add in regional calorie info, calculate ADESA
adesa <- left_join(adesa_denominator, total_regional_calories) %>%
  # select(-population) %>%
  group_by(year, region, scenario) %>%
  reframe(adesa = (value / denominator_sum) * population * 100, # convert to unitless and percentage
          min_adesa = (value / min_denominator_sum) * population * 100,
          max_adesa = (value / max_denominator_sum) * population * 100,
          .groups = "keep")

# Method two
# ADESA = DES/MDER
# This gives us a percentage value for quick comparison with FAO
regional_mder <- weighted_pop_sex_age %>%
  left_join(mder, by = "variable") %>%
  group_by(region) %>%
  summarize(avg_mder = mean(mder),
            avg_min = mean(min),
            avg_max = mean(max))

adesa_method2 <- left_join(dietary_energy_supply, regional_mder) %>%
  select(-population)  %>%
  group_by(region, scenario, year) %>%
  reframe(adesa_per = (value * 100) / avg_mder,
          adesa_per_min = (value * 100) / avg_min,
          adesa_per_max = (value * 100)/ avg_max,
          units = "%")

# Write outputs
write.csv(adesa, paste0(tmp_output_data_path,"/average_des_adequacy.csv"))
write.csv(adesa_method2, paste0(tmp_output_data_path,"/average_des_adequacy_percent.csv"))

## Macronutrients ==============================================================
# Read in gcamdata file and change NAs to 0
# This comes from module_aglu_L100.FAO_SUA_connection
gcamdata_macro <- read.csv(paste0(folder_analysis_path,"data/gcamdata_macronutrient.csv"))
gcamdata_macro[is.na(gcamdata_macro)] <- 0

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

consumption_cals <- getQuery(prj, "food consumption by type (specific)") %>%
  select(-4, -5) %>%
  filter(year >= year_s, year <= year_e) %>%
  left_join(filter(staples_nonstaples),
            by = c("scenario", "region", "year")) %>%
  select(-7, -8, -9)

# Share of staple commodities in staple calories (and total)
consumption_shares_staples <- consumption_cals %>%
  filter(technology %in% staples) %>%
  select(-NonStaples) %>%
  mutate(share_of_comm = value / Staples, # Share of staple commodity in staple calories
         share_of_comm_total = value / Total) %>% # Share of staple commodity in total calories
  select(-Total, -Staples, -value, -Units.x)

# Share of non-staple commodities in non-staple calories (and total)
consumption_shares_nonstaples <- consumption_cals %>%
  filter(!(technology %in% staples)) %>%
  select(-Staples) %>%
  mutate(share_of_comm = value / NonStaples,
         share_of_comm_total = value / Total) %>%
  select(-Total, -NonStaples, -value, -Units.x)

# Get calorie information
consumption_by_comm_deciles_staples <- staples_nonstaples %>%
  select(-nodeinput) %>%
  left_join(consumption_shares_staples, by = c("region", "year", "scenario"), multiple = "all") %>%
  mutate(commodity_cals = Staples * share_of_comm)

consumption_by_comm_deciles_nonstaples <- staples_nonstaples %>%
  select(-nodeinput) %>%
  left_join(consumption_shares_nonstaples, by = c("region", "year", "scenario"), multiple = "all") %>%
  mutate(commodity_cals = NonStaples * share_of_comm)

# Combine staples and non-staples information
consumption_by_commodity_decile <- bind_rows(consumption_by_comm_deciles_staples,
                                             consumption_by_comm_deciles_nonstaples)

# Calculate macronutrient information
consum_macronutrient <- consumption_by_commodity_decile %>%
  # Add in GCAM region ID
  left_join(id_to_region, by = "region") %>%
  # Add population by region
  left_join(pop_all_regions, by = c("scenario", "region", "year")) %>%
  # Add regional macronutrient data
  left_join(macronutrients, by = c("GCAM_region_ID", "technology")) %>%
  rename(commodity = technology) %>%
  mutate(population = population / 10) # account for deciles = 1/10 pop

consum_macronutrient$fat_per_100g <- as.numeric(consum_macronutrient$fat_per_100g)
consum_macronutrient$protein_per_100g <- as.numeric(consum_macronutrient$protein_per_100g)

### Avg protein supply =========================================================
# SUM_commodity(protein/kg_c * consumption_c) / population
protein_supply_by_commodity <- consum_macronutrient %>%
  select(1:5, 9:14, 16, 17, grams_protein_per_kcal_comm) %>%
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
  filter(scenario == "Reference") %>%
  # Add in GCAM region ID
  left_join(id_to_region, by = "region") %>%
  # Add population by region
  left_join(pop_all_regions, by = c("scenario", "region", "year")) %>%
  # Add regional macronutrient data
  left_join(macronutrients, by = c("GCAM_region_ID", "technology")) %>%
  rename(commodity = technology) %>%
  select(1:5, 9:14, 17, 18, grams_protein_per_kcal_comm) %>%
  mutate(protein_per_100g = as.numeric(protein_per_100g)) %>%
  filter(protein_per_100g != 0) %>%
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

# Write outputs
write.csv(protein_supply, paste0(tmp_output_data_path,"/protein_supply_region.csv"))

### Protein supply from animals ================================================
# Same process as above but filtered for animal commodities
animal_protein_supply_by_commodity <- consum_macronutrient %>%
  select(1:5, 9:14, 16, 17, grams_protein_per_kcal_comm) %>%
  filter(protein_per_100g != 0,
         commodity %in% animal_commodities) %>%
  mutate(commodity_cals = commodity_cals * 1e12, # convert Pcal to kcal
         Units = "kcal") %>%
  group_by(scenario, region, year, commodity) %>%
  reframe(numerator = (as.numeric(grams_protein_per_kcal_comm) * commodity_cals), # protein (g per kcal) * calories
          denominator = population,
          animal_protein_supply = numerator / (denominator * 365), # convert year to day
          units = "g/capita/day",
          data = "FAO") %>%
  select(-numerator, -denominator)

# Total regional supply
animal_protein_supply <- animal_protein_supply_by_commodity %>%
  group_by(scenario, region, year) %>%
  # Sum protein supply by commodity to get total protein supply for each region
  reframe(value = sum(animal_protein_supply),
          units = units) %>%
  distinct() %>%
  mutate(year = as.character(year))


# Get SingleConsumer info
animal_protein_oneconsumer <- consumption_by_commodity_decile %>%
  filter(scenario == "Reference") %>%
  # Add in GCAM region ID
  left_join(id_to_region, by = "region") %>%
  # Add population by region
  left_join(pop_all_regions, by = c("scenario", "region", "year")) %>%
  # Add regional macronutrient data
  left_join(macronutrients, by = c("GCAM_region_ID", "technology")) %>%
  rename(commodity = technology) %>%
  select(1:5, 9:14, 17, 18, grams_protein_per_kcal_comm) %>%
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

# Write outputs
write.csv(animal_protein_supply,paste0(tmp_output_data_path,"/animal_protein_supply.csv"))

### Avg fat supply =============================================================
# SUM_commodity(fat/kg_c * consumption_c) / population
fat_supply_by_commodity <- consum_macronutrient %>%
  select(1:5, 9:16, 18:20) %>%
  filter(fat_per_100g != 0) %>%
  mutate(commodity_cals = commodity_cals * 1e12, # convert Pcal to kcal
         Units = "kcal") %>%
  group_by(scenario, region, year, commodity) %>%
  reframe(numerator = (as.numeric(grams_fat_per_kcal_comm) * commodity_cals), # protein (g per kcal) * calories
          denominator = population,
          fat_supply = numerator / (denominator * 365), # convert year to day
          units = "g/capita/day",
          data = "FAO") %>%
  select(-numerator, -denominator)

# Total regional values
fat_supply <- fat_supply_by_commodity %>%
  group_by(scenario, region, year) %>%
  # Sum protein supply by commodity to get total protein supply for each region
  reframe(fat_agg = sum(fat_supply),
          units = units) %>%
  distinct() %>%
  mutate(year = as.character(year),
         data = "FAO")

# Get SingleConsumer info
fat_oneconsumer <- consumption_by_commodity_decile %>%
  filter(scenario == "Reference") %>%
  # Add in GCAM region ID
  left_join(id_to_region, by = "region") %>%
  # Add population by region
  left_join(pop_all_regions, by = c("scenario", "region", "year")) %>%
  # Add regional macronutrient data
  left_join(macronutrients, by = c("GCAM_region_ID", "technology")) %>%
  rename(commodity = technology) %>%
  select(1:5, 9:14, 16, 18, grams_fat_per_kcal_comm) %>%
  mutate(fat_per_100g = as.numeric(fat_per_100g)) %>%
  filter(fat_per_100g != 0) %>%
  mutate(commodity_cals = commodity_cals * 1e12, # convert Pcal to kcal
         Units = "kcal") %>%
  group_by(scenario, region, year, commodity) %>%
  reframe(numerator = (as.numeric(grams_fat_per_kcal_comm) * commodity_cals), # protein (g per kcal) * calories
          denominator = population,
          fat_supply = numerator / (denominator * 365), # convert year to day
          units = "g/capita/day") %>%
  select(-numerator, -denominator) %>%
  group_by(scenario, region, year) %>%
  # Sum fat supply by commodity to get total fat supply for each region
  reframe(value = sum(fat_supply),
          units = units,
          data = "FAO") %>%
  distinct()

# Write outputs
write.csv(fat_supply, paste0(tmp_output_data_path,"/fat_supply.csv"))

### Plant protein supply =======================================================
# Total regional supply
plant_protein_supply <- protein_supply %>%
  left_join(animal_protein_supply, by = c('scenario', 'region', 'year', 'units')) %>%
  group_by(scenario, region, year, units) %>%
  summarise(value = protein_agg - value)

write.csv(plant_protein_supply, paste0(tmp_output_data_path,"/plant_protein_supply.csv"))


### Plots ======================================================================

# Create a palette of green colors
green_palette <- c(colorRampPalette(c("#017C20", "#00FF40"))(length(selected_scen)-1),'black')

## Protein
ggplot(data = bind_rows(animal_protein_supply %>%
                          mutate(type = 'animal protein'),
                        plant_protein_supply %>%
                          mutate(type = 'plant protein')) %>%
         mutate(year = as.numeric(year)) %>%
         mutate(value = as.numeric(value))) +
  geom_line(aes(x = year, y = value, color = scenario, linetype = type)) +
  facet_wrap(. ~ region, nrow = 2) +
  scale_color_manual(values = green_palette) +
  labs(title = 'Animal and Plan protein supply')
ggsave(file = paste0('figures_',model,'/protein_by_type.png'), width = 1000, height = 500, units = 'mm')


ggplot(data = bind_rows(animal_protein_supply %>%
                          mutate(protein_type = 'animal protein'),
                        plant_protein_supply %>%
                          mutate(protein_type = 'plant protein')) %>%
         mutate(year = as.numeric(year)) %>%
         mutate(value = as.numeric(value)) %>%
         mutate(scenario_type = if_else(scenario == 'Reference', 'Reference', 'Behavior change')) %>%
         group_by(region,year,protein_type,scenario_type) %>%
         mutate(median_value = median(value)) %>%
         # mutate(min_value = if_else(any(scenario == "Reference"), min(value[scenario != "Reference"]), 0)) %>%
         mutate(min_value = min(value)) %>%
         mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = interaction(scenario, protein_type, scenario_type), color = interaction(protein_type,scenario_type))) +
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = interaction(protein_type,scenario_type)), alpha = 0.15) +  # Shadow
  facet_wrap(. ~ region, nrow = 2) +
  scale_color_manual(values = protein_scenario_palette) +
  scale_fill_manual(values = protein_scenario_palette) +
  labs(title = 'Animal and Plan protein supply') +
  theme(legend.text = element_text(size = 18), legend.title = element_text(size = 20),
        strip.text = element_text(size = 15))
ggsave(file = paste0('figures_',model,'/protein_by_type_ci.png'), width = 1000, height = 500, units = 'mm')
