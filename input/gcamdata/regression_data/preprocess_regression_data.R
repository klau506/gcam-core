library(rgcam)
library(dplyr)
library(ggplot2)
library(rfasst)
library(tidyr)

folder_analysis_path = 'study7_analysis/'
country_gcam_regions <- read.csv(paste0(folder_analysis_path,"data/country_to_gcam_id.csv"))
iso_gcam_regions <- read.csv(paste0(folder_analysis_path,"data/iso_GCAM_regID.csv"), skip = 6)
id_gcam_regions <- read.csv(paste0(folder_analysis_path,"data/gcam_id_to_region.csv"))
colnames(id_gcam_regions) = c('GCAM_region_ID', 'region')
regions_key <- left_join(country_gcam_regions, id_gcam_regions, by = "GCAM_region_ID") %>%
  select(-1) %>%
  left_join(iso_gcam_regions %>%
              select(iso, country_name, GCAM_region_ID),
            by = c('GCAM_region_ID','country_name'))
protein_commodities = c('Legumes','NutsSeeds', 'Nuts_Seeds','Pork','Beef','Dairy','OtherMeat_Fish','Poultry','SheepGoat')


##### =================================================
##### ============== Debugging data ===================
##### =================================================


L240.GrossExports_Mt_R_C_Y = data %>% filter(GCAM_commodity %in% protein_commodities) %>%
  tidyr::pivot_wider(names_from = GCAM_commodity, values_from = GrossExp_Mt)
write.csv(L240.GrossExports_Mt_R_C_Y, file = 'regression_data/L240.GrossExports_Mt_R_C_Y.csv')

L240.GrossImports_Mt_R_C_Y = L240.GrossImports_Mt_R_C_Y %>%
  separate(supplysector, into = c("type", "GCAM_commodity"), sep = " ") %>%
  filter(GCAM_commodity %in% tolower(protein_commodities)) %>%
  select(-type) %>%
  tidyr::pivot_wider(names_from = GCAM_commodity, values_from = GrossImp_Mt)
write.csv(L240.GrossImports_Mt_R_C_Y, file = 'regression_data/L240.GrossImports_Mt_R_C_Y.csv')


L240.Prod_Mt_R_C_Y = L240.Prod_Mt_R_C_Y %>%
  filter(GCAM_commodity %in% protein_commodities) %>%
  tidyr::pivot_wider(names_from = GCAM_commodity, values_from = Prod_Mt)
write.csv(L240.Prod_Mt_R_C_Y, file = 'regression_data/L240.Prod_Mt_R_C_Y.csv')


L100.FAO_SUA_APE_balance = L100.FAO_SUA_APE_balance %>%
  filter(GCAM_commodity %in% protein_commodities,
         element == 'Food') %>%
  rename(value_Mt = value) %>%
  tidyr::pivot_wider(names_from = GCAM_commodity, values_from = value_Mt) %>%
  left_join(regions_key %>%
              select(GCAM_region_ID, region),
            by = 'GCAM_region_ID',
            multiple = 'all') %>%
  distinct(.)
write.csv(L100.FAO_SUA_APE_balance, file = 'regression_data/L100.FAO_SUA_APE_balance.csv')



L1321.ag_an_prP_R_C_75USDkg = bind_rows(L1321.ag_prP_R_C_75USDkg,L1321.an_prP_R_C_75USDkg) %>%
  filter(GCAM_commodity %in% protein_commodities) %>%
  mutate(value = value * 1e3) %>% # from 1975$/kg to 1975$/Mt
  tidyr::pivot_wider(names_from = GCAM_commodity, values_from = value)
write.csv(L1321.ag_an_prP_R_C_75USDkg, file = 'regression_data/L1321.ag_an_prP_R_C_75USDkg.csv')


##### =================================================
##### ================== GCAM data ====================
##### =================================================
library(rgcam)

prj_name = 'regression_prj.dat'
if (!file.exists(paste0('regression_data/',prj_name))) {
  conn <- localDBConn('C:/Users/claudia.rodes/Documents/GitHub/gcam-core/output', 'diets_basexdb')
  prj <<- addScenario(conn, prj_name, 'Diets_ref',
                      paste0('regression_data/', 'regression_queries.xml'),
                      clobber = FALSE)
} else {
  prj <<- loadProject(paste0('regression_data/',prj_name))
}

## PRODUCTION
production_ag = getQuery(prj, 'ag production by subsector (land use region)') %>%
  dplyr::filter(sector %in% protein_commodities) %>%
  dplyr::group_by(Units, scenario, region, sector, year) %>%
  dplyr::summarise(value = sum(value))
production_an = getQuery(prj, 'meat and dairy production by tech') %>%
  dplyr::filter(sector %in% protein_commodities) %>%
  dplyr::group_by(Units, scenario, region, sector, year) %>%
  dplyr::summarise(value = sum(value))
production = bind_rows(production_ag, production_an) %>%
  dplyr::left_join(regions_key %>%
                     select(region, GCAM_region_ID), by = 'region') %>%
  distinct(.) %>%
  arrange(GCAM_region_ID) %>%
  arrange(year) %>%
  tidyr::pivot_wider(names_from = 'sector', values_from = 'value') %>%
  # interpolate
  group_by(Units, region, GCAM_region_ID, scenario) %>%
  do(linear_interpolation(.)) %>%
  ungroup() %>%
  distinct(.)
writexl::write_xlsx(production, file.path('regression_data/production_data.xlsx'))

## CONSUMPTION
# save(DF_Macronutrient_FoodItem3_calperg, file = paste0('regression_data/DF_Macronutrient_FoodItem3_calperg.RData'))
consumption = getQuery(prj, 'food consumption by type (specific)') %>%
  dplyr::select(region,sector = technology, year, Pcal = value) %>%
  dplyr::filter(sector %in% protein_commodities) %>%
  dplyr::left_join(regions_key %>%
                     select(region, GCAM_region_ID), by = 'region') %>%
  distinct(.) %>%
  dplyr::left_join(DF_Macronutrient_FoodItem3_calperg %>%
                     dplyr::filter(GCAM_commodity %in% protein_commodities) %>%
                     dplyr::rename(sector = GCAM_commodity),
                   by = c('sector','GCAM_region_ID')) %>%
  # Units: from Pcal to Mt
  dplyr::mutate(Mt = 1e3 * Pcal / calperg) %>%
  # interpolate
  select(-Pcal, -calperg) %>%
  tidyr::pivot_wider(names_from = 'sector', values_from = 'Mt') %>%
  group_by(region, GCAM_region_ID) %>%
  do(linear_interpolation(.)) %>%
  ungroup() %>%
  # arrange
  distinct(.) %>%
  arrange(GCAM_region_ID) %>%
  arrange(year) %>%
  select('region','year','GCAM_region_ID','Legumes','NutsSeeds','Beef','Dairy','OtherMeat_Fish','Pork','Poultry','SheepGoat')
writexl::write_xlsx(consumption, file.path('regression_data/consumption_data.xlsx'))


## IMPORTS
imports = getQuery(prj, 'ag import vs. domestic supply (Regional Armington competition)') %>%
  separate(subsector, into = c("type", "product"), sep = " ", remove = FALSE) %>%
  dplyr::filter(type == 'imported') %>%
  dplyr::select(Units, region, sector = product, year, value) %>%
  dplyr::mutate(sector = ifelse(sector == 'nuts_seeds', 'NutsSeeds',
                                ifelse(sector == 'sheepgoat', 'SheepGoat', stringr::str_to_title(sector)))) %>%
  dplyr::filter(sector %in% protein_commodities) %>%
  # join regions
  dplyr::left_join(regions_key %>%
                     select(region, GCAM_region_ID), by = 'region', multiple = 'all') %>%
  distinct(.) %>%
  # interpolate
  tidyr::pivot_wider(names_from = 'sector', values_from = 'value') %>%
  group_by(region, GCAM_region_ID) %>%
  do(linear_interpolation(.)) %>%
  ungroup() %>%
  # arrange
  distinct(.) %>%
  arrange(GCAM_region_ID) %>%
  arrange(year) %>%
  select('region','year','GCAM_region_ID','Legumes','NutsSeeds','Beef','Dairy','Pork','Poultry','SheepGoat')
writexl::write_xlsx(imports, file.path('regression_data/imports_data.xlsx'))

## EXPORTS
exports = getQuery(prj, 'ag export to the world center (USA) (Intl. Armington competition)') %>%
  dplyr::select(Units, region = subsector, sector = input, year, value) %>%
  dplyr::filter(sector %in% protein_commodities) %>%
  dplyr::mutate(region = stringr::str_extract(region, ".*(?=\\straded)")) %>%
  # join regions
  dplyr::left_join(regions_key %>%
                     select(region, GCAM_region_ID), by = 'region', multiple = 'all') %>%
  distinct(.) %>%
  # interpolate
  tidyr::pivot_wider(names_from = 'sector', values_from = 'value') %>%
  group_by(region, GCAM_region_ID) %>%
  do(linear_interpolation(.)) %>%
  ungroup() %>%
  # arrange
  distinct(.) %>%
  arrange(GCAM_region_ID) %>%
  arrange(year) %>%
  select('region','year','GCAM_region_ID','Legumes','NutsSeeds','Beef','Dairy','Pork','Poultry','SheepGoat')
writexl::write_xlsx(exports, file.path('regression_data/exports_data.xlsx'))


##### =================================================
##### ============== Socio economics ==================
##### =================================================


##### ============= POPULATION (TOTAL) ==================

population = read.csv('regression_data/data_to_select/API_SP.POP.TOTL_DS2_en_csv_v2_5871594/API_SP.POP.TOTL_DS2_en_csv_v2_5871594.csv', skip = 3) %>%
  rename_with(~gsub("^X", "", .x), starts_with("X")) %>%
  select(-ncol(.)) %>%
  # add Taiwan
  bind_rows(.,
            read.csv('regression_data/data_to_select/taiwan-population-2023-09-27.csv', skip = 15) %>%
              select(Population,year) %>%
              filter(year >= 1960, year <= 2022) %>%
              mutate(Country.Name = 'Taiwan',
                     Country.Code = 'twn',
                     Indicator.Name = 'Population, total',
                     Indicator.Code = 'SP.POP.TOTL') %>%
              tidyr::pivot_wider(names_from = year, values_from = Population)) %>%
  # merge with GCAM regions
  rename(iso = Country.Code) %>%
  mutate(iso = tolower(iso)) %>%
  left_join(regions_key, by = c('iso')) %>%
  # sum population regionally
  filter(!is.na(GCAM_region_ID)) %>%
  group_by(GCAM_region_ID, Indicator.Name, region) %>%
  summarize(across(`1960`:`2022`, \(x) sum(x, na.rm = TRUE), .names = "{.col}")) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = `1960`:`2022`, names_to = 'year') %>%
  arrange(year)
writexl::write_xlsx(population, 'regression_data/population.xlsx')

population_raw = read.csv('regression_data/data_to_select/API_SP.POP.TOTL_DS2_en_csv_v2_5871594/API_SP.POP.TOTL_DS2_en_csv_v2_5871594.csv', skip = 3) %>%
  rename_with(~gsub("^X", "", .x), starts_with("X")) %>%
  select(-ncol(.)) %>%
  # add Taiwan
  bind_rows(.,
            read.csv('regression_data/data_to_select/taiwan-population-2023-09-27.csv', skip = 15) %>%
              select(Population,year) %>%
              filter(year >= 1960, year <= 2022) %>%
              mutate(Country.Name = 'Taiwan',
                     Country.Code = 'twn',
                     Indicator.Name = 'Population, total',
                     Indicator.Code = 'SP.POP.TOTL') %>%
              tidyr::pivot_wider(names_from = year, values_from = Population)) %>%
  # merge with GCAM regions
  rename(iso = Country.Code) %>%
  mutate(iso = tolower(iso)) %>%
  left_join(regions_key, by = c('iso')) %>%
  tidyr::pivot_longer(cols = `1960`:`2022`, names_to = 'year') %>%
  # sum population regionally
  filter(!is.na(GCAM_region_ID)) %>%
  # reshape dataset
  arrange(year)
writexl::write_xlsx(population_raw, 'regression_data/population_raw.xlsx')


##### SSP2 POPULATION ======
ssp2_population <- read.csv(paste0(folder_analysis_path,"data/SSP2 population by demographic.csv"), skip = 1) %>%
  rename_with(~gsub("^X", "", .x), starts_with("X")) %>%
  filter(VARIABLE == 'Population') %>%
  select(iso,unit = UNIT,`2010`:`2100`) %>%
  tidyr::pivot_longer(cols = `2010`:`2100`, names_to = 'year') %>%
  # add Taiwan
  bind_rows(.,
            read.csv('regression_data/data_to_select/taiwan-population-2023-09-27.csv', skip = 15) %>%
              select(value = Population, year) %>%
              filter(year >= 1960) %>%
              mutate(unit = 'million',
                     iso = 'twn',
                     year = as.character(year),
                     value = value / 1e6)) %>%
  # merge country_name
  left_join(regions_key %>%
              select(iso, region, GCAM_region_ID), by = 'iso') %>%
  distinct(.) %>%
  # add population by region; units from million to nº
  group_by(unit, year, region, GCAM_region_ID) %>%
  summarise(value = sum(value) * 1e6) %>%
  # interpolate
  group_by(region, GCAM_region_ID) %>%
  do(linear_interpolation(., 'population')) %>%
  mutate(value = round(value)) %>%
  ungroup() %>%
  # arrange
  distinct(.) %>%
  arrange(GCAM_region_ID) %>%
  arrange(year)
writexl::write_xlsx(ssp2_population, file.path('regression_data/ssp2_population_data.xlsx'))

ssp2_population_raw <- read.csv(paste0(folder_analysis_path,"data/SSP2 population by demographic.csv"), skip = 1) %>%
  rename_with(~gsub("^X", "", .x), starts_with("X")) %>%
  filter(VARIABLE == 'Population') %>%
  select(iso,unit = UNIT,`2010`:`2100`) %>%
  tidyr::pivot_longer(cols = `2010`:`2100`, names_to = 'year') %>%
  # add Taiwan
  bind_rows(.,
            read.csv('regression_data/data_to_select/taiwan-population-2023-09-27.csv', skip = 15) %>%
              select(value = Population, year) %>%
              filter(year >= 1960) %>%
              mutate(unit = 'million',
                     iso = 'twn',
                     year = as.character(year),
                     value = value / 1e6)) %>%
  # merge country_name
  left_join(regions_key %>%
              select(iso, region, GCAM_region_ID), by = 'iso') %>%
  distinct(.) %>%
  # units from million to nº
  group_by(unit, year, region, GCAM_region_ID, iso) %>%
  summarise(value = sum(value) * 1e6) %>%
  # interpolate
  group_by(iso, region, GCAM_region_ID) %>%
  do(linear_interpolation(., 'population')) %>%
  mutate(value = round(value)) %>%
  ungroup() %>%
  # arrange
  distinct(.) %>%
  arrange(GCAM_region_ID,year)
writexl::write_xlsx(ssp2_population_raw, file.path('regression_data/ssp2_population_raw_data.xlsx'))


##### ============= POPULATION (BY GENDER) ==================

populationF = read.csv('regression_data/data_to_select/API_SP.POP.TOTL.FE.IN_DS2_en_csv_v2_5872498/API_SP.POP.TOTL.FE.IN_DS2_en_csv_v2_5872498.csv', skip = 3) %>%
  rename_with(~gsub("^X", "", .x), starts_with("X")) %>%
  select(-ncol(.)) %>%
  # add Taiwan as an empty line
  bind_rows(.,
            data.frame(
              Country.Name = 'Taiwan',
              Country.Code = 'twn',
              Indicator.Name = 'Population, total',
              Indicator.Code = 'SP.POP.TOTL',
              years = seq(1960, 1959 + 62),
              values = rep(NA, 62)) %>%
              tidyr::pivot_wider(names_from = years, values_from = values)) %>%
  # merge with GCAM regions
  rename(iso = Country.Code) %>%
  mutate(iso = tolower(iso)) %>%
  left_join(regions_key, by = c('iso')) %>%
  # sum GDP regionally
  filter(!is.na(GCAM_region_ID)) %>%
  group_by(GCAM_region_ID, Indicator.Name, region) %>%
  summarize(across(`1960`:`2022`, \(x) sum(x, na.rm = TRUE), .names = "{.col}")) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = `1960`:`2022`, names_to = 'year') %>%
  arrange(year)
write.csv(populationF, 'regression_data/populationF.csv')

populationM = read.csv('regression_data/data_to_select/API_SP.POP.TOTL.MA.IN_DS2_en_csv_v2_5874531/API_SP.POP.TOTL.MA.IN_DS2_en_csv_v2_5874531.csv', skip = 3) %>%
  rename_with(~gsub("^X", "", .x), starts_with("X")) %>%
  select(-ncol(.)) %>%
  # add Taiwan as an empty line
  bind_rows(.,
            data.frame(
              Country.Name = 'Taiwan',
              Country.Code = 'twn',
              Indicator.Name = 'Population, total',
              Indicator.Code = 'SP.POP.TOTL',
              years = seq(1960, 1959 + 62),
              values = rep(NA, 62)) %>%
              tidyr::pivot_wider(names_from = years, values_from = values)) %>%
  # merge with GCAM regions
  rename(iso = Country.Code) %>%
  mutate(iso = tolower(iso)) %>%
  left_join(regions_key, by = c('iso')) %>%
  # sum GDP regionally
  filter(!is.na(GCAM_region_ID)) %>%
  group_by(GCAM_region_ID, Indicator.Name, region) %>%
  summarize(across(`1960`:`2022`, \(x) sum(x, na.rm = TRUE), .names = "{.col}")) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = `1960`:`2022`, names_to = 'year') %>%
  arrange(year)
write.csv(populationM, 'regression_data/populationM.csv')

# check
population_check = merge(population %>%
                           rename(value_Total = value),
                         populationF %>%
                           rename(value_F = value),
                         by = c('GCAM_region_ID', 'region', 'year')) %>%
  merge(populationM %>%
          rename(value_M = value),
        by = c('GCAM_region_ID', 'region', 'year')) %>%
  select(GCAM_region_ID, region, year, value_Total, value_F, value_M) %>%
  mutate(sum_F_M = value_F + value_M) %>%
  mutate(diff = value_Total - sum_F_M)
write.csv(population_check, 'regression_data/population_check.csv')


##### SSP2 POPULATION ======
ssp2_population_F <- read.csv(paste0(folder_analysis_path,"data/SSP2 population by demographic.csv"), skip = 1) %>%
  rename_with(~gsub("^X", "", .x), starts_with("X")) %>%
  filter(VARIABLE == 'Population|Female') %>%
  select(iso,unit = UNIT,`2010`:`2100`) %>%
  tidyr::pivot_longer(cols = `2010`:`2100`, names_to = 'year') %>%
  # add Taiwan
  bind_rows(.,
            read.csv('regression_data/data_to_select/taiwan-population-2023-09-27.csv', skip = 15) %>%
              select(value = Population, year) %>%
              filter(year >= 2010) %>%
              mutate(unit = 'million',
                     iso = 'twn',
                     year = as.character(year),
                     value = value / 1e6)) %>%
  # merge country_name
  left_join(regions_key %>%
              select(iso, region, GCAM_region_ID), by = 'iso') %>%
  distinct(.) %>%
  # add population by region; units from million to nº
  group_by(unit, year, region, GCAM_region_ID) %>%
  summarise(value = sum(value) * 1e6) %>%
  # interpolate
  group_by(region, GCAM_region_ID) %>%
  do(linear_interpolation(., 'population')) %>%
  mutate(value = round(value)) %>%
  ungroup() %>%
  # arrange
  distinct(.) %>%
  arrange(GCAM_region_ID) %>%
  arrange(year)
writexl::write_xlsx(ssp2_population_F, file.path('regression_data/ssp2_population_F_data.xlsx'))



##### ============= POPULATION (URBAN) ==================

population_urban = read.csv('regression_data/data_to_select/API_SP.URB.TOTL.IN.ZS_DS2_en_csv_v2_5871659/API_SP.URB.TOTL.IN.ZS_DS2_en_csv_v2_5871659.csv', skip = 3) %>%
  rename_with(~gsub("^X", "", .x), starts_with("X")) %>%
  select(-ncol(.)) %>%
  # compute nº of people
  tidyr::pivot_longer(cols = `1960`:`2022`, names_to = 'year', values_to = 'urban_P') %>%
  left_join(read.csv('regression_data/data_to_select/API_SP.POP.TOTL_DS2_en_csv_v2_5871594/API_SP.POP.TOTL_DS2_en_csv_v2_5871594.csv', skip = 3) %>%
              rename_with(~gsub("^X", "", .x), starts_with("X")) %>%
              select(-ncol(.)) %>% select(-c(Indicator.Name,Indicator.Code)) %>%
              tidyr::pivot_longer(cols = `1960`:`2022`, names_to = 'year', values_to = 'total_pop_N'),
            by = c('Country.Name','Country.Code','year')) %>%
  mutate('urban_N' = round(total_pop_N * urban_P / 100)) %>%
  mutate(Indicator.Name = 'Urban population (nº of population)') %>%
  select(-c(urban_P,total_pop_N)) %>%
  # add Taiwan data
  bind_rows(read.csv('regression_data/data_to_select/taiwan_extradata.csv', skip = 1) %>%
              select(urban_N = Urban_Population_N, year = Year) %>%
              mutate(year = as.character(year),
                     urban_N = as.double(urban_N),
                     Country.Name = 'Taiwan',
                     Country.Code = 'twn',
                     Indicator.Name = 'Urban population (nº of total population)',
                     Indicator.Code = 'SP.URB.TOTL.IN.ZS')) %>%
  # merge with GCAM regions
  tidyr::pivot_wider(names_from = 'year', values_from = 'urban_N') %>%
  rename(iso = Country.Code) %>%
  mutate(iso = tolower(iso)) %>%
  left_join(regions_key, by = c('iso')) %>%
  # sum population regionally
  filter(!is.na(GCAM_region_ID)) %>%
  group_by(GCAM_region_ID, Indicator.Name, region) %>%
  summarize(across(`1960`:`2022`, \(x) sum(x, na.rm = TRUE), .names = "{.col}")) %>%
  # reshape dataset
  tidyr::pivot_longer(cols = `1960`:`2022`, names_to = 'year') %>%
  # remove 0's
  mutate(value = ifelse(value == 0, NA, value)) %>%
  # reorder
  arrange(year)
writexl::write_xlsx(population_urban, 'regression_data/population_urban.xlsx')

##### SSP2 POPULATION (URBAN) ======
ssp2_population_urban <- read.csv(paste0("regression_data/data_to_select/SspDb_country_data_2013-06-12.csv/SspDb_country_data_2013-06-12.csv")) %>%
  rename_with(~gsub("^X", "", .x), starts_with("X")) %>%
  filter(VARIABLE %in% c('Population|Urban|Share')) %>%
  select(iso = REGION, variable = VARIABLE, unit = UNIT,`2010`:`2100`) %>%
  mutate(iso = tolower(iso)) %>%
  tidyr::pivot_longer(cols = `2010`:`2100`, names_to = 'year', values_to = 'value_P') %>%
  # merge population
  left_join(ssp2_population_raw %>%
              rename(country_pop = value) %>%
              mutate(year = as.character(year)),
            by = c('iso','year')) %>%
  distinct(.) %>%
  # compute total nº of urban population by iso and region
  mutate(urban_N = (value_P/100) * country_pop) %>%
  group_by(year, region, GCAM_region_ID) %>%
  summarise(urban_N = sum(urban_N),
            reg_pop = sum(country_pop)) %>%
  # compute percentage
  mutate(value = 100 * urban_N / reg_pop) %>%
  # interpolate
  filter(!is.na(value)) %>%
  group_by(region, GCAM_region_ID) %>%
  do(linear_interpolation(., 'population')) %>%
  ungroup() %>%
  # add Taiwan
  bind_rows(.,
            read.csv('regression_data/data_to_select/taiwan_extradata.csv', skip = 1) %>%
              select(value = Urban_Pop_P, year = Year) %>%
              filter(year >= 2010) %>%
              # extra columns
              mutate(region = 'Taiwan',
                     GCAM_region_ID = 30)) %>%
  # arrange
  distinct(.) %>%
  arrange(GCAM_region_ID,year) %>%
  right_join(tidyr::expand_grid(regions_key, data.frame(year = as.character(seq(2010,2100,1)))) %>%
               select(GCAM_region_ID, region, year) %>%
               mutate(year = as.double(year)),
             by = c('GCAM_region_ID', 'region', 'year')) %>% distinct(.)
writexl::write_xlsx(ssp2_population_urban, file.path('regression_data/ssp2_population_urban_data.xlsx'))



##### ============= GDP ==================

gdp =
  # basic dataset (population)
  population_raw %>%
  rename(country_pop = value) %>%
  select(-c(Indicator.Name, Indicator.Code)) %>%
  # add gdp value 1
  left_join(read.csv('regression_data/data_to_select/API_NY.GDP.MKTP.KD_DS2_en_csv_v2_5871611/API_NY.GDP.MKTP.KD_DS2_en_csv_v2_5871611.csv', skip = 3) %>%
              rename_with(~gsub("^X", "", .x), starts_with("X")) %>%
              select(-ncol(.)) %>% select(-c(Country.Name, Indicator.Name, Indicator.Code)) %>%
              tidyr::pivot_longer(cols = `1960`:`2022`, names_to = 'year', values_to = 'gdp_value1') %>%
              rename(iso = Country.Code) %>%
              mutate(iso = tolower(iso)),
            by = c('iso','year')) %>%
  # add gdp value 2
  left_join(readxl::read_excel('regression_data/data_to_select/statistic_id727592_gross-domestic-product--gdp--per-capita-in-taiwan-1987-2028.xlsx',
                               sheet = 'Data', skip = 3) %>%
              setNames(c('year','gdp_value2')) %>%
              mutate(year = as.character(year),
                     iso = 'twn') %>%
              filter(!is.na(gdp_value2), year <= 2022),
            by = c('iso','year')) %>%
  left_join(read.csv('regression_data/data_to_select/RGDPNACAA666NRUG.csv') %>%
              select(3:4) %>%
              setNames(c('year','gdp_value3')) %>%
              mutate(year = as.character(year),
                     iso = 'can') %>%
              filter(!is.na(gdp_value3), year <= 2022),
            by = c('iso','year')) %>%
  # from GDP per capita to GDP (only for gdp_value2)
  mutate(gdp_value2 = gdp_value2 * country_pop) %>%
  # median GDP among the available data
  group_by(country_name, iso, GCAM_region_ID, region, year, country_pop) %>%
  mutate(gdp_value = mean(c(gdp_value1,gdp_value2,gdp_value3), na.rm = TRUE)) %>%
  ungroup() %>%
  select(-c(gdp_value1,gdp_value2,gdp_value3)) %>%
  # sum GDP regionally
  group_by(GCAM_region_ID, region, year) %>%
  summarise(gdp_value = sum(gdp_value, na.rm = TRUE)) %>%
  mutate(gdp_value = ifelse(gdp_value == 0, NA, gdp_value)) %>%
  ungroup() %>%
  # reorder
  arrange(year)
writexl::write_xlsx(gdp, 'regression_data/gdp.xlsx')

##### SSP2 GDP ======
ssp2_gdp <- readxl::read_excel(paste0("regression_data/data_to_select/iamc_db_gdp.xlsx")) %>%
  rename_all(~sub("\\..*", "", .)) %>%
  select(iso = Region, variable = Variable, unit = Unit, `2010`:`2100`) %>%
  filter(!is.na(iso)) %>%
  # merge country_name
  mutate(iso = tolower(iso)) %>%
  left_join(regions_key %>%
              select(iso, region, GCAM_region_ID), by = 'iso') %>%
  distinct(.) %>%
  # add gdp by region; units from billion to nº, and from 2005$ to 2015$
  tidyr::pivot_longer(cols = `2010`:`2100`, names_to = 'year') %>%
  group_by(unit, year, region, GCAM_region_ID) %>%
  summarise(value = sum(value) * 1e9 * gcamdata::gdp_deflator(2015,2005)) %>%
  # interpolate
  group_by(region, GCAM_region_ID) %>%
  do(linear_interpolation(., 'population')) %>%
  mutate(value = round(value)) %>%
  ungroup() %>%
  # arrange
  distinct(.) %>%
  arrange(GCAM_region_ID) %>%
  arrange(year)
writexl::write_xlsx(ssp2_gdp, file.path('regression_data/ssp2_gdp_data.xlsx'))


##### ============= GINI index ==================

gini =
  # basic dataset (population)
  population_raw %>%
  rename(country_pop = value) %>%
  select(-c(Indicator.Name, Indicator.Code)) %>%
  # add gini index 1
  left_join(read.csv('regression_data/data_to_select/API_SI.POV.GINI_DS2_en_csv_v2_5871599/API_SI.POV.GINI_DS2_en_csv_v2_5871599.csv', skip = 3) %>%
              rename_with(~gsub("^X", "", .x), starts_with("X")) %>%
              select(-ncol(.)) %>% select(-c(Country.Name, Indicator.Name, Indicator.Code)) %>%
              tidyr::pivot_longer(cols = `1960`:`2022`, names_to = 'year', values_to = 'gini_index1') %>%
              rename(iso = Country.Code) %>%
              mutate(iso = tolower(iso)),
            by = c('iso','year')) %>%
  # add gini index 2
  left_join(readxl::read_excel('regression_data/data_to_select/NRao_et_al_GiniProjections_2018/Gini_projections_SSPs.xlsx',
                               sheet = 'projected_ginis_full-set') %>%
              tidyr::pivot_longer(cols = 3:ncol(.), names_to = 'iso', values_to = 'gini_index2') %>%
              mutate(year = as.character(year), iso = tolower(iso)) %>%
              filter(scenario == 'SSP2', !is.na(gini_index2), year <= 2022) %>% select(-scenario),
            by = c('iso','year')) %>%
  # add gini index 3
  left_join(read.csv('regression_data/data_to_select/pip_worldbank.csv') %>%
              select(iso = country_code, year = reporting_year, gini_index3 = gini) %>%
              mutate(year = as.character(year), iso = tolower(iso)) %>%
              filter(!is.na(gini_index3), year <= 2022) %>%
              group_by(iso,year) %>%
              summarise(gini_index3 = 100*mean(gini_index3)),
            by = c('year','iso')) %>%
  # add gini index 4
  left_join(readxl::read_xlsx('regression_data/data_to_select/statistic_id922574_gini-index-of-taiwan-1980-2021.xlsx',
                     sheet = 'Data', skip = 3) %>%
              setNames(c('year','gini_index4')) %>%
              mutate(year = as.character(year),
                     iso = 'twn') %>%
              filter(!is.na(gini_index4), year <= 2022),
            by = c('year','iso')) %>%
  # add gini index 5
  left_join(readxl::read_xlsx('regression_data/data_to_select/gini_extradata.xlsx',
                              sheet = 'gini_extradata') %>%
              setNames(c('Country.Name','year','gini_index5','extra')) %>%
              select(-ncol(.)) %>%
              mutate(year = as.character(year)) %>%
              filter(!is.na(gini_index5), year <= 2022),
            by = c('Country.Name','year')) %>%
  ungroup() %>%
  group_by(country_name, iso, GCAM_region_ID, region, year, country_pop) %>%
  mutate(gini_index = mean(c(gini_index1,gini_index2,gini_index3,gini_index4,gini_index5), na.rm = TRUE)) %>%
  ungroup() %>%
  select(-c(gini_index1,gini_index2,gini_index3,gini_index4,gini_index5)) %>%
  # compute weight of each country within its region (percentage of population among the
  # countries that have data for that year and are from that region)
  # compute regional population and countries weight
  filter(!is.na(gini_index)) %>%
  group_by(year,GCAM_region_ID,region) %>%
  mutate(region_pop = round(sum(country_pop))) %>%
  ungroup() %>%
  mutate(weight = country_pop / region_pop) %>%
  mutate(gini_w = gini_index * weight) %>%
  group_by(year,GCAM_region_ID,region) %>%
  summarise(gini_reg = round(sum(gini_w))) %>%
  ungroup() %>%
  # add empty lines when data missing
  right_join(regions_key %>%
               select(region, GCAM_region_ID) %>%
               crossing(year = as.character(seq(1960, 1959 + 62)))
  ) %>%
  # reorder
  arrange(year,GCAM_region_ID)
writexl::write_xlsx(gini, 'regression_data/gini.xlsx')


##### SSP2 GINI =========
ssp2_gini = tidyr::expand_grid(regions_key, data.frame(year = as.character(c(seq(2010,2022,1),seq(2025,2100,5))))) %>%
  # add gini index 1
  left_join(read.csv('regression_data/data_to_select/API_SI.POV.GINI_DS2_en_csv_v2_5871599/API_SI.POV.GINI_DS2_en_csv_v2_5871599.csv', skip = 3) %>%
              rename_with(~gsub("^X", "", .x), starts_with("X")) %>%
              select(-ncol(.)) %>% select(-c(Country.Name, Indicator.Name, Indicator.Code)) %>%
              tidyr::pivot_longer(cols = `1960`:`2022`, names_to = 'year', values_to = 'gini_index1') %>%
              rename(iso = Country.Code) %>%
              mutate(iso = tolower(iso)),
            by = c('iso','year')) %>%
  # add gini index 2
  left_join(readxl::read_excel('regression_data/data_to_select/NRao_et_al_GiniProjections_2018/Gini_projections_SSPs.xlsx',
                               sheet = 'projected_ginis_full-set') %>%
              tidyr::pivot_longer(cols = 3:ncol(.), names_to = 'iso', values_to = 'gini_index2') %>%
              mutate(year = as.character(year), iso = tolower(iso)) %>%
              filter(scenario == 'SSP2', !is.na(gini_index2)) %>% select(-scenario),
            by = c('iso','year')) %>%
  # add gini index 3
  left_join(read.csv('regression_data/data_to_select/pip_worldbank.csv') %>%
              select(iso = country_code, year = reporting_year, gini_index3 = gini) %>%
              mutate(year = as.character(year), iso = tolower(iso)) %>%
              filter(!is.na(gini_index3)) %>%
              group_by(iso,year) %>%
              summarise(gini_index3 = 100*mean(gini_index3)),
            by = c('year','iso')) %>%
  # add gini index 4
  left_join(readxl::read_xlsx('regression_data/data_to_select/statistic_id922574_gini-index-of-taiwan-1980-2021.xlsx',
                              sheet = 'Data', skip = 3) %>%
              setNames(c('year','gini_index4')) %>%
              mutate(year = as.character(year),
                     iso = 'twn') %>%
              filter(!is.na(gini_index4)),
            by = c('year','iso')) %>%
  # add gini index 5
  left_join(readxl::read_xlsx('regression_data/data_to_select/gini_extradata.xlsx',
                              sheet = 'gini_extradata') %>%
              setNames(c('country_name','year','gini_index5','extra')) %>%
              select(-ncol(.)) %>%
              mutate(year = as.character(year)) %>%
              filter(!is.na(gini_index5)) %>%
              rename_reg(.) %>%
              left_join(regions_key,
                        by = 'country_name'),
            by = c('iso','region','GCAM_region_ID','year','country_name')) %>%
  ungroup() %>%
  group_by(country_name, iso, GCAM_region_ID, region, year) %>%
  mutate(gini_index = mean(c(gini_index1,gini_index2,gini_index3,gini_index4,gini_index5), na.rm = TRUE)) %>%
  ungroup() %>%
  select(-c(gini_index1,gini_index2,gini_index3,gini_index4,gini_index5)) %>%
  # compute weight of each country within its region (percentage of population among the
  # countries that have data for that year and are from that region)
  # compute regional population and countries weight
  # basic dataset (population)
  left_join(ssp2_population_raw %>%
              mutate(year = as.character(year)) %>%
              rename(country_pop = value),
            by = c('iso','GCAM_region_ID','region','year')) %>%
  # filter(!is.na(gini_index)) %>%
  group_by(year,GCAM_region_ID,region) %>%
  mutate(region_pop = round(sum(country_pop, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(weight = country_pop / region_pop) %>%
  mutate(gini_w = gini_index * weight) %>%
  group_by(year,GCAM_region_ID,region) %>%
  summarise(value = round(sum(gini_w, na.rm = TRUE))) %>%
  ungroup() %>%
  # interpolate
  group_by(region, GCAM_region_ID) %>%
  do(linear_interpolation(., 'gini')) %>%
  ungroup() %>% distinct(.) %>%
  # reorder
  arrange(year,GCAM_region_ID)
writexl::write_xlsx(ssp2_gini, 'regression_data/ssp2_gini_data.xlsx')


##### ============= EDUCATION (upper secondary) ==================

education_UpperSec = read.csv('regression_data/data_to_select/API_SE.SEC.CUAT.UP.ZS_DS2_en_csv_v2_5874055/API_SE.SEC.CUAT.UP.ZS_DS2_en_csv_v2_5874055.csv', skip = 3) %>%
  rename_with(~gsub("^X", "", .x), starts_with("X")) %>%
  select(-ncol(.)) %>%
  # merge with population data
  tidyr::pivot_longer(cols = `1960`:`2022`, names_to = 'year') %>%
  left_join(read.csv('regression_data/data_to_select/API_SP.POP.TOTL_DS2_en_csv_v2_5871594/API_SP.POP.TOTL_DS2_en_csv_v2_5871594.csv', skip = 3) %>%
              rename_with(~gsub("^X", "", .x), starts_with("X")) %>%
              select(-ncol(.)) %>% select(-c(Indicator.Name,Indicator.Code)) %>%
              tidyr::pivot_longer(cols = `1960`:`2022`, names_to = 'year', values_to = 'country_pop'),
            by = c('Country.Name','Country.Code','year')) %>%
  # merge with GCAM regions
  rename(iso = Country.Code) %>%
  mutate(iso = tolower(iso)) %>%
  left_join(regions_key, by = c('iso')) %>%
  # compute regional population and countries weight
  filter(!is.na(value)) %>%
  mutate(value_N = (value / 100) * country_pop) %>%
  group_by(year,GCAM_region_ID,region) %>%
  mutate(region_pop = round(sum(country_pop))) %>%
  ungroup() %>%
  mutate(weight = country_pop / region_pop) %>%
  mutate(value_Nw = value_N * weight) %>%
  group_by(year,GCAM_region_ID,region,Indicator.Name,Indicator.Code) %>%
  summarise(value_reg = round(sum(value_Nw))) %>%
  ungroup() %>%
  # add empty lines when data missing
  right_join(regions_key %>%
               select(region, GCAM_region_ID) %>%
               mutate(Indicator.Name = 'Educational attainment, at least completed upper secondary, population 25+, total (%) (cumulative)',
                      Indicator.Code = 'SE.SEC.CUAT.UP.ZS') %>%
               crossing(year = as.character(seq(1960, 1959 + 62)))
  ) %>%
  # reorder
  arrange(year,GCAM_region_ID)
writexl::write_xlsx(education_UpperSec, 'regression_data/education_UpperSec.xlsx')

##### ============= EDUCATION (primary) ==================

education_Primary = read.csv('regression_data/data_to_select/API_SE.PRM.CMPT.ZS_DS2_en_csv_v2_5873993/API_SE.PRM.CMPT.ZS_DS2_en_csv_v2_5873993.csv', skip = 3) %>%
  rename_with(~gsub("^X", "", .x), starts_with("X")) %>%
  select(-ncol(.)) %>%
  tidyr::pivot_longer(cols = `1960`:`2022`, names_to = 'year', values_to = 'value_1') %>%
  # add extra data
  select(-Country.Name,-Indicator.Name,-Indicator.Code) %>%
  bind_rows(read.csv('regression_data/data_to_select/world-population-level-education.csv') %>%
              setNames(c('Entity','Code','Year','Post Secondary','Upper Secondary','Lower Secondary','Primary','Incomplete Primary','No Education','Under 15')) %>%
              select(Country.Code = Code, year = Year, value_2 = Primary) %>%
              mutate(year = as.character(year),
                     value_2 = ifelse(value_2 == 0, NA, value_2))) %>%
  # merge with population data
  left_join(read.csv('regression_data/data_to_select/API_SP.POP.TOTL_DS2_en_csv_v2_5871594/API_SP.POP.TOTL_DS2_en_csv_v2_5871594.csv', skip = 3) %>%
              rename_with(~gsub("^X", "", .x), starts_with("X")) %>%
              select(-ncol(.)) %>% select(-c(Indicator.Name,Indicator.Code)) %>%
              tidyr::pivot_longer(cols = `1960`:`2022`, names_to = 'year', values_to = 'country_pop'),
            by = c('Country.Code','year')) %>%
  # units: value_2 from N to P
  mutate(value_2 = 100*value_2/country_pop) %>%
  # mean between both values
  group_by_at(vars(-value_1, -value_2)) %>%
  mutate(value = mean(c(value_1, value_2), na.rm = TRUE)) %>%
  select(-value_1,-value_2) %>% ungroup() %>%
  # merge with GCAM regions
  rename(iso = Country.Code) %>%
  mutate(iso = tolower(iso)) %>%
  left_join(regions_key, by = c('iso')) %>%
  # compute regional population and countries weight
  filter(!is.na(value)) %>%
  mutate(value_N = (value / 100) * country_pop) %>%
  group_by(year,GCAM_region_ID,region) %>%
  mutate(region_pop = round(sum(country_pop))) %>%
  ungroup() %>%
  mutate(weight = country_pop / region_pop) %>%
  mutate(value_Nw = value_N * weight) %>%
  group_by(year,GCAM_region_ID,region) %>%
  summarise(value_reg = round(sum(value_Nw))) %>%
  ungroup() %>%
  # add empty lines when data missing
  right_join(regions_key %>%
               select(region, GCAM_region_ID) %>%
               crossing(year = as.character(seq(1960, 1959 + 62)))
  ) %>%
  # reorder
  arrange(year,GCAM_region_ID)
writexl::write_xlsx(education_Primary, 'regression_data/education_Primary.xlsx')


##### SSP2 EDUCATIONN (PRIMARY) =========
ssp2_education_Primary = tidyr::expand_grid(regions_key, data.frame(year = as.character(seq(2010,2100,5)))) %>%
  left_join(read.csv('regression_data/data_to_select/world-population-level-education.csv') %>%
            setNames(c('Entity','Code','Year','Post Secondary','Upper Secondary','Lower Secondary','Primary','Incomplete Primary','No Education','Under 15')) %>%
            select(iso = Code, year = Year, value = Primary) %>%
            mutate(year = as.character(year),
                   value = ifelse(value == 0, NA, value),
                   iso = tolower(iso)),
            by = c('year','iso')) %>%
  # merge with population data
  left_join(ssp2_population_raw %>%
              rename(country_pop = value) %>%
              mutate(year = as.character(year)) %>%
              filter(year >= 2010),
            by = c('iso','year','region','GCAM_region_ID')) %>%
  # compute regional population and countries weight
  filter(!is.na(value)) %>%
  group_by(year,GCAM_region_ID,region) %>%
  mutate(region_pop = round(sum(country_pop))) %>%
  ungroup() %>%
  mutate(weight = country_pop / region_pop) %>%
  mutate(value_Nw = value * weight) %>%
  group_by(year,GCAM_region_ID,region) %>%
  summarise(value = round(sum(value_Nw))) %>%
  ungroup() %>%
  # interpolate
  group_by(region, GCAM_region_ID) %>%
  do(linear_interpolation(., 'population')) %>%
  mutate(value = round(value)) %>%
  ungroup() %>%
  # arrange
  distinct(.) %>%
  arrange(GCAM_region_ID,year)
writexl::write_xlsx(ssp2_education_Primary, 'regression_data/ssp2_education_Primary_data.xlsx')



##### ============= FAO ==================


fao = read.csv('regression_data/data_to_select/FAOSTAT_data_en_9-25-2023.csv') %>%
  setNames(c('Domain Code','Domain','Area Code (M49)','Area','Element Code','Element','Item Code','Item','Year Code','Year','Unit','Value','Flag','Flag Description','Note')) %>%
  # merge with GCAM regions
  rename_area() %>%
  rename(country_name = Area) %>%
  left_join(regions_key %>%
              filter(iso != 'rou'), by = c('country_name')) %>%
  filter(!is.na(iso))
write.csv(fao, 'regression_data/fao.csv')


####### OBESITY
fao_obesity =
  # basic dataset
  tidyr::expand_grid(regions_key, data.frame(year = as.character(seq(1960,2100,1)))) %>% select(-iso,-country_name) %>%
  distinct(.) %>%
  # join fao dataset
  left_join(fao %>%
              filter(Item == 'Number of obese adults (18 years and older) (million)') %>%
              select(year = Year, region, country_name, iso, GCAM_region_ID, value = Value, Unit, Item) %>%
              # to Million No to No
              mutate(value = as.numeric(value)) %>%
              mutate(value = 1e6 * value) %>%
              mutate(Item = 'Number of obese adults (18 years and older) (nº)') %>%
              # regional obesity
              group_by(year,GCAM_region_ID,region,Item) %>%
              summarise(value_reg = sum(value, na.rm = TRUE)) %>%
              ungroup(),
            by = c('region','GCAM_region_ID','year')) %>%
  # reorder
  arrange(year,GCAM_region_ID)
writexl::write_xlsx(fao_obesity, 'regression_data/fao_obesity.xlsx')


####### UNDERNOURISHMENT
fao_undernourishment =
  # basic dataset
  regions_key %>%
  select(region, GCAM_region_ID) %>%
  crossing(year = seq(1960, 1959 + 62)) %>%
  # join fao dataset
  left_join(fao %>%
              filter(Item == 'Number of people undernourished (million) (3-year average)') %>%
              select(year = Year, region, country_name, iso, GCAM_region_ID, value = Value, Unit, Item) %>%
              # compute year
              mutate(year = as.numeric(substr(year, 6, 9)) + 1) %>%
              # to Million No to No
              mutate(value = as.numeric(value)) %>%
              mutate(value = 1e6 * value) %>%
              mutate(Item = 'Number of people undernourished (nº)') %>%
              # regional obesity
              group_by(year,GCAM_region_ID,region,Item) %>%
              summarise(value_reg = sum(value)) %>%
              ungroup(),
            by = c('region','GCAM_region_ID','year')) %>%
  # reorder
  arrange(year,GCAM_region_ID)
writexl::write_xlsx(fao_undernourishment, 'regression_data/fao_undernourishment.xlsx')

