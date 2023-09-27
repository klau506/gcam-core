protein_commodities = c('Legumes','NutsSeeds', 'Nuts_Seeds','Pork','Beef','Dairy','OtherMeat_Fish','Poultry','SheepGoat')


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
##### ============== Socio economics ==================
##### =================================================

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

##### ============= GDP ==================

gdp = read.csv('regression_data/data_to_select/API_NY.GDP.MKTP.KD_DS2_en_csv_v2_5871611/API_NY.GDP.MKTP.KD_DS2_en_csv_v2_5871611.csv', skip = 3) %>%
  rename_with(~gsub("^X", "", .x), starts_with("X")) %>%
  select(-ncol(.)) %>%
  # add Taiwan as an empty line
  bind_rows(.,
            data.frame(
              Country.Name = 'Taiwan',
              Country.Code = 'twn',
              Indicator.Name = 'GDP (constant 2015 US$)',
              Indicator.Code = 'NY.GDP.MKTP.KD',
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
write.csv(gdp, 'regression_data/gdp.csv')

##### ============= POPULATION (TOTAL) ==================

population = read.csv('regression_data/data_to_select/API_SP.POP.TOTL_DS2_en_csv_v2_5871594/API_SP.POP.TOTL_DS2_en_csv_v2_5871594.csv', skip = 3) %>%
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
write.csv(population, 'regression_data/population.csv')

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
  mutate('urban_N' = round(total_pop_N * urban_P / 100),0) %>%
  mutate(Indicator.Name = 'Urban population (nº of population)') %>%
  select(-c(urban_P,total_pop_N)) %>%
  tidyr::pivot_wider(names_from = 'year', values_from = 'urban_N') %>%
  # add Taiwan as an empty line
  bind_rows(.,
            data.frame(
              Country.Name = 'Taiwan',
              Country.Code = 'twn',
              Indicator.Name = 'Urban population (nº of total population)',
              Indicator.Code = 'SP.URB.TOTL.IN.ZS',
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
write.csv(population_urban, 'regression_data/population_urban.csv')



##### ============= GINI index ==================

gini = read.csv('regression_data/data_to_select/API_SI.POV.GINI_DS2_en_csv_v2_5871599/API_SI.POV.GINI_DS2_en_csv_v2_5871599.csv', skip = 3) %>%
  rename_with(~gsub("^X", "", .x), starts_with("X")) %>%
  select(-ncol(.)) %>%
  # add gini index from RAO paper
  tidyr::pivot_longer(cols = `1960`:`2022`, names_to = 'year', values_to = 'gini_index1') %>%
  filter(!is.na(gini_index1)) %>%
  full_join(readxl::read_excel('regression_data/data_to_select/NRao_et_al_GiniProjections_2018/Gini_projections_SSPs.xlsx',
                               sheet = 'projected_ginis_full-set') %>%
              tidyr::pivot_longer(cols = 3:ncol(.), names_to = 'Country.Code', values_to = 'gini_index2') %>%
              mutate(year = as.character(year)) %>%
              filter(scenario == 'SSP2', !is.na(gini_index2), year <= 2022) %>% select(-scenario)) %>%
  full_join(read.csv('regression_data/data_to_select/pip_worldbank.csv') %>%
              select(Country.Code = country_code, year = reporting_year, gini_index3 = gini) %>%
              mutate(year = as.character(year)) %>%
              filter(!is.na(gini_index3), year <= 2022) %>%
              group_by(Country.Code,year) %>%
              summarise(gini_index3 = 100*mean(gini_index3)),
            by = c('year','Country.Code')) %>%
  ungroup() %>%
  group_by(Country.Name, Country.Code, Indicator.Name, Indicator.Code, year) %>%
  mutate(gini_index = mean(c(gini_index1,gini_index2,gini_index3), na.rm = TRUE)) %>%
  ungroup() %>%
  select(-c(gini_index1,gini_index2,gini_index3)) %>%
  # compute weight of each country within its region (percentage of population among the
  # countries that have data for that year and are from that region)
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
  filter(!is.na(gini_index)) %>%
  group_by(year,GCAM_region_ID,region) %>%
  mutate(region_pop = round(sum(country_pop))) %>%
  ungroup() %>%
  mutate(weight = country_pop / region_pop) %>%
  mutate(gini_w = gini_index * weight) %>%
  group_by(year,GCAM_region_ID,region,Indicator.Name,Indicator.Code) %>%
  summarise(gini_reg = round(sum(gini_w))) %>%
  ungroup() %>%
  # add empty lines when data missing
  right_join(regions_key %>%
               select(region, GCAM_region_ID) %>%
               mutate(Indicator.Name = 'Gini index',
                      Indicator.Code = 'SI.POV.GINI') %>%
               crossing(year = as.character(seq(1960, 1959 + 62)))
  ) %>%
  # reorder
  arrange(year,GCAM_region_ID)
write.csv(gini, 'regression_data/gini.csv')

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
               mutate(Indicator.Name = 'Primary completion rate, total (% of relevant age group)',
                      Indicator.Code = 'SE.PRM.CMPT.ZS') %>%
               crossing(year = as.character(seq(1960, 1959 + 62)))
  ) %>%
  # reorder
  arrange(year,GCAM_region_ID)
writexl::write_xlsx(education_Primary, 'regression_data/education_Primary.xlsx')

##### ============= EDUCATION (primary) ==================

education_Primary = read.csv('regression_data/data_to_select/API_SE.PRM.CMPT.ZS_DS2_en_csv_v2_5873993/API_SE.PRM.CMPT.ZS_DS2_en_csv_v2_5873993.csv', skip = 3) %>%
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
  # compute regional education
  mutate(value_N = value * country_pop) %>%
  group_by(year,GCAM_region_ID,region,Indicator.Name,Indicator.Code) %>%
  summarise(value_reg = round(sum(value_N))) %>%
  ungroup() %>%
  # add empty lines when data missing
  right_join(regions_key %>%
               select(region, GCAM_region_ID) %>%
               mutate(Indicator.Name = 'Primary completion rate, total (% of relevant age group)',
                      Indicator.Code = 'SE.PRM.CMPT.ZS') %>%
               crossing(year = as.character(seq(1960, 1959 + 62)))
  ) %>%
  # reorder
  arrange(year,GCAM_region_ID)
write.csv(education_Primary, 'regression_data/education_Primary.csv')

##### ============= FAO ==================

rename_area = function(data) {
  data = data %>%
    mutate(Area = ifelse(Area == 'Antigua and Barbuda', 'Antigua & Barbuda', Area)) %>%
    mutate(Area = if_else(Area %in% c('China, Macao SAR', 'China, mainland'), 'China', Area)) %>%
    mutate(Area = ifelse(Area == 'China, Hong Kong SAR', 'Hong Kong', Area)) %>%
    mutate(Area = ifelse(Area == 'China, Taiwan Province of', 'Taiwan', Area)) %>%
    mutate(Area = ifelse(Area == 'Czechia', 'Czech Republic', Area)) %>%
    mutate(Area = ifelse(Area == "CÃ´te d'Ivoire", 'Cote dIvoire', Area)) %>%
    mutate(Area = ifelse(Area == "Democratic Republic of the Congo", 'Congo, the Democratic Republic of the', Area)) %>%
    mutate(Area = ifelse(Area == "Iran (Islamic Republic of)", 'Iran, Islamic Republic of', Area)) %>%
    mutate(Area = ifelse(Area == "Libya", 'Libyan Arab Jamahiriya', Area)) %>%
    mutate(Area = ifelse(Area == "Netherlands (Kingdom of the)", 'Netherlands', Area)) %>%
    mutate(Area = ifelse(Area == "Palestine", 'Palestinian Territory, Occupied', Area)) %>%
    mutate(Area = ifelse(Area == "Republic of Moldova", 'Moldova, Republic of', Area)) %>%
    mutate(Area = ifelse(Area == "TÃ¼rkiye", 'Turkey', Area)) %>%
    mutate(Area = ifelse(Area == "United Republic of Tanzania", 'Tanzania, United Republic of', Area)) %>%
    mutate(Area = ifelse(Area == "Venezuela (Bolivarian Republic of)", 'Venezuela', Area)) %>%
    mutate(Area = ifelse(Area == "United Kingdom of Great Britain and Northern Ireland", 'United Kingdom', Area)) %>%
    mutate(Area = ifelse(Area == "Timor-Leste", 'Timor Leste', Area)) %>%
    mutate(Area = ifelse(Area == "Republic of Korea", 'Korea, Republic of', Area)) %>%
    mutate(Area = ifelse(Area == "North Macedonia", 'Macedonia, the former Yugoslav Republic of', Area)) %>%
    mutate(Area = ifelse(Area == "Micronesia (Federated States of)", 'Micronesia, Federated States of', Area)) %>%
    mutate(Area = ifelse(Area == "Lao People's Democratic Republic", 'Lao Peoples Democratic Republic', Area)) %>%
    mutate(Area = ifelse(Area == "Bolivia (Plurinational State of)", 'Bolivia', Area)) %>%
    mutate(Area = ifelse(Area == "Democratic People's Republic of Korea", 'Korea, Democratic Peoples Republic of', Area))

  return(invisible(data))
}

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
fao_obesity = fao %>%
  filter(Item == 'Number of obese adults (18 years and older) (million)') %>%
  select(year = Year, region, country_name, iso, GCAM_region_ID, value = Value, Unit, Item) %>%
  # to Million No to No
  mutate(value = as.numeric(value)) %>%
  mutate(value = 1e6 * value) %>%
  mutate(Item = 'Number of obese adults (18 years and older) (nº)') %>%
  # regional obesity
  group_by(year,GCAM_region_ID,region,Item) %>%
  summarise(value_reg = round(sum(value))) %>%
  ungroup() %>%
  # add empty lines when data missing
  right_join(regions_key %>%
               select(region, GCAM_region_ID) %>%
               mutate(Item = 'Number of obese adults (18 years and older) (million)') %>%
               crossing(year = as.character(seq(1960, 1959 + 62)))
  ) %>%
  # reorder
  arrange(year,GCAM_region_ID)
write.csv(fao_obesity, 'regression_data/fao_obesity.csv')


####### UNDERNOURISHMENT
fao_undernourishment = fao %>%
  filter(Item == 'Number of people undernourished (million) (3-year average)') %>%
  select(year = Year, region, country_name, iso, GCAM_region_ID, value = Value, Unit, Item) %>%
  # compute year
  mutate(year = as.numeric(substr(year, 6, 9)) + 1) %>%
  # to Million No to No
  mutate(value = as.numeric(value)) %>%
  mutate(value = 1e6 * value) %>%
  mutate(Item = 'Number of people undernourished (nº)') %>%
  # regional undernourishment
  group_by(year,GCAM_region_ID,region,Item) %>%
  summarise(value_reg = round(sum(value))) %>%
  ungroup() %>%
  # add empty lines when data missing
  right_join(regions_key %>%
               select(region, GCAM_region_ID) %>%
               mutate(Item = 'Number of people undernourished (nº)') %>%
               crossing(year = as.double(seq(1960, 1959 + 62)))
  ) %>%
  # reorder
  arrange(year,GCAM_region_ID)
write.csv(fao_undernourishment, 'regression_data/fao_undernourishment.csv')

