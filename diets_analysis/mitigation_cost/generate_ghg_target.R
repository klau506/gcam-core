library(tidyr)
library(dplyr)
library(tibble)
library(readxl)

GWP <- read.csv("mitigation_cost/ghg_GWP_AR5.csv", skip = 1) %>%
  rename(ghg = GHG_gases)

co2 <- read_excel("mitigation_cost/Study1.xlsx", sheet = "CO2") %>%
  gather(year, value, -sce, -region, -ghg, -unit) %>%
  group_by(unit, sce, region, year) %>%
  summarise(value = sum(value)) %>%
  ungroup %>%
  mutate(ghg = "CO2",
         unit = "MTC")

luc <- read_excel("mitigation_cost/Study1.xlsx", sheet = "LUC_reg") %>%
  gather(year, value, -scenario, -region, -ghg, -unit) %>%
  group_by(unit, scenario, region, year) %>%
  summarise(value = sum(value)) %>%
  ungroup %>%
  mutate(ghg = "CO2LUC",
         unit = "MTC") %>%
  rename(sce = scenario)

ghg_total <- read_excel("mitigation_cost/Study1.xlsx", sheet = "nonCO2_reg") %>%
  gather(year, value, -sce, -region, -ghg, -unit) %>%
  bind_rows(luc) %>%
  bind_rows(co2) %>%
  filter(year >= 2015) %>%
  left_join(GWP, by = c("ghg")) %>%
  mutate(GWP = if_else(region == "Africa_Eastern" & ghg == "CO2LUC", 0, GWP)) %>%
  mutate(value = value * GWP,
         Units = "MtCO2e") %>%
  group_by(sce, region, year, Units) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  filter(year > 2015,
         year <= 2050)
write.csv(ghg_total, "mitigation_cost/ghg_policy_regional.csv", row.names = F)

ghg_total_world <- ghg_total %>%
  group_by(sce, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()
write.csv(ghg_total_world, "mitigation_cost/ghg_policy_world.csv", row.names = F)



write.csv(ghg_total, "mitigation_cost/ghg_policy.csv", row.names = F)


