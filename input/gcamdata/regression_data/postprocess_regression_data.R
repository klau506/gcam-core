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

reg_data = readxl::read_xlsx('regression_data/regression_data.xlsx', sheet = 'aggregated_data')

##### plot consumption protein share PLANT by region
ggplot(reg_data,
       aes(year,`share plant/(plant+animal) protein consumption`)) +
  geom_line() +
  facet_wrap(. ~ country, scales = 'free') +
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'none', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 30),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 35),
        title = element_text(size = 40)) +
  labs(x = '', title = 'Annual regional share of plant protein/total protein (free scales)')
ggsave('regression_data/fig/share_PLANT_protein_reg_freeScales.png', width = 100, height = 100, units = 'cm')
ggplot(reg_data,
       aes(year,`share plant/(plant+animal) protein consumption`)) +
  geom_line() +
  facet_wrap(. ~ country, scales = 'fixed') +
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'none', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 30),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 35),
        title = element_text(size = 40)) +
  labs(x = '', title = 'Annual regional share of plant protein/total protein (fixed scales)')
ggsave('regression_data/fig/share_PLANT_protein_reg_fixedScales.png', width = 100, height = 100, units = 'cm')


##### plot consumption protein share ANIMAL by region
ggplot(reg_data %>%
         mutate(`share animal/(plant+animal) protein consumption` =
                  `animal protein consumption (Mt)` / (`animal protein consumption (Mt)` + `plant protein consumption (Mt)`)),
       aes(year,`share animal/(plant+animal) protein consumption`)) +
  geom_line() +
  facet_wrap(. ~ country, scales = 'free') +
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'none', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 30),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 35),
        title = element_text(size = 40)) +
  labs(x = '', title = 'Annual regional share of animal protein/total protein (free scales)')
ggsave('regression_data/fig/share_ANIMAL_protein_reg_freeScales.png', width = 100, height = 100, units = 'cm')
ggplot(reg_data %>%
         mutate(`share animal/(plant+animal) protein consumption` =
                  `animal protein consumption (Mt)` / (`animal protein consumption (Mt)` + `plant protein consumption (Mt)`)),
       aes(year,`share animal/(plant+animal) protein consumption`)) +
  geom_line() +
  facet_wrap(. ~ country, scales = 'fixed') +
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'none', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 30),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 35),
        title = element_text(size = 40)) +
  labs(x = '', title = 'Annual regional share of animal protein/total protein (fixed scales)')
ggsave('regression_data/fig/share_ANIMAL_protein_reg_fixedScales.png', width = 100, height = 100, units = 'cm')


##### plot consumption protein share ANIMAL VS PLANT by region
ggplot(reg_data %>%
         mutate(`share animal/(plant+animal) protein consumption` =
                  `animal protein consumption (Mt)` / (`animal protein consumption (Mt)` + `plant protein consumption (Mt)`)) %>%
         select(year, country, `share plant/(plant+animal) protein consumption`, `share animal/(plant+animal) protein consumption`) %>%
         tidyr::pivot_longer(cols = 3:4, names_to = 'share_type')) +
  geom_col(aes(x = year, y = value, fill = share_type)) +
  facet_wrap(. ~ country, scales = 'free') +
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 30),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 35),
        title = element_text(size = 40)) +
  labs(x = '', title = 'Annual regional share of animal and plant protein')
ggsave('regression_data/fig/share_BOTH_protein_reg.png', width = 100, height = 100, units = 'cm')
