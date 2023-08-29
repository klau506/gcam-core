
model = 'binomial'  # binomial

folder = paste0('outputs_',model)
library(ggplot2)
library(dplyr)

# combine the data frames into a single dataset
file_list <- list.files(folder, pattern = "^L202", full.names = TRUE)

data_list <- list()
for (f in file_list) {
  data <- get(load(f))
  data$id <- as.numeric(gsub(".*_(.*)\\.RData$", "\\1", f))
  data_list[[f]] <- data
}
all_data <- do.call(rbind, data_list) %>%
  group_by(year, region) %>%
  mutate(median_flex = median(flex),
         min_flex = min(flex),
         max_flex = max(flex)) %>%
  ungroup()
save(all_data, file = paste0(folder,'/L202.flexitarian_population_all.RData'))

# load the parameters data
param = get(load(paste0(folder,'/L201.flexitarian_parameters.RData')))



##############
###### figures
##############

# flex percentage
ggplot(all_data %>%
         filter(year >= 2015) %>%
         mutate(flex_percentage = 100*flex/population) %>%
         mutate(median_flex_percentage = 100*median_flex/population) %>%
         mutate(min_flex_percentage = 100*min_flex/population) %>%
         mutate(max_flex_percentage = 100*max_flex/population)) +
  geom_line(aes(x = year, y = flex_percentage, group = interaction(id, region), color = region), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_flex_percentage, color = region), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_flex_percentage, ymax = max_flex_percentage, fill = region), alpha = 0.15) +  # Shadow
  geom_text(data = all_data %>%
              filter(year >= 2015) %>%
              mutate(median_flex_percentage = 100*median_flex/population) %>%
              group_by(region, id) %>%
              mutate(last_median_flex_percentage = median_flex_percentage[which(year == 2100)]) %>%
              ungroup() %>%
              select(year, region, last_median_flex_percentage) %>%
              distinct(., .keep_all = TRUE),
            aes(x = 2100, y = last_median_flex_percentage, label = region, color = region), hjust = -0.1, vjust = 0.5, size = 3) +
  labs(x = 'Year', y = 'Regional percentage of flexitarians', title = paste('Cumulative percentage of flexitarians with', model, 'model')) +
  theme_minimal() +
  theme(legend.position = 'bottom')
ggsave(file = paste0('figures_',model,'/flex_percentage_ci.pdf'), width = 500, height = 300, units = 'mm')

# cum flex
ggplot(all_data %>%
         filter(year >= 2015)) +
  geom_line(aes(x = year, y = flex, group = interaction(id, region), color = region), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_flex, color = region), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_flex, ymax = max_flex, fill = region), alpha = 0.15) +  # Shadow
  geom_text(data = all_data %>%
              filter(year >= 2015) %>%
              group_by(region, id) %>%
              mutate(last_median_flex = median_flex[which(year == 2100)]) %>%
              ungroup() %>%
              select(year, region, last_median_flex) %>%
              distinct(., .keep_all = TRUE),
            aes(x = 2100, y = last_median_flex, label = region, color = region), hjust = -0.1, vjust = 0.5, size = 3) +  # Text
  labs(x = 'Year', y = 'Regional nº of flexitarians', title = paste('Cumulative nº of flexitarians with', model, 'model')) +
  theme_minimal() +
  theme(legend.position = 'bottom')
ggsave(file = paste0('figures_',model,'/flex_number_ci.pdf'), width = 500, height = 300, units = 'mm')

# new flex
dd = all_data %>%
  filter(year >= 2015) %>% #filter(region == 'China') %>%
  group_by(id, region) %>%
  mutate(new_flex = flex - lag(flex))

ggplot(all_data %>%
         filter(year >= 2015) %>%
         group_by(region, id) %>%
         mutate(new_flex = flex - lag(flex)) %>%
         mutate(min_new_flex = min_flex - lag(min_flex)) %>%
         mutate(max_new_flex = max_flex - lag(max_flex)) %>%
         mutate(median_new_flex = median(new_flex))) +
  geom_line(aes(x = year, y = new_flex, group = interaction(region, id), color = region), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_new_flex, color = region), linewidth = 1, alpha = 1) +  # Median line
  geom_ribbon(aes(x = year, ymin = min_new_flex, ymax = max_new_flex, fill = region), alpha = 0.15) +  # Shadow
  geom_text(data = all_data %>%
              filter(year >= 2015) %>%
              group_by(region, id) %>%
              mutate(new_flex = flex - lag(flex)) %>%
              mutate(mid_new_flex = new_flex[which(year == 2030)]) %>%
              ungroup() %>% group_by(region) %>%
              mutate(mid_new_flex = median(mid_new_flex)),
            aes(x = 2030, y = mid_new_flex, label = region, color = region), hjust = -0.1, vjust = 0.5, size = 3) +
  labs(x = 'Year', y = 'Regional nº of flexitarians', title = paste('Nº of new flexitarians with', model, 'model')) +
  theme_minimal() +
  theme(legend.position = 'bottom')
ggsave(file = paste0('figures_',model,'/flex_new_ci.pdf'), width = 500, height = 300, units = 'mm')
