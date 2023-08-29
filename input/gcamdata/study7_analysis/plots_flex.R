
model = 'binomial'  # binomial or bass

folder = paste0('outputs_',model)
library(ggplot2)
library(dplyr)

i = 3
data = get(load(paste0(folder,'/L202.Flexitarian_population_',i,'.RData')))
param = get(load(paste0(folder,'/L201.flexitarian_parameters.RData')))

# flex percentage
ggplot(data %>%
         filter(year >= 2015) %>%
         mutate(flex_percentage = 100*flex/population) %>%
         group_by(region) %>%
         mutate(last_flex_percentage = flex_percentage[which(year == 2100)])) +
  geom_line(aes(x = year, y = flex_percentage, color = region)) +
  geom_text(aes(x = 2100, y = last_flex_percentage, label = region, color = region), hjust = -0.1, vjust = 0.5, size = 3) +
  labs(x = 'Year', y = 'Regional percentage of flexitarians', title = paste('Cumulative percentage of flexitarians with', model, 'model'))
ggsave(file = paste0('figures_',model,'/flex_percentage_',i,'.png'), width = 400, height = 180, units = 'mm')

# cum flex
ggplot(data %>%
         filter(year >= 2015) %>%
         group_by(region) %>%
         mutate(last_flex = flex[which(year == 2100)])) +
  geom_line(aes(x = year, y = flex, color = region)) +
  geom_text(aes(x = 2100, y = last_flex, label = region, color = region), hjust = -0.1, vjust = 0.5, size = 3) +
  labs(x = 'Year', y = 'Regional nº of flexitarians', title = paste('Cumulative nº of flexitarians with', model, 'model'))
ggsave(file = paste0('figures_',model,'/flex_number_',i,'.png'), width = 400, height = 180, units = 'mm')

# new flex
ggplot(data %>%
         filter(year >= 2015) %>%
         group_by(region) %>%
         mutate(new_flex = flex - lag(flex)) %>%
         mutate(mid_new_flex = new_flex[which(year == 2050)])) +
  geom_line(aes(x = year, y = new_flex, color = region)) +
  geom_text(aes(x = 2050, y = mid_new_flex, label = region, color = region), hjust = -0.1, vjust = 0.5, size = 3) +
  labs(x = 'Year', y = 'Regional nº of flexitarians', title = paste('Nº of new flexitarians with', model, 'model'))
ggsave(file = paste0('figures_',model,'/flex_new_',i,'.png'), width = 400, height = 180, units = 'mm')
