library(tidyverse)
library(ggtext)
library(ggdist)
library(glue)
library(patchwork)

df_plot <- plant_percentage %>%
  cut_region_names() %>%
  dplyr::mutate(value = 100 * value) %>%
  dplyr::filter(year == year_fig, !scen_type %in% c('ref','snr')) %>%
  dplyr::group_by(scen_type, scen_path, region) %>%
  dplyr::mutate(median_value = median(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scen_type = toupper(scen_type)) %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('REF', 'SPP','SNR','SPPNR'))) %>%
  dplyr::mutate(region = factor(region, levels = (sort(unique(region)))))
data_ref <- plant_percentage %>%
  cut_region_names() %>%
  dplyr::mutate(value = 100 * value) %>%
  dplyr::filter(year == year_fig, scen_type %in% c('ref')) %>%
  dplyr::select(value, region)
data_reff <- bind_rows(data_ref %>%
                         mutate(scen_type = 'SPP'),
                       data_ref %>%
                         mutate(scen_type = 'SPPNR')) %>%
  dplyr::mutate(region = factor(region, levels = rev(sort(unique(region)))))

# plot
p <- ggplot(data = df_plot %>%
              dplyr::mutate(region = factor(region, levels = rev(sort(unique(region)))))) +
  stat_halfeye(data = df_plot %>% filter(scen_path == 'all') %>%
                 dplyr::mutate(region = factor(region, levels = rev(sort(unique(region))))),
               aes(x = region, y = value, fill = scen_path,
                   color = scen_path),
               side = "right", alpha = 0.3) +
  stat_halfeye(data = df_plot %>% filter(scen_path == 'plus') %>%
                 dplyr::mutate(region = factor(region, levels = rev(sort(unique(region))))),
               aes(x = region, y = value, fill = scen_path,
                   color = scen_path),
               side = "left", alpha = 0.3) +
  geom_point(data = df_plot %>% filter(scen_path == 'all') %>%
               dplyr::mutate(region = factor(region, levels = rev(sort(unique(region))))),
             aes(x = region, y = median_value, fill = scen_path,
                 color = scen_path), alpha = 1, size = 6, shape = 19) +
  geom_point(data = df_plot %>% filter(scen_path == 'plus') %>%
               dplyr::mutate(region = factor(region, levels = rev(sort(unique(region))))),
             aes(x = region, y = median_value, fill = scen_path,
                 color = scen_path), alpha = 1, size = 6, shape = 19) +
  geom_point(data = data_reff,
             aes(x = region, y = value), size = 7, alpha = 0.95, shape = 18, stroke = 2) +
  scale_color_manual(values = scen_path_palette_col_refVsSppVsSnrVsSppnr,
                     labels = scen_path_palette_col_refVsSppVsSnrVsSppnr.labs,
                     name = 'Pathway') +
  scale_fill_manual(values = scen_path_palette_col_refVsSppVsSnrVsSppnr,
                    labels = scen_path_palette_col_refVsSppVsSnrVsSppnr.labs,
                    name = 'Pathway') +
  coord_flip() +
  facet_grid(. ~ scen_type) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=27.5),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        plot.background = element_rect(fill = "white", color = NA),
        axis.title = element_text(size = 35)) +
  labs(x = '', y = 'Plant protein intake share')



# create the dataframe for the legend (inside plot)
df_for_legend <- df_plot %>%
  filter(region == 'Africa_Western',
         scen_type == 'SPP')

p_legend <- ggplot(df_for_legend) +
  stat_halfeye(data = df_for_legend %>% filter(scen_path == 'all') %>%
                 dplyr::mutate(region = factor(region, levels = rev(sort(unique(region))))),
               aes(x = region, y = value, fill = scen_path,
                   color = scen_path),
               side = "right", alpha = 0.3) +
  stat_halfeye(data = df_for_legend %>% filter(scen_path == 'plus') %>%
                 dplyr::mutate(region = factor(region, levels = rev(sort(unique(region))))),
               aes(x = region, y = value, fill = scen_path,
                   color = scen_path),
               side = "left", alpha = 0.3) +
  geom_point(data = df_for_legend %>% filter(scen_path == 'all') %>%
               dplyr::mutate(region = factor(region, levels = rev(sort(unique(region))))),
             aes(x = region, y = median_value, fill = scen_path,
                 color = scen_path), alpha = 1, size = 6, shape = 19) +
  geom_point(data = df_for_legend %>% filter(scen_path == 'plus') %>%
               dplyr::mutate(region = factor(region, levels = rev(sort(unique(region))))),
             aes(x = region, y = median_value, fill = scen_path,
                 color = scen_path), alpha = 1, size = 6, shape = 19) +
  scale_color_manual(values = scen_path_palette_col_refVsSppVsSnrVsSppnr,
                     labels = scen_path_palette_col_refVsSppVsSnrVsSppnr.labs,
                     name = 'Pathway') +
  scale_fill_manual(values = scen_path_palette_col_refVsSppVsSnrVsSppnr,
                    labels = scen_path_palette_col_refVsSppVsSnrVsSppnr.labs,
                    name = 'Pathway') +
  coord_flip() +
  labs(x = '',y = '') +
  annotate(
    "richtext",
    y = c(68, 72, 65, 81),
    x = c(1.5,0.5,0.6,0.9),
    label = c("Distribution<br>of protein intake shares<br>following the all pathway",
              "Distribution<br>of protein intake shares<br>following the plus pathway",
              "Median following<br>the all pathway",
              "Median following<br>the plus pathway"),
    fill = NA, label.size = NA, size = 2, vjust = 1
  ) +
  geom_curve(
    data = data.frame(
      x = c(1.4,0.5,0.6,0.9),
      xend = c(1.25, 0.75, 0.975, 1.025),
      y = c(67, 73, 65, 80.5),
      yend = c(66, 74, 64.25, 77.5)),
    aes(x = x, xend = xend, y = y, yend = yend),
    stat = "unique", curvature = 0.2, linewidth = 0.2, color = "grey12",
    arrow = arrow(angle = 20, length = unit(1, "mm"))
  ) +
  theme_light() +
  theme(legend.position = 'none',
        strip.background = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        axis.title = element_blank(),
        axis.text = element_blank())

# Insert the custom legend into the plot
pl_plant <- p#+ inset_element(p_legend, l = 0.6, r = 1.0,  t = 0.99, b = 0.7, clip = FALSE)
ggsave(pl_plant, filename = file.path(figures_path, 'sdg0_pl_plant.pdf'),
       width = 750, height = 800, units = 'mm', limitsize = F)

