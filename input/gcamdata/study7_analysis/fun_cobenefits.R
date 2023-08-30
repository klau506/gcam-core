## fig2 plot A
#' @param mort_diff: dataset
#' @param type: percentage or abs_numbers
do_fig2_plA = function(mort_diff, type) {

  # subset dataset
  pl_mort_diff = mort_diff %>%
    dplyr::filter(year == selected_year) %>%
    dplyr::mutate('fasst_region' = region)

  # merge dataset with world regions
  world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
    dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM',adm0_a3)) %>%
    dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD',adm0_a3))
  world <- subset(world,!adm0_a3 %in% c("ATA","FJI"))

  fasst_reg = rfasst::fasst_reg %>%
    dplyr::rename('ISO3' = 'subRegionAlt')
  pl_mort_diff = merge(pl_mort_diff, fasst_reg, by = 'fasst_region')

  world <- merge(world,pl_mort_diff, by.x = "adm0_a3", by.y = "ISO3")

  leg = ifelse(type == 'percentage', 'Avoided premature deaths (%)\n',
               'Avoided premature deaths\n')

  # plot
  pl_mort_map <<- ggplot() +
    # color map by regions
    geom_sf(data = world, aes(fill = value)) +
    scale_fill_gradient2(low = "#C60000", high = "#0DA800",
                         mid = '#C2DAC1', midpoint = 0,
                         name = leg) +
    facet_wrap(.~scenario) +
    # theme
    theme_light() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#ffffff",
                                          colour = "#ffffff"),
          legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
          legend.text = element_text(size = 30), legend.title = element_text(size = 40),
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="white"), title = element_text(size = 40))

  file_name = ifelse(type == 'percentage', 'pl2_Apercentage',
                     'pl2_Aabsnum')
  ggsave(pl_mort_map, file = paste0(figures_path,"tmp_figs/",file_name,'.png'), width = 300, height = 125, units = 'mm')



  ## -- total deaths
  deaths = mort_simpl %>% rename_scen() %>%
    group_by(year,scenario) %>%
    summarise(value = sum(total_mort)) %>%
    ungroup()
  pl_mort_global <<- ggplot(data = deaths,
                            aes(x = as.factor(year), y = value, group = scenario,
                                color = scenario), alpha = 0.5) +
    geom_line(alpha = 0.75, linewidth = 2) +
    scale_color_manual(values = mypal_scen, name = 'Scenario') +
    # labs
    labs(y = 'Premature deaths', x = '') +
    # theme
    theme_light() +
    guides(color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
    theme(plot.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = 'bottom', legend.direction = 'horizontal',
          strip.background = element_blank(),
          axis.text.x = element_text(size=30),
          axis.text.y = element_text(size=30),
          axis.title = element_text(size=30),
          legend.text = element_text(size = 30),
          legend.title = element_text(size = 40),
          title = element_text(size = 40))

  ggsave(pl_mort_global, file = paste0(figures_path,'tmp_figs/pl_deaths_global.png'), width = 500, height = 300, units = 'mm')

}
