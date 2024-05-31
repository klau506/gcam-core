prob_distrib_macronutrients = function(df, y) {

  df = df %>%
    dplyr::filter(year == y) %>%
    dplyr::mutate(scen_type = toupper(scen_type)) %>%
    cut_region_names()

  df_medi <- df[, .(medi = quantile(value, 0.5)),
                by=c('year','macronutrient','region',
                     'scen_type','scen_path')]

  df = data.table(df)

  for (i in c(1,2)) {
    reg = unique(df$region)
    if (i == 1) {
      reg = reg[1:(length(reg)/2)]
    } else {
      reg = reg[(length(reg)/2+1):length(reg)]
    }

    df_reg <- df %>%
      dplyr::filter(region %in% reg)
    df_medi_reg <- df_medi %>%
      dplyr::filter(region %in% reg)

    pl <- ggplot(df_reg) +
      geom_density(data = df_reg %>% filter(scen_type != 'REF'), aes(x = value,
                                                                 color = scen_type, fill = scen_type, linetype = scen_path),
                   linewidth = 0.8, alpha = 0.25) +
      geom_vline(aes(color = scen_type, fill = scen_type, linetype = scen_path, xintercept = medi),
                 data = df_medi_reg, linewidth = 1) +
      facet_grid(region ~ macronutrient, scales = 'free') +
      scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                        name = 'Scenario type',
                        labels = scen_palette_refVsSppVsSnrVsSppnr.labs)+
      scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                         name = 'Scenario type',
                         labels = scen_palette_refVsSppVsSnrVsSppnr.labs) +
      scale_linetype_manual(values = scen_path_palette_refVsSppVsSnrVsSppnr,
                            name = 'Scenario path',
                            labels = scen_path_palette_refVsSppVsSnrVsSppnr.labs) +
      labs(y = 'Probability density', x = 'Macronutrient consumption as a percentage of energy intake [capita/day]') +
      theme_light() +
      theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
            strip.background = element_blank(),
            strip.text = element_text(color = 'black', size = 45),
            axis.title = element_text(size=50),
            axis.text = element_text(size=45),
            legend.text = element_text(size = 45),
            legend.title = element_text(size = 50))

    ggsave(pl, filename = file.path(figures_path, paste0('sdg3_SI_macronutrient_probdistrib_reg_',i,'.pdf')),
           width = 1000, height = 3500, units = 'mm', limitsize = F)

    if (i == 1) { pl1 <- pl } else { pl2 <- pl }

  }

  blank_p <- patchwork::plot_spacer() + theme_void()
  legend = ggpubr::get_legend(pl1 +
                                theme(legend.direction = 'horizontal'))

  pl <- cowplot::ggdraw() +
    cowplot::draw_plot(pl1 + theme(legend.position = 'none'),
                       x = 0.0, y = 0.025, width = 0.5, height = 0.975) +
    cowplot::draw_plot(pl2 + theme(legend.position = 'none',
                                   axis.title.y = element_blank()),
                       x = 0.5, y = 0.025, width = 0.5, height = 0.975) +
    cowplot::draw_plot(cowplot::plot_grid(legend,blank_p,nrow=1), x = 0.225, y = -0.485, width = 1, height = 1)

  ggsave(pl, filename = file.path(figures_path, paste0('sdg3_SI_macronutrient_probdistrib_reg.pdf')),
         width = 2000, height = 2000, units = 'mm', limitsize = F)

}
