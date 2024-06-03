require(plyr)
require(vioplot)


prob_distrib_nutrients = function(df, y, type) {

  df = df %>%
    dplyr::filter(year == y) %>%
    dplyr::mutate(scen_type = toupper(scen_type)) %>%
    cut_region_names()

  df_medi <- df[, .(medi = quantile(value, 0.5)),
                by=c('year','type','region',
                     'scen_type','scen_path')]

  df = data.table(df)

  if (type == 'micronutrient') {
    xlab = 'Micronutrient consumption [msg/capita/day]'
  } else {
    xlab = paste0('M',substr(type, 2, nchar(type)),' consumption as a percentage of energy intake [capita/day]')
  }

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
      geom_density(data = df_reg %>% filter(scen_type != 'REF'),
                   aes(x = value,color = scen_type,
                       fill = scen_type, linetype = scen_path),
                   linewidth = 0.8, alpha = 0.25) +
      geom_vline(aes(color = scen_type, fill = scen_type, linetype = scen_path, xintercept = medi),
                 data = df_medi_reg, linewidth = 1.5) +
      facet_grid(region ~ type, scales = 'free') +
      scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                        name = 'Scenario',
                        labels = scen_palette_refVsSppVsSnrVsSppnr.labs)+
      scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                         name = 'Scenario',
                         labels = scen_palette_refVsSppVsSnrVsSppnr.labs) +
      scale_linetype_manual(values = scen_path_palette_refVsSppVsSnrVsSppnr3,
                            name = 'Pathway',
                            labels = scen_path_palette_refVsSppVsSnrVsSppnr3.labs) +
      labs(y = 'Probability density', x = xlab) +
      theme_light() +
      theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
            strip.background = element_blank(),
            strip.text = element_text(color = 'black', size = 45),
            axis.title = element_text(size=50),
            axis.text = element_text(size=45),
            legend.text = element_text(size = 45),
            legend.title = element_text(size = 50)) +
      guides(color = guide_legend(override.aes = list(linewidth = 5)),
             fill = guide_legend(override.aes = list(linewidth = 5)),
             linetype = guide_legend(keywidth = 10,override.aes = list(linewidth = 5)))

    # ggsave(pl, filename = file.path(figures_path, paste0('sdg3_SI_',type,'_probdistrib_reg_',i,'.pdf')),
    #        width = 1000, height = 3500, units = 'mm', limitsize = F)

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

  ggsave(pl, filename = file.path(figures_path, paste0('sdg3_SI_',type,'_probdistrib_reg.pdf')),
         width = 2000, height = 2000, units = 'mm', limitsize = F)

}



cum_fun_nutrients = function(df, y, type) {

  df = df %>%
    dplyr::filter(year == y) %>%
    dplyr::mutate(scen_type = toupper(scen_type)) %>%
    cut_region_names()

  dat_tmp <- ddply(df, .(year,type,region,scen_type,scen_path),
                   summarize,
                   value = unique(value),
                   ecdf = ecdf(value)(unique(value)))

  if (type == 'micronutrient') {
    xlab = 'Micronutrient consumption [mcg/capita/day]'
  } else {
    xlab = paste0('M',substr(type, 2, nchar(type)),' consumption as a percentage of energy intake [capita/day]')
  }

  for (i in c(1,2)) {
    reg = unique(df$region)
    if (i == 1) {
      reg = reg[1:(length(reg)/2)]
    } else {
      reg = reg[(length(reg)/2+1):length(reg)]
    }

    dat_tmp_reg <- dat_tmp %>%
      dplyr::filter(region %in% reg)
    df_reg <- df %>%
      dplyr::filter(region %in% reg)

    pl <- ggplot(dat_tmp_reg %>%
                   filter(scen_type != 'REF'),
                 aes(value, ecdf, color = scen_type, fill = scen_type, linetype = scen_path)) +
      geom_line(linewidth = 2) +
      geom_point(data = df_reg %>%
                   filter(scen_type == 'REF'),
                 aes(x = value, y = 1, color = scen_type, fill = scen_type),
                 size = 7, alpha = 0.95, shape = 23, stroke = 2) +
      facet_grid(region ~ type, scales = 'free') +
      scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                        name = 'Scenario',
                        labels = scen_palette_refVsSppVsSnrVsSppnr.labs)+
      scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                         name = 'Scenario',
                         labels = scen_palette_refVsSppVsSnrVsSppnr.labs) +
      scale_linetype_manual(values = scen_path_palette_refVsSppVsSnrVsSppnr2,
                            name = 'Pathway',
                            labels = scen_path_palette_refVsSppVsSnrVsSppnr2.labs) +
      labs(y = 'Cumulative frequency', x = xlab) +
      theme_light() +
      theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
            strip.background = element_blank(),
            strip.text = element_text(color = 'black', size = 45),
            axis.title = element_text(size=50),
            axis.text = element_text(size=45),
            legend.text = element_text(size = 45),
            legend.title = element_text(size = 50)) +
      guides(color = guide_legend(override.aes = list(linewidth = 5)),
             fill = guide_legend(override.aes = list(linewidth = 5)),
             linetype = guide_legend(keywidth = 10,override.aes = list(linewidth = 5)))

    # ggsave(pl, filename = file.path(figures_path, paste0('sdg3_SI_',type,'_cumfun_reg_',i,'.pdf')),
    #        width = 1000, height = 3500, units = 'mm', limitsize = F)

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

  ggsave(pl, filename = file.path(figures_path, paste0('sdg3_SI_',type,'_cumfun_reg.pdf')),
         width = 2000, height = 2000, units = 'mm', limitsize = F)

}

#' violin_plot_landtype
#' @param data dataset
#' @param y figure year
#' @param type abs or per
#'
violin_plot_landtype <- function(data, y, type) {
  if (type == 'abs') {
    ylab = expression(paste('Change in thous. ', km^2, ' compared to Reference'))
  } else {
    ylab = 'Percentual change compared to Reference [%]'
  }
  pl_SI_land_area_violin <- ggplot(data = data %>%
                                     dplyr::filter(year == y) %>%
                                     dplyr::select(scen_type, scen_path, land_use_type, diff) %>%
                                     dplyr::distinct(),
                                   aes(x = land_use_type,
                                       y = diff,
                                       color = land_use_type,
                                       fill = land_use_type)) +
    geom_violin(trim = FALSE, alpha = 0.5, width = 1.2) +
    geom_jitter(width = 0.2, size = 0.5, alpha = 0.7) +
    geom_boxplot(width=0.2, color="black", alpha=0.5) +
    geom_hline(aes(yintercept = 0)) +
    facet_grid(scen_type ~ scen_path) +
    scale_fill_manual(values = land_use_scenario_palette, name = 'Land Type',
                      breaks = land_use_order_palette) +
    scale_color_manual(values = land_use_scenario_palette, name = 'Land Type',
                       breaks = land_use_order_palette) +
    # labs
    labs(y = ylab, x = '') +
    # theme
    theme_light() +
    theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
          strip.background = element_blank(),
          strip.text = element_text(color = 'black', size = 40),
          strip.text.y = element_text(angle = 0),
          axis.text.x = element_text(size=30, angle = 45, hjust = 1, vjust = 1),
          axis.text.y = element_text(size=30),
          legend.text = element_text(size = 35),
          legend.title = element_text(size = 40),
          title = element_text(size = 40))
  ggsave(pl_SI_land_area_violin, file = file.path(figures_path,paste0('sdg15_SI_land_area_violin_world_',type,'.pdf')),
         width = 500, height = 500, units = 'mm')

}

#' violin_plot_landtype
#' @param data dataset
#' @param y figure year
#' @param type abs or per
#'
violin_plot_landtype_regional <- function(data, y, type) {
  if (type == 'abs') {
    ylab = expression(paste('Change in thous. ', km^2, ' compared to Reference'))
  } else {
    ylab = 'Percentual change compared to Reference [%]'
  }

  data <- data %>%
    dplyr::filter(year == y) %>%
    dplyr::select(region, scen_type, scen_path, land_use_type, diff) %>%
    dplyr::distinct()

  for (i in c(1,2)) {
    reg = unique(data$region)
    if (i == 1) {
      reg = reg[1:(length(reg)/2)]
    } else {
      reg = reg[(length(reg)/2+1):length(reg)]
    }

    data_reg <- data %>%
      dplyr::filter(region %in% reg)

    pl_SI_land_area_violin <- ggplot(data = data_reg,
                                     aes(x = land_use_type,
                                         y = diff,
                                         color = land_use_type,
                                         fill = land_use_type)) +
      geom_violin(trim = FALSE, alpha = 0.5) +
      geom_jitter(width = 0.2, size = 0.5, alpha = 0.7) +
      geom_boxplot(width=0.2, color="black", alpha=0.5) +
      geom_hline(aes(yintercept = 0)) +
      facet_grid(region ~ scen_type + scen_path) +
      scale_fill_manual(values = land_use_scenario_palette, name = 'Land Type',
                        breaks = land_use_order_palette) +
      scale_color_manual(values = land_use_scenario_palette, name = 'Land Type',
                         breaks = land_use_order_palette) +
      # labs
      labs(y = ylab, x = '') +
      # theme
      theme_light() +
      theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
            strip.background = element_blank(),
            strip.text = element_text(color = 'black', size = 40),
            strip.text.y = element_text(angle = 0),
            axis.text.x = element_blank(), #element_text(size=30, angle = 45, hjust = 1, vjust = 1),
            axis.text.y = element_text(size=30),
            legend.text = element_text(size = 35),
            legend.title = element_text(size = 40),
            title = element_text(size = 40))
    ggsave(pl_SI_land_area_violin, file = file.path(figures_path,paste0('sdg15_SI_land_area_violin_regional_',type,'_',i,'.pdf')),
           width = 1000, height = 1500, units = 'mm', limitsize = FALSE)
  }

}
