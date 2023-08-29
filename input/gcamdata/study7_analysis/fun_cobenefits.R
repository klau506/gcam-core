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


## fig1 plot C: price
do_fig_prices = function() {
  # by abs value
  ag_regional_prices_diff = tidyr::pivot_wider(ag_regional_prices, names_from = 'scenario', values_from = 'value') %>%
    dplyr::mutate(across(Flex.ds.beh1:Flex.ds.beh2, ~ Reference - ., .names = "diff_protein_{.Flex.ds.beh}"))
  # dplyr::mutate('diff_trade' = Reference - Diets_Trade.adj21) %>%
    dplyr::filter(year == selected_year, sector %in% c('regional beef', 'regional pork', 'regional poultry', 'regional legumes',
                                                       'regional fruits', 'regional vegetables', 'regional oilcrop', 'regional root_tuber',
                                                       'regional corn', 'regional wheat', 'regional rice')) %>%
    dplyr::mutate(sector = gsub("_", " ", sector, fixed=TRUE)) %>%
    dplyr::mutate(sector = stringr::str_to_title(sub(".*regional ", "", sector)))
  ag_regional_prices_diff = tidyr::pivot_longer(ag_regional_prices_diff %>% dplyr::select(region,sector,year,diff_protein,diff_trade),
                                                cols = c('diff_protein','diff_trade')) %>%
    dplyr::rename('scenario' = 'name') %>%
    dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
    dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))

  pl_C1 <- ag_regional_prices_diff %>% dplyr::filter(scenario == 'Protein') %>%
    ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
    geom_bar(aes(x=reorder(region,value), y=value, fill=sector),stat="identity",col='black',size=0.2) +
    scale_fill_manual(name="",
                      values=c('#2C39FC','#3EA1DA','#1FDDED','#B800AC','#0E8600','#00BB19','#85E892','#B00000','#D05959','#D1D400','#CF8400'),
                      breaks=c("Beef","Pork","Poultry","Legumes","Rice","Corn","Wheat","Fruits","Vegetables","Oilcrop","Root Tuber")) +
    guides(fill = guide_legend(nrow = 2)) +
    geom_hline(yintercept = 0, linewidth = 1.5) +
    labs(y="$/Mt",x="",title="Protein")+
    theme_bw()+
    theme(
      strip.background = element_blank(),
      axis.text.x = element_text(size=30),
      axis.text.y = element_text(size=30),
      legend.text = element_text(size = 30),
      legend.title = element_text(size = 40),
      title = element_text(size = 40)
    ) +
    coord_flip()
  pl_C2 <- ag_regional_prices_diff %>% dplyr::filter(scenario == 'Trade') %>%
    ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
    geom_bar(aes(x=reorder(region,value), y=value, fill=sector),stat="identity",col='black',size=0.2) +
    scale_fill_manual(name="",
                      values=c('#2C39FC','#3EA1DA','#1FDDED','#B800AC','#0E8600','#00BB19','#85E892','#B00000','#D05959','#D1D400','#CF8400'),
                      breaks=c("Beef","Pork","Poultry","Legumes","Rice","Corn","Wheat","Fruits","Vegetables","Oilcrop","Root Tuber")) +
    guides(fill = guide_legend(nrow = 2)) +
    geom_hline(yintercept = 0, linewidth = 1.5) +
    labs(y="$/Mt",x="",title="Trade")+
    theme_bw()+
    theme(
      strip.background = element_blank(),
      axis.text.x = element_text(size=30),
      axis.text.y = element_text(size=30),
      legend.text = element_text(size = 30),
      legend.title = element_text(size = 40),
      title = element_text(size = 40)
    ) +
    coord_flip()

  pl_C = ggpubr::ggarrange(pl_C1, pl_C2, ncol=2,common.legend = T,legend = "bottom")

  ggsave(pl_C, file = paste0(figures_path,'extra_figs/pl_prices_absvalue.png'), width = 600, height = 700, units = 'mm')




  # by percentage
  ag_regional_prices_diff = tidyr::pivot_wider(ag_regional_prices, names_from = 'scenario', values_from = 'value') %>%
    dplyr::mutate('diff_protein' = 100*(Reference  - Diets_Protein)/Reference) %>%
    dplyr::mutate('diff_trade' = 100*(Reference - Diets_Trade.adj21)/Reference) %>%
    dplyr::filter(year == selected_year, sector %in% c('regional beef', 'regional pork', 'regional poultry', 'regional legumes',
                                                       'regional fruits', 'regional vegetables', 'regional oilcrop', 'regional root_tuber',
                                                       'regional corn', 'regional wheat', 'regional rice')) %>%
    dplyr::mutate(sector = gsub("_", " ", sector, fixed=TRUE)) %>%
    dplyr::mutate(sector = stringr::str_to_title(sub(".*regional ", "", sector)))
  ag_regional_prices_diff = tidyr::pivot_longer(ag_regional_prices_diff %>% dplyr::select(region,sector,year,diff_protein,diff_trade),
                                                cols = c('diff_protein','diff_trade')) %>%
    dplyr::rename('scenario' = 'name') %>%
    dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
    dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))

  pl_C1 <<- ag_regional_prices_diff %>% dplyr::filter(scenario == 'Protein') %>%
    ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
    geom_bar(aes(x=reorder(region,value), y=value, fill=sector),stat="identity",col='black',size=0.2) +
    scale_fill_manual(name="",
                      values=c('#2C39FC','#3EA1DA','#1FDDED','#B800AC','#0E8600','#00BB19','#85E892','#B00000','#D05959','#D1D400','#CF8400'),
                      breaks=c("Beef","Pork","Poultry","Legumes","Rice","Corn","Wheat","Fruits","Vegetables","Oilcrop","Root Tuber")) +
    guides(fill = guide_legend(nrow = 2)) +
    geom_hline(yintercept = 0, linewidth = 1.5) +
    labs(y="% $/MT",x="",title="Protein")+
    theme_bw()+
    theme(
      strip.background = element_blank(),
      axis.text.x = element_text(size=30),
      axis.text.y = element_text(size=35),
      legend.text = element_text(size = 35),
      legend.title = element_text(size = 40),
      title = element_text(size = 40),
      legend.spacing.x = unit(1, 'cm')
    ) +
    coord_flip()
  pl_C2 <<- ag_regional_prices_diff %>% dplyr::filter(scenario == 'Trade') %>%
    ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
    geom_bar(aes(x=reorder(region,value), y=value, fill=sector),stat="identity",col='black',size=0.2) +
    scale_fill_manual(name="",
                      values=c('#2C39FC','#3EA1DA','#1FDDED','#B800AC','#0E8600','#00BB19','#85E892','#B00000','#D05959','#D1D400','#CF8400'),
                      breaks=c("Beef","Pork","Poultry","Legumes","Rice","Corn","Wheat","Fruits","Vegetables","Oilcrop","Root Tuber")) +
    guides(fill = guide_legend(nrow = 2)) +
    geom_hline(yintercept = 0, linewidth = 1.5) +
    labs(y="% $/MT",x="",title="Trade")+
    theme_bw()+
    theme(
      strip.background = element_blank(),
      axis.text.x = element_text(size=30),
      axis.text.y = element_text(size=35),
      legend.text = element_text(size = 35),
      legend.title = element_text(size = 40),
      title = element_text(size = 40),
      legend.spacing.x = unit(1, 'cm')
    ) +
    coord_flip()

  pl_C = ggpubr::ggarrange(pl_C1, pl_C2, ncol=2,common.legend = T,legend = "right")

  ggsave(pl_C, file = paste0(figures_path,'tmp_figs/fig1/pl_prices.png'), width = 600, height = 700, units = 'mm')
  ggsave(pl_C, file = paste0(figures_path,'extra_figs/pl_prices_p.png'), width = 600, height = 700, units = 'mm')
}

do_water_figs = function() {

  ## -- total global water consumption
  pl_water_global <<- ggplot(data = water_consumption_total %>% rename_scen(),
                             aes(x = year, y = value, color = scenario), alpha = 0.5) +
    geom_line(alpha = 0.75, linewidth = 2) +
    scale_color_manual(values = mypal_scen, name = 'Scenario') +
    # labs
    labs(y = expression(paste("Annual Water flows (billion ",m^3,")")), x = '') +
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

  ggsave(pl_water_global, file = paste0(figures_path,'tmp_figs/pl_water_global.png'), width = 500, height = 300, units = 'mm')



  ## -- extra figures water by region (and sector)
  water_consumption_by_reg_p_diff = water_consumption_by_reg %>%
    dplyr::filter(year == selected_year, sector %in% food_sector) %>%
    group_by(year,scenario,region) %>%
    summarise(value = sum(value)) %>%
    ungroup()
  water_consumption_by_reg_p_diff = tidyr::pivot_wider(water_consumption_by_reg_p_diff , names_from = 'scenario', values_from = 'value') %>%
    dplyr::mutate('diff_protein' = 100*(Diets_Protein - Reference)/Reference) %>%
    dplyr::mutate('diff_trade' = 100*(Diets_Trade.adj21 - Reference)/Reference)
  water_consumption_by_reg_p_diff = tidyr::pivot_longer(water_consumption_by_reg_p_diff %>% dplyr::select(region,year,diff_protein,diff_trade),
                                                        cols = c('diff_protein','diff_trade')) %>%
    dplyr::rename('scenario' = 'name') %>%
    dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
    dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))

  water_consumption_by_reg_absval_diff = water_consumption_by_reg %>%
    dplyr::filter(year == selected_year, sector %in% food_sector) %>%
    group_by(year,scenario,region) %>%
    summarise(value = sum(value)) %>%
    ungroup()
  water_consumption_by_reg_absval_diff = tidyr::pivot_wider(water_consumption_by_reg_absval_diff , names_from = 'scenario', values_from = 'value') %>%
    dplyr::mutate('diff_protein' = Diets_Protein - Reference) %>%
    dplyr::mutate('diff_trade' = Diets_Trade.adj21 - Reference)
  water_consumption_by_reg_absval_diff = tidyr::pivot_longer(water_consumption_by_reg_absval_diff %>% dplyr::select(region,year,diff_protein,diff_trade),
                                                             cols = c('diff_protein','diff_trade')) %>%
    dplyr::rename('scenario' = 'name') %>%
    dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
    dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))


  water_consumption_by_reg_sector_p_diff = water_consumption_by_reg %>%
    dplyr::filter(year == selected_year, sector %in% food_sector) %>%
    group_by(year,scenario,sector,region) %>%
    summarise(value = sum(value)) %>%
    ungroup()
  water_consumption_by_reg_sector_p_diff = tidyr::pivot_wider(water_consumption_by_reg_sector_p_diff , names_from = 'scenario', values_from = 'value') %>%
    dplyr::mutate('diff_protein' = 100*(Diets_Protein - Reference)/Reference) %>%
    dplyr::mutate('diff_trade' = 100*(Diets_Trade.adj21 - Reference)/Reference)
  water_consumption_by_reg_sector_p_diff = tidyr::pivot_longer(water_consumption_by_reg_sector_p_diff %>% dplyr::select(region,year,sector,diff_protein,diff_trade),
                                                               cols = c('diff_protein','diff_trade')) %>%
    dplyr::rename('scenario' = 'name') %>%
    dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
    dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))

  water_consumption_by_reg_sector_absval_diff = water_consumption_by_reg %>%
    dplyr::filter(year == selected_year, sector %in% food_sector) %>%
    group_by(year,scenario,sector,region) %>%
    summarise(value = sum(value)) %>%
    ungroup()
  water_consumption_by_reg_sector_absval_diff = tidyr::pivot_wider(water_consumption_by_reg_sector_absval_diff , names_from = 'scenario', values_from = 'value') %>%
    dplyr::mutate('diff_protein' = Diets_Protein - Reference) %>%
    dplyr::mutate('diff_trade' = Diets_Trade.adj21 - Reference)
  water_consumption_by_reg_sector_absval_diff = tidyr::pivot_longer(water_consumption_by_reg_sector_absval_diff %>% dplyr::select(region,year,sector,diff_protein,diff_trade),
                                                                    cols = c('diff_protein','diff_trade')) %>%
    dplyr::rename('scenario' = 'name') %>%
    dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
    dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))

  #########################################################################
  # map p
  #########################################################################
  pl_water_diff = water_consumption_by_reg_p_diff %>%
    dplyr::filter(year == selected_year) %>%
    dplyr::mutate('GCAM Region' = region)

  # merge dataset with world regions
  world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
    dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM',adm0_a3)) %>%
    dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD',adm0_a3))
  world <- subset(world,!adm0_a3 %in% c("ATA","FJI"))

  pl_water_diff = merge(pl_water_diff, GCAM_reg, by = 'GCAM Region')

  world <- merge(world,pl_water_diff, by.x = "adm0_a3", by.y = "ISO 3")

  leg = 'Water consumption percentual difference'

  # plot
  pl_w_map <- ggplot() +
    # color map by regions
    geom_sf(data = world, aes(fill = value)) +
    scale_fill_gradient2(low = "#0DA800", high = "#C60000",
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
          legend.text = element_text(size = 15), legend.title = element_text(size = 20),
          strip.text = element_text(size = 20, color = 'black'),
          strip.background =element_rect(fill="white"))
  ggsave(pl_w_map, file = paste0(figures_path,'extra_figs/plt_water_consumption_p_by_reg_map.png'), width = 500, height = 300, units = 'mm')

  #########################################################################
  # map absval
  #########################################################################
  pl_water_diff = water_consumption_by_reg_absval_diff %>%
    dplyr::filter(year == selected_year) %>%
    dplyr::mutate('GCAM Region' = region)

  # merge dataset with world regions
  world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
    dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM',adm0_a3)) %>%
    dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD',adm0_a3))
  world <- subset(world,!adm0_a3 %in% c("ATA","FJI"))

  pl_water_diff = merge(pl_water_diff, GCAM_reg, by = 'GCAM Region')

  world <- merge(world,pl_water_diff, by.x = "adm0_a3", by.y = "ISO 3")

  leg = expression(paste("Annual Water flows difference (billion ",m^3,")","\n"))

  # plot
  pl_w_map <<- ggplot() +
    # color map by regions
    geom_sf(data = world, aes(fill = value)) +
    scale_fill_gradient2(low = "#0DA800", high = "#C60000",
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
  ggsave(pl_w_map, file = paste0(figures_path,'tmp_figs/plt_water_consumption_p_by_reg_map.png'), width = 500, height = 300, units = 'mm')


  ##############################################################
  # bars p by reg & sector
  ##############################################################
  for (sc in c('Trade','Protein')) {
    pl = water_consumption_by_reg_sector_p_diff %>% dplyr::filter(scenario == sc) %>%
      ggplot() +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
      geom_bar(aes(x=reorder(region,value), y=value, fill=sector),stat="identity",col='black',size=0.2) +
      geom_hline(yintercept = 0, linewidth = 1.5) +
      scale_fill_manual(values = c25) +
      labs(y=expression(paste("Annual Water flows difference (% billion ",m^3,")")),x="",title=sc)+
      theme_bw()+
      theme(
        strip.background = element_blank(),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 15),
        title = element_text(size = 15)
      ) +
      coord_flip()
    ggsave(pl, file = paste0(figures_path,'extra_figs/plt_water_consumption_absval_by_reg_sector_bars_',sc,'.png'), width = 500, height = 300, units = 'mm')
  }
  ##############################################################
  # bars p by reg
  ##############################################################
  for (sc in c('Trade','Protein')) {
    pl = water_consumption_by_reg_p_diff %>% dplyr::filter(scenario == sc) %>%
      ggplot() +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
      geom_bar(aes(x=reorder(region,value), y=value, fill=scenario),stat="identity",col='black',size=0.2) +
      scale_fill_manual(values = mypal_scen, name = 'Scenario') +
      geom_hline(yintercept = 0, linewidth = 1.5) +
      labs(y=expression(paste("Annual Water flows difference (% billion ",m^3,")")),x="",title=sc)+
      theme_bw()+
      theme(
        strip.background = element_blank(),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        title = element_text(size = 30),
        legend.position = 'none'
      ) +
      coord_flip()
    ggsave(pl, file = paste0(figures_path,'extra_figs/plt_water_consumption_p_by_reg_line',sc,'.png'), width = 500, height = 300, units = 'mm')
  }

  ##############################################################
  # bars absval by reg & sector
  ##############################################################
  for (sc in c('Trade','Protein')) {
    pl = water_consumption_by_reg_sector_absval_diff %>% dplyr::filter(scenario == sc) %>%
      ggplot() +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
      geom_bar(aes(x=reorder(region,value), y=value, fill=sector),stat="identity",col='black',size=0.2) +
      geom_hline(yintercept = 0, linewidth = 1.5) +
      scale_fill_manual(values = c25) +
      labs(y=expression(paste("Annual Water flows difference (billion ",m^3,")")),x="",title=sc)+
      theme_bw()+
      theme(
        strip.background = element_blank(),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 15),
        title = element_text(size = 15)
      ) +
      coord_flip()
    ggsave(pl, file = paste0(figures_path,'extra_figs/plt_water_consumption_absval_by_reg_sector_bars_',sc,'.png'), width = 500, height = 300, units = 'mm')
  }
  ##############################################################
  # time-line val by reg & sector
  ##############################################################
  pl = water_consumption_by_reg %>% rename_scen() %>% dplyr::filter(scenario != 'Trade') %>%
    ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
    geom_line(aes(x=year, y=value, color=sector, linetype = scenario),linewidth=2) +
    scale_color_manual(values = c25) +
    scale_linetype_manual(values = c('Reference' = 'solid', 'Protein' = 'dotted'))
  labs(y=expression(paste("Annual Water flows (billion ",m^3,")")),x="",title=sc)+
    theme_bw()+
    theme(
      strip.background = element_blank(),
      axis.text.x = element_text(size=8),
      axis.text.y = element_text(size=10),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 15),
      title = element_text(size = 15)
    )
  ggsave(pl, file = paste0(figures_path,'extra_figs/plt_water_consumption_annual_by_reg_sector_bars_',sc,'.png'), width = 500, height = 300, units = 'mm')

  ##############################################################
  # bars absval by reg
  ##############################################################
  for (sc in c('Trade','Protein')) {
    pl = water_consumption_by_reg_absval_diff %>% dplyr::filter(scenario == sc) %>%
      ggplot() +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
      geom_bar(aes(x=reorder(region,value), y=value, fill=scenario),stat="identity",col='black',size=0.2) +
      scale_fill_manual(values = mypal_scen, name = 'Scenario') +
      guides(fill = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
      geom_hline(yintercept = 0, linewidth = 1.5) +
      labs(y=expression(paste("Annual Water flows difference (billion ",m^3,")")),x="",title=sc)+
      theme_bw()+
      theme(
        strip.background = element_blank(),
        axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=25),
        axis.title = element_text(size=30),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        title = element_text(size = 30),
        legend.position = 'none'
      ) +
      coord_flip()
    ggsave(pl, file = paste0(figures_path,'tmp_figs/plt_water_consumption_absval_by_reg_line',sc,'.png'), width = 500, height = 300, units = 'mm')
    assign(paste0('pl_w_bars_',sc), pl, envir = .GlobalEnv)
  }

  # pl = water_consumption_by_reg_diff %>%
  #   ggplot() +
  #   annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3,fill=c("#C6EEB3"))+
  #   geom_bar(aes(x=reorder(region,value), y=value, fill=scenario),stat="identity",col='black',size=0.2) +
  #   scale_fill_manual(values = mypal_scen, name = 'Scenario') +
  #   geom_hline(yintercept = 0, linewidth = 1.5) +
  #   labs(y="%km^3",x="",title='')+
  #   theme_bw()+
  #   theme(
  #     strip.background = element_blank(),
  #     axis.text.x = element_text(size=8),
  #     axis.text.y = element_text(size=10),
  #     legend.text = element_text(size = 10),
  #     legend.title = element_text(size = 15),
  #     title = element_text(size = 15)
  #   ) +
  #   coord_flip()
  # ggsave(pl, file = paste0(figures_path,'extra_figs/plt_water_consumption_by_reg_line_all.png'), width = 250, height = 150, units = 'mm')


}

do_ghg_figs = function() {

  ## -- annual trend
  pl_ghg_emissions <<- ggplot(data = ghg_total %>% rename_scen(),
                              aes(x = year, y = value, color = scenario)) +
    geom_line(alpha = 0.75, linewidth = 2) +
    scale_color_manual(values = mypal_scen, name = 'Scenario') +
    labs(x = '', y = expression(MtCO[2]), title = 'Total ghg emissions') +
    guides(color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
    theme_light() +
    theme(panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#ffffff",
                                          colour = "#ffffff"),
          legend.position = 'bottom',
          strip.background =element_rect(fill="white"),
          axis.text.x = element_text(size=30),
          axis.text.y = element_text(size=30),
          legend.text = element_text(size = 30),
          legend.title = element_text(size = 40),
          title = element_text(size = 40))
  pl_ghg_emissions
  ggsave(pl_ghg_emissions, file = paste0(figures_path,'tmp_figs/pl_ghg_emissions.png'), width = 200, height = 150, units = 'mm')

  ## -- map (abs difference)
  ghg_by_reg_diff = tidyr::pivot_wider(ghg_by_reg , names_from = 'scenario', values_from = 'value') %>%
    dplyr::mutate('diff_protein' = Protein - Reference) %>%
    dplyr::mutate('diff_trade' = Trade - Reference)
  ghg_by_reg_diff = tidyr::pivot_longer(ghg_by_reg_diff %>% dplyr::select(region,year,diff_protein,diff_trade),
                                        cols = c('diff_protein','diff_trade')) %>%
    dplyr::rename('scenario' = 'name') %>%
    dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
    dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))


  ghg_by_reg_diff_map = ghg_by_reg_diff %>%
    dplyr::filter(year == selected_year) %>%
    dplyr::mutate('GCAM Region' = region)

  # merge dataset with world regions
  world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
    dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM',adm0_a3)) %>%
    dplyr::mutate('adm0_a3' = if_else(sovereignt=='South Sudan', 'SSD',adm0_a3))
  world <- subset(world,!adm0_a3 %in% c("ATA","FJI"))

  ghg_by_reg_diff_map = merge(ghg_by_reg_diff_map, GCAM_reg, by = 'GCAM Region')

  world <- merge(world,ghg_by_reg_diff_map, by.x = "adm0_a3", by.y = "ISO 3")

  leg = expression(paste(MtCO[2],' difference'))

  # plot
  pl_ghg_map <<- ggplot() +
    # color map by regions
    geom_sf(data = world, aes(fill = value)) +
    scale_fill_gradient2(low = "#0DA800", high = "#C60000",
                         mid = '#C2DAC1', midpoint = 0,
                         name = leg) +
    facet_wrap(.~scenario) +
    # theme
    guides(fill = guide_colorbar(title.position = "left")) +
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
          legend.text = element_text(size = 30), legend.title = element_text(size = 30, vjust = 0.95),
          strip.text = element_text(size = 40, color = 'black'),
          strip.background =element_rect(fill="white"), title = element_text(size = 40))
  ggsave(pl_ghg_map, file = paste0(figures_path,'tmp_figs/plt_ghg_map.png'), width = 500, height = 300, units = 'mm')


  ## -- ghg emission by type
  ghg_by_ghg_diff = tidyr::pivot_wider(ghg_by_ghg, names_from = 'scenario', values_from = 'value') %>%
    dplyr::mutate('diff_protein' = 100*(Protein - Reference)/Reference) %>%
    dplyr::mutate('diff_trade' = 100*(Trade - Reference)/Reference)
  ghg_by_ghg_diff = tidyr::pivot_longer(ghg_by_ghg_diff %>% dplyr::select(group,year,diff_protein,diff_trade),
                                        cols = c('diff_protein','diff_trade')) %>%
    dplyr::rename('scenario' = 'name') %>%
    dplyr::mutate('scenario' = ifelse(scenario == 'diff_protein', 'Protein', scenario)) %>%
    dplyr::mutate('scenario' = ifelse(scenario == 'diff_trade', 'Trade', scenario))


  pl_ghg_bars <<- ggplot() +
    # barchart
    geom_bar(data = ghg_by_ghg_diff |> filter(year == selected_year),
             aes(x = scenario, y = value, fill = as.factor(group)),
             stat = "identity", color = NA, width = 0.5,
             position = position_stack(reverse = TRUE)) +
    scale_fill_brewer(palette = 'Paired', name = '') +
    # horizontal line at y = 0
    geom_hline(yintercept = 0, linewidth = 1.2) +
    labs(x = '', y = expression(MtCO[2])) +
    theme_light() +
    theme(panel.grid.major.y = element_line(color = 'grey20'),
          panel.grid.major.x = element_blank(),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "transparent",
                                         colour = 'grey',linewidth = 2),
          panel.background = element_rect(fill = "transparent"),
          legend.position = 'left', legend.direction = 'vertical',
          strip.text = element_text(size = 20, color = 'black'),
          strip.background =element_rect(fill="transparent"),
          axis.text.x = element_text(size=30),
          axis.text.y = element_text(size=30),
          legend.text = element_text(size = 30),
          legend.title = element_text(size = 40),
          title = element_text(size = 40))
  ggsave(pl_ghg_bars, file = paste0(figures_path,'tmp_figs/pl_ghg_emissions_bars.png'), width = 200, height = 150, units = 'mm')
}
