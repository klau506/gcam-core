library(rfasst)

compute_premature_mortalities = function(desired_scen, file_name) {

  pm25.mort = dplyr::bind_rows(m3_get_mort_pm25(prj_name = 'dummy.dat',
                                                prj = prj,
                                                scen_name=desired_scen,
                                                final_db_year=2050,
                                                saveOutput=F,
                                                map=F)) %>%
    # subset to >25
    dplyr::filter(age == '>25') %>%
    # reframe
    tidyr::pivot_longer(cols = c('GBD',
                                 'GEMM',
                                 'FUSION'),
                        names_to = 'method', values_to = 'mort_by_disease') %>%
    # compute total deaths
    dplyr::group_by(region,year,scenario,method) %>%
    dplyr::summarise('mort' = sum(mort_by_disease)) %>%
    dplyr::ungroup()

  o3.mort = dplyr::bind_rows(m3_get_mort_o3(prj_name = 'dummy.dat',
                                            prj = prj,
                                            scen_name=desired_scen,
                                            final_db_year=2050,
                                            saveOutput=F,
                                            map=F)) %>%
    # reframe
    tidyr::pivot_longer(cols = c('Jerret2009',
                                 'GBD2016'),
                        names_to = 'method', values_to = 'mort_by_disease') %>%
    # compute total deaths
    dplyr::group_by(region,year,scenario,method) %>%
    dplyr::summarise('mort' = sum(mort_by_disease)) %>%
    dplyr::ungroup()

  mort = dplyr::bind_rows(pm25.mort %>% dplyr::mutate('pollutant' = 'pm25'),
                          o3.mort %>% dplyr::mutate('pollutant' = 'o3'))


  mort <- mort %>%
    tidyr::separate(scenario, into = c("scen_type", "scen_path",
                                       "final_share",
                                       "peak_year_title", "peak_year",
                                       "slope_title", "slope"),
                    sep = "_", remove = FALSE) %>%
    dplyr::select(-peak_year_title) %>%
    dplyr::select(-slope_title)

  save(mort, file = file.path('diets_analysis','outputs',paste0('mort_',file_name,'.RData')))

  # return(mort)
}


