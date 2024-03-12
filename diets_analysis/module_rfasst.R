library(rfasst)

### compute pm and o3 premature mortalities
add_mort_scen = function(scen) {

  pm25.mort = dplyr::bind_rows(m3_get_mort_pm25(prj_name = 'dummy.dat', prj = prj, scen_name=scen, final_db_year=final_db_year, saveOutput=F, map=F)) %>%
    dplyr::mutate('scenario' = scen) %>%
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

  o3.mort = dplyr::bind_rows(m3_get_mort_o3(prj_name = 'dummy.dat', prj = prj, scen_name=scen, final_db_year=final_db_year, saveOutput=F, map=F)) %>%
    dplyr::mutate('scenario' = scen) %>%
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

  return(mort)
}



compute_premature_mortalities = function(desired_scen, file_name) {

  mort = add_mort_scen(desired_scen[1])
  for (i in unique(desired_scen)[2:length(desired_scen)]) {
    print(i)
    mort = dplyr::bind_rows(mort,add_mort_scen(i))
  }

  save(mort, file = file.path('diets_analysis','outputs',paste0('mort_',file_name,'.RData')))

  return(mort)
}


