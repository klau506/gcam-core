value_interpolation = function(a,b,years) {
  if (is.na(a) | is.na(b)) {
    if (!is.na(a)) {
      value = rep(a, length(years))
    } else if (!is.na(b)) {
      value = rep(b, length(years))
    } else {
      value = rep(NA, length(years))
    }
  } else {
    value = seq(from = a, to = b, length.out = years)
  }

  return(value)
}


#' @param variable: c('GCAM_commodities','population','gdp')
linear_interpolation <- function(df, variable = 'GCAM_commodities') {
  print(unique(df$region))
  df = df %>% arrange(year)
  interpolated_df = data.frame()
  if (variable == 'GCAM_commodities') {
    for (rr in 1:(nrow(df) - 1)) {
      years <- seq(df$year[rr], df$year[rr+1], by = 1)
      Legumes <- value_interpolation(df$Legumes[rr], df$Legumes[rr+1], length(years))
      NutsSeeds <- value_interpolation(df$NutsSeeds[rr], df$NutsSeeds[rr+1], length(years))
      Beef <- value_interpolation(df$Beef[rr], df$Beef[rr+1], length(years))
      Dairy <- value_interpolation(df$Dairy[rr], df$Dairy[rr+1], length(years))
      Pork <- value_interpolation(df$Pork[rr], df$Pork[rr+1], length(years))
      Poultry <- value_interpolation(df$Poultry[rr], df$Poultry[rr+1], length(years))
      SheepGoat <- value_interpolation(df$SheepGoat[rr], df$SheepGoat[rr+1], length(years))
      if ('OtherMeat_Fish' %in% colnames(df)) {
        OtherMeat_Fish <- value_interpolation(df$OtherMeat_Fish[rr], df$OtherMeat_Fish[rr+1], length(years))
        interpolated_df <- bind_rows(interpolated_df, data.frame(year = years,
                                                                 Legumes = Legumes,
                                                                 NutsSeeds = NutsSeeds,
                                                                 Beef = Beef,
                                                                 Dairy = Dairy,
                                                                 Pork = Pork,
                                                                 Poultry = Poultry,
                                                                 SheepGoat = SheepGoat,
                                                                 OtherMeat_Fish = OtherMeat_Fish))
      } else {
        interpolated_df <- bind_rows(interpolated_df, data.frame(year = years,
                                                                 Legumes = Legumes,
                                                                 NutsSeeds = NutsSeeds,
                                                                 Beef = Beef,
                                                                 Dairy = Dairy,
                                                                 Pork = Pork,
                                                                 Poultry = Poultry,
                                                                 SheepGoat = SheepGoat))
      }
    }
  } else {
    for (rr in 1:(nrow(df) - 1)) {
      years <- seq(df$year[rr], df$year[rr+1], by = 1)
      value <- value_interpolation(df$value[rr], df$value[rr+1], length(years))
      interpolated_df <- bind_rows(interpolated_df, data.frame(year = years,
                                                               value = value))
    }
  }
  return(interpolated_df)
}



rename_reg = function(data) {
  data = data %>%
    mutate(country_name = ifelse(country_name %in% c('Lao PDR'), 'Lao Peoples Democratic Republic',
                                                     ifelse(country_name %in% c('Moldova'), 'Moldova, Republic of',
                                                            ifelse(country_name %in% c('North Macedonia'), 'Macedonia, the former Yugoslav Republic of',
                                                                   ifelse(country_name %in% c('Slovak Republic'), 'Slovakia',
                                                                          ifelse(country_name %in% c('St. Lucia'), 'Saint Lucia',
                                                                                 ifelse(country_name %in% c('Tanzania'), 'Tanzania, United Republic of',
                                                                                        ifelse(country_name %in% c('United States'), 'United States of America',
                                                                                               ifelse(country_name %in% c('Timor-Leste'), 'Timor Leste',
                                                                                                      ifelse(country_name %in% c('Venezuela, RB'), 'Venezuela',
                                                                                                             ifelse(country_name %in% c('Vietnam'), 'Viet Nam',country_name)
                                                                                                             )
                                                                                                      )

                                                                                               )

                                                                                        )

                                                                                 )
                                                                          )
                                                                   )
                                                     )
                                   )
           )
    return(invisible(data))
}


rename_area = function(data) {
  data = data %>%
    mutate(Area = ifelse(Area == 'Antigua and Barbuda', 'Antigua & Barbuda', Area)) %>%
    mutate(Area = if_else(Area %in% c('China, Macao SAR', 'China, mainland'), 'China', Area)) %>%
    mutate(Area = ifelse(Area == 'China, Hong Kong SAR', 'Hong Kong', Area)) %>%
    mutate(Area = ifelse(Area == 'China, Taiwan Province of', 'Taiwan', Area)) %>%
    mutate(Area = ifelse(Area == 'Czechia', 'Czech Republic', Area)) %>%
    mutate(Area = ifelse(Area == "CÃ´te d'Ivoire", 'Cote dIvoire', Area)) %>%
    mutate(Area = ifelse(Area == "Democratic Republic of the Congo", 'Congo, the Democratic Republic of the', Area)) %>%
    mutate(Area = ifelse(Area == "Iran (Islamic Republic of)", 'Iran, Islamic Republic of', Area)) %>%
    mutate(Area = ifelse(Area == "Libya", 'Libyan Arab Jamahiriya', Area)) %>%
    mutate(Area = ifelse(Area == "Netherlands (Kingdom of the)", 'Netherlands', Area)) %>%
    mutate(Area = ifelse(Area == "Palestine", 'Palestinian Territory, Occupied', Area)) %>%
    mutate(Area = ifelse(Area == "Republic of Moldova", 'Moldova, Republic of', Area)) %>%
    mutate(Area = ifelse(Area == "TÃ¼rkiye", 'Turkey', Area)) %>%
    mutate(Area = ifelse(Area == "United Republic of Tanzania", 'Tanzania, United Republic of', Area)) %>%
    mutate(Area = ifelse(Area == "Venezuela (Bolivarian Republic of)", 'Venezuela', Area)) %>%
    mutate(Area = ifelse(Area == "United Kingdom of Great Britain and Northern Ireland", 'United Kingdom', Area)) %>%
    mutate(Area = ifelse(Area == "Timor-Leste", 'Timor Leste', Area)) %>%
    mutate(Area = ifelse(Area == "Republic of Korea", 'Korea, Republic of', Area)) %>%
    mutate(Area = ifelse(Area == "North Macedonia", 'Macedonia, the former Yugoslav Republic of', Area)) %>%
    mutate(Area = ifelse(Area == "Micronesia (Federated States of)", 'Micronesia, Federated States of', Area)) %>%
    mutate(Area = ifelse(Area == "Lao People's Democratic Republic", 'Lao Peoples Democratic Republic', Area)) %>%
    mutate(Area = ifelse(Area == "Bolivia (Plurinational State of)", 'Bolivia', Area)) %>%
    mutate(Area = ifelse(Area == "Democratic People's Republic of Korea", 'Korea, Democratic Peoples Republic of', Area))

  return(invisible(data))
}

