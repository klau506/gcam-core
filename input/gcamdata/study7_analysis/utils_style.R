scen_palette_studyReference = c(
  'St7_Reference_original' = '#25CA11',
  'SPP_fuelPrefElast_0.45' = '#8100D5'
)

scen_palette_calibrateSppFuelPrefElast = c(
  'St7_Reference_original' = '#25CA11',
  'SPP_fuelPrefElast_0.45' = '#8100D5'
)

c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)


scen_palette_refVsAllSpp = c(
  'St7_Reference' = 'black',
  "spp_all10" = '#9D44E7',
  "spp_all20" = '#9D44E7',
  "spp_all30" = '#9D44E7',
  "spp_all40" = '#9D44E7',
  "spp_all50" = '#9D44E7',
  "spp_all60" = '#9D44E7',
  "spp_p10" = '#9D44E7',
  "spp_p20" = '#9D44E7',
  "spp_p30" = '#9D44E7',
  "spp_p40" = '#9D44E7',
  "spp_p50" = '#9D44E7',
  "spp_p60" = '#9D44E7',
  "spp_x2" = '#9D44E7'
)

scen_palette_refVsSpp = c(
  'St7_Reference' = 'black',
  "spp_all10" = '#9D44E7',
  "spp_all20" = '#E74444',
  "spp_all30" = '#E744E2',
  "spp_all40" = '#E7AE44',
  "spp_all50" = '#F7ED29',
  "spp_all60" = '#EE99EC',
  "spp_p10" = '#93F729',
  "spp_p20" = '#62AF12',
  "spp_p30" = '#12AFAA',
  "spp_p40" = '#123AAF',
  "spp_p50" = '#64B0EB',
  "spp_p60" = '#38F3E5',
  "spp_x2" = '#B9C6C5'
)

scen_palette_refVsSppVsSnr = c(
  'SPP' = '#198a22',
  'SNR' = '#500f96',
  'REF' = '#7d0c1a'
)
scen_palette_refVsSppVsSnr.labs <- c('SPP', "SNR", 'REF')
names(scen_palette_refVsSppVsSnr.labs) = c("SPP", "SNR", 'REF')


irr_rfd_scenario_palette = c(
  'RFD.SNR' = '#500f96',
  'IRR.SNR' = '#9b50e6',
  'RFD.SPP' = '#198a22',
  'IRR.SPP' = '#5bcf74',
  'RFD.REF' = '#7d0c1a',
  'IRR.REF' = '#e65566'
)
irr_rfd_scenario.labs <- c("SNR - RFD", 'SNR - IRR',
                           'SPP - RFD', 'SPP - IRR',
                           'REF - RFD', 'REF - IRR')
names(irr_rfd_scenario.labs) = c("RFD.SNR", "IRR.SNR",
                                 "RFD.SPP", "IRR.SPP",
                                 'RFD.REF', 'IRR.REF')

macronutrients_scenario_palette = c(
  'gProteinPerCapita.SNR' = '#500f96',
  'gFatPerCapita.SNR' = '#9b50e6',
  'gProteinPerCapita.SPP' = '#198a22',
  'gFatPerCapita.SPP' = '#5bcf74',
  'gProteinPerCapita.REF' = '#7d0c1a',
  'gFatPerCapita.REF' = '#e65566'
)
macronutrients_scenario.labs <- c('SNR - Protein',"SNR - Fat",
                                  'SPP - Protein','SPP - Fat',
                                  'REF - Protein','REF - Fat')
names(macronutrients_scenario.labs) = c("gProteinPerCapita.SNR", "gFatPerCapita.SNR",
                                        "gProteinPerCapita.SPP", "gFatPerCapita.SPP",
                                        'gProteinPerCapita.REF', 'gFatPerCapita.REF')

land_use_scenario_palette =
  c('Cropland' = 'chocolate4',
    'Pasture' = '#E6AB02',
    'Forest' = 'darkgreen',
    'Shrubs & Grass' = '#66A61E',
    'Other Natural' = '#b1e378')
land_use_order_palette =
  c('Cropland',
    'Pasture',
    'Forest',
    'Shrubs & Grass',
    'Other Natural')


rename_scen = function(data) {

  # data = data %>%
  #   mutate(scenario = ifelse(stringr::str_detect(scenario, ",date"),
  #                            stringr::str_extract(scenario, "^[^,]+(?=,date)"),
  #                            scenario))

  return(invisible(data))
}

order_facets = function(data) {

  data = data %>%
    dplyr::mutate(scen_type = toupper(scen_type)) %>%
    dplyr::mutate(scen_type = ifelse(scen_type == 'ST7', 'REF', scen_type))
  data$scen_type = factor(data$scen_type, levels = c('SPP','SNR','REF'))

  return(invisible(data))
}

# return vector of colors: color_positive if the position of the value in the values'
# vector is positive, color_negative otherwise
color_by_sign = function(values) {
  color_positive = 6
  color_negative = 7
  color_vector <- ifelse(values > 0, color_positive, color_negative)
  return(color_vector)
}
