scen_palette_studyReference = c(
  'St7_Reference_original' = '#25CA11',
  'SPP_fuelPrefElast_0.45' = '#8100D5'
)

scen_palette_calibrateSppFuelPrefElast = c(
  'St7_Reference_original' = '#25CA11',
  'SPP_fuelPrefElast_0.45' = '#8100D5'
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


rename_scen = function(data) {

  data = data %>%
    mutate(scenario = ifelse(stringr::str_detect(scenario, ",date"),
                             stringr::str_extract(scenario, "^[^,]+(?=,date)"),
                             scenario))

  return(invisible(data))
}
