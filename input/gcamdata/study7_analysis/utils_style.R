scen_palette_studyReference = c(
  'St7_Reference_original' = '#8100D5',
  'St7_Reference_R-M-F' = '#25CA11'
)

scen_palette_calibrateSppFuelPrefElast = c(
  'RvsM_Rreduced_0.45' = '#8100D5',
  'St7_Reference_R-M-F' = '#25CA11'
)

scen_palette_refVsAllSnr = c(
  'St7_Reference' = 'black',
  "snr_all10" = '#9D44E7',
  "snr_all20" = '#9D44E7',
  "snr_all30" = '#9D44E7',
  "snr_all40" = '#9D44E7',
  "snr_all50" = '#9D44E7',
  "snr_all60" = '#9D44E7',
  "snr_p10" = '#9D44E7',
  "snr_p20" = '#9D44E7',
  "snr_p30" = '#9D44E7',
  "snr_p40" = '#9D44E7',
  "snr_p50" = '#9D44E7',
  "snr_p60" = '#9D44E7',
  "snr_x2" = '#9D44E7'
)

scen_palette_refVsSnr = c(
  'St7_Reference' = 'black',
  "snr_all10" = '#9D44E7',
  "snr_all20" = '#E74444',
  "snr_all30" = '#E744E2',
  "snr_all40" = '#E7AE44',
  "snr_all50" = '#F7ED29',
  "snr_all60" = '#EE99EC',
  "snr_p10" = '#93F729',
  "snr_p20" = '#62AF12',
  "snr_p30" = '#12AFAA',
  "snr_p40" = '#123AAF',
  "snr_p50" = '#64B0EB',
  "snr_p60" = '#38F3E5',
  "snr_x2" = '#B9C6C5'
)


rename_scen = function(data) {

  data = data %>%
    mutate(scenario = ifelse(stringr::str_detect(scenario, ",date"),
                             stringr::str_extract(scenario, "^[^,]+(?=,date)"),
                             scenario))

  return(invisible(data))
}
