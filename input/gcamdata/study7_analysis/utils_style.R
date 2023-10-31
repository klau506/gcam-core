# scen_palette_studyReference = c(
#   'St7_Reference_original' = '#25CA11',
#   'SPP_fuelPrefElast_0.45' = '#8100D5'
# )
#
# scen_palette_calibrateSppFuelPrefElast = c(
#   'St7_Reference_original' = '#25CA11',
#   'SPP_fuelPrefElast_0.45' = '#8100D5'
# )
#
#
# rename_scen = function(data) {
#
#   data = data %>%
#     mutate(scenario = ifelse(stringr::str_detect(scenario, ",date"),
#                              stringr::str_extract(scenario, "^[^,]+(?=,date)"),
#                              scenario))
#
#   return(invisible(data))
# }
