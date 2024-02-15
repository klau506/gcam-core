# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_diets_L203.dietary_share_weights
#'
#' Builds agriculture demand inputs for the core and each SSP scenario.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L203.diets_plant_sw_list} (diets level3).
#' @details This chunk specifies the share weights for the new plant or ruminant percentual intakes for all the
#' dietary shift scenarios
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else group_by lag left_join mutate select summarise
#' @importFrom tidyr gather replace_na spread
#' @author CR February 2024
module_diets_L203.dietary_share_weights <- function(command, ...) {
  MODULE_INPUTS <-
    c(
      FILE = "aglu/diets_conversion_factor_SPP",
      FILE = "aglu/diets_plant_sw_REF",
      "L202.diets_plant_per_intake_list"
    )

  if (command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if (command == driver.DECLARE_OUTPUTS) {
    return(c("L203.diets_plant_sw_list")) # TODO: add SNR
  } else if (command == driver.MAKE) {
    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    diets_plant_sw_REF <- diets_plant_sw_REF %>%
      tidyr::pivot_longer(cols = X1990:X2050, names_to = "year", values_to = "ref_share_weight") %>%
      dplyr::mutate(year = as.numeric(gsub("X", "", year))) %>%
      dplyr::select(-scenario) %>%
      dplyr::filter(year >= 2015)

    # Useful functions ----
    calibrate_sw <- function(data) {
      for (y in unique(data$year)[2:length(unique(data$year))]) {
        data[data$year == y,]$share_weight =
          data[data$year == y-5,]$share_weight + data[data$year == y,]$per_incr * data[data$year == y,]$conv_factor * 0.01
      }
      return(data)
    }

    # Compute annual plant share weight
    L203.diets_plant_sw_list <- list()
    for (i in 1:length(L202.diets_plant_per_intake_list)) {
      L203.diets_plant_sw_list_tmp <- L202.diets_plant_per_intake_list[[i]] %>%
        # compute percentual difference between each year and the previous one
        group_by(scenario, region, Units) %>%
        arrange(year) %>%
        # mutate(value_lag = lag(value)) %>%
        mutate(per_incr = value / lag(value) - 1) %>%
        ungroup() %>%
        # set NA to 0
        mutate(per_incr = ifelse(is.na(per_incr), 0, per_incr)) %>%
        # add share-weights ref data
        left_join_error_no_match(diets_plant_sw_REF,
          by = c("region", "year")
        ) %>%
        # add conversion factor data
        left_join_error_no_match(diets_conversion_factor_SPP,
          by = c("region")
        ) %>%
        # compute new share-weights: sw = sw_ini + %incr * conv_factor
        mutate(share_weight = ref_share_weight + per_incr * conv_factor * 0.01) %>%
        group_by(scenario, region, Units) %>%
        calibrate_sw() %>%
        ungroup()

      # save the sw to create an xml
      scen_name_complete <- unique(L203.diets_plant_sw_list_tmp$scenario)
      L203.diets_plant_sw_list[[scen_name_complete]] <- L203.diets_plant_sw_list_tmp %>%
        mutate(
          supplysector = "FoodDemand_NonStaples",
          subsector_nest1 = "Protein",
          subsector_nest2 = "Plant"
        ) %>%
        select(region, supplysector, subsector_nest1, subsector_nest2, year, share.weight = share_weight)
    }

    L203.diets_plant_sw_list %>%
      add_title("Plant protein share weights list (entry by scenario)") %>%
      add_units("Unitless") %>%
      add_precursors(
        "aglu/diets_conversion_factor", "aglu/diets_plant_sw_REF",
        "L202.diets_plant_per_intake_list"
      ) ->
    L203.diets_plant_sw_list

    if (!dir.exists("tmp_outputs")) dir.create("tmp_outputs")
    save(L203.diets_plant_sw_list, file = file.path("tmp_outputs", "L203.diets_plant_sw_list.RData"))

    return_data(L203.diets_plant_sw_list)
  } else {
    stop("Unknown command")
  }
}
