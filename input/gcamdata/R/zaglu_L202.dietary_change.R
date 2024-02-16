# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_diets_L202.dietary_change
#'
#' Builds agriculture demand inputs for the core and each SSP scenario.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L202.diets_plant_sw_list} (diets level2).
#' @details This chunk specifies the new plant or ruminant percentual protein intake for the dietary shift scenarios
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else group_by lag left_join mutate select summarise
#' @importFrom tidyr gather replace_na spread
#' @author CR February 2024
module_diets_L202.dietary_change <- function(command, ...) {
  MODULE_INPUTS <-
    c(
      FILE = "aglu/diets_scenarios",
      FILE = "aglu/diets_plant_percentage_REF",
      FILE = "aglu/diets_conversion_factor_SPP",
      FILE = "aglu/diets_plant_sw_REF"
    )

  if (command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if (command == driver.DECLARE_OUTPUTS) {
    return(c(
      "L202.diets_plant_sw_list"
    )) # TODO: add SNR
  } else if (command == driver.MAKE) {
    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Useful functions
    check_no_higher_100_and_no_diminishing <- function(data) {
      data <- data %>%
        # no lower than the REF value
        dplyr::mutate(X2050_new = if_else(X2050_new < X2050, X2050, X2050_new)) %>%
        # no higher than 1
        dplyr::mutate(X2050_new = if_else(X2050_new > 1, 1, X2050_new)) %>%
        # higher than the 2050 value
        dplyr::mutate(X2050_new = if_else(X2050_new < X2015, X2015, X2050_new)) %>%
        # store in a column if a sw increase will be necessary or not
        dplyr::mutate(sw_incr_need = if_else(X2050 != X2050_new, TRUE, FALSE)) %>%
        # the new value is now ok
        dplyr::select(-X2050) %>%
        dplyr::rename(X2050 = X2050_new)
      return(invisible(data))
    }

    logit_function <- function(data) {
      data <- data %>%
        dplyr::mutate(
          A = sf - ((1 + exp(-k * (MODEL_FINAL_BASE_YEAR - x0))) * (sf - si) / (exp(-k * (MODEL_FINAL_BASE_YEAR - x0)) - exp(-k * (MODEL_HALF_CENTURY_YEAR - x0)))),
          B = (1 + exp(-k * (MODEL_FINAL_BASE_YEAR - x0))) * (1 + exp(-k * (MODEL_HALF_CENTURY_YEAR - x0))) / (exp(-k * (MODEL_FINAL_BASE_YEAR - x0)) - exp(-k * (MODEL_HALF_CENTURY_YEAR - x0)))
        ) %>%
        dplyr::mutate(share_weight = A + B * ((sf - si) / (1 + exp(-k * (year - x0)))))
      return(invisible(data))
    }

    # Reference plant percentage intake and share weights
    diets_plant_per_intake_REF <- diets_plant_percentage_REF %>%
      dplyr::mutate(scenario = "ref_ref_ref") %>%
      dplyr::select(-X1990, -X2005, -X2010)

    diets_plant_sw_REF <- diets_plant_sw_REF %>%
      tidyr::pivot_longer(cols = X1990:X2050, names_to = "year", values_to = "ref_share_weight") %>%
      dplyr::mutate(year = as.numeric(gsub("X", "", year))) %>%
      dplyr::select(-scenario) %>%
      dplyr::filter(year >= 2015)

    # Interpolation parameters
    k_all <- seq(from = 0.005, to = 1.6, by = 0.4)
    x0_all <- seq(from = 2025, to = MODEL_HALF_CENTURY_YEAR, by = 10)
    combinations <- expand.grid(x0 = x0_all, k = k_all)

    # Compute annual "new" plant protein intake
    L202.diets_plant_sw_list <- list()
    for (i in 1:nrow(diets_scenarios)) {
      # scenario name
      scen_name <- paste(diets_scenarios[i, ], collapse = "_")
      diets_plant_per_intake_tmp <- diets_plant_per_intake_REF %>%
        dplyr::mutate(scenario = scen_name)

      # final value in 2050 according to the scenario name
      if ("all" == diets_scenarios[i, ]$scen_reg_type) {
        diets_plant_per_intake_tmp <- diets_plant_per_intake_tmp %>%
          dplyr::mutate(X2050_new = diets_scenarios[i, ]$scen_goal / 100) %>%
          check_no_higher_100_and_no_diminishing()
      }

      if ("plus" == diets_scenarios[i, ]$scen_reg_type) {
        diets_plant_per_intake_tmp <- diets_plant_per_intake_tmp %>%
          dplyr::mutate(X2050_new = X2050 + diets_scenarios[i, ]$scen_goal / 100) %>%
          check_no_higher_100_and_no_diminishing()
      }

      diets_plant_per_intake_tmp <- diets_plant_per_intake_tmp %>%
        dplyr::mutate(value_f = X2050, value_i = X2015) %>%
        dplyr::mutate(per_incr = if_else(sw_incr_need, value_f/value_i - 1, 0)) %>%
        tidyr::pivot_longer(cols = X2015:X2050, names_to = "year", values_to = "value") %>%
        dplyr::mutate(year = as.numeric(gsub("X", "", year)))

      # compute the 2050 share weight: sw = sw_ini + %incr * conv_factor
      diets_plant_sw_tmp <- diets_plant_per_intake_tmp %>%
        # add share-weights ref data
        left_join_error_no_match(diets_plant_sw_REF,
                                 by = c("region", "year")
        ) %>%
        # add conversion factor data
        left_join_error_no_match(diets_conversion_factor_SPP,
                                 by = c("region")
        )
      diets_plant_sw_tmp = data.table(diets_plant_sw_tmp)
      diets_plant_sw_tmp = diets_plant_sw_tmp[, sf := ref_share_weight[year == MODEL_HALF_CENTURY_YEAR]
                                              + per_incr[year == MODEL_HALF_CENTURY_YEAR] * conv_factor[year == MODEL_HALF_CENTURY_YEAR],
                                                                  by = region]
      diets_plant_sw_tmp = diets_plant_sw_tmp[, si := ref_share_weight[year == MODEL_FINAL_BASE_YEAR], by = region]


      # recompute the values from 2020 to 2050
      for (j in 1:nrow(combinations)) {
        diets_plant_sw_tmp2 <- diets_plant_sw_tmp %>%
          dplyr::mutate(
            x0 = combinations[j, ]$x0,
            k = combinations[j, ]$k
          ) %>%
          mutate(share_weight = ref_share_weight)
        scen_name_complete <- paste(unique(diets_plant_sw_tmp2$scenario),
          "x0", unique(diets_plant_sw_tmp2$x0),
          "k", unique(diets_plant_sw_tmp2$k),
          sep = "_"
        )
        L202.diets_plant_sw_list_tmp <- logit_function(diets_plant_sw_tmp2) %>%
          dplyr::mutate(scenario = scen_name_complete) %>%
          dplyr::mutate(
            supplysector = "FoodDemand_NonStaples",
            subsector_nest1 = "Protein",
            subsector_nest2 = "Plant"
          ) %>%
          dplyr::select(region, supplysector, subsector_nest1, subsector_nest2, year, share.weight = share_weight)
        L202.diets_plant_sw_list[[scen_name_complete]] <- L202.diets_plant_sw_list_tmp
      }
    }

    L202.diets_plant_sw_list %>%
      add_title("Plant protein share weights list (entry by scenario)") %>%
      add_units("Unitless") %>%
      add_precursors("aglu/diets_plant_percentage_REF", "aglu/diets_scenarios",
                     "aglu/diets_conversion_factor_SPP", "aglu/diets_plant_sw_REF") ->
      L202.diets_plant_sw_list

    # Save the output to produce later multiple XMLs with a single chunk
    if (!dir.exists("tmp_outputs")) dir.create("tmp_outputs")
    save(L202.diets_plant_sw_list, file = file.path("tmp_outputs", "L202.diets_plant_sw_list.RData"))

    return_data(L202.diets_plant_sw_list)
  } else {
    stop("Unknown command")
  }
}
