# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L201.snr_parameters
#'
#' Builds SharePlantProtein diet parameters
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L201.snr_parameters}, \code{L201.snr_scenarios_names} (aglu level2).
#' @details This chunk computes the parameters to generate the cumulative number of plant protein intake by region by scenario
#' to modify GCAM's agriculture demand: generic information for supply sector, subsector and technology,
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else group_by lag left_join mutate select summarise
#' @importFrom tidyr gather replace_na spread
#' @author CR July 2023
module_aglu_L201.snr_parameters <- function(command, ...) {

  outputs_path = paste0(getwd(),"/outputs/")
  if (!dir.exists(outputs_path)) dir.create(outputs_path)

  MODULE_INPUTS <-
    c(FILE = "aglu/A_snr_initial_shares",
      FILE = "aglu/A_snr_scenarios")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.snr_parameters",
             "L201.snr_scenarios_names"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- value <- region_GCAM3 <- GCAM_region_ID <- iso <- scenario <-
      region <- technology <- supplysector <- calOutputValue <- OtherUses_Mt <-
      subsector <- L201.snr_parameters <- NULL   # silence package check notes

    A_snr_initial_shares <- get_data(all_data, "aglu/A_snr_initial_shares")
    A_snr_scenarios <- get_data(all_data, "aglu/A_snr_scenarios")

    # sensitivity parameters (all possibilities)
    k_all = seq(from = 0.005, to = 1.6, by = 0.4)
    x0_all = seq(from = 2025, to = MODEL_HALF_CENTURY_YEAR, by = 10)
    combinations = expand.grid(x0 = x0_all, k = k_all)

    check_no_higher_100_and_no_diminishing = function(data) {
      data = data %>%
        # no higher than 100
        dplyr::mutate(snr_f = if_else(snr_f > 100, 100, snr_f)) %>%
        # snr_i <= snr_f
        dplyr::mutate(snr_f = if_else(snr_f < snr_i, snr_i, snr_f))
      return(invisible(data))
    }

    L201.snr_parameters_1 = list()
    for (i in 1:nrow(A_snr_scenarios)) {
      tmp_list = A_snr_initial_shares %>%
        dplyr::mutate(snr_f = eval(parse(text = A_snr_scenarios[i,]$snr_f_computation))) %>%
        dplyr::mutate(scenario = A_snr_scenarios[i,]$scen_name) %>%
        check_no_higher_100_and_no_diminishing(.)

      L201.snr_parameters_1[[A_snr_scenarios[i,]$scen_name]] = tmp_list
    }

    L201.snr_parameters_2 = list()
    for(i in 1:length(L201.snr_parameters_1)) {
      for(j in 1:nrow(combinations)) {
        sc = paste('snr',unique(L201.snr_parameters_1[[i]]$scenario),combinations[j,]$x0,combinations[j,]$k, sep = '_')
        tmp_list = cbind(L201.snr_parameters_1[[i]] %>%
                           dplyr::mutate(scenario = sc),
                         combinations[j,] %>% tibble::as_tibble())
        L201.snr_parameters_2[[sc]] = tmp_list
      }
    }

    ## save results
    L201.snr_parameters_2 %>% tibble::as_tibble() %>%
      add_legacy_name("L201.snr_parameters") %>%
      add_precursors("aglu/A_snr_initial_shares") ->
      L201.snr_parameters

    # save the database - scenario (- type) mapping to run the scenarios in parallel
    tmp = data.frame(scenarios = names(L201.snr_parameters_2)) %>%
      dplyr::mutate(database = sub("_[0-9]{4}_", "_", scenarios)) %>%
      dplyr::mutate(database = gsub("\\.", "-", database)) %>%
      dplyr::mutate(scenarios = gsub("\\.", "d", scenarios))
    write.csv(tmp, file = paste0(outputs_path,'L201.snr_scenarios_database_mapping.csv'), row.names = FALSE)
    tmp = tmp %>%
      dplyr::mutate(type = database)
    write.csv(tmp, file = paste0(outputs_path,'L201.snr_scenarios_database_mapping_type.csv'), row.names = FALSE)

    paste0(names(L201.snr_parameters_2), ".xml") %>% tibble::as_tibble() %>%
      add_legacy_name("L201.snr_parameters") %>%
      add_precursors("aglu/A_snr_initial_shares") ->
      L201.snr_scenarios_names

    save(L201.snr_parameters, file = paste0(outputs_path, '/L201.snr_parameters.RData'))
    save(L201.snr_scenarios_names, file = paste0(outputs_path, '/L201.snr_scenarios_names.RData'))

    return_data(L201.snr_parameters,L201.snr_scenarios_names)
  } else {
    stop("Unknown command")
  }
}
