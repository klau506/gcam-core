# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L201.flexitarian_parameters
#'
#' Builds flexitarian diet parameters
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L201.flexitarian_parameters} (aglu level2).
#' @details This chunk computes the parameters to generate the cumulative number of people by region that
#' adopts the flexitarian scenario tables for agriculture demand: generic information for supply sector, subsector and technology,
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else group_by lag left_join mutate select summarise
#' @importFrom tidyr gather replace_na spread
#' @author CR July 2023
module_aglu_L201.flexitarian_parameters <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "aglu/A_flexitarianDiet_parameters")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.flexitarian_parameters"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- value <- region_GCAM3 <- GCAM_region_ID <- iso <- scenario <-
      region <- technology <- supplysector <- calOutputValue <- OtherUses_Mt <-
      subsector <- L201.flexitarian_parameters <- NULL   # silence package check notes

    A_flexitarianDiet_parameters <- get_data(all_data, "aglu/A_flexitarianDiet_parameters")
    set.seed(123)

    # Save the computed the parameters
    L201.flexitarian_parameters <- A_flexitarianDiet_parameters %>%
      select(region,percent.flex.ini,s)


    # helper function: generate random values, filter them, and return 3 selected values
    generate_random_values <- function(row, trials) {
      mu <- row['mu'][[1]] %>% as.integer()
      sigma <- row['sigma'][[1]] %>% as.integer()
      min_value <- row['m'][[1]] %>% as.integer()
      max_value <- row['M'][[1]] %>% as.integer()

      # Generate random values from the normal distribution
      random_values <- rnorm(1000, mean = mu, sd = sigma)

      # Filter out the values that fall within the desired range
      random_values_filtered <- random_values[random_values >= min_value & random_values <= max_value]

      # Sample "trials" random values from the filtered values and round them to integers
      three_random_values <- round(sample(random_values_filtered, trials))

      return(three_random_values)
    }

    # Generate t0
    random_values_df <- t(apply(A_flexitarianDiet_parameters %>%
                                  select(region,t0_mu,t0_m,t0_M,t0_sigma) %>%
                                  dplyr::rename_with(~gsub("^t0_", "", .), starts_with("t0_")),
                                1, generate_random_values, trials = beh.NUM_RANDOM_TRIALS))
    for (i in 1:beh.NUM_RANDOM_TRIALS) {
      column_name <- paste0("t0_", i)
      L201.flexitarian_parameters[[column_name]] <- random_values_df[, i]
    }


    # Generate wf
    random_values_df <- t(apply(A_flexitarianDiet_parameters %>%
                                  select(region,wf_mu,wf_m,wf_M,wf_sigma) %>%
                                  dplyr::rename_with(~gsub("^wf_", "", .), starts_with("wf_")),
                                1, generate_random_values, trials = beh.NUM_RANDOM_TRIALS))
    for (i in 1:beh.NUM_RANDOM_TRIALS) {
      column_name <- paste0("wf_", i)
      L201.flexitarian_parameters[[column_name]] <- random_values_df[, i]
    }


    # Generate k
    for (i in 1:beh.NUM_RANDOM_TRIALS) {
      dat_tmp = data.frame(region = A_flexitarianDiet_parameters$region,
                           wf = L201.flexitarian_parameters[[paste0('wf_',i)]],
                           wf_m = A_flexitarianDiet_parameters$wf_m,
                           wf_M = A_flexitarianDiet_parameters$wf_M,
                           k_m = A_flexitarianDiet_parameters$k_m,
                           k_M = A_flexitarianDiet_parameters$k_M,
                           k_sigma = A_flexitarianDiet_parameters$k_sigma) %>%
        mutate(k_mu = k_m + ((k_M - k_m) / (wf_M - wf_m)) * (wf - wf_m))

      random_values_df <- t(apply(dat_tmp %>%
                                    select(region,k_mu,k_m,k_M,k_sigma) %>%
                                    dplyr::rename_with(~gsub("^k_", "", .), starts_with("k_")),
                                  1, generate_random_values, trials = 1))

      column_name <- paste0("k_", i)
      L201.flexitarian_parameters[[column_name]] <- random_values_df[1,]
    }


    ## - save results
    L201.flexitarian_parameters %>% tibble::as_tibble() %>%
      add_legacy_name("L201.flexitarian_parameters") %>%
      add_precursors("aglu/A_flexitarianDiet_parameters") ->
      L201.flexitarian_parameters

    save(L201.flexitarian_parameters, file = 'outputs_binomial/L201.flexitarian_parameters.RData')

    return_data(L201.flexitarian_parameters)
  } else {
    stop("Unknown command")
  }
}
