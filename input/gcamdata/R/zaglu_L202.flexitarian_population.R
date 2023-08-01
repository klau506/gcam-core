# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L202.flexitarian_population
#'
#' Builds flexitarian diet adoption
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L202.Flexitarian_population} (aglu level2).
#' @details This chunk specifies the cumulative number of people by region that adopts the flexitarian scenario tables for agriculture demand: generic information for supply sector, subsector and technology,
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else group_by lag left_join mutate select summarise
#' @importFrom tidyr gather replace_na spread
#' @author CR July 2023
module_aglu_L202.flexitarian_population <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/iso_GCAM_regID",
      FILE = "common/GCAM_region_names",
      FILE = "aglu/A_flexitarianDiet_parameters",
      "L101.Pop_thous_GCAM3_ctry_Y")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L202.Flexitarian_population"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- value <- region_GCAM3 <- GCAM_region_ID <- iso <- scenario <-
      region <- technology <- supplysector <- calOutputValue <- OtherUses_Mt <-
      subsector <- L202.Flexitarian_population <- NULL   # silence package check notes

    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_flexitarianDiet_parameters <- get_data(all_data, "aglu/A_flexitarianDiet_parameters")
    L101.Pop_thous_GCAM3_ctry_Y <- get_data(all_data, "L101.Pop_thous_GCAM3_ctry_Y")
    set.seed(123)

    # helper function: computes dynamically the probability of becoming flexitarian
    # following a logistic probability function of location parameter t_0 and scale k
    dynamic_probability_logistic <- function(dat) {
      dat <- dat %>%
        mutate(x = year - t0) %>%
        mutate(flex.factor = wf * flex/population) %>%
        mutate(prob = exp(-x/s) / ( s * (1 + exp(-x/s))^2 ) * (flex.factor))

      return(invisible(dat))
    }


    # helper function: computes dynamically the nº of people that shift to a
    # flexitarian diet. The change might occur to 'no.flex' people with a 'p'
    # probability following a binomial distribution
    flex_model = function(dat) {
      dat <- dat %>%
        mutate(no.flex = population - flex) %>%
        as.data.table()

      for (i in unique(dat$iso)) {
        print(i)
        for (t in min(MODEL_FUTURE_YEARS):max(MODEL_FUTURE_YEARS)) {
          dat_tmp <- dat %>%
            filter(year == t, iso == i) %>%
            mutate(no.flex = population - flex) %>%
            # compute the probability of changing to a flexitarian diet
            dynamic_probability_logistic() %>%
            # add the new flexitarian people estimated as the result of the binomial prob fun.
            mutate(new.flex = round(mean(rbinom(1, no.flex, prob)))) %>%
            # rescale new.flex
            mutate(percent.new.flex = 100 * (1 - (flex - new.flex) / flex)) %>%
            mutate(percent.flex = 100 * (new.flex / population)) %>%
            mutate(new.flex = round(population * percent.flex/100 * k/100))

          # update data
          dat[year == t & iso == i,] = dat_tmp

          # add nº of flex
          dat[year == t+1 & iso == i,]$flex =
            dat[year == t & iso == i,]$new.flex +
            dat[year == t & iso == i,]$flex
        }
      }


      return(invisible(dat))
    }


    # Estimate the cumulative flexitarian people over time by country
    L202.Flexitarian_population <- L101.Pop_thous_GCAM3_ctry_Y %>% filter(iso %in% c('can','usa','ind')) %>%
      rename(population = value) %>%
      mutate(population = round(1e3 * population)) %>%  # units: nº of people
      left_join_error_no_match(iso_GCAM_regID %>%
                                 select(iso, GCAM_region_ID) %>%
                                 left_join_error_no_match(GCAM_region_names, by = 'GCAM_region_ID'), by = 'iso') %>%
      left_join_error_no_match(A_flexitarianDiet_parameters, by = 'region') %>%
      mutate(flex = round(if_else(year <= min(MODEL_FUTURE_YEARS), 1e-3*population, 0))) %>%   # flexitarian people (add some data)
      mutate(prob = 0, x = 0, flex.factor = 0, new.flex = 0, percent.new.flex = 0, percent.flex = 0) %>% # void columns
      flex_model() %>%
      mutate(percent.flex = 100 * flex / population) %>%
      mutate(flex = 1e-3 * flex) %>% # units: in thousands
      mutate(population = 1e-3 * population)     # units: in thousands


    attr(L202.Flexitarian_population, "title") <- NULL
    attr(L202.Flexitarian_population, "units") <- NULL
    attr(L202.Flexitarian_population, "comments") <- NULL
    attr(L202.Flexitarian_population, "legacy_name") <- NULL
    attr(L202.Flexitarian_population, "precursors") <- NULL

    L202.Flexitarian_population %>%
      select(iso, year, flex, population, region) %>% tibble::as_tibble() %>%
      add_title("Number of people following a flexitarian diet") %>%
      add_units("in thousands") %>%
      add_comments("Estimation of the dietary shift through a binomial distribution") %>%
      add_comments("with dynamic probability computed by a logistic probability function") %>%
      add_legacy_name("L202.Flexitarian_population") %>%
      add_precursors("common/iso_GCAM_regID",
                     "common/GCAM_region_names",
                     "L101.Pop_thous_GCAM3_ctry_Y") ->
      L202.Flexitarian_population

    return_data(L202.Flexitarian_population)
  } else {
    stop("Unknown command")
  }
}
