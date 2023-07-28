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
    L101.Pop_thous_GCAM3_ctry_Y <- get_data(all_data, "L101.Pop_thous_GCAM3_ctry_Y")

    L101.Pop_thous_GCAM3_ctry_Y = L101.Pop_thous_GCAM3_ctry_Y %>%
      group_by(year) %>%
      summarise(value = sum(value)) %>%
      mutate(iso = 'world')

    # helper function: computes dynamically the probability of becoming flexitarian
    # following a logistic probability function of location parameter t_0 and scale k
    dynamic_probability_logistic <- function(dat) {
      dat <- dat %>%
        mutate(x = year - t0) %>%
        mutate(vegans.factor = wv * flex/population) %>%
        mutate(prob = exp(-x/s) / ( s * (1 + exp(-x/s))^2 ) * (vegans.factor))

      return(invisible(dat))
    }


    # helper function: computes dynamically the nº of people that shift to a
    # flexitarian diet. The change might occur to 'no.flex' people with a 'p'
    # probability following a binomial distribution
    flex_model = function(dat) {
      dat <- dat %>%
        mutate(no.flex = population - flex) %>%
        as.data.table()
      dat_new = data.table()

      for (t in min(MODEL_FUTURE_YEARS):max(MODEL_FUTURE_YEARS)) {
        dat_tmp <- dat %>%
          filter(year == t) %>%
          mutate(no.flex = population - flex) %>%
          # compute the probability of changing to a flexitarian diet
          dynamic_probability_logistic() %>%
          # add the new flexitarian people estimated as the result of the binomial prob fun.
          mutate(new.flex = mean(rbinom(n = no.flex, prob = prob, size = 1000))) %>%
          mutate(percent.new.flex = 100 * new.flex / population)

        # update data
        dat[year == t,] = dat_tmp
        dat[year == t+1,]$flex = dat[year == t,]$percent.new.flex * dat[year == t+1,]$population + dat[year == t,]$flex
      }

      return(invisible(dat))
    }


    # Estimate the cumulative flexitarian people over time by country
    # TODO: fix assumptions
    L202.Flexitarian_population <- L101.Pop_thous_GCAM3_ctry_Y %>%
      rename(population = value) %>%
      mutate(flex = if_else(year <= min(MODEL_FUTURE_YEARS), 1e-3*population, 0)) %>%   # flexitarian people (add some data)
      mutate(s = 20) %>%     # scale; fixed
      mutate(t0 = 2050) %>%  # add sensitivity; year where the max nº of population will shift to flexitarian
      mutate(wv = 20) %>%    # add sensitivity; weight of the social pressure due to nº of flexitarian
      mutate(prob = 0) %>% mutate(x = 0) %>% mutate(vegans.factor = 0) %>% mutate(new.flex = 0) %>% mutate(percent.new.flex = 0) %>%   # void column to keep the probability of the dietary shift
      flex_model() %>%
      mutate(percent.flex = 100 * flex / population)


    L202.Flexitarian_population %>%
      select(iso, year, value = flex) %>% tibble::as_tibble() %>%
      add_title("Number of people following a flexitarian diet") %>%
      add_units("in thousands") %>%
      add_comments("Estimation of the dietary shift through a binomial distribution") %>%
      add_comments("with dynamic probability computed by a logistic probability function") %>%
      add_legacy_name("L202.Flexitarian_population") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L101.Pop_thous_GCAM3_ctry_Y") ->
      L202.Flexitarian_population

    return_data(L202.Flexitarian_population)
  } else {
    stop("Unknown command")
  }
}
