# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L202.flexitarian_population
#'
#' Builds flexitarian diet adoption
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs:
#'   \code{L202.Flexitarian_population_1}, \code{L202.Flexitarian_population_2}, \code{L202.Flexitarian_population_3}, \code{L202.Flexitarian_population_4},
#'   \code{L202.Flexitarian_population_5}, \code{L202.Flexitarian_population_6}, \code{L202.Flexitarian_population_7}, \code{L202.Flexitarian_population_8},
#'   \code{L202.Flexitarian_population_9}, \code{L202.Flexitarian_population_10}, \code{L202.Flexitarian_population_11}, \code{L202.Flexitarian_population_12},
#'   \code{L202.Flexitarian_population_13}, \code{L202.Flexitarian_population_14}, \code{L202.Flexitarian_population_15}, \code{L202.Flexitarian_population_16},
#'   \code{L202.Flexitarian_population_17}, \code{L202.Flexitarian_population_18}, \code{L202.Flexitarian_population_19}, \code{L202.Flexitarian_population_20},
#'   \code{L202.Flexitarian_population_21}, \code{L202.Flexitarian_population_22}, \code{L202.Flexitarian_population_23}, \code{L202.Flexitarian_population_24},
#'   \code{L202.Flexitarian_population_25} (aglu level2).
#' @details This chunk specifies the cumulative number of people by region that adopts the flexitarian scenario tables for agriculture demand: generic information for supply sector, subsector and technology,
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else group_by lag left_join mutate select summarise
#' @importFrom tidyr gather replace_na spread
#' @author CR July 2023
module_aglu_L202.flexitarian_population <- function(command, ...) {

  FLEX_NUMS = 1:beh.NUM_RANDOM_TRIALS

  MODULE_INPUTS <-
    c(FILE = "common/iso_GCAM_regID",
      FILE = "common/GCAM_region_names",
      "L201.flexitarian_parameters",
      "L101.Pop_thous_GCAM3_ctry_Y")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(paste0("L202.Flexitarian_population_", FLEX_NUMS)))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- value <- region_GCAM3 <- GCAM_region_ID <- iso <- scenario <-
      region <- technology <- supplysector <- calOutputValue <- OtherUses_Mt <-
      subsector <- L202.Flexitarian_population <- NULL   # silence package check notes

    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    L201.flexitarian_parameters <- get_data(all_data, "L201.flexitarian_parameters")
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
        # no flex people
        mutate(no.flex = population - flex) %>%
        # no flex people that can shift to flex (it can not be more than the expected number of people at the end of the century)
        mutate(no.flex.can.change = population * pmin(no.flex/population, k/100 - flex/population)) %>%
        # mutate(no.flex.can.change = round(min(no.flex, no.flex.diff))) %>%
        as.data.table()

      for (r in unique(dat$region)) {
        print(r)
        for (t in MODEL_FINAL_BASE_YEAR:max(MODEL_FUTURE_YEARS)) {
          dat_tmp <- dat %>%
            filter(year == t, region == r) %>%
            # update no.flex population
            mutate(no.flex = population - flex) %>%
            mutate(no.flex.can.change = population * pmin(no.flex/population, k/100 - flex/population)) %>%
            # compute the probability of changing to a flexitarian diet
            dynamic_probability_logistic() %>%
            # add the new flexitarian people estimated as the result of the binomial prob fun.
            mutate(new.flex = round(mean(rbinom(1, as.integer(no.flex.can.change), prob)))) %>%
            # rescale new.flex
            mutate(percent.new.flex = 100 * (1 - (flex - new.flex) / flex)) %>%
            mutate(percent.flex = 100 * (new.flex / population)) %>%
            mutate(new.flex = round(population * percent.flex/100 * k/100))

          if (sum(is.na(dat_tmp)) == 0) {          # update data
            dat[year == t & region == r,] = dat_tmp

            # add nº of flex
            dat[year == t+1 & region == r,]$flex =
              dat[year == t & region == r,]$new.flex +
              dat[year == t & region == r,]$flex
          } else {
            dat[year == t+1 & region == r,]$flex =
              dat[year == t & region == r,]$flex
          }
        }
      }


      return(invisible(dat))
    }


    for (i in 1:beh.NUM_RANDOM_TRIALS) {
      print(i)
      # # Estimate the cumulative flexitarian people over time by country
      # L202.Flexitarian_population <- L101.Pop_thous_GCAM3_ctry_Y %>%
      #   rename(population = value) %>%
      #   mutate(population = round(CONV_THOUS_VALUE * population)) %>%  # units: nº of people
      #   # read the parameters to define the variability of the behavior change
      #   left_join_error_no_match(iso_GCAM_regID %>%
      #                              select(iso, GCAM_region_ID) %>%
      #                              left_join_error_no_match(GCAM_region_names, by = 'GCAM_region_ID'), by = 'iso') %>%
      #   group_by(year, region) %>%
      #   summarise(population = sum(population)) %>% ungroup() %>%
      #   left_join_error_no_match(L201.flexitarian_parameters %>%
      #                              select(region, percent.flex.ini, s, paste0("t0_", i), paste0("wf_", i), paste0("k_", i)) %>%
      #                              rename(t0 = paste0("t0_", i), wf = paste0("wf_", i), k = paste0("k_", i)),
      #                            by = 'region') %>%
      #   mutate(flex = round(if_else(year <= max(MODEL_BASE_YEARS), ( percent.flex.ini/100 )*population, 0))) %>%   # flexitarian people
      #   mutate(max.flex = population[which(year == 2100)] * k/100) %>%  # max nº of people that can become flex
      #   mutate(prob = 0, x = 0, flex.factor = 0, new.flex = 0, percent.new.flex = 0, percent.flex = 0) %>% # void columns
      #   flex_model() %>%
      #   mutate(percent.flex = 100 * flex / population) %>%
      #   mutate(flex = CONV_VALUE_THOUS * flex) %>%           # units: in thousands
      #   mutate(population = CONV_VALUE_THOUS * population)   # units: in thousands

      L202.Flexitarian_population = get(load(file = paste0('outputs_binomial/L202.Flexitarian_population_',i,'.RData')))

      assign(paste0('L202.Flexitarian_population_',i), L202.Flexitarian_population %>%
               select(region, year, flex, population) %>% tibble::as_tibble() %>%
               add_title("Number of people following a flexitarian diet") %>%
               add_units("in thousands") %>%
               add_comments("Estimation of the dietary shift through a binomial distribution") %>%
               add_comments("with dynamic probability computed by a logistic probability function") %>%
               add_legacy_name("L202.Flexitarian_population") %>%
               add_precursors("common/iso_GCAM_regID","common/GCAM_region_names",
                              "L201.flexitarian_parameters","L101.Pop_thous_GCAM3_ctry_Y"))

      # save(L202.Flexitarian_population, file = paste0('outputs_binomial/L202.Flexitarian_population_',i,'.RData'))

    }


    return_data(L202.Flexitarian_population_1,L202.Flexitarian_population_2,L202.Flexitarian_population_3,L202.Flexitarian_population_4,
                L202.Flexitarian_population_5,L202.Flexitarian_population_6,L202.Flexitarian_population_7,L202.Flexitarian_population_8,
                L202.Flexitarian_population_9,L202.Flexitarian_population_10,L202.Flexitarian_population_11,L202.Flexitarian_population_12,
                L202.Flexitarian_population_13,L202.Flexitarian_population_14,L202.Flexitarian_population_15,L202.Flexitarian_population_16,
                L202.Flexitarian_population_17,L202.Flexitarian_population_18,L202.Flexitarian_population_19,L202.Flexitarian_population_20,
                L202.Flexitarian_population_21,L202.Flexitarian_population_22,L202.Flexitarian_population_23,L202.Flexitarian_population_24,
                L202.Flexitarian_population_25)
  } else {
    stop("Unknown command")
  }
}
