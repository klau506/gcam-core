# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_food_flexitarian_xml
#'
#' Construct XML data structure for \code{food_flexitarian_[1_3].xml}
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{food_flexitarian_1.xml},
#' \code{food_flexitarian_2.xml}, \code{food_flexitarian_3.xml}. Based on
#' \code{zaglu_xml_food_SSP1.R} (aglu XML).
module_aglu_food_flexitarian_xml <- function(command, ...) {

  FLEX_NUMS = 1:beh.NUM_RANDOM_TRIALS

  if(command == driver.DECLARE_INPUTS) {
    return(c(paste0("L203.FuelPrefElast_flexitarian_", FLEX_NUMS)))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "food_flexitarian_1.xml",
             XML = "food_flexitarian_2.xml",
             XML = "food_flexitarian_3.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    L203.FuelPrefElast_flexitarian_1.xml <- L203.FuelPrefElast_flexitarian_2.xml <-
      L203.FuelPrefElast_flexitarian_3.xml <- NULL

    for (fn in FLEX_NUMS) {
      # load required inputs
      L203.FuelPrefElast_flexitarian <- get_data(all_data, paste0("L203.FuelPrefElast_flexitarian_",fn))
      xmlfn <- paste0("food_flexitarian_", fn, ".xml")

      # produce outputs
      create_xml(xmlfn) %>%
        add_xml_data_generate_levels(L203.FuelPrefElast_flexitarian, "FuelPrefElast", "subsector", "nesting-subsector",1,FALSE) %>%
        add_node_equiv_xml("subsector") %>%
        add_precursors(paste0("L203.FuelPrefElast_flexitarian_",fn)) ->
        x

      # ...and assign into environment
      assign(xmlfn, x)
    }

    return_data(food_flexitarian_1.xml,food_flexitarian_2.xml,food_flexitarian_3.xml)
  } else {
    stop("Unknown command")
  }
}
