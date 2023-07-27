# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_food_flexitarian_xml
#'
#' Construct XML data structure for \code{food_flexitarian.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{food_flexitarian.xml}. Based on
#' \code{zaglu_xml_food_SSP1.R} (aglu XML).
module_aglu_food_flexitarian_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L203.FuelPrefElast_flexitarian"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "food_flexitarian.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L203.FuelPrefElast_flexitarian <- get_data(all_data, "L203.FuelPrefElast_flexitarian")

    # ===================================================

    # Produce outputs
    create_xml("food_flexitarian.xml") %>%
      add_xml_data_generate_levels(L203.FuelPrefElast_flexitarian, "FuelPrefElast", "subsector", "nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("subsector") %>%
      add_precursors("L203.FuelPrefElast_flexitarian") ->
      food_flexitarian.xml

    return_data(food_flexitarian.xml)
  } else {
    stop("Unknown command")
  }
}
