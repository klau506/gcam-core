# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_diets_snr_sw_xml
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{snr_x2_2025_0.005.xml}
module_diets_snr_sw_xml <- function(command, ...) {

  all_xml_names <- return_xml_names(file.path('tmp_outputs','L202.diets_rumin_sw_list.RData'))

  MODULE_INPUTS <-
    c("L202.diets_rumin_sw_list")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    for (xml_name in all_xml_names) {
      print(xml_name)
      data = tibble::as_tibble(L202.diets_rumin_sw_list[[gsub("\\.xml$", "", xml_name)]])

      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(data, "L3_SubsectorShrwt") %>%
               add_node_equiv_xml("subsector") %>%
               add_rename_foodsubsec_xml() %>%
               add_precursors("L202.diets_rumin_sw_list"))
    }

    # Need this for loop because having issues with lapply(all_xml_names, get)
    list_of_xmls <- list()
    for(xml_name in all_xml_names){
      list_of_xmls[[xml_name]] <- get(xml_name)
    }
    return_multiple_xmls(list_of_xmls, all_xml_names)
  } else {
    stop("Unknown command")
  }
}
