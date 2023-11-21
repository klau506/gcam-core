# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_negative_emissions_budget_xml
#'
#' Construct XML data structure for \code{paste0("negative_emissions_budget_", c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5)), ".xml")}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{paste0("negative_emissions_budget_", c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5)), ".xml")}. The corresponding file in the
#' original data system was \code{L270.limits.R} (energy XML).
module_energy_negative_emissions_budget_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L270.CTaxInput",
      "L270.LandRootNegEmissMkt",
      "L270.NegEmissBudgetMaxPrice",
      "L270.NegEmissBudgetDefaultPrice",
      "L270.NegEmissBudget",
      "L270.NegEmissBudgetFraction")

  MODULE_OUTPUTS <-
    c("XML" = "negative_emissions_budget.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    . <- NULL # silence package check note

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # ===================================================

    Separating1G2G = TRUE

    if (Separating1G2G) {

      Tech_1G <- c("regional corn for ethanol", "regional sugar for ethanol",
                   "biomassOil", "OilCrop", "OilPalm", "Soybean")


      L270.CTaxInput %>% filter(technology %in% Tech_1G) %>%
        mutate(ctax.input = "negative_emiss_budget_1G")-> L270.CTaxInput_1G
      L270.CTaxInput %>% filter(!technology %in% Tech_1G) -> L270.CTaxInput

      L270.LandRootNegEmissMkt %>%
        mutate(negative.emiss.market = "negative_emiss_budget_1G") ->
        L270.LandRootNegEmissMkt_1G

      L270.NegEmissBudgetMaxPrice %>%
        mutate(policy.portfolio.standard = "negative_emiss_budget_1G") ->
        L270.NegEmissBudgetMaxPrice_1G

      L270.NegEmissBudgetDefaultPrice %>%
        mutate(policy.portfolio.standard = "negative_emiss_budget_1G") %>%
        mutate(price = if_else(year >= 2020, 0.55, price)) -> L270.NegEmissBudgetDefaultPrice_1G

      L270.NegEmissBudget %>%
        mutate(policy.portfolio.standard = "negative_emiss_budget_1G") -> L270.NegEmissBudget_1G

      L270.NegEmissBudgetDefaultPrice %>%
        mutate(price = if_else(year >= 2020, 0.15, price)) -> L270.NegEmissBudgetDefaultPrice

      # Using 0.1% as budget for 1G bioenergy but adding 55% min.price below
      L270.NegEmissBudgetFraction %>% mutate(negative.emissions.budget.fraction = 0.001) %>%
        mutate(negative.emissions.budget.name = "negative_emiss_budget_1G")->
        L270.NegEmissBudgetFraction_1G

      create_xml("negative_emissions_budget.xml") %>%
        # adding 1G budget limits
        add_xml_data(L270.CTaxInput_1G, "GlobalTechCTaxInput") %>%
        add_xml_data(L270.LandRootNegEmissMkt_1G, "LandRootNegEmissMkt") %>%
        add_xml_data(L270.NegEmissBudgetMaxPrice_1G, "PortfolioStdMaxPrice") %>%
        # add a min policy price here at 55% for 1G
        add_xml_data(L270.NegEmissBudgetMaxPrice_1G %>% mutate(min.price = 0.55), "PortfolioStdMinPrice") %>%
        add_xml_data(L270.NegEmissBudgetDefaultPrice_1G, "PortfolioStdFixedTax") %>%
        add_xml_data(L270.NegEmissBudget_1G, "PortfolioStd") %>%
        add_xml_data(L270.NegEmissBudgetFraction_1G, "NegEmissBudgetFraction") %>%
        # adding 2G budget limits
        add_xml_data(L270.CTaxInput, "GlobalTechCTaxInput") %>%
        add_xml_data(L270.LandRootNegEmissMkt, "LandRootNegEmissMkt") %>%
        add_xml_data(L270.NegEmissBudgetMaxPrice, "PortfolioStdMaxPrice") %>%
        # add a min policy price here at 15% for 1G
        add_xml_data(L270.NegEmissBudgetMaxPrice %>% mutate(min.price = 0.15),
                     "PortfolioStdMinPrice") %>%
        add_xml_data(L270.NegEmissBudgetDefaultPrice, "PortfolioStdFixedTax") %>%
        add_xml_data(L270.NegEmissBudget, "PortfolioStd") %>%
        add_xml_data(L270.NegEmissBudgetFraction, "NegEmissBudgetFraction") %>%
        add_precursors(MODULE_INPUTS) ->
        negative_emissions_budget.xml

    } else{

      create_xml("negative_emissions_budget.xml") %>%
        add_xml_data(L270.CTaxInput, "GlobalTechCTaxInput") %>%
        add_xml_data(L270.LandRootNegEmissMkt, "LandRootNegEmissMkt") %>%
        add_xml_data(L270.NegEmissBudgetMaxPrice, "PortfolioStdMaxPrice") %>%
        add_xml_data(L270.NegEmissBudgetDefaultPrice, "PortfolioStdFixedTax") %>%
        add_xml_data(L270.NegEmissBudget, "PortfolioStd") %>%
        add_xml_data(L270.NegEmissBudgetFraction, "NegEmissBudgetFraction") %>%
        add_precursors(MODULE_INPUTS) ->
        negative_emissions_budget.xml
    }

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
