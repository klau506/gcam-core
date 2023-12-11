# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L113_ag_storage
#'
#' Preparing and processing agricultural storage data
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L113.ag_Storage_Mt_R_C_Y_adj}.
#' @details This chunk calculates ag storage values, losses, and costs by GCAM region and commodity.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter group_by mutate select transmute
#' @importFrom tidyr gather spread replace_na
#' @author XZ 2023
module_aglu_L113_ag_storage <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "aglu/A_agStorageSector",
      "L109.ag_ALL_Mt_R_C_Y",
      "L109.an_ALL_Mt_R_C_Y",
      "L1321.ag_prP_R_C_75USDkg",
      "L1321.an_prP_R_C_75USDkg")

  MODULE_OUTPUTS <-
    c("L113.StorageTechAndPassThrough")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # [ToDo: adding forest (L110) to here later when seeing fit]
    # so regional forest and total forest would be separated


    # 1. Prepare storage cost shares ----
    # # USA storage cost (EU is likely similar to the US)
    # # 12-month is about $0.4 per bu. higher than 6-month
    # # $0.2-0.4 per bu. *  39.4 bu per tonne (Corn) = $16 per tonne

    #
    # USACornPrice <-
    #   L1321.ag_prP_R_C_75USDkg %>%
    #   filter(region == "USA", GCAM_commodity == "Corn") %>% pull(value)


    # .2 * 39.4 * gdp_deflator(1975, 2015) / 1000 / USACornPrice
    # .4 * 39.4 * gdp_deflator(1975, 2015) / 1000 / USACornPrice
    # 5.4% - 10.8%

    # If directly using Iowa storage cost model
    # Corn 11% - 13%    Last 6 month: 3% - 7%
    # Soybean 1% - 8%   Last 6 month: 3% - 5%
    # soybeans would be smaller in total, but similarly in marginal
    #
    # # Africa storage cost
    # # 12.5$ per tonne per year for PICS bag (ownership) adding 100% operation cost
    # # 1.7$ per tonne per year for standard bags
    # AfricaGrainPrice <-
    #   L1321.ag_prP_R_C_75USDkg %>%
    #   filter(grepl("Africa", region),
    #          GCAM_commodity %in% c("Corn", "Rice", "OtherGrain", "Wheat")) %>%
    #   summarize(value = mean(value)) %>% pull
    #
    # Assuming other overhead cost is 2 time bag costs
    # 12.5 * 2 * gdp_deflator(1975, 2015) / 1000 / AfricaGrainPrice
    # (1.7) * 2  * gdp_deflator(1975, 2015) / 1000 / AfricaGrainPrice
    # (12.5 + 1.7)  * gdp_deflator(1975, 2015) / 1000 / AfricaGrainPrice
    # # 4.7% - 8.3%

    # Assuming 50% is marginal: 2.3% - 4.2%
    #
    # # Middle East
    # # $2 (1.69 Jordan - 3.47 Tunisia) per tonne per month in 2009
    #
    # MiddEastGrainPrice <-
    #   L1321.ag_prP_R_C_75USDkg %>%
    #   filter(grepl("Middle East", region),
    #          GCAM_commodity %in% c("Corn", "Rice", "OtherGrain", "Wheat")) %>%
    #   summarize(value = mean(value)) %>% pull
    #
    # 1.69 * 12 * gdp_deflator(1975, 2009) / 1000 / MiddEastGrainPrice
    # 3.47 * 12 * gdp_deflator(1975, 2009) / 1000 / MiddEastGrainPrice
    # # 4 - 9%
    # Assuming 50% is marginal: 2% - 4.5%

    # Storage cost shares summary ----
    # There is no available data of storage cost worldwide
    # Based on the data points shown above (grains), the cost shares (marginal; second-half of the year)
    # are usually in 2 - 7%
    # However, the difference between the cost for interannual storage vs average storage is not clear.
    # Storage cost vs. market price of commercial storage service could be another source of uncertainty.
    # Here we will assume 3% of the price is the storage cost everywhere in all sectors.


    ## Storage cost share in producer prices ----
    InterAnnualStorageCostShare <- 0.03

    L1321.ag_prP_R_C_75USDkg %>%
      bind_rows(L1321.an_prP_R_C_75USDkg) %>%
      filter(GCAM_commodity %in% (A_agStorageSector %>%
                                    filter(storage_model == T) %>% pull(GCAM_commodity))) %>%
      mutate(value = value * InterAnnualStorageCostShare) ->
      L113.ClosingStockCost_R_C

    # 2. Get storage data from the adjusted SUA balances ----
    # And get parameters ready
    L109.ag_ALL_Mt_R_C_Y %>%
      gather(element, value, -GCAM_commodity, -year, -GCAM_region_ID) %>%
      bind_rows(
        L109.an_ALL_Mt_R_C_Y %>%
          gather(element, value, -GCAM_commodity, -year, -GCAM_region_ID)
      ) %>%
      # Keep relevant elements, storage comm., and base years only
      filter(element %in% c("Opening stocks", "Closing stocks", "InterAnnualStockLoss"),
             year %in% MODEL_BASE_YEARS) %>%
      # Keep sectors in A_agStorageSector which are the sectors with regional markets
      # And join sector mappings and parameters
      # all storage commodities are in L109 SUA data; this was asserted in the earlier stage
      inner_join(A_agStorageSector, by = "GCAM_commodity") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(-GCAM_region_ID) ->
      L113.ag_Storage_Mt_R_C_Y_adj1


    # 3. Compute loss-coefficients and arrange data to get table needed ----
    L113.ag_Storage_Mt_R_C_Y_adj1 %>%
      spread(element, value) %>%
      mutate(LossCoef = 1 - InterAnnualStockLoss / `Closing stocks`) %>%
      replace_na(list(LossCoef = 0)) %>%
      group_by(GCAM_commodity, region) %>%
      # compute Carry-forward here
      mutate(Carryforward = lead(`Opening stocks`),
             Carryforward = if_else(is.na(Carryforward),
                                    `Closing stocks` * LossCoef, Carryforward)) %>% ungroup %>%
      select(supplysector, region, year, LossCoef, `Closing stocks`, `Opening stocks`,
             Carryforward, logit.exponent, technology, minicam_energy_input, GCAM_commodity, storage_model) ->
      L113.ag_Storage_Mt_R_C_Y_adj2


    ## Join storage cost ----
    L113.ag_Storage_Mt_R_C_Y_adj2 %>%
      left_join(L113.ClosingStockCost_R_C %>%
                  select(region, GCAM_commodity, ClosingStockCost = value),
                by = c("region", "GCAM_commodity")) %>%
      replace_na(list(ClosingStockCost = 0))->
      L113.ag_Storage_Mt_R_C_Y_adj



    # 3. Prepare a storage tech data table for generating XMLs. ----

    # Calculate lifetime in the carry-over structure
    # may need adjustments here when years/steps are changing
    L113.StorageLifeTime <-
      tibble(year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      mutate(lifetime = lead(year, 2) - year) %>%
      mutate(lifetime = if_else(year == 1990, 16, lifetime),
             lifetime = if_else(year == 2005, 6, lifetime))

    # Construct the key table for storage tech.
    # ToDo: storage.cost
    L113.StorageTechAndPassThrough <-
      L113.ag_Storage_Mt_R_C_Y_adj %>%
      dplyr::transmute(region, supplysector,
                       subsector = supplysector,
                       food.storage.technology = technology,
                       year,
                       share.weight = 1,
                       logit.exponent,
                       logit.type = NA, # Note that this is not used but added to avoid warnings
                       storage.cost = ClosingStockCost,
                       closing.stock = `Closing stocks`,
                       loss.coefficient = LossCoef,
                       opening.stock = Carryforward,
                       minicam.energy.input = minicam_energy_input,
                       GCAM_commodity, storage_model) %>%
      left_join_error_no_match(L113.StorageLifeTime, by = "year")

    # add a future year here for storage commodities (storage_model == TRUE)
    L113.StorageTechAndPassThrough <-
      L113.StorageTechAndPassThrough %>%
      bind_rows(
        L113.StorageTechAndPassThrough %>% filter(storage_model == TRUE) %>%
          filter(year == max(MODEL_BASE_YEARS)) %>%
          mutate(year = min(MODEL_FUTURE_YEARS),
                 # setting carried.forward and closing stock to zero
                 # The two are not important because the values are not used
                 opening.stock = closing.stock * loss.coefficient,
                 closing.stock = opening.stock,
                 lifetime = sort(MODEL_FUTURE_YEARS)[2] - max(MODEL_BASE_YEARS))
      )

    # Produce outputs ----
    L113.StorageTechAndPassThrough %>%
      add_title("Ag storage data and parameter assumptions") %>%
      add_units("various") %>%
      add_comments("Table inlcudes the data and parameter of the storage tech or the pass-through sector of generating, e.g., regional corn from total corn") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agStorageSector",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L1321.ag_prP_R_C_75USDkg",
                     "L1321.an_prP_R_C_75USDkg") ->
      L113.StorageTechAndPassThrough


    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
