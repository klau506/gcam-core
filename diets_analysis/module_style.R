
################################################################################
################################################################################
# PALETTES & LABELS

scen_palette_refVsSppVsSnrVsSppnr = c(
  'SPP' = '#32a0a8',
  'SNR' = '#500f96',
  'SPPNR' = '#198a22',
  'REF' = '#7d0c1a'
)
scen_palette_refVsSppVsSnrVsSppnr.labs <- c('SPP', "SNR", "SPPNR", 'REF')
names(scen_palette_refVsSppVsSnrVsSppnr.labs) = c("SPP", "SNR", "SPPNR", 'REF')


land_use_scenario_palette =
  c('Cropland' = 'chocolate4',
    'Pasture' = '#E6AB02',
    'Forest' = 'darkgreen',
    'Shrubs & Grass' = '#66A61E',
    'Other Natural' = '#b1e378')
land_use_order_palette =
  c('Cropland',
    'Pasture',
    'Forest',
    'Shrubs & Grass',
    'Other Natural')



# creates palette: "random" or from palette colors for all scenarios, except for reference one, which is black
create_palette <- function(scenarios, reference = "ref", palette = viridis::viridis(length(scenarios), option = 'D')) {
  #  RColorBrewer::brewer.pal(length(scenarios), "Paired")
  palette <- setNames(palette, scenarios)
  palette[[reference]] <- "black"
  return(palette)
}

