
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


deaths_scenario_palette =
  c('PM25' = '#6b0ead',
    'O3' = '#E6AB02')
deaths_order_palette =
  c('PM25',
    'O3')

macronutrients_scenario_palette = c(
  'gProteinPerCapita.SNR' = '#500f96',
  'gFatPerCapita.SNR' = '#1558bd',
  'gProteinPerCapita.SPP' = '#2ec74f',
  'gFatPerCapita.SPP' = '#9ac217',
  'gProteinPerCapita.REF' = '#a61230',
  'gFatPerCapita.REF' = '#e85fd1'
)
macronutrients_scenario.labs <- c('SNR - Protein',"SNR - Fat",
                                  'SPP - Protein','SPP - Fat',
                                  'REF - Protein','REF - Fat')
names(macronutrients_scenario.labs) = c("gProteinPerCapita.SNR", "gFatPerCapita.SNR",
                                        "gProteinPerCapita.SPP", "gFatPerCapita.SPP",
                                        'gProteinPerCapita.REF', 'gFatPerCapita.REF')

micronutrients_scenario_palette = c(
  'calcium' = '#a63603',
  'iron' = '#e6550d',
  'magnesium' = '#fd8d3c',
  'selenium' = '#fdae6b',
  'sodium' = '#fdd0a2',
  'zinc' = '#feedde',
  'folate' = '#006d2c',
  'niacin' = '#31a354',
  'riboflavin' = '#74c476',
  'thiamin' = '#bae4b3',
  'vitamin a' = '#a5faed',
  'vitamin b6' = '#52e9f2',
  'vitamin b12' = '#3ebce6',
  'vitamin c' = '#1394bf',
  'vitamin d' = '#002a9c',
  'vitamin k' = '#7293ed'
)
micronutrients_scenario.labs <- c('Calcium',"Iron",
                                  'Magnesium','Selenium',
                                  'Sodium','Zinc',
                                  'Folate', 'Niacin',
                                  'Riboflavin','Thiamin',
                                  'Vitamin A', 'Vitamin B6',
                                  'Vitamin B12', 'Vitamin C',
                                  'Vitamin D', 'Vitamin K')
names(micronutrients_scenario.labs) = c("calcium", "iron",
                                        "magnesium", "selenium",
                                        'sodium', 'zinc',
                                        'folate', 'niacin',
                                        'riboflavin','thiamin',
                                        'vitamin a', 'vitamin b6',
                                        'vitamin b12', 'vitamin c',
                                        'vitamin d', 'vitamin k')


# creates palette: "random" or from palette colors for all scenarios, except for reference one, which is black
create_palette <- function(scenarios, reference = "ref", palette = viridis::viridis(length(scenarios), option = 'D')) {
  #  RColorBrewer::brewer.pal(length(scenarios), "Paired")
  palette <- setNames(palette, scenarios)
  palette[[reference]] <- "black"
  return(palette)
}

