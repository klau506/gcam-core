#### Libraries =================================================================
# ==============================================================================
# .libPaths('C:\\Users\\claudia.rodes\\Documents\\R\\win-library\\4.1 - gcamdata_CP')
library(rgcam)
library(dplyr)
# library(tidyr)
library(ggplot2)
library(rfasst)
library(data.table)
library(hrbrthemes)
# library(GGally)
library(viridis)


#### Paths =====================================================================
# ==============================================================================
base_path = getwd()
outputs_path <<- paste0(base_path, "/outputs/")
inputs_path <<- paste0(base_path, "/inputs/")
figures_path <<- paste0(base_path, "/figures/")

#### Folders ===================================================================
# ==============================================================================
if(!dir.exists(outputs_path)) dir.create(outputs_path)
if(!dir.exists(inputs_path)) dir.create(inputs_path)
if(!dir.exists(figures_path)) dir.create(figures_path)

