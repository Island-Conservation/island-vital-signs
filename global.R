# global.R

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------------- GENERAL-------------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# # DEFINE packages ----
# packages <- c( 
#   "RColorBrewer",
#   #"bslib",
#   #"colorspace",
#   "cowplot",
#   "dplyr",
#   "ggplot2",
#   "ggspatial",
#   "glue",
#   "grDevices",
#   "leaflet",
#   "leaflet.extras2",
#   #"magrittr",
#   "markdown",
#   "readr",
#   #"renv",
#   #"scales",
#   "sf",
#   #"shinipsum",
#   "shiny",
#   #"shinyWidgets",
#   "shinybusy",
#   "shinycssloaders",
#   "shinydashboard",
#   "shinyjs",
#   "stringr",
#   "terra",
#   "tidyr",
#   "tmap",
#   "zip"
# )

# INSTALL packages ----
# for (pkg in packages) {
#   if (!requireNamespace(pkg, quietly = TRUE)) {
#     install.packages(pkg)
#   }
# }

# install_if_missing <- function(pkgs) {
#   to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
#   if (length(to_install)) renv::install(to_install)
#   invisible(lapply(pkgs, library, character.only = TRUE))
# }
# 
# install_if_missing(packages)
# 
# # LOAD packages ----
# for (pkg in packages) {
#   library(pkg, character.only = TRUE)
# }

library(shiny)
library(shinydashboard)
# library(shinyWidgets)
library(shinycssloaders)
library(shinybusy)
library(shinyjs)
library(markdown)
# library(bslib)
library(ggplot2)
# library(shinipsum)
library(glue)
library(sf)
library(terra)
library(leaflet)
library(leaflet.extras2)
# library(magrittr)
library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(ggspatial)
library(cowplot)
library(zip)
library(tmap)
# library(scales)
library(grDevices)
library(RColorBrewer)
library(scico)
library(kableExtra)
library(ggiraph)
library(elementalist)

# library(colorspace)

#library(plotly)
#library(tidyverse)
#library(ARTofR)
#library(leafsync)
#library(leafem)
#library(viridis)

# SET digits ----
options(digits = 9)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------------------- AESTHETICS-----------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# SET colors of island conservation ----
deep_blue <- "#00223D"
surf_blue <- "#ADCAEA"
coral <- "#F19B79"
sand <- "#FED098"
palm <- "#DFE565"
light_neutral <- "#F4F4F4"
dark_neutral <- "#423f43"

# SET color palettes
div_pal <- scico(n = 256, palette = "brocO", direction = 1) 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------------- FUNCTIONS-----------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# LOAD sourced functions ----
source("R/prep_tif_for_leaflet.R")
source("R/create_leaflet_splitmap.R")
source("R/create_pixel_trend_plot.R")
source("R/prep_change_tif_for_leaflet.R")
source("R/create_leaflet_splitmap_change.R")
source("R/generate_static_annual_map.R")
source("R/generate_static_change_map.R")
#source("R/generate_static_accumulated_change_map.R")
#source("R/create_leaflet_splitmap_accumulated.R")
#source("R/prep_accumulated_change_tif_for_leaflet.R")
source("R/addLegend_decreasing.R")
source("R/create_action_map.R")
source("R/get_palette_range.R") 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------------------- DATA--------------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SET data directory ----
data_dir <- "data"
#data_dir <- "X:/CONOPS/GIS/GIS Data/NASA Pilot Islands/04_DSM"

# LOAD island metadata ----
island_metadata <- st_read(file.path(data_dir, "island-metadata.shp"))
intervention_metadata <- read_csv(file.path(data_dir, "intervention-metadata.csv"))
metric_metadata <- read_csv(file.path(data_dir, "metric-metadata.csv"))
tcc_yrs_metadata <- read_csv(file.path(data_dir, "tcc-yrs-metadata.csv"))
si_yrs_metadata <- read_csv(file.path(data_dir, "si-yrs-metadata.csv"))
time_periods_metadata <- read_csv(file.path(data_dir, "time-periods-metadata.csv"))
time_periods_tcc_metadata <- read_csv(file.path(data_dir, "time-periods-tcc-metadata.csv"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------------- OBJECTS-------------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# SET metric map ----
# mapping between user choices and actual column names
metric_map <- c("Tree Cover" = "tcc", 
                "NDVI" = "ndvi", 
                "EVI" = "evi",
                "NDMI" = "ndmi",
                "MSAVI2" = "msavi2")

# SET annualized map ----
change_map <- c("Fulltime" = "fulltime", 
                "Pre-Intervention" = "pre", 
                "Pre/Post1 Delta" = "post1pre_delta", 
                "Post-Intervention 1" = "post1", 
                "Post1/Post2 Delta" = "post2post1_delta", 
                "Post-Intervention 2" = "post2")

# SET accumulated map ----
accumulated_map <- c("Fulltime" = "fulltime", 
                     "Pre-Intervention" = "pre", 
                     "Pre/Post1 Delta" = "post1pre_delta", 
                     "Post-Intervention 1" = "post1", 
                     "Post1/Post2 Delta" = "post2post1_delta", 
                     "Post-Intervention 2" = "post2")

# SET ecosystem types ----
ecosystem_map <- c("Tree" = 10,
                   "Shrubland" = 20,
                   "Grassland" = 30,
                   "Cropland" = 40,
                   "Urban" = 50,
                   "Bare" = 60,
                   "Snow" = 70,
                   "Water" = 80,
                   "Wetland" = 90,
                   "Mangrove" = 95,
                   "Moss" = 100,
                   "Undefined" = 999)

# SET aspect types ----
aspect_map <- c("North (315° - 45°)" = 0,
                "East (45° - 135°)," = 1,
                "South (135° - 225°)" = 2,
                "West (225° - 315°)" = 3)

# SET probability ----
p <- 0.85

choice <- "hdi"
