# global.R

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------------- GENERAL-------------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#devtools::install_github("teunbrand/elementalist")

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinybusy)
library(shinyjs)
library(markdown)
library(ggplot2)
library(glue)
library(sf)
library(terra)
library(leaflet)
library(leaflet.extras2)
library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(ggspatial)
library(cowplot)
library(zip)
library(tmap)
library(grDevices)
library(RColorBrewer)
library(scico)
library(kableExtra)
library(ggiraph)
library(elementalist)

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
# "broc" is the diverging ramp: dark purple for the largest decrease, near
# white at no change, dark olive for the largest increase. Its cyclic sibling
# "brocO" wraps back on itself, so the largest decrease and the largest
# increase came out as the same colour on the change map.
div_pal <- scico(n = 256, palette = "broc", direction = 1)

# SET basemap ----
# CARTO began watermarking anonymous basemap requests in August 2026 -- without
# a key their endpoint returns an "API KEY REQUIRED" tile for every coordinate.
# The key is read from the CARTO_API_KEY environment variable. Note that tile
# requests are made by the browser, so the key is visible to anyone using the
# app; CARTO expects this and lets you restrict a key by domain.
#
# Locally the key comes from ~/.Renviron. That file never travels with a deploy,
# and shinyapps.io has no dashboard setting for environment variables, so the
# key has to ride along in the bundle -- rsconnect excludes neither of the files
# read below.
#
# carto.env rather than a .Renviron in the app directory, because R reads only
# ONE user environment file: a .Renviron in the working directory REPLACES
# ~/.Renviron rather than adding to it. One in this folder would therefore hide
# every other key in ~/.Renviron -- the database and ArcGIS credentials the
# data-prep scripts need -- from any R session started here. carto.env means
# nothing to R, so it costs nothing at startup and is read explicitly here.
#
# Anything already in the environment wins, so local development is untouched.
if (!nzchar(Sys.getenv("CARTO_API_KEY"))) {
  for (env_file in c("carto.env", ".Renviron")) {
    if (file.exists(env_file)) {
      readRenviron(env_file)
      break
    }
  }
}

carto_api_key <- trimws(Sys.getenv("CARTO_API_KEY"))
have_carto_key <- nzchar(carto_api_key)

if (have_carto_key) {
  basemap_dark_url <- paste0(
    "https://basemaps.cartocdn.com/rastertiles/dark_all/{z}/{x}/{y}{r}.png?key=",
    carto_api_key
  )

  basemap_light_url <- paste0(
    "https://basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}{r}.png?key=",
    carto_api_key
  )

  # CARTO's basemap terms require attribution to both OpenStreetMap and CARTO
  basemap_attribution <- paste0(
    '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors ',
    '&copy; <a href="https://carto.com/attributions">CARTO</a>'
  )
} else {
  # No key available: fall back to Esri's gray canvas tiles, which need no key
  # and look close enough to CARTO's dark_all / light_all that the maps still
  # read correctly. Without this the app would draw CARTO's watermark tile under
  # every layer. Note Esri orders its path {z}/{y}/{x} and has no retina ({r})
  # variant, so these are not drop-in substitutes for the CARTO templates.
  # There is no template file to copy, so this message is where the format is
  # written down: it has to be enough on its own to fix the problem.
  message(
    "CARTO_API_KEY is not set -- falling back to Esri gray canvas basemaps.\n",
    "  To restore the CARTO basemaps, put this one line in ~/.Renviron for\n",
    "  local work, or in a file named carto.env in the app directory, which\n",
    "  is what carries the key to shinyapps.io:\n",
    "      CARTO_API_KEY=your-key-here\n",
    "  A free key comes from https://carto.com/basemaps/apikey"
  )

  basemap_dark_url <- paste0(
    "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/",
    "World_Dark_Gray_Base/MapServer/tile/{z}/{y}/{x}"
  )

  basemap_light_url <- paste0(
    "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/",
    "World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
  )

  basemap_attribution <- paste0(
    '&copy; <a href="https://www.esri.com">Esri</a>, HERE, Garmin, ',
    '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
  )
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------------- FUNCTIONS-----------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# LOAD sourced functions ----
source("R/prep_tif_for_leaflet.R")
source("R/add_paired_popups.R")
source("R/inline_help.R")
source("R/download_bundle.R")
source("R/create_leaflet_splitmap.R")
source("R/create_pixel_trend_plot.R")
source("R/prep_change_tif_for_leaflet.R")
source("R/create_leaflet_splitmap_change.R")
source("R/generate_static_annual_map.R")
source("R/generate_static_change_map.R")
source("R/static_map_layout.R")
source("R/diverging_bins.R")
source("R/addLegend_decreasing.R")
source("R/get_palette_range.R")

# LOAD overview module functions ----
source("R/build_island_overview.R")
source("R/create_overview_map.R")

# LOAD trends & conditions module functions ----
source("R/high_condition.R")
source("R/low_condition.R")
source("R/high_status.R")
source("R/low_status.R")
source("R/increasing_trend.R")
source("R/decreasing_trend.R")
source("R/prep_classified_tif_for_leaflet.R")
source("R/classified_area.R")
source("R/create_leaflet_classified.R")
source("R/island_view_bounds.R")
source("R/add_carto_basemap.R")
source("R/add_sidebyside_multi.R")

# LOAD management questions module functions ----
source("R/accumulated_layer.R")
source("R/status_high_increasing.R")
source("R/status_low_decreasing.R")
source("R/greatest_change.R")
source("R/least_change.R")
source("R/forest_change.R")
source("R/question_categories.R")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------------------- DATA--------------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SET data directory ----
data_dir <- "data"

# LOAD island metadata ----
island_metadata <- st_read(file.path(data_dir, "island-metadata.shp"))
intervention_metadata <- read_csv(file.path(
  data_dir,
  "intervention-metadata.csv"
))
metric_metadata <- read_csv(file.path(data_dir, "metric-metadata.csv"))
tcc_yrs_metadata <- read_csv(file.path(data_dir, "tcc-yrs-metadata.csv"))
si_yrs_metadata <- read_csv(file.path(data_dir, "si-yrs-metadata.csv"))
time_periods_metadata <- read_csv(file.path(
  data_dir,
  "time-periods-metadata.csv"
))
time_periods_tcc_metadata <- read_csv(file.path(
  data_dir,
  "time-periods-tcc-metadata.csv"
))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------------- OBJECTS-------------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# SET metric map ----
# mapping between user choices and actual column names
metric_map <- c(
  "Tree Cover" = "tcc",
  "NDVI" = "ndvi",
  "EVI" = "evi",
  "NDMI" = "ndmi",
  "MSAVI2" = "msavi2"
)

# The metrics modeled for one island, as the display names metric_map uses.
#
# Eight of the seventy-five islands -- Santa Catalina, Rapa Nui, Santiago,
# Santa Fe, Seymour Norte, Jemo, La Catalina and Luis Pena -- have no rows in
# metric-metadata.csv and no metric folders under data/, so this returns
# character(0) for them and the modules have to say so rather than hand the
# reader an empty dropdown. Matching with %in% rather than == keeps a name
# that is not in island_metadata from recycling a zero-length GID2 across the
# whole metric table.
metrics_for_island <- function(island) {
  gid2 <- island_metadata$GID2_ID[island_metadata$Island == island]
  available <- metric_metadata$metric[metric_metadata$gid2 %in% gid2]

  names(metric_map)[metric_map %in% available]
}

# BUILD island overview ----
# One row per island for the Overview tab. Built once at startup rather than
# reactively: it is a join of three metadata tables that never change while
# the app is running, and it costs a few milliseconds.
island_overview <- build_island_overview(
  island_metadata,
  metric_metadata,
  intervention_metadata
)

# The filter choices come from the data rather than a hard-coded list, so an
# island added to the metadata shows up in the filters without a code change.
overview_countries <- sort(unique(island_overview$country))

overview_metric_choices <- names(metric_map)

overview_intervention_choices <- sort(unique(
  intervention_metadata$RemTyp
))

# The module tabs the Overview map can send a reader to. These strings are
# both the button labels in the map popups and the values updateTabsetPanel()
# matches on, so they have to stay character-for-character identical to the
# tabPanel titles in ui.R.
overview_module_tabs <- c(
  "1. Annual Values",
  "2. Change Over Time",
  "3. Classified Change",
  "4. Management Questions"
)

# The shortcut cards above the island catalogue. One line each: enough to tell
# a first-time reader what the four numbered tabs are for, short enough that
# the catalogue below them still starts on the first screen. The fuller
# descriptions live in text/m0-tool-overview-text.md, under the catalogue.
module_summaries <- list(
  list(
    tab = "1. Annual Values",
    blurb = paste(
      "Annual modeled vegetation metric values for each year within the time series."
    )
  ),
  list(
    tab = "2. Change Over Time",
    blurb = paste(
      "Total and annual rates of change for each vegetation metric."
    )
  ),
  list(
    tab = "3. Classified Change",
    blurb = paste(
      "Find where a vegetation metric is high, low, increasing, or decreasing."
    )
  ),
  list(
    tab = "4. Management Questions",
    blurb = paste(
      "Identify areas where productivity is high and increasing, low and decreasing, changing the most, or changing the least. Also find areas of forest regeneration or loss."
    )
  )
)

# SET annualized map ----
change_map <- c(
  "Fulltime" = "fulltime",
  "Pre-Intervention" = "pre",
  "Pre/Post1 Delta" = "post1pre_delta",
  "Post-Intervention 1" = "post1",
  "Post1/Post2 Delta" = "post2post1_delta",
  "Post-Intervention 2" = "post2"
)

# SET accumulated map ----
accumulated_map <- c(
  "Fulltime" = "fulltime",
  "Pre-Intervention" = "pre",
  "Pre/Post1 Delta" = "post1pre_delta",
  "Post-Intervention 1" = "post1",
  "Post1/Post2 Delta" = "post2post1_delta",
  "Post-Intervention 2" = "post2"
)

# SET ecosystem types ----
ecosystem_map <- c(
  "Tree" = 10,
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
  "Undefined" = 999
)

# SET aspect types ----
aspect_map <- c(
  "North (315° - 45°)" = 0,
  "East (45° - 135°)," = 1,
  "South (135° - 225°)" = 2,
  "West (225° - 315°)" = 3
)

# SET trends & conditions questions ----
# Each entry drives one choice in the Trends & Conditions module: which
# function answers the question, which raster stack that function runs on,
# and how the matching pixels are labelled and coloured on the map. The
# functions keep their own defaults for prob and na.values; only layer_name,
# years and hpdi are passed in, so the legend and the layer agree.
#
# uses_hpdi says whether the question reads an HPDI layer at all. The four
# condition and status questions are pure medians over the annual stack, so
# the probability level does not enter into them.
tc_status_years <- 5

tc_question_specs <- list(
  "Where is this metric high?" = list(
    fn = "high_condition",
    source = "annual",
    uses_hpdi = FALSE,
    layer_name = "baseline.high",
    categories = list(
      "1" = list(
        label = "High (top 25% across the full time series)",
        color = palm
      )
    )
  ),

  "Where is this metric low?" = list(
    fn = "low_condition",
    source = "annual",
    uses_hpdi = FALSE,
    layer_name = "baseline.low",
    categories = list(
      "1" = list(
        label = "Low (bottom 25% across the full time series)",
        color = coral
      )
    )
  ),

  "Where has this metric been high in the last 5 years?" = list(
    fn = "high_status",
    source = "annual",
    uses_hpdi = FALSE,
    layer_name = "status.high",
    categories = list(
      "1" = list(label = "High (top 25% over the last 5 years)", color = palm)
    )
  ),

  "Where has this metric been low in the last 5 years?" = list(
    fn = "low_status",
    source = "annual",
    uses_hpdi = FALSE,
    layer_name = "status.low",
    categories = list(
      "1" = list(
        label = "Low (bottom 25% over the last 5 years)",
        color = coral
      )
    )
  ),

  "Where is this metric increasing?" = list(
    fn = "increasing_trend",
    source = "accumulated",
    uses_hpdi = TRUE,
    layer_name = "increasing.trend",
    categories = list(
      "1" = list(label = "Increasing ({hpdi}% HPDI above zero)", color = palm)
    )
  ),

  "Where is this metric decreasing?" = list(
    fn = "decreasing_trend",
    source = "accumulated",
    uses_hpdi = TRUE,
    layer_name = "decreasing.trend",
    categories = list(
      "1" = list(label = "Decreasing ({hpdi}% HPDI below zero)", color = coral)
    )
  )
)

tc_question_choices <- names(tc_question_specs)

# SET management questions ----
# Each entry drives one choice in the Management Questions module. Unlike the
# Trends & Conditions module the user does not pick a metric: the productivity
# questions run on whichever index island_metadata$Prod_Met names for the
# island, and the forest question always runs on tree cover.
#
# source says which rasters the function needs:
#   productivity_status -- annual productivity stack (years only) + accumulated
#   productivity_trend  -- accumulated productivity stack only
#   productivity_cv     -- full annual productivity stack (the function drops
#                          the cell_id layer itself) + accumulated
#   forest              -- accumulated tree cover stack
mq_status_years <- 5

mq_question_specs <- list(
  "Where is productivity high and increasing over the last 5 years?" = list(
    fn = "status_high_increasing",
    source = "productivity_status",
    uses_hpdi = TRUE,
    layer_name = "high.increasing",
    categories = list(
      "1" = list(label = "High productivity, increasing trend", color = palm)
    )
  ),

  "Where is productivity low and decreasing over the last 5 years?" = list(
    fn = "status_low_decreasing",
    source = "productivity_status",
    uses_hpdi = TRUE,
    layer_name = "low.decreasing",
    categories = list(
      "1" = list(label = "Low productivity, decreasing trend", color = coral)
    )
  ),

  "Where has productivity changed the most?" = list(
    fn = "greatest_change",
    source = "productivity_trend",
    uses_hpdi = TRUE,
    layer_name = "greatest.change",
    categories = list(
      "1" = list(label = "Greatest increase", color = palm),
      "2" = list(label = "Greatest decrease", color = coral)
    )
  ),

  "Where does vegetation appear to resist change?" = list(
    fn = "least_change",
    source = "productivity_cv",
    uses_hpdi = TRUE,
    layer_name = "least.change",
    # The only question on this tab that asks where a trend is ABSENT, so the
    # probability level works the other way round here: a wider interval is
    # easier to overlap, and 95% therefore classifies more pixels than 75%,
    # not fewer. Naming the level in the legend is what tells a reader the
    # map in front of them moved at all -- without the {hpdi} token the
    # legend read the same at all three settings while the pixel count
    # changed underneath it.
    categories = list(
      "1" = list(
        label = "Stable (no trend at {hpdi}% HPDI, low variability)",
        color = surf_blue
      )
    )
  ),

  "Where is forest potentially regenerating or declining?" = list(
    fn = "forest_change",
    source = "forest",
    uses_hpdi = TRUE,
    layer_name = "forest.trend",
    categories = list(
      "1" = list(label = "Forest increasing", color = palm),
      "2" = list(label = "Forest decreasing", color = coral)
    )
  )
)

mq_question_choices <- names(mq_question_specs)

# SET probability ----
p <- 0.85

choice <- "hdi"
