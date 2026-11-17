# Function for mapping where forest is potentially regenerating or declining
#
# Returns a raster with two categories based on the accumulated trend in tree
# canopy cover at the selected HPDI level: (1) increasing and (2) decreasing.
#
# x.trend:    accumulated change raster stack for tree canopy cover; must
#             contain a posterior.mean_hpdi<level>_<period> layer for at
#             least one period (e.g. tcc.accumulated)
# na.values:  value in the raster that is replaced with NA; default -9999
# hpdi:       probability level for the highest posterior density interval,
#             one of 75, 85 or 95; higher means stronger evidence that the
#             change is real. Default 85
# layer_name: name given to the output layer; default "forest.trend"

forest_change <- function(x.trend, na.values = -9999, layer_name = "forest.trend", hpdi = 85) {
  
  # the trend layer at this probability level, for whichever time period this
  # island was modeled over (see accumulated_layer)
  hpdi_layer <- accumulated_layer(x.trend, paste0("posterior.mean_hpdi", hpdi))
  
  # change -9999 values to NA
  hpdi_layer[hpdi_layer == na.values] <- NA
  
  # define positive and negative trends
  positive_trend <- hpdi_layer > 0
  negative_trend <- hpdi_layer < 0
  
  # create a blank raster that can be used to fill in the trend classes
  forest_trend <- rast(hpdi_layer) # use the same CRS and dimensions as the veg metric rasters
  values(forest_trend) <- NA # fill all of the pixels with NA for now
  
  # assign trend to each pixel
  forest_trend[positive_trend] <- 1
  forest_trend[negative_trend] <- 2
  
  # rename layer based on layer_name in the function
  names(forest_trend) <- layer_name
  
  return(forest_trend)
}
