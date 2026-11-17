# Function for mapping where a vegetation metric is decreasing over a time series
#
# Returns a raster containing only the pixels where the predicted accumulated
# trend (at the selected HPDI level) is negative.
#
# x:          accumulated change raster stack for a vegetation metric; must
#             contain a posterior.mean_hpdi<level>_<period> layer for at
#             least one period (e.g. ndmi.accumulated,
#             green.accumulated, tcc.accumulated)
# na.values:  value in the raster that is replaced with NA; default -9999
# hpdi:       probability level for the highest posterior density interval,
#             one of 75, 85 or 95; higher means stronger evidence that the
#             change is real. Default 85
# layer_name: name given to the output layer

decreasing_trend <- function(x, na.values = -9999, layer_name = "decreasing.trend", hpdi = 85) {
  
  # the trend layer at this probability level, for whichever time period this
  # island was modeled over (see accumulated_layer)
  hpdi_layer <- accumulated_layer(x, paste0("posterior.mean_hpdi", hpdi))
  
  # change -9999 values to NA
  hpdi_layer[hpdi_layer == na.values] <- NA
  
  # define negative trends
  negative_trend <- hpdi_layer < 0
  
  # create a blank raster that can be used to fill in the trend classes
  decreasing <- rast(hpdi_layer) # use the same CRS and dimensions as the veg metric rasters
  values(decreasing) <- NA # fill all of the pixels with NA for now
  
  # assign trend to each pixel
  decreasing[negative_trend] <- 1
  
  # rename layer based on layer_name in the function
  names(decreasing) <- layer_name
  
  return(decreasing)
}
