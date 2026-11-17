# Function for mapping where a vegetation metric is low over a time series
#
# Returns a raster containing only the pixels whose median value across the
# entire time series falls in the bottom `prob` of the distribution of pixel
# medians.
#
# x:          spatial raster stack of predicted values for a vegetation metric,
#             one layer per year (e.g. ndmi, evi, msavi2, ndvi, tcc)
# prob:       percentile (0-1) below which pixels are kept; default 0.25 keeps
#             the bottom 25% of values
# layer_name: name given to the output layer

low_condition <- function(x, prob = 0.25, layer_name = "baseline.low") {
  
  # find the median value for each pixel across all years
  median_value <- app(x, fun = median, na.rm = TRUE)
  
  # find cutoff
  low_cutoff <- quantile(values(median_value)[, 1], prob = prob, na.rm = TRUE)
  
  # create an empty raster
  baseline_low <- rast(median_value)
  values(baseline_low) <- NA # assign NA values to all pixels
  
  # assign baseline categories to pixels
  baseline_low[median_value < low_cutoff] <- 1
  
  # rename layer based on layer_name in the function
  names(baseline_low) <- layer_name
  
  return(baseline_low)
  
}
