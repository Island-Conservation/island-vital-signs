# Function for mapping where a vegetation metric is high over a time series
#
# Returns a raster containing only the pixels whose median value across the
# entire time series falls in the top (1 - prob) of the distribution of pixel
# medians.
#
# x:          spatial raster stack of predicted values for a vegetation metric,
#             one layer per year (e.g. ndmi, evi, msavi2, ndvi, tcc)
# prob:       percentile (0-1) above which pixels are kept; default 0.75 keeps
#             the top 25% of values
# layer_name: name given to the output layer; default "baseline.high"

high_condition <- function(x, prob = 0.75, layer_name = "baseline.high") {
  
  # find the median value for each pixel across all years
  median_value <- app(x, fun = median, na.rm = TRUE)
  
  # find cutoff
  high_cutoff <- quantile(values(median_value)[, 1], prob = prob, na.rm = TRUE)
  
  # create an empty raster
  baseline_high <- rast(median_value)
  values(baseline_high) <- NA # assign NA values to all pixels
  
  # assign baseline categories to pixels
  baseline_high[median_value > high_cutoff] <- 1
  
  # rename layer based on layer_name in the function
  names(baseline_high) <- layer_name
  
  return(baseline_high)
  
}
