# Function for mapping where a vegetation metric has been low over the last n years
#
# Returns a raster containing only the pixels whose median value across the last
# n years of the time series falls in the bottom `prob` of the distribution of
# pixel medians.
#
# x:          spatial raster stack of predicted values for a vegetation metric,
#             one layer per year (e.g. ndmi, evi, msavi2, ndvi, tcc)
# years:      number of years, n, the median is calculated across; must be
#             between 1 and the length of the time series
# prob:       percentile (0-1) below which pixels are kept; default 0.25 keeps
#             the bottom 25% of values
# layer_name: name given to the output layer; default "status.low"

low_status <- function(x, years, prob = 0.25, layer_name = "status.low") {
  
  # validation for years argument--must be > 1 and < length of time series
  if (years < 1 || years > nlyr(x)) {
    stop("`years` must be at least 1, and no more than the length of the time series")
  }
  
  # find median value for each pixel across the specified years
  status_median <- app(x[[(nlyr(x) - (years - 1)):nlyr(x)]], fun = median, na.rm = TRUE)
  
  # find cutoff
  low_cutoff <- quantile(values(status_median)[, 1], prob = prob, na.rm = TRUE)
  
  # create an empty raster
  status_low <- rast(status_median)
  values(status_low) <- NA # assign NA values to all pixels
  
  # assign status categories to pixels
  status_low[status_median < low_cutoff] <- 1
  
  # rename layer based on layer_name in the function
  names(status_low) <- layer_name
  
  return(status_low)
  
}
