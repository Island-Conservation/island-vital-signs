# Function for mapping where productivity is low and decreasing over the last n years
#
# Returns a raster containing only the pixels whose median value across the last
# n years is in the bottom `prob` of the distribution of pixel medians AND whose
# accumulated trend is negative at the selected HPDI level.
#
# x:          spatial raster stack of predicted productivity values, one layer
#             per year (e.g. evi, msavi2, ndvi)
# x.trend:    accumulated change raster stack for the same metric; must contain
#             a posterior.mean_hpdi<level>_<period> layer for at least
#             one period (e.g. green.accumulated)
# years:      number of years, n, the median is calculated across; must be
#             between 1 and the length of the time series; default 5
# na.values:  value in the raster that is replaced with NA; default -9999
# prob:       percentile (0-1) below which pixels are kept; default 0.25 keeps
#             the bottom 25% of values
# hpdi:       probability level for the highest posterior density interval,
#             one of 75, 85 or 95; higher means stronger evidence that the
#             change is real. Default 85
# layer_name: name given to the output layer; default "low.decreasing"

status_low_decreasing <- function(x, x.trend, years = 5, na.values = -9999,
                                  prob = 0.25, layer_name = "low.decreasing", hpdi = 85) {
  
  # validation for years argument--must be > 1 and < length of time series
  if (years < 1 || years > nlyr(x)) {
    stop("`years` must be at least 1, and no more than the length of the time series")
  }
  
  ## GET STATUS FIRST
  # find median value for each pixel across the specified years
  status_median <- app(x[[(nlyr(x) - (years - 1)):nlyr(x)]], fun = median, na.rm = TRUE)
  
  # find cutoff
  low_cutoff <- quantile(values(status_median)[, 1], prob = prob, na.rm = TRUE)
  
  ## GET TREND NEXT
  # the trend layer at this probability level, for whichever time period this
  # island was modeled over (see accumulated_layer)
  hpdi_layer <- accumulated_layer(x.trend, paste0("posterior.mean_hpdi", hpdi))
  
  # change -9999 values to NA
  hpdi_layer[hpdi_layer == na.values] <- NA
  
  # define negative trends
  negative_trend <- hpdi_layer < 0
  
  ## DEFINE CATEGORIES
  # create an empty raster
  low_decreasing <- rast(status_median)
  values(low_decreasing) <- NA # assign NA values to all pixels
  
  # Status = Low, Trend = Negative
  low_decreasing[status_median < low_cutoff & negative_trend] <- 1 # bottom 25%, decreasing
  
  # rename layer based on layer_name in the function
  names(low_decreasing) <- layer_name
  
  return(low_decreasing)
  
}
