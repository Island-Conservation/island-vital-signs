# Function for mapping where vegetation appears to resist change
#
# Returns a raster containing only the pixels that were modeled but show no
# directional trend (HPDI overlaps zero) AND have the lowest variability
# (coefficient of variation) across the predicted time series.
#
# x:          spatial raster stack of predicted values for a vegetation metric,
#             one layer per year (e.g. evi, msavi2, ndvi), plus a cell_id layer
# x.trend:    accumulated change raster stack for the same metric; must contain
#             a posterior mean layer and an HPDI layer at the selected level,
#             either the fulltime or the pre-intervention version
#             (e.g. green.accumulated)
# na.values:  value in the raster that is replaced with NA; default -9999
# prob:       percentile (0-1) below which coefficient of variation values are
#             kept; default 0.25 keeps the least variable 25% of pixels
# hpdi:       probability level for the highest posterior density interval,
#             one of 75, 85 or 95; higher means stronger evidence that the
#             change is real. Default 85
# layer_name: name given to the output layer; default "least.change"

least_change <- function(x, x.trend, na.values = -9999, prob = 0.25,
                         layer_name = "least.change", hpdi = 85) {
  
  # the trend layer at this probability level, for whichever time period this
  # island was modeled over (see accumulated_layer)
  hpdi_layer <- accumulated_layer(x.trend, paste0("posterior.mean_hpdi", hpdi))
  
  # change -9999 values to NA
  hpdi_layer[hpdi_layer == na.values] <- NA
  
  # the posterior mean for the same period the hpdi layer above came from
  posterior_mean <- accumulated_layer(x.trend, "posterior.mean")
  
  # change -9999 values to NA
  posterior_mean[posterior_mean == na.values] <- NA
  
  # find modeled cells where HPDI overlaps zero, i.e., no directional trend
  no_change <- !is.na(posterior_mean) & # identifies cells that have data
    is.na(hpdi_layer) # identifies cells that are masked by hpdi_layer
  no_change_ras <- ifel(no_change, 1, NA) # maps modeled cells with no directional change
  
  # calculate variability in predicted time series
  year_layers <- x[[names(x) != "cell_id"]] # drop the layer for cell ID, keep layers for years
  cv <- stdev(year_layers) / abs(mean(year_layers)) # cv = sd/mean
  
  # only keep cells that were modeled
  cv <- mask(cv, posterior_mean)
  
  # find the cells with the lowest variability
  cv_cutoff <- quantile(values(cv)[, 1], probs = prob, na.rm = TRUE)
  
  # create a blank raster that can be used to fill in the trend classes
  cv_low <- rast(cv) # use the same CRS and dimensions as the veg metric rasters
  values(cv_low) <- NA # assign NA values to all pixels
  
  # assign pixels with low variability and no change
  cv_low[(cv < cv_cutoff) & no_change] <- 1
  
  names(cv_low) <- layer_name
  
  return(cv_low)
}
