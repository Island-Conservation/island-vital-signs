# Function for mapping where productivity has changed the most
#
# Returns a raster with two categories: (1) pixels with the greatest increase
# and (2) pixels with the greatest decrease. Cutoffs are taken separately
# within the positive and negative accumulated trend distributions.
#
# x.trend:    accumulated change raster stack for a vegetation metric; must
#             contain a posterior.mean_hpdi<level>_<period> layer for at
#             least one period (e.g. green.accumulated)
# na.values:  value in the raster that is replaced with NA; default -9999
# prob:       percentile (0-1) defining the cutoffs; default 0.75 keeps the top
#             25% of positive values and the bottom 25% of negative values
# hpdi:       probability level for the highest posterior density interval,
#             one of 75, 85 or 95; higher means stronger evidence that the
#             change is real. Default 85
# layer_name: name given to the output layer; default "greatest.change"

greatest_change <- function(x.trend, na.values = -9999, prob = 0.75,
                            layer_name = "greatest.change", hpdi = 85) {
  
  # the trend layer at this probability level, for whichever time period this
  # island was modeled over (see accumulated_layer)
  hpdi_layer <- accumulated_layer(x.trend, paste0("posterior.mean_hpdi", hpdi))
  
  # change -9999 values to NA
  hpdi_layer[hpdi_layer == na.values] <- NA
  
  # define positive and negative trends
  positive_trend <- hpdi_layer > 0
  negative_trend <- hpdi_layer < 0
  
  # create vectors with values extracted from the accumulated trend raster
  positive_acc_values <- values(hpdi_layer)[values(positive_trend) == 1] # positive values
  negative_acc_values <- values(hpdi_layer)[values(negative_trend) == 1] # negative values
  
  # find greatest change cutoffs
  positive_cutoff <- quantile(positive_acc_values, probs = prob, na.rm = TRUE)
  negative_cutoff <- quantile(negative_acc_values, probs = 1 - prob, na.rm = TRUE)
  
  # create a blank raster that can be used to fill in the trend classes
  greatest_change_ras <- rast(hpdi_layer) # use the same CRS and dimensions as the veg metric rasters
  values(greatest_change_ras) <- NA # fill all of the pixels with NA for now
  
  # assign trend to each pixel
  greatest_change_ras[positive_trend & hpdi_layer > positive_cutoff] <- 1 # most positive (far from 0)
  greatest_change_ras[negative_trend & hpdi_layer < negative_cutoff] <- 2 # most negative (far from 0)
  
  # rename layer based on layer_name in the function
  names(greatest_change_ras) <- layer_name
  
  return(greatest_change_ras)
}
