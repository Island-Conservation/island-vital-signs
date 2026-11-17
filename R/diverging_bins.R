# Function for building symmetric change bins that fit the data
#
# The static change map used to bin on fixed breaks of +/-0.01 and +/-0.05.
# Those suit accumulated spectral indices and nothing else: annualized change
# is a per-year rate, so almost every pixel fell inside +/-0.01 and the map
# came out white, while tree cover is measured in percentage points, so almost
# every pixel fell outside +/-0.05 and the map came out as two flat colours.
#
# Breaks are instead placed symmetrically about zero at a third and two thirds
# of the spread, so the classes mean the same thing whatever the metric and
# whichever change type is being mapped.
#
# values: the layer's values, NAs and the -9999 fill already removed
#
# Returns a list with breaks, labels and fill_colors, ready for cut() and
# scale_fill_manual().

diverging_bins <- function(values, na.values = -9999) {
  
  v <- values[is.finite(values) & values != na.values]

  # Thresholds come from the distribution of change magnitudes rather than
  # from the spread, so roughly a third of the pixels land in each band. Fixed
  # fractions of the range leave the middle band swallowing most of the map
  # whenever change is concentrated near zero, which it usually is.
  if (length(v)) {
    lo <- unname(stats::quantile(abs(v), 1 / 3, na.rm = TRUE))
    hi <- unname(stats::quantile(abs(v), 2 / 3, na.rm = TRUE))
  } else {
    lo <- hi <- 0
  }

  # degenerate layer (empty, all one value, or more than a third exactly zero):
  # still produce five ordered, distinctly labelled classes rather than erroring
  if (!is.finite(hi) || hi <= 0) hi <- if (length(v) && max(abs(v)) > 0) max(abs(v)) else 1
  if (!is.finite(lo) || lo <= 0) lo <- hi / 2
  if (lo >= hi) lo <- hi / 2

  # enough decimals to tell the breaks apart, whether the metric is tree cover
  # in percentage points or an annualized index in thousandths
  digits <- max(0, min(5, ceiling(-log10(lo)) + 1))
  fmt <- function(x) formatC(x, format = "f", digits = digits)
  
  labels <- c(paste0("< -", fmt(hi)),
              paste0("-", fmt(hi), " to -", fmt(lo)),
              paste0("-", fmt(lo), " to ", fmt(lo)),
              paste0(fmt(lo), " to ", fmt(hi)),
              paste0("> ", fmt(hi)))
  
  list(
    breaks = c(-Inf, -hi, -lo, lo, hi, Inf),
    labels = labels,
    fill_colors = setNames(
      c("magenta4", "magenta1", "white", "olivedrab1", "olivedrab4"),
      labels)
  )
  
}
