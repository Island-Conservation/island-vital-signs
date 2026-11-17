# How much ground a classified question layer covers
#
# The trends and conditions and management questions functions each return a
# layer where the pixels that answer the question hold a category value and
# every other pixel is NA. A pixel count alone does not tell the reader how
# much of the island that is, which is the question a manager actually asks of
# these maps.
#
# Area is measured rather than assumed. The rasters are stored in lon/lat
# (EPSG:4326) at a nominal 112 m, so a cell covers less ground the further the
# island sits from the equator, and terra::cellSize() accounts for that.
#
# classified_rast: single layer raster of category values and NAs
# cell_id_rast:    the cell_id layer that pairs with the source stack. Its
#                  non-NA cells are the extent this island was modeled over,
#                  which is what the percentages are taken against -- the
#                  bounding box around an island picks up ocean that was never
#                  modeled, and counting that would shrink every percentage.
#
# Returns a list of:
#   modeled_km2: the modeled extent, in square kilometres
#   by_value:    one row per category present, with the category value, its
#                pixel count, its area and its share of the modeled extent

classified_area <- function(classified_rast, cell_id_rast) {

  cell_km2 <- terra::cellSize(classified_rast, unit = "km")

  modeled_km2 <- unname(terra::global(
    terra::mask(cell_km2, cell_id_rast),
    "sum",
    na.rm = TRUE
  )[1, 1])

  # zonal() sums the cell areas within each category and skips the NA pixels,
  # so only the categories this island actually produced come back
  areas <- terra::zonal(cell_km2, classified_rast, fun = "sum", na.rm = TRUE)
  names(areas) <- c("value", "area_km2")

  counts <- terra::freq(classified_rast)
  areas$pixels <- counts$count[match(areas$value, counts$value)]

  areas$percent <- if (isTRUE(is.finite(modeled_km2)) && modeled_km2 > 0) {
    100 * areas$area_km2 / modeled_km2
  } else {
    NA_real_
  }

  list(
    modeled_km2 = modeled_km2,
    by_value = areas[order(areas$value), , drop = FALSE]
  )

}

# An area written the way a reader would say it. Islands in this catalogue run
# from 0.2 to 627 km², and a single category on a small island can be a
# fraction of that, so the number of decimals follows the size rather than
# being fixed -- 0.04 km² would otherwise round away to 0.0.

format_km2 <- function(x) {

  if (length(x) == 0 || is.na(x) || !is.finite(x)) return("not available")

  digits <- if (x >= 100) 0 else if (x >= 10) 1 else 2

  # a single pixel is about 0.01 km², so a real area never rounds to nothing
  # -- but say so rather than print a bare 0.00 if one ever does
  if (x > 0 && round(x, digits) == 0) return("<0.01 km²")

  paste0(formatC(x, format = "f", digits = digits, big.mark = ","), " km²")

}
