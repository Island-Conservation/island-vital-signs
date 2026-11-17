# Helpers for turning a question spec's categories into the shapes that
# prep_classified_tif_for_leaflet() and create_leaflet_classified() expect.
#
# A spec's categories are keyed by the value the classifying function writes
# into the raster, e.g. greatest_change() writes 1 for the greatest increase
# and 2 for the greatest decrease:
#
#   categories = list("1" = list(label = "Greatest increase", color = palm),
#                     "2" = list(label = "Greatest decrease", color = coral))
#
# A label may contain the token {hpdi}, which is replaced with the probability
# level the user selected. Passing hpdi = NULL leaves the token alone, which
# only matters for questions that never read an HPDI layer.

# fill the probability level into a label, if it asks for one
fill_hpdi <- function(label, hpdi) {
  
  if (is.null(hpdi)) return(label)
  
  gsub("{hpdi}", hpdi, label, fixed = TRUE)
  
}

# named character vector of labels, keyed by raster value -- used for popups
question_labels <- function(spec, hpdi = NULL) {
  
  setNames(vapply(spec$categories,
                  function(x) fill_hpdi(x$label, hpdi),
                  character(1)),
           names(spec$categories))
  
}

# data frame of value / label / colour -- used for fill colours and the legend
question_categories <- function(spec, hpdi = NULL) {
  
  data.frame(
    value = as.numeric(names(spec$categories)),
    label = vapply(spec$categories,
                   function(x) fill_hpdi(x$label, hpdi),
                   character(1)),
    color = vapply(spec$categories, function(x) x$color, character(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  
}
