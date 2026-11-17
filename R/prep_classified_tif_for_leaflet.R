# Function for prepping a classified tif layer for leaflet
#
# The trends and conditions and management questions functions each return a
# single layer where pixels that answer the question hold a category value and
# every other pixel is NA. Most questions have one category; greatest_change()
# and forest_change() have two. This pairs that layer with its cell ids and
# turns the matching pixels into clickable polygons.
#
# classified_rast:  single layer raster of category values and NAs
# cell_id_rast:     the cell_id layer that pairs with the source stack
# metric_selection: display name of the metric, used in the popup
# category_labels:  named character vector of labels keyed by category value,
#                   from question_labels()
#
# Returns an sf object, or NULL when no pixels answered the question.

prep_classified_tif_for_leaflet <- function(classified_rast, cell_id_rast,
                                            metric_selection, category_labels) {
  
  # Force the rasters before they are used inside an S4 generic. They are
  # normally lazily evaluated reactives, and terra's dispatch rewraps a Shiny
  # validate() condition raised mid-dispatch into a hard error, which would
  # show the user R internals instead of the message.
  force(classified_rast)
  force(cell_id_rast)
  
  # count the pixels that met the criteria
  matched_count <- terra::global(classified_rast, fun = function(x) sum(!is.na(x)))
  
  if (matched_count[1, 1] == 0) {
    
    message("No pixels met the criteria for this question.")
    
    return(NULL)
    
  }
  
  # pair the classified layer with its cell ids so pixels stay clickable
  r <- c(classified_rast, cell_id_rast)
  
  # create a polygon for reactivity -- pixels that are NA in the classified
  # layer are dropped, so only the matching pixels are drawn
  v <- st_as_sf(as.polygons(r, aggregate = FALSE))
  
  # get the coordinates at the polygon centroid and assign the names
  v_coords <- st_coordinates(st_centroid(v))
  colnames(v_coords) <- c("X", "Y")
  
  # bind the coordinates to the vector shape as columns
  sp <- cbind(v, v_coords)
  
  # set the value column name to value
  colnames(sp)[1] <- "value"
  
  # grab the cell_id column
  cell_id_col <- grep("^cell_id", colnames(sp), value = TRUE)[1]
  
  # set label -- each pixel is described by the category it falls in
  sp <- sp %>%
    mutate(X = round(X, 6),
           Y = round(Y, 6)) %>%
    rename(cell_id = all_of(cell_id_col)) %>%
    mutate(category = unname(category_labels[as.character(value)]),
           popup_text = paste("Latitude: ", Y,
                              "<br>Longitude: ", X,
                              "<br>", metric_selection, ": ", category))
  
  # Return the sp object
  return(sp)
  
}
