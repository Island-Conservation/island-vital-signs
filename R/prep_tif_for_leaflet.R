# Function for prepping a tif layer for leaflet

prep_tif_for_leaflet <- function(r_stack, selected_layer, metric_selection) {
  
  # subset the selected layer
  r <- r_stack[[which(selected_layer)]]
  
  if (names(r) == "cell_id") {
  
  # create a polygon for reactivity
  v <- st_as_sf(as.polygons(r, aggregate = FALSE))
  
  # get the coordinates at the polygon centroid
  v_coords <- st_coordinates(st_centroid(v))
  
  # bind the coordinates to the vector shape as columns
  sp <- cbind(v, v_coords)
  
  # create an id column
  #sp$id <- 1:nrow(sp)
  
  # set the value column name to value
  colnames(sp)[1] <- "value"
  
  # set label
  sp <- sp %>%
    mutate(X = round(X, 6),
           Y = round(Y, 6)) %>% 
    mutate(popup_text = paste("Latitude: ", Y, 
                              "<br>Longitude: ", X))
  
  # Return the sp object
  return(sp)
  
  } else {
    
    # subset the selected layer
    r <- r_stack[[which(selected_layer)]]
    
    c <- r_stack[["cell_id"]]
    
    r <- c(r, c)
    
    # create a polygon for reactivity
    v <- st_as_sf(as.polygons(r, aggregate = FALSE))
    
    # get the coordinates at the polygon centroid
    v_coords <- st_coordinates(st_centroid(v))
    
    # bind the coordinates to the vector shape as columns
    sp <- cbind(v, v_coords)
    
    # create an id column
    #sp$id <- 1:nrow(sp)
    
    # set the value column name to value
    colnames(sp)[1] <- "value"
    
    # set label
    sp <- sp %>%
      mutate(X = round(X, 6),
             Y = round(Y, 6)) %>% 
      mutate(popup_text = paste("Latitude: ", Y, 
                                "<br>Longitude: ", X, 
                                "<br>", metric_selection, ": ", round(value, 6)))
    
    # Return the sp object
    return(sp)
    
  }
}


