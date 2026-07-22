# Function for prepping a tif layer for leaflet

prep_change_tif_for_leaflet <- function(r_stack, selected_layer, selected_time, metric_selection) {
  
  # subset the selected layer
  r <- r_stack[[which(selected_layer)]]
  
  if (any(grepl("cell_id", names(r)))) {
    
    # create a polygon for reactivity
    v <- st_as_sf(as.polygons(r, aggregate = FALSE))
    
    # get the coordinates at the polygon centroid
    v_coords <- st_coordinates(st_centroid(v))
    
    # bind the coordinates to the vector shape as columns
    sp <- cbind(v, v_coords)
    
    # set the value column name to value
    colnames(sp)[1] <- "value"
    
    # grab the cell_id column
    cell_id_col <- grep("^cell_id", colnames(sp), value = TRUE)[1]
    
    # set label
    sp <- sp %>%
      mutate(X = round(X, 6),
             Y = round(Y, 6)) %>% 
      mutate(popup_text = paste("Latitude: ", Y, 
                                "<br>Longitude: ", X, 
                                "<br>Cell Id: ", round(value, 6)))
    
    # Return the sp object
    return(sp)
    
  } else {
    
    # subset the selected layer
    r <- r_stack[[which(selected_layer)]]
    
    
    # print(r)
    # 
    # 
    # terra::global(r, fun = function(x) sum(!is.na(x)))
    
    
    
    no_na_count <- terra::global(r, fun = function(x) sum(!is.na(x)))
    
    c <- r_stack[[paste0("cell_id_", selected_time)]]
    
    r <- c(r, c)
    
    # create a polygon for reactivity
    v <- st_as_sf(as.polygons(r, aggregate = FALSE))
    
    
    # # check if v has any features
    # if (nrow(v) == 0) {
    #   stop("No features found in raster layer — possibly all NA values.")
    # }
    
    
    # get the coordinates at the polygon centroid and assign the names
    v_coords <- st_coordinates(st_centroid(v))
    colnames(v_coords) <- c("X", "Y")
    
    # bind the coordinates to the vector shape as columns
    sp <- cbind(v, v_coords)
    
    # set the value column name to value
    colnames(sp)[1] <- "value"
    
    # grab the cell_id column
    cell_id_col <- grep("^cell_id", colnames(sp), value = TRUE)[1]
    
    # print(head(sp))
    # print(colnames(sp))
    # 
    # 
    # if (!all(c("X", "Y") %in% colnames(sp))) {
    #   stop("X and Y coordinates not found in spatial object.")
    # }
    
    if (no_na_count[1, 1] == 0) {
      
      message("Raster contains only NA values.")
      
      sp <- NULL
      
      return(sp)
      
      stop()
      
    } else {
    
    # set label
    sp <- sp %>%
      mutate(X = round(X, 6),
             Y = round(Y, 6)) %>% 
      rename(cell_id = all_of(cell_id_col)) %>% 
      mutate(popup_text = paste("Latitude: ", Y, 
                                "<br>Longitude: ", X, 
                                "<br>Cell Id: ", cell_id,
                                "<br>", metric_selection, " Trend: ", round(value, 6)))
    
    # Return the sp object
    return(sp)
    
    }
    
  }
  
}


