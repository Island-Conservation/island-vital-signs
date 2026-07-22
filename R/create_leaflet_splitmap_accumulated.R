create_leaflet_splitmap_accumulated <- function(sp_left, title_left, sp_right, title_right, pal, value_range) {
  
  # Check if sp_right is valid
  valid_sp_right <- !is.null(sp_right) &&
    inherits(sp_right, "sf") &&
    nrow(sp_right) > 0 &&
    !all(st_is_empty(sp_right))
  
  # If not valid, create a dummy transparent layer
  if (valid_sp_right == TRUE) {
    
    leaflet() %>%
      addMapPane("left", zIndex = 400) %>%
      addMapPane("right", zIndex = 400) %>%
      addProviderTiles(providers$CartoDB.DarkMatter, 
                       group = "Dark", 
                       layerId = "baseid1", 
                       options = pathOptions(pane = "right")) %>%
      addProviderTiles(providers$CartoDB.DarkMatter, 
                       group = "Dark", 
                       layerId = "baseid2", 
                       options = pathOptions(pane = "left")) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OSM") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addPolygons(
        data = sp_left,
        options = leafletOptions(pane = "left"),
        group = "left",
        fillColor = ~pal(value),
        fillOpacity = 1,
        color = ~pal(value),
        weight = 1,
        opacity = 1,
        highlight = highlightOptions(
          weight = 2,
          color = "purple",
          fillOpacity = 1,
          bringToFront = TRUE
        ),
        layerId = ~cell_id,
        popup = ~popup_text,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto",
          html = TRUE,
          opacity = 0.2)) %>%
      addPolygons(
        data = sp_right,
        options = leafletOptions(pane = "right"),
        group = "right",
        fillColor = ~pal(value),
        fillOpacity = 1,
        color = ~pal(value),
        weight = 1,
        opacity = 1,
        highlight = highlightOptions(
          weight = 2,
          color = "purple",
          fillOpacity = 1,
          bringToFront = TRUE
        ),
        layerId = ~cell_id,
        popup = ~popup_text,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto",
          html = TRUE,
          opacity = 0.2)) %>%
      addLayersControl(
        baseGroups = c("Dark", "Light", "OSM", "Satellite"),
        overlayGroups = c("left", "right"),
        position = "topright"
      ) %>%
      addLegend_decreasing(
        pal = pal,
        values = value_range,
        title = "Value",
        position = "bottomright",
        decreasing = TRUE
        
      ) %>%
      addSidebyside(layerId = "sidecontrols",
                    rightId = "baseid1",
                    leftId = "baseid2") %>% 
      addControl(
        html = sprintf("<div style='font-size:14px; color:black; padding:4px;'>%s</div>", title_left),
        position = "topleft"
      ) %>% 
      
      addControl(
        html = sprintf("<div style='font-size:14px; color:black; padding:4px;'>%s</div>", title_right),
        position = "topright"
      )
    
  } else {
    
    sp_right <- st_sf(
      value = numeric(0),
      cell_id = integer(0),
      popup_text = character(0),
      geometry = st_sfc(),
      crs = st_crs(sp_left)
    )
  
  leaflet() %>%
    addMapPane("left", zIndex = 400) %>%
    addMapPane("right", zIndex = 400) %>%
    addProviderTiles(providers$CartoDB.DarkMatter, 
                     group = "Dark", 
                     layerId = "baseid1", 
                     options = pathOptions(pane = "right")) %>%
    addProviderTiles(providers$CartoDB.DarkMatter, 
                     group = "Dark", 
                     layerId = "baseid2", 
                     options = pathOptions(pane = "left")) %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OSM") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addPolygons(
      data = sp_left,
      options = leafletOptions(pane = "left"),
      group = "left",
      fillColor = ~pal(value),
      fillOpacity = 1,
      color = ~pal(value),
      weight = 1,
      opacity = 1,
      highlight = highlightOptions(
        weight = 2,
        color = "purple",
        fillOpacity = 1,
        bringToFront = TRUE
      ),
      layerId = ~cell_id,
      popup = ~popup_text,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto",
        html = TRUE,
        opacity = 0.2)) %>%
    addPolygons(
      data = sp_right,
      options = leafletOptions(pane = "right"),
      group = "right",
      fillColor = "transparent",
      fillOpacity = 1,
      color = "transparent",
      weight = 1,
      opacity = 1,
      highlight = highlightOptions(
        weight = 2,
        color = "purple",
        fillOpacity = 1,
        bringToFront = TRUE
      ),
      layerId = ~cell_id,
      popup = ~popup_text,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto",
        html = TRUE,
        opacity = 0.2)) %>%
    addLayersControl(
      baseGroups = c("Dark", "Light", "OSM", "Satellite"),
      overlayGroups = c("left", "right"),
      position = "topright"
    ) %>%
    addLegend_decreasing(
      pal = pal,
      values = value_range,
      title = "Value",
      position = "bottomright",
      decreasing = TRUE
      
    ) %>%
    addSidebyside(layerId = "sidecontrols",
                  rightId = "baseid1",
                  leftId = "baseid2") %>% 
    addControl(
      html = sprintf("<div style='font-size:14px; color:black; padding:4px;'>%s</div>", title_left),
      position = "topleft"
    ) %>% 
    
    addControl(
      html = sprintf("<div style='font-size:14px; color:black; padding:4px;'>%s</div>", title_right),
      position = "topright"
    )
  
  }
  
}
