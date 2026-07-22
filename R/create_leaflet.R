create_leaflet <- function(sp, pal, value_range, title) {
  leaflet() %>%
    #addMapPane("data", zIndex = 400) %>%
    addProviderTiles(providers$CartoDB.DarkMatter, 
                     group = "Dark", 
                     layerId = "baseid1") %>%  
                     #options = pathOptions(pane = "data")) %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OSM") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addControl(
      html = sprintf("<div style='font-size:14px; color:black; padding:4px;'>%s</div>", title),
      position = "topright"
    ) %>% 
    addPolygons(
      data = sp,
      #options = leafletOptions(pane = "data"),
      #group = "data",
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
      #overlayGroups = c("data"),
      position = "topright"
    ) %>%
    addLegend(
      pal = pal,
      values = value_range,
      title = "Value",
      position = "bottomright"
      
    ) 
}
