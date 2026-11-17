create_leaflet_splitmap <- function(sp_left, title_left, sp_right, title_right, pal, value_range,
                                    legend_title = "Value") {
  leaflet() %>%
    addMapPane("left", zIndex = 400) %>%
    addMapPane("right", zIndex = 400) %>%
    # Three basemaps per side. The side-by-side control is handed all of them
    # and clips whichever pair the layers control currently has on the map, so
    # the divider keeps working when the basemap is switched. Dark is added
    # last so it sits on top as the default.
    addProviderTiles(providers$Esri.WorldImagery,
                     group = "Satellite",
                     layerId = "sat_right",
                     options = pathOptions(pane = "right")) %>%
    addProviderTiles(providers$Esri.WorldImagery,
                     group = "Satellite",
                     layerId = "sat_left",
                     options = pathOptions(pane = "left")) %>%
    add_carto_basemap(group = "Light",
                      layerId = "light_right",
                      options = pathOptions(pane = "right"),
                      style = "light") %>%
    add_carto_basemap(group = "Light",
                      layerId = "light_left",
                      options = pathOptions(pane = "left"),
                      style = "light") %>%
    add_carto_basemap(group = "Dark",
                      layerId = "dark_right",
                      options = pathOptions(pane = "right")) %>%
    add_carto_basemap(group = "Dark",
                      layerId = "dark_left",
                      options = pathOptions(pane = "left")) %>%
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
      baseGroups = c("Dark", "Light", "Satellite"),
      overlayGroups = c("left", "right"),
      position = "topright"
    ) %>%
    addLegend_decreasing(
      pal = pal,
      values = value_range,
      title = legend_title,
      position = "bottomright",
      decreasing = TRUE
    ) %>%
    add_sidebyside_multi(layerId = "sidecontrols",
                         leftIds = c("dark_left", "light_left", "sat_left"),
                         rightIds = c("dark_right", "light_right", "sat_right")) %>%
    addControl(
      html = sprintf("<div style='font-size:14px; color:black; padding:4px;'>%s</div>", title_left),
      position = "topleft"
    ) %>% 
    
    addControl(
      html = sprintf("<div style='font-size:14px; color:black; padding:4px;'>%s</div>", title_right),
      position = "topright"
    )
}
