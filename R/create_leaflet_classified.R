# Function for mapping a classified layer on a single leaflet
#
# Pixels are coloured by the category they fall in, and the legend carries one
# entry per category. Only the categories actually present in the data are
# drawn, so the legend never promises a class the map does not contain.
#
# sp:         sf object from prep_classified_tif_for_leaflet()
# categories: data frame of value / label / color, from question_categories()
# title:      text shown in the top corner of the map
# bounds:     named lng1/lat1/lng2/lat2 window to open on, from
#             island_view_bounds(). NULL leaves leaflet to frame the drawn
#             pixels, which on a question that returns a handful of them
#             means opening at street level with no island in sight.

create_leaflet_classified <- function(sp, categories, title, bounds = NULL) {
  
  # Force sp before it is used inside an S4 generic. sp is normally a lazily
  # evaluated reactive, and terra's dispatch rewraps a Shiny validate()
  # condition raised mid-dispatch into a hard error, which would show the
  # user R internals instead of the message.
  force(sp)
  
  # keep only the categories that this island actually produced
  present <- categories[categories$value %in% unique(sp$value), , drop = FALSE]
  
  pal <- colorFactor(palette = present$color,
                     levels = present$value,
                     na.color = "transparent")
  
  map <- leaflet() %>%
    add_carto_basemap(group = "Dark",
                      layerId = "baseid1") %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OSM") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addControl(
      html = sprintf("<div style='font-size:14px; color:black; padding:4px;'>%s</div>", title),
      position = "topleft"
    ) %>%
    addPolygons(
      data = sp,
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
      baseGroups = c("Dark", "OSM", "Satellite"),
      position = "topright"
    ) %>%
    addLegend(
      colors = present$color,
      labels = present$label,
      title = "Pixels Shown",
      position = "bottomright",
      opacity = 1
    )
  
  if (is.null(bounds)) return(map)
  
  map %>%
    fitBounds(
      lng1 = bounds[["lng1"]],
      lat1 = bounds[["lat1"]],
      lng2 = bounds[["lng2"]],
      lat2 = bounds[["lat2"]]
    )
  
}
