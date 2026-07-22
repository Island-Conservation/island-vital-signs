generate_static_annual_map <- function(raster_layer, island_data, metric) {
  
  if (metric == "NDMI") {
    value_range <- c(-1, 1) 
    pal <- "hcl.purple_green"
  } else { 
    value_range <- c(0, 1)
    pal <- "-hcl.greens2"
  }
  
  tmap_mode("plot")
  
  tm_shape(raster_layer) + 
    tm_raster(
      #title = metric,
      col.scale = tm_scale_continuous(values = pal,
                                      ticks = seq(value_range[[1]], value_range[[2]], length.out = 4)),
      col.legend = tm_legend(title = metric, 
                             reverse = TRUE,
                             show = TRUE),
    ) +
    tm_shape(island_data) +
    tm_borders(lwd = 2) +
    tm_title(paste0(names(raster_layer), " ", metric, " on ", island_data$Island)) +
    tm_layout(
      legend.outside = TRUE,
      legend.outside.position = "right",
      outer.margins = c(0, 0, 0, 0), # top, right, bottom, left
      #main.title = paste0(names(raster_layer), " ", metric, " on ", island_data$Island),
      main.title.size = 1.5,
      main.title.fontface = "bold",
      frame = FALSE,
      inner.margins = 0.04,
      legend.title.size = 1.2,
      legend.text.size = 0.8
    ) 
}
