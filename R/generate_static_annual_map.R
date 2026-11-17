# Function for building the downloadable static annual map
#
# One panel per year, with the legend sitting outside the map to the right.
# The tm_layout() arguments this used to pass (legend.outside,
# legend.outside.position, main.title.*) are tmap v3 names that tmap v4
# ignores silently, which is why the legend used to land in the bottom-left
# corner. The v4 spelling is tm_pos_out().
#
# raster_layer: single year layer to draw
# island_data:  sf object for the island, outlined over the pixels
# metric:       display name of the metric, used in the title and legend

generate_static_annual_map <- function(raster_layer, island_data, metric) {
  
  if (metric == "NDMI") {
    value_range <- c(-1, 1)
    pal <- "hcl.purple_green"
  } else if (metric == "Tree Cover") {
    value_range <- c(0, 100)
    pal <- "-hcl.greens2"
  } else {
    value_range <- c(0, 1)
    pal <- "-hcl.greens2"
  }
  
  # a very wide island leaves no vertical room for a legend beside the map, so
  # the layout helper puts it underneath for those
  plan <- static_map_layout(island_data, panels = 1)

  if (identical(plan$legend_side, "bottom")) {
    legend_position <- tm_pos_out("center", "bottom")
    legend_orientation <- "landscape"
  } else {
    legend_position <- tm_pos_out("right", "center")
    legend_orientation <- "portrait"
  }

  tmap_mode("plot")

  tm_shape(raster_layer) +
    tm_raster(
      col.scale = tm_scale_continuous(
        values = pal,
        limits = value_range,
        ticks = seq(value_range[[1]], value_range[[2]], length.out = 5)),
      col.legend = tm_legend(
        title = metric,
        # a vertical legend reads naturally high-to-low, a horizontal one
        # should run low-to-high left to right
        reverse = identical(legend_orientation, "portrait"),
        show = TRUE,
        title.size = 1,
        text.size = 0.8,
        frame = FALSE,
        orientation = legend_orientation,
        position = legend_position)
    ) +
    tm_shape(island_data) +
    tm_borders(lwd = 1.2, col = "grey20") +
    tm_title(paste0(names(raster_layer), " ", metric, " on ", island_data$Island),
             size = 1.1,
             fontface = "bold") +
    tm_layout(
      frame = FALSE,
      inner.margins = c(0.02, 0.02, 0.02, 0.02),
      outer.margins = c(0.01, 0.01, 0.01, 0.01)
    )
  
}
