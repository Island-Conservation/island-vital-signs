# Function for choosing a canvas size that matches an island's shape
#
# Islands in this app range from very wide and thin (Anacapa) to tall and
# narrow (Guadalupe). A fixed canvas leaves most of the image blank for one
# shape and squashes the other until the axis labels collide, so downloaded
# maps work out their own size from the island's bounding box.
#
# island_data: sf object for the island being mapped
# panels:      how many map panels the figure holds (1 or 2)
#
# Returns a list with:
#   width, height  canvas size in inches
#   layout         "vertical" or "horizontal", how 2 panels are arranged
#   aspect         width/height of the island itself, latitude corrected

static_map_layout <- function(island_data, panels = 1) {
  
  bb <- st_bbox(island_data)
  
  # a degree of longitude is shorter than a degree of latitude away from the
  # equator, so correct before comparing the two spans
  mid_lat <- mean(c(bb[["ymin"]], bb[["ymax"]]))
  span_x <- (bb[["xmax"]] - bb[["xmin"]]) * cos(mid_lat * pi / 180)
  span_y <- bb[["ymax"]] - bb[["ymin"]]
  
  aspect <- if (span_y > 0) span_x / span_y else 1
  
  # size one map panel so its longest side is about 6 inches, but never let the
  # short side collapse to the point where labels have nowhere to go
  if (aspect >= 1) {
    panel_w <- 6
    panel_h <- max(6 / aspect, 1.8)
  } else {
    panel_h <- 6
    panel_w <- max(6 * aspect, 1.8)
  }
  
  legend_w <- 2.4 # room for the legend to the right of the map
  title_h <- 0.9  # room for the overall title and the panel titles
  
  if (panels < 2) {

    layout <- "single"

    # a very wide island gives a panel too short for a legend standing beside
    # it, so that legend goes underneath the map rather than being squeezed
    legend_side <- if (aspect >= 2.5) "bottom" else "right"

    if (legend_side == "bottom") {
      width <- panel_w
      height <- panel_h + title_h + 1
    } else {
      width <- panel_w + legend_w
      height <- max(panel_h + title_h, 3.6)
    }

  } else if (aspect >= 1.2) {

    # wide island: stack the panels so each one stays wide
    layout <- "vertical"
    legend_side <- "right"
    width <- panel_w + legend_w
    height <- 2 * panel_h + title_h + 0.6

  } else {

    # tall island: sit the panels side by side rather than stacking them
    layout <- "horizontal"
    legend_side <- "right"
    width <- 2 * panel_w + legend_w
    height <- panel_h + title_h

  }

  # keep the output within sizes that are sensible to open and print
  list(width = min(max(width, 5), 14),
       height = min(max(height, 3.5), 14),
       layout = layout,
       legend_side = legend_side,
       aspect = aspect)
  
}
