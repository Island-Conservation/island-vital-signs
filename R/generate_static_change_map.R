# Function for building the downloadable static change map
#
# Two panels: the modeled change for every pixel, and the direction of change
# for the pixels that are significant at the chosen probability level. The
# panels are stacked for a wide island and placed side by side for a tall one,
# and the number of axis breaks is held down so coordinate labels stay legible
# on narrow panels.
#
# raster_layer_top:    posterior mean layer for the selected time period
# raster_layer_bottom: the same layer masked to the chosen probability level
# title_top:           panel title for the left/top map
# title_bottom:        panel title for the right/bottom map
# island_data:         sf object for the island, drawn underneath the pixels
# metric:              display name of the metric, used in the figure title

generate_static_change_map <- function(raster_layer_top, raster_layer_bottom,
                                       title_top, title_bottom, island_data, metric) {
  
  # bins follow the data, so the same five classes work for an accumulated
  # index, a per-year rate and tree cover in percentage points alike
  bin_config <- diverging_bins(values(raster_layer_top)[, 1])
  
  plan <- static_map_layout(island_data, panels = 2)
  
  # A narrow panel cannot carry many coordinate labels without them running
  # into each other, which is what made these downloads unreadable. Ask for
  # few breaks on whichever axis is short, and let the long axis have more.
  if (plan$layout == "horizontal") {
    n_x <- 3; n_y <- 5
  } else {
    n_x <- 5; n_y <- 3
  }

  # only tilt the longitude labels when the panel is too narrow to lay them
  # flat; on a wide panel they read better horizontally
  x_angle <- if (plan$layout == "horizontal") 30 else 0
  x_hjust <- if (x_angle > 0) 1 else 0.5

  # shared look for both panels
  map_theme <- theme_cowplot(font_size = 11) +
    theme(
      panel.background = element_rect(fill = "white", color = "black"),
      plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
      axis.text = element_text(size = 8),
      axis.text.x = element_text(angle = x_angle, hjust = x_hjust),
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.4, "cm"),
      plot.margin = margin(4, 4, 4, 4)
    )
  
  build_panel <- function(df, fill_scale, title) {
    ggplot() +
      geom_sf(data = island_data, fill = "lightgray", colour = "grey30", linewidth = 0.2) +
      geom_tile(aes(x, y, fill = binned), data = df) +
      scale_x_continuous(expand = c(0, 0), breaks = function(x) pretty(x, n = n_x)) +
      scale_y_continuous(expand = c(0, 0), breaks = function(x) pretty(x, n = n_y)) +
      fill_scale +
      xlab("") + ylab("") +
      ggtitle(title) +
      map_theme
  }
  
  # TOP / LEFT -- modeled change for every pixel, binned
  raster_top_df <- as.data.frame(raster_layer_top, xy = TRUE)
  colnames(raster_top_df) <- c("x", "y", "value")
  raster_top_df$binned <- cut(raster_top_df$value,
                              breaks = bin_config$breaks,
                              labels = bin_config$labels,
                              include.lowest = TRUE,
                              right = TRUE)
  
  top <- build_panel(
    raster_top_df,
    scale_fill_manual(values = bin_config$fill_colors,
                      na.translate = FALSE, na.value = "lightgray",
                      drop = FALSE,
                      name = paste0(metric, " change")),
    title_top)
  
  # BOTTOM / RIGHT -- direction of change for the significant pixels
  raster_bottom_df <- as.data.frame(raster_layer_bottom, xy = TRUE)
  colnames(raster_bottom_df) <- c("x", "y", "value")
  raster_bottom_df <- raster_bottom_df %>%
    mutate(binned = factor(ifelse(value < 0, "negative", "positive"),
                           levels = c("positive", "negative")))
  
  bottom <- build_panel(
    raster_bottom_df,
    scale_fill_manual(values = c("positive" = "olivedrab1", "negative" = "magenta1"),
                      na.value = "grey50", na.translate = FALSE,
                      labels = c("positive" = "Positive", "negative" = "Negative"),
                      name = "Direction"),
    title_bottom)
  
  # ARRANGE
  if (plan$layout == "horizontal") {
    
    # align on all four edges so the two panels and their titles line up, even
    # though their legends are different widths
    panels <- plot_grid(top, bottom, nrow = 1, align = "hv", axis = "tblr")
    
  } else {
    
    aligned <- align_plots(top, bottom, align = "v", axis = "lr")
    panels <- plot_grid(aligned[[1]], aligned[[2]], ncol = 1)
    
  }
  
  # one title for the whole figure, so the island is named once
  figure_title <- ggdraw() +
    draw_label(paste0(metric, " on ", island_data$Island),
               fontface = "bold", size = 13, x = 0.5, hjust = 0.5)
  
  combined_plot <- plot_grid(figure_title, panels,
                             ncol = 1,
                             rel_heights = c(0.07, 1))
  
  # the download handler asks the plot how big it wants to be
  attr(combined_plot, "download_size") <- c(plan$width, plan$height)
  
  return(combined_plot)
  
}
