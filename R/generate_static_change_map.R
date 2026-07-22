generate_static_change_map <- function(raster_layer_top, raster_layer_bottom, 
                                title_top, title_bottom, island_data, metric) {
  
  if (metric == "NDMI") {
    value_range <- c(-1, 1) 
    pal <- "hcl.purple_green"
  } else { 
    value_range <- c(0, 1)
    pal <- "-hcl.greens2"
  }
  
  bin_config <- list(
    breaks = c(-Inf, -0.05, -0.01, 0.01, 0.05, Inf),
    labels = c("< -0.05", "-0.05 to -0.01", "-0.01 to 0.01", "0.01 to 0.05", "> 0.05"),
    fill_colors = c("> 0.05" = "olivedrab4", "0.01 to 0.05" = "olivedrab1",
                    "-0.01 to 0.01" = "white", "-0.05 to -0.01" = "magenta1", "< -0.05" = "magenta4"))
  
  raster_top_df <- as.data.frame(raster_layer_top, xy = TRUE)
  colnames(raster_top_df) <- c('x', 'y', 'value')
  
  raster_top_df$binned <- cut(
    raster_top_df$value,
    breaks = bin_config$breaks,
    labels = bin_config$labels,
    include.lowest = TRUE,
    right = TRUE
  )
  
  top <- ggplot() +
    geom_sf(data = island_data, fill = "lightgray") +
    geom_tile(aes(x, y, fill = binned), data = raster_top_df) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = bin_config$fill_colors,
                      na.translate = FALSE, na.value = "lightgray",
                      drop = FALSE, name = metric) +
    xlab("") + ylab("") +
    ggtitle(title_top) +
    theme_cowplot() +
    theme(panel.background = element_rect(fill = "white", color = "black"),
          plot.title = element_text(hjust = 0.5))
  
  
  
  
  
  raster_bottom_df <- as.data.frame(raster_layer_bottom, xy = TRUE)
  colnames(raster_bottom_df) <- c('x', 'y', 'value')
  raster_bottom_df <- raster_bottom_df %>% 
    mutate(binned = factor(ifelse(value < 0, "negative", "positive"), 
                            levels = c("positive", "negative")))
  
  bottom <- ggplot() +
    geom_sf(data = island_data, fill = "lightgray") +
    geom_tile(aes(x, y, fill = binned), data = raster_bottom_df) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = c("positive" = "olivedrab1", "negative" = "magenta1"),
                      na.value = "grey50", na.translate = FALSE,
                      labels = c("positive" = "Positive", "negative" = "Negative"),
                      name = metric) +
    xlab("") + ylab("") +
    ggtitle(title_bottom) +
    theme_cowplot() +
    theme(panel.background = element_rect(fill = "white", color = "black"),
          plot.title = element_text(hjust = 0.5))
  
  plot_align <- align_plots(top, bottom,
                            align = "v", axis = "lr")
  
  combined_plot <- plot_grid(plot_align[[1]], plot_align[[2]], ncol = 1)
  
  return(combined_plot)
  
}
