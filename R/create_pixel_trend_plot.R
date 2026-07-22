# # Function for creating the cell timeseries plots
# 
# create_pixel_trend_plot <- function(intervention_data, 
#                                     selected_metric,
#                                     min_year,
#                                     max_year,
#                                     cell_id_filter, 
#                                     predicted_df, 
#                                     observed_df) {
#   
#   # base annotation height on max range of data
#   if(selected_metric == "tcc") {
#     y_height = 104
#   } else {
#     y_height = 1.04
#   }
#   
#   # filter data to the selected cell
#   filtered_predicted_data <- predicted_df %>%
#     #mutate(cell_id = as.numeric(cell_id)) %>% 
#     mutate(cell_id = as.numeric(as.character(cell_id))) %>%
#     filter(cell_id == cell_id_filter)
#   
#   filtered_observed_data <- observed_df %>%
#     filter(cell_id == cell_id_filter)
#   
#   # split the data into the smoothed and annual varitions
#   pixel_data_a <- filtered_predicted_data %>% 
#     filter(data_type == "smoothed")
#   
#   pixel_data_b <- filtered_predicted_data %>% 
#     filter(data_type == "annual.variation")
#   
#   # grab the observed data points
#   observation_points <- filtered_observed_data %>% 
#     dplyr::select(year, posterior.mean = all_of(unname(selected_metric))) %>% 
#     rename(time = year)
#   
#   if(length(intervention_data$IntYr) == 2 ) {
#     
#     # pull the eradication specifications
#     erad1 <- intervention_data$IntYr[[1]]
#     erad2 <- intervention_data$IntYr[[2]]
#     erad1.sp <- paste(intervention_data$Genus[[1]], intervention_data$Species[[1]], sep = "\n")
#     erad2.sp <- paste(intervention_data$Genus[[2]], intervention_data$Species[[2]], sep = "\n")
#     
#     # create the smoothed plot
#     p.a <- pixel_data_a %>% 
#       dplyr::mutate(time = time + min_year) %>%
#       rename(lwr = paste0("l", choice, ".", p * 100),
#              upr = paste0("u", choice, ".", p * 100)) %>% 
#       ggplot(., aes(x = time, y = posterior.mean)) +
#       geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.33) +
#       geom_line(linewidth = 1) +
#       scale_x_continuous(limits = c(min_year, max_year)) +
#       (if (selected_metric %in% c("ndmi")) {                              #adjust y axis based on range of veg index   
#         scale_y_continuous(limits = c(-1, 1.23), breaks = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1), expand = c(0, 0))
#       } else if (selected_metric %in% c("tcc")) {
#         scale_y_continuous(limits = c(0, 123), breaks = c(0, 25, 50, 75, 100), expand = c(0,0))
#       } else {
#         scale_y_continuous(limits = c(0, 1.23), breaks = c(0, 0.25, 0.5, 0.75, 1), expand = c(0, 0))
#       }) +
#       geom_vline(xintercept = {erad1 - 1}, color = erad_line_color, linewidth = 1, linetype = "dashed") + #add a line for first intervention
#       geom_vline(xintercept = {erad2 - 1}, color = erad_line_color, linewidth = 1, linetype = "dashed") + #add a line for second intervention
#       annotate("text", x = {erad1 - 0.8}, y = y_height, label = {erad1.sp}, color = erad_line_color, hjust = 0, fontface = 'italic') + #add annotation for first line
#       annotate("text", x = {erad2 - 0.8}, y = y_height, label = {erad2.sp}, color = erad_line_color, hjust = 0, fontface = 'italic') + #add annotation for second line
#       geom_point(data = observation_points) +
#       scale_fill_identity() +
#       scale_color_identity() +
#       labs(x = "Year",
#            y = paste0("Predicted ", toupper(selected_metric), ": ", cell_id_filter, "\n")) +
#       theme_cowplot()
#     
#     ## generate the plot
#     p.b <- pixel_data_b %>% 
#       dplyr::mutate(time = time + min_year) %>%
#       rename(lwr = paste0("l", choice, ".", p * 100),
#              upr = paste0("u", choice, ".", p * 100)) %>% 
#       ggplot(., aes(x = time, y = posterior.mean)) +
#       geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.33) +
#       geom_line(linewidth = 1) +
#       scale_x_continuous(limits = c(min_year, max_year)) +
#       (if (selected_metric %in% c("ndmi")) {                              #adjust y axis based on range of veg index   
#         scale_y_continuous(limits = c(-1, 1.23), breaks = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1), expand = c(0, 0))
#       } else if (selected_metric %in% c("tcc")) {
#         scale_y_continuous(limits = c(0, 123), breaks = c(0, 25, 50, 75, 100), expand = c(0,0))
#       } else {
#         scale_y_continuous(limits = c(0, 1.23), breaks = c(0, 0.25, 0.5, 0.75, 1), expand = c(0, 0))
#       }) +
#       geom_vline(xintercept = {erad1 - 1}, color = erad_line_color, linewidth = 1, linetype = "dashed") + #add a line for first intervention
#       geom_vline(xintercept = {erad2 - 1}, color = erad_line_color, linewidth = 1, linetype = "dashed") + #add a line for second intervention
#       annotate("text", x = {erad1 - 0.8}, y = y_height, label = {erad1.sp}, color = erad_line_color, hjust = 0, fontface = 'italic') + #add annotation for first line
#       annotate("text", x = {erad2 - 0.8}, y = y_height, label = {erad2.sp}, color = erad_line_color, hjust = 0, fontface = 'italic') + #add annotation for second line
#       geom_point(data = observation_points) +
#       scale_fill_identity() +
#       scale_color_identity() +
#       labs(x = "Year",
#            y = paste0("Predicted ", toupper(selected_metric), "\nwith annual variation: ", cell_id_filter)) +
#       theme_cowplot()
#     
#     p_panel <- plot_grid(p.a, p.b,            #identify panels to include; exclude temporal lag
#                          labels = c('a', 'b'),      #label
#                          hjust = -9)
#     
#     # view figure
#     print(p_panel)
#     
#   } else if (length(intervention_data$IntYr) == 1) {
#     
#     # pull the eradication specifications
#     erad1 <- intervention_data$IntYr[[1]]
#     erad1.sp <- paste(intervention_data$Genus[[1]], intervention_data$Species[[1]], sep = "\n")
#     
#     # create the smoothed plot
#     p.a <- pixel_data_a %>% 
#       dplyr::mutate(time = time + min_year) %>%
#       rename(lwr = paste0("l", choice, ".", p * 100),
#              upr = paste0("u", choice, ".", p * 100)) %>% 
#       ggplot(., aes(x = time, y = posterior.mean)) +
#       geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.33) +
#       geom_line(linewidth = 1) +
#       scale_x_continuous(limits = c(min_year, max_year)) +
#       (if (selected_metric %in% c("ndmi")) {                              #adjust y axis based on range of veg index   
#         scale_y_continuous(limits = c(-1, 1.23), breaks = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1), expand = c(0, 0))
#       } else if (selected_metric %in% c("tcc")) {
#         scale_y_continuous(limits = c(0, 123), breaks = c(0, 25, 50, 75, 100), expand = c(0,0))
#       } else {
#         scale_y_continuous(limits = c(0, 1.23), breaks = c(0, 0.25, 0.5, 0.75, 1), expand = c(0, 0))
#       }) +
#       geom_vline(xintercept = {erad1 - 1}, color = erad_line_color, linewidth = 1, linetype = "dashed") + #add a line for first intervention
#       annotate("text", x = {erad1 - 0.8}, y = y_height, label = {erad1.sp}, color = erad_line_color, hjust = 0, fontface = 'italic') + #add annotation for first line
#       geom_point(data = observation_points) +
#       scale_fill_identity() +
#       scale_color_identity() +
#       labs(x = "Year",
#            y = paste0("Predicted ", toupper(selected_metric), ": ", cell_id_filter, "\n")) +
#       theme_cowplot()
#     
#     ## generate the plot
#     p.b <- pixel_data_b %>% 
#       dplyr::mutate(time = time + min_year) %>%
#       rename(lwr = paste0("l", choice, ".", p * 100),
#              upr = paste0("u", choice, ".", p * 100)) %>% 
#       ggplot(., aes(x = time, y = posterior.mean)) +
#       geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.33) +
#       geom_line(linewidth = 1) +
#       scale_x_continuous(limits = c(min_year, max_year)) +
#       (if (selected_metric %in% c("ndmi")) {                              #adjust y axis based on range of veg index   
#         scale_y_continuous(limits = c(-1, 1.23), breaks = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1), expand = c(0, 0))
#       } else if (selected_metric %in% c("tcc")) {
#         scale_y_continuous(limits = c(0, 123), breaks = c(0, 25, 50, 75, 100), expand = c(0,0))
#       } else {
#         scale_y_continuous(limits = c(0, 1.23), breaks = c(0, 0.25, 0.5, 0.75, 1), expand = c(0, 0))
#       }) +
#       geom_vline(xintercept = {erad1 - 1}, color = erad_line_color, linewidth = 1, linetype = "dashed") + #add a line for first intervention
#       annotate("text", x = {erad1 - 0.8}, y = y_height, label = {erad1.sp}, color = erad_line_color, hjust = 0, fontface = 'italic') + #add annotation for first line
#       geom_point(data = observation_points) +
#       scale_fill_identity() +
#       scale_color_identity() +
#       labs(x = "Year",
#            y = paste0("Predicted ", toupper(selected_metric), "\nwith annual variation: ", cell_id_filter)) +
#       theme_cowplot()
#     
#     p_panel <- plot_grid(p.a, p.b,            #identify panels to include; exclude temporal lag
#                          labels = c('a', 'b'),      #label
#                          hjust = -9)
#     
#     # view figure
#     print(p_panel)
#     
#   } else if (length(intervention_data$IntYr) == 0) {
#     
#     # create the smoothed plot
#     p.a <- pixel_data_a %>% 
#       dplyr::mutate(time = time + min_year) %>%
#       rename(lwr = paste0("l", choice, ".", p * 100),
#              upr = paste0("u", choice, ".", p * 100)) %>% 
#       ggplot(., aes(x = time, y = posterior.mean)) +
#       geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.33) +
#       geom_line(linewidth = 1) +
#       scale_x_continuous(limits = c(min_year, max_year)) +
#       (if (selected_metric %in% c("ndmi")) {                              #adjust y axis based on range of veg index   
#         scale_y_continuous(limits = c(-1, 1.23), breaks = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1), expand = c(0, 0))
#       } else if (selected_metric %in% c("tcc")) {
#         scale_y_continuous(limits = c(0, 123), breaks = c(0, 25, 50, 75, 100), expand = c(0,0))
#       } else {
#         scale_y_continuous(limits = c(0, 1.23), breaks = c(0, 0.25, 0.5, 0.75, 1), expand = c(0, 0))
#       }) +
#       geom_point(data = observation_points) +
#       scale_fill_identity() +
#       scale_color_identity() +
#       labs(x = "Year",
#            y = paste0("Predicted ", toupper(selected_metric), ": ", cell_id_filter, "\n")) +
#       theme_cowplot()
#     
#     ## generate the plot
#     p.b <- pixel_data_b %>% 
#       dplyr::mutate(time = time + min_year) %>%
#       rename(lwr = paste0("l", choice, ".", p * 100),
#              upr = paste0("u", choice, ".", p * 100)) %>% 
#       ggplot(., aes(x = time, y = posterior.mean)) +
#       geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.33) +
#       geom_line(linewidth = 1) +
#       scale_x_continuous(limits = c(min_year, max_year)) +
#       (if (selected_metric %in% c("ndmi")) {                              #adjust y axis based on range of veg index   
#         scale_y_continuous(limits = c(-1, 1.23), breaks = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1), expand = c(0, 0))
#       } else if (selected_metric %in% c("tcc")) {
#         scale_y_continuous(limits = c(0, 123), breaks = c(0, 25, 50, 75, 100), expand = c(0,0))
#       } else {
#         scale_y_continuous(limits = c(0, 1.23), breaks = c(0, 0.25, 0.5, 0.75, 1), expand = c(0, 0))
#       }) +
#       geom_point(data = observation_points) +
#       scale_fill_identity() +
#       scale_color_identity() +
#       labs(x = "Year",
#            y = paste0("Predicted ", toupper(selected_metric), "\nwith annual variation: ", cell_id_filter)) +
#       theme_cowplot()
#     
#     p_panel <- plot_grid(p.a, p.b,            #identify panels to include; exclude temporal lag
#                          labels = c('a', 'b'),      #label
#                          hjust = -9)
#     
#     # view figure
#     print(p_panel)
#     
#   } else {
#     
#     print("ERROR: Unexpected number of interventions")
#     
#   }
#   
# }

# Function for creating the cell timeseries plots
# START ----
create_pixel_trend_plot <- function(intervention_data, 
                                    selected_metric,
                                    min_year,
                                    max_year,
                                    cell_id_filter, 
                                    predicted_df, 
                                    observed_df) {
  
  # DATA PREP ----
  # filter data to the selected cell
  filtered_predicted_data <- predicted_df %>%
    #mutate(cell_id = as.numeric(cell_id)) %>% 
    mutate(cell_id = as.numeric(as.character(cell_id))) %>%
    filter(cell_id == cell_id_filter)
  
  filtered_observed_data <- observed_df %>%
    filter(cell_id == cell_id_filter)
  
  # split the data into the smoothed and annual varitions
  pixel_data_a <- filtered_predicted_data %>% 
    filter(data_type == "smoothed")
  
  pixel_data_b <- filtered_predicted_data %>% 
    filter(data_type == "annual.variation")
  
  # grab the observed data points
  observation_points <- filtered_observed_data %>% 
    dplyr::select(year, posterior.mean = all_of(unname(selected_metric))) %>% 
    rename(time = year) %>% 
    mutate(obs_id = paste0("obs_", row_number()))
  
  # PLOT PREP ----
  
  # base annotation height on max range of data
  if(selected_metric == "tcc") {
    y_height = 104
  } else {
    y_height = 1.04
  }
  
  y_scale <- if (selected_metric %in% "ndmi") {
    scale_y_continuous(
      limits = c(-1, 1.23),
      breaks = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1),
      expand = c(0, 0)
    )
  } else if (selected_metric %in% "tcc") {
    scale_y_continuous(
      limits = c(0, 123),
      breaks = c(0, 25, 50, 75, 100),
      expand = c(0, 0)
    )
  } else {
    scale_y_continuous(
      limits = c(0, 1.23),
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      expand = c(0, 0)
    )
  }
  
  # custom theme ----
  
  erad_line_color <- coral
  raw_points_color <- "black"
  minor_font_size <- 16
  label_font_size <- 16
  caption_size <- 14
  annotation_font_size <- 6
  theme_components <- 
    theme(axis.text = element_text(size = minor_font_size,
                                   color = dark_neutral),
          plot.background  = element_rect(fill = light_neutral, color = NA),
          panel.background = element_rect(fill = light_neutral, color = NA),
          axis.title = element_text(size = label_font_size,
                                    color = dark_neutral),
          axis.title.x = element_text(margin = margin(t = 8, unit = "pt")),
          axis.title.y = element_text(margin = margin(r = 8, unit = "pt")),
          plot.caption = element_text(color = dark_neutral,
                                      size = caption_size,
                                      margin = margin(t = 15),
                                      hjust = 0.5))
  
  
  if(length(intervention_data$IntYr) == 2 ) {
    
    # INTYR = 2 PLOT ----
    # pull the eradication specifications
    erad1 <- intervention_data$IntYr[[1]]
    erad2 <- intervention_data$IntYr[[2]]
    erad1.sp <- paste(intervention_data$Genus[[1]], intervention_data$Species[[1]], sep = "\n")
    erad2.sp <- paste(intervention_data$Genus[[2]], intervention_data$Species[[2]], sep = "\n")
    
    # plot a ----
    # create the smoothed plot
    p.a <- pixel_data_a %>% 
      dplyr::mutate(time = time + min_year) %>%
      rename(lwr = paste0("l", choice, ".", p * 100),
             upr = paste0("u", choice, ".", p * 100)) %>% 
      ggplot() +
      geom_ribbon_interactive(aes(x = time, 
                                  y = posterior.mean, 
                                  ymin = lwr, 
                                  ymax = upr), alpha = 0.33) +
      geom_point_interactive(
        aes(
          x = time,
          y = posterior.mean,
          tooltip = paste0("MODELED<br>",
                           "Year: ", time, 
                           "<br>", toupper(selected_metric), ": ", round(posterior.mean, 3)),
          data_id = "trend"  # or "trend", but don't let it affect grouping
        ),
        size = 1.1
      )+
      geom_line_interactive(
        aes(
          x = time,
          y = posterior.mean,
          #tooltip = paste0("Year: ", time, "<br>Mean: ", round(posterior.mean, 3)),
          #data_id = "trend"  # or "trend", but don't let it affect grouping
        ),
        group = 1,
        linewidth = 1
      )+
      scale_x_continuous(limits = c(min_year, max_year)) +
      y_scale +
      geom_vline_interactive(xintercept = {erad1 - 1}, color = erad_line_color, linewidth = 1, linetype = "dashed") + #add a line for first intervention
      geom_vline_interactive(xintercept = {erad2 - 1}, color = erad_line_color, linewidth = 1, linetype = "dashed") + #add a line for second intervention
      annotate_interactive("text", x = {erad1 - 0.8}, y = y_height, label = {erad1.sp}, color = erad_line_color, hjust = 0, fontface = 'italic', size = annotation_font_size) + #add annotation for first line
      annotate_interactive("text", x = {erad2 - 0.8}, y = y_height, label = {erad2.sp}, color = erad_line_color, hjust = 0, fontface = 'italic', size = annotation_font_size) + #add annotation for second line
      geom_point_interactive(
        data = observation_points,
        aes(
          x = (time),
          y = posterior.mean,
          tooltip = paste0(
            "RAW<br>",
            "Year: ", time,
            "<br>", toupper(selected_metric), ": ", round(posterior.mean, 3)
          ),
          data_id = obs_id
        ),
        size = 2,
        color = raw_points_color
      ) +
      scale_fill_identity() +
      scale_color_identity() +
      labs(x = "Year",
           y = toupper(selected_metric),
           caption = paste0("Predicted ", toupper(selected_metric))) +
      theme_minimal() + theme_components
    
    # plot b ----
    ## generate the plot
    p.b <- pixel_data_b %>% 
      dplyr::mutate(time = time + min_year) %>%
      rename(lwr = paste0("l", choice, ".", p * 100),
             upr = paste0("u", choice, ".", p * 100)) %>% 
      ggplot() +
      geom_ribbon_interactive(aes(x = time, 
                                  y = posterior.mean, 
                                  ymin = lwr, 
                                  ymax = upr), alpha = 0.33) +
      geom_point_interactive(
        aes(
          x = time,
          y = posterior.mean,
          tooltip = paste0("MODELED<br>",
                           "Year: ", time, 
                           "<br>", toupper(selected_metric), ": ", round(posterior.mean, 3)),
          data_id = "trend"  # or "trend", but don't let it affect grouping
        ),
        size = 1.1
      )+
      geom_line_interactive(
        aes(
          x = time,
          y = posterior.mean,
          #tooltip = paste0("Year: ", time, "<br>Mean: ", round(posterior.mean, 3)),
          #data_id = "trend"  # or "trend", but don't let it affect grouping
        ),
        group = 1,
        linewidth = 1
      )+
      scale_x_continuous(limits = c(min_year, max_year)) +
      y_scale +
      geom_vline_interactive(xintercept = {erad1 - 1}, color = erad_line_color, linewidth = 1, linetype = "dashed") + #add a line for first intervention
      geom_vline_interactive(xintercept = {erad2 - 1}, color = erad_line_color, linewidth = 1, linetype = "dashed") + #add a line for second intervention
      annotate_interactive("text", x = {erad1 - 0.8}, y = y_height, label = {erad1.sp}, color = erad_line_color, hjust = 0, fontface = 'italic', size = annotation_font_size) + #add annotation for first line
      annotate_interactive("text", x = {erad2 - 0.8}, y = y_height, label = {erad2.sp}, color = erad_line_color, hjust = 0, fontface = 'italic', size = annotation_font_size) + #add annotation for second line
      geom_point_interactive(
        data = observation_points,
        aes(
          x = (time),
          y = posterior.mean,
          tooltip = paste0(
            "RAW<br>",
            "Year: ", time,
            "<br>", toupper(selected_metric), ": ", round(posterior.mean, 3)
          ),
          data_id = obs_id
        ),
        size = 2,
        color = raw_points_color
      ) +
      scale_fill_identity() +
      scale_color_identity() +
      labs(x = "Year",
           y = toupper(selected_metric),
           caption = paste0("Predicted ", toupper(selected_metric), " with annual\nvariation included in the model")) +
      theme_minimal() + theme_components
    
    # p_panel <- plot_grid(p.a, p.b,            #identify panels to include; exclude temporal lag
    #                      labels = c('a', 'b'),      #label
    #                      hjust = -9)
    # 
    # # view figure
    # print(p_panel)
    
    # p.a <- girafe(p.a)
    # p.b <- girafe(p.b)
    list(left = p.a, right = p.b)
    
  } else if (length(intervention_data$IntYr) == 1) {
    
    # INTYR = 1 PLOT ----
    # pull the eradication specifications
    erad1 <- intervention_data$IntYr[[1]]
    erad1.sp <- paste(intervention_data$Genus[[1]], intervention_data$Species[[1]], sep = "\n")
    
    # plot a ----
    # create the smoothed plot
    p.a <- pixel_data_a %>% 
      dplyr::mutate(time = time + min_year) %>%
      rename(lwr = paste0("l", choice, ".", p * 100),
             upr = paste0("u", choice, ".", p * 100)) %>% 
      ggplot() +
      geom_ribbon_interactive(aes(x = time, 
                                  y = posterior.mean, 
                                  ymin = lwr, 
                                  ymax = upr), alpha = 0.33) +
      geom_point_interactive(
        aes(
          x = time,
          y = posterior.mean,
          tooltip = paste0("MODELED<br>",
                           "Year: ", time, 
                           "<br>", toupper(selected_metric), ": ", round(posterior.mean, 3)),
          data_id = "trend"  # or "trend", but don't let it affect grouping
        ),
        size = 1.1
      )+
      geom_line_interactive(
        aes(
          x = time,
          y = posterior.mean,
          #tooltip = paste0("Year: ", time, "<br>Mean: ", round(posterior.mean, 3)),
          #data_id = "trend"  # or "trend", but don't let it affect grouping
        ),
        group = 1,
        linewidth = 1
      )+
      scale_x_continuous(limits = c(min_year, max_year)) +
      y_scale +
      geom_vline_interactive(xintercept = {erad1 - 1}, color = erad_line_color, linewidth = 1, linetype = "dashed") + #add a line for first intervention
      annotate_interactive("text", x = {erad1 - 0.8}, y = y_height, label = {erad1.sp}, color = erad_line_color, hjust = 0, fontface = 'italic', size = annotation_font_size) + #add annotation for first line
      geom_point_interactive(
        data = observation_points,
        aes(
          x = (time),
          y = posterior.mean,
          tooltip = paste0(
            "RAW<br>",
            "Year: ", time,
            "<br>", toupper(selected_metric), ": ", round(posterior.mean, 3)
          ),
          data_id = obs_id
        ),
        size = 2,
        color = raw_points_color
      ) +
      scale_fill_identity() +
      scale_color_identity() +
      labs(x = "Year",
           y = toupper(selected_metric),
           caption = paste0("Predicted ", toupper(selected_metric))) +
      theme_minimal() + theme_components
    
    # plot b ----
    ## generate the plot
    p.b <- pixel_data_b %>% 
      dplyr::mutate(time = time + min_year) %>%
      rename(lwr = paste0("l", choice, ".", p * 100),
             upr = paste0("u", choice, ".", p * 100)) %>% 
      ggplot() +
      geom_ribbon_interactive(aes(x = time, 
                                  y = posterior.mean, 
                                  ymin = lwr, 
                                  ymax = upr), alpha = 0.33) +
      geom_point_interactive(
        aes(
          x = time,
          y = posterior.mean,
          tooltip = paste0("MODELED<br>",
                           "Year: ", time, 
                           "<br>", toupper(selected_metric), ": ", round(posterior.mean, 3)),
          data_id = "trend"  # or "trend", but don't let it affect grouping
        ),
        size = 1.1
      )+
      geom_line_interactive(
        aes(
          x = time,
          y = posterior.mean,
          #tooltip = paste0("Year: ", time, "<br>Mean: ", round(posterior.mean, 3)),
          #data_id = "trend"  # or "trend", but don't let it affect grouping
        ),
        group = 1,
        linewidth = 1
      )+
      scale_x_continuous(limits = c(min_year, max_year)) +
      y_scale +
      geom_vline_interactive(xintercept = {erad1 - 1}, color = erad_line_color, linewidth = 1, linetype = "dashed") + #add a line for first intervention
      annotate_interactive("text", x = {erad1 - 0.8}, y = y_height, label = {erad1.sp}, color = erad_line_color, hjust = 0, fontface = 'italic', size = annotation_font_size) + #add annotation for first line
      geom_point_interactive(
        data = observation_points,
        aes(
          x = (time),
          y = posterior.mean,
          tooltip = paste0(
            "RAW<br>",
            "Year: ", time,
            "<br>", toupper(selected_metric), ": ", round(posterior.mean, 3)
          ),
          data_id = obs_id
        ),
        size = 2,
        color = raw_points_color
      ) +
      scale_fill_identity() +
      scale_color_identity() +
      labs(x = "Year",
           y = toupper(selected_metric),
           caption = paste0("Predicted ", toupper(selected_metric), " with annual\nvariation included in the model")) +
      theme_minimal() + theme_components
    
    # p_panel <- plot_grid(p.a, p.b,            #identify panels to include; exclude temporal lag
    #                      labels = c('a', 'b'),      #label
    #                      hjust = -9)
    
    # view figure
    #print(p_panel)
    # p.a <- girafe(p.a)
    # p.b <- girafe(p.b)
    list(left = p.a, right = p.b)
    
    # INTYR = 2 PLOT ----
  } else if (length(intervention_data$IntYr) == 0) {
    
    # plot a ----
    # create the smoothed plot
    p.a <- pixel_data_a %>% 
      dplyr::mutate(time = time + min_year) %>%
      rename(lwr = paste0("l", choice, ".", p * 100),
             upr = paste0("u", choice, ".", p * 100)) %>% 
      ggplot() +
      geom_ribbon_interactive(aes(x = time, 
                                  y = posterior.mean, 
                                  ymin = lwr, 
                                  ymax = upr), alpha = 0.33) +
      geom_point_interactive(
        aes(
          x = time,
          y = posterior.mean,
          tooltip = paste0("MODELED<br>",
                           "Year: ", time, 
                           "<br>", toupper(selected_metric), ": ", round(posterior.mean, 3)),
          data_id = "trend"  # or "trend", but don't let it affect grouping
        ),
        size = 1.1
      )+
      geom_line_interactive(
        aes(
          x = time,
          y = posterior.mean,
          #tooltip = paste0("Year: ", time, "<br>Mean: ", round(posterior.mean, 3)),
          #data_id = "trend"  # or "trend", but don't let it affect grouping
        ),
        group = 1,
        linewidth = 1
      )+
      scale_x_continuous(limits = c(min_year, max_year)) +
      y_scale +
      geom_point_interactive(
        data = observation_points,
        aes(
          x = (time),
          y = posterior.mean,
          tooltip = paste0(
            "RAW<br>",
            "Year: ", time,
            "<br>", toupper(selected_metric), ": ", round(posterior.mean, 3)
          ),
          data_id = obs_id
        ),
        size = 2,
        color = raw_points_color
      ) +
      scale_fill_identity() +
      scale_color_identity() +
      labs(x = "Year",
           y = toupper(selected_metric),
           caption = paste0("Predicted ", toupper(selected_metric), " with annual\nvariation included in the model")) +
      theme_minimal() + theme_components
    
    # plot b ----
    ## generate the plot
    p.b <- pixel_data_b %>% 
      dplyr::mutate(time = time + min_year) %>%
      rename(lwr = paste0("l", choice, ".", p * 100),
             upr = paste0("u", choice, ".", p * 100)) %>% 
      ggplot() +
      geom_ribbon_interactive(aes(x = time, 
                                  y = posterior.mean, 
                                  ymin = lwr, 
                                  ymax = upr), alpha = 0.33) +
      geom_point_interactive(
        aes(
          x = time,
          y = posterior.mean,
          tooltip = paste0("MODELED<br>",
                           "Year: ", time, 
                           "<br>", toupper(selected_metric), ": ", round(posterior.mean, 3)),
          data_id = "trend"  # or "trend", but don't let it affect grouping
        ),
        size = 1.1
      )+
      geom_line_interactive(
        aes(
          x = time,
          y = posterior.mean,
          #tooltip = paste0("Year: ", time, "<br>Mean: ", round(posterior.mean, 3)),
          #data_id = "trend"  # or "trend", but don't let it affect grouping
        ),
        group = 1,
        linewidth = 1
      )+
      scale_x_continuous(limits = c(min_year, max_year)) +
      y_scale +
      geom_point_interactive(
        data = observation_points,
        aes(
          x = (time),
          y = posterior.mean,
          tooltip = paste0(
            "RAW<br>",
            "Year: ", time,
            "<br>", toupper(selected_metric), ": ", round(posterior.mean, 3)
          ),
          data_id = obs_id
        ),
        size = 2,
        color = raw_points_color
      ) +
      scale_fill_identity() +
      scale_color_identity() +
      labs(x = "Year",
           y = toupper(selected_metric),
           caption = paste0("Predicted ", toupper(selected_metric), " with annual\nvariation included in the model")) +
      theme_minimal() + theme_components
    
    # p_panel <- plot_grid(p.a, p.b,            #identify panels to include; exclude temporal lag
    #                      labels = c('a', 'b'),      #label
    #                      hjust = -9)
    
    # view figure
    #print(p_panel)
    # p.a <- girafe(p.a)
    # p.b <- girafe(p.b)
    list(left = p.a, right = p.b)
    
  } else {
    
    print("ERROR: Unexpected number of interventions")
    
  }
  
}

