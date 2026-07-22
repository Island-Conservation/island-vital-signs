# server.R

# START server instructions ----
server <- function(input, output, session) { 
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                                                            ~~
  ##                             ISLAND-WIDE MODULE                           ----
  ##                                                                            ~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # ACTIVATE island ----
  shared_island <- reactiveVal("")
  
  observeEvent(input$island_choice1, {
    if (input$island_choice1 != shared_island()) {
      shared_island(input$island_choice1)
    }
  })
  
  observeEvent(input$island_choice2, {
    if (input$island_choice2 != shared_island()) {
      shared_island(input$island_choice2)
    }
  })
  
  observeEvent(input$island_choice3, {
    if (input$island_choice3 != shared_island()) {
      shared_island(input$island_choice3)
    }
  })
  
  observeEvent(input$island_choice4, {
    if (input$island_choice4 != shared_island()) {
      shared_island(input$island_choice4)
    }
  })
  
  observeEvent(input$island_choice5, {
    if (input$island_choice5 != shared_island()) {
      shared_island(input$island_choice5)
    }
  })
  
  observe({
    updateSelectInput(session, "island_choice1", selected = shared_island())
    updateSelectInput(session, "island_choice2", selected = shared_island())
    updateSelectInput(session, "island_choice3", selected = shared_island())
    updateSelectInput(session, "island_choice4", selected = shared_island())
    updateSelectInput(session, "island_choice5", selected = shared_island())
  })
  
  # island_data2 <- reactive({
  #   
  #   req(input$island_choice2)
  #   
  #   island_metadata[island_metadata$Island == input$island_choice2, ]
  #   
  # })
  #   
  # island_data3 <- reactive({
  #   
  #   req(input$island_choice3)
  #       
  #       island_metadata[island_metadata$Island == input$island_choice3, ]
  #   
  # })
  
  island_data <- reactive({
    
    req(shared_island())
    
    island_metadata[island_metadata$Island == shared_island(), ]
    
  })
  
  
  
  intervention_data <- reactive({
    
    req(shared_island())
    
    intervention_metadata[intervention_metadata$Island == shared_island(), ]
  })
  
  # ACTIVATE metric ----
  shared_metric <- reactiveVal("")
  
  observeEvent(input$metric_choice1, {
    if (input$metric_choice1 != shared_metric()) {
      shared_metric(input$metric_choice1)
    }
  })
  
  observeEvent(input$metric_choice2, {
    if (input$metric_choice2 != shared_metric()) {
      shared_metric(input$metric_choice2)
    }
  })
  
  observeEvent(input$metric_choice3, {
    if (input$metric_choice3 != shared_metric()) {
      shared_metric(input$metric_choice3)
    }
  })
  
  observeEvent(input$metric_choice4, {
    if (input$metric_choice4 != shared_metric()) {
      shared_metric(input$metric_choice4)
    }
  })
  
  observe({
    updateSelectInput(session, "metric_choice1", selected = shared_metric())
    updateSelectInput(session, "metric_choice2", selected = shared_metric())
    updateSelectInput(session, "metric_choice3", selected = shared_metric())
    updateSelectInput(session, "metric_choice4", selected = shared_metric())
  })
  
  metric_selection <- reactive({
    
    req(shared_metric())
    
    shared_metric()
    
  })
  
  # ACTIVATE results type ----
  results_selection <- reactive({
    
    req(input$results_choice)
    
    input$results_choice
    
  })
  
  
  
  
  
  
  
  
  
  
  
  update_metric_choices <- function(island_input_id, metric_input_id) {
    observeEvent(input[[island_input_id]], {
      req(input[[island_input_id]])
      
      # Get GID2 from island_metadata
      selected_gid2 <- island_metadata %>%
        filter(Island == input[[island_input_id]]) %>%
        pull(GID2_ID)
      
      # Filter final_df for available backend metrics
      available_metrics <- metric_metadata %>%
        filter(gid2 == selected_gid2) %>%
        pull(metric) %>%
        unique() %>%
        sort()
      
      # Map backend metrics to display names
      display_metrics <- names(metric_map)[metric_map %in% available_metrics]
      
      # Update the corresponding metric input
      updateSelectInput(session, metric_input_id,
                        choices = c("", display_metrics),
                        selected = "")
    })
  }
  
  # Apply to all island-metric input pairs
  update_metric_choices("island_choice1", "metric_choice1")
  update_metric_choices("island_choice2", "metric_choice2")
  update_metric_choices("island_choice3", "metric_choice3")
  update_metric_choices("island_choice4", "metric_choice4")
  
  
  
  
  
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                                                            ~~
  ##                        PER-PIXEL ANNUAL VEG MODULE                       ----
  ##                                                                            ~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # DISPLAY help button ----
  observeEvent(input$help_btn_annual, {
    
    showModal(modalDialog(
      title = "Help: Per-Pixel Modeled Annual Vegetation Values",
      tagList(includeMarkdown("text/m2-per-pixel-annual-vegetation.md")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    
  })
  
  # ACTIVATE year (left) ----
  year_choice_left <- reactive({
    
    req(input$year_choice_left)
    
    as.numeric(input$year_choice_left)
    
  })
  
  # ACTIVATE year (right) ----
  year_choice_right <- reactive({
    
    req(input$year_choice_right)
    
    as.numeric(input$year_choice_right)
    
  })
  
  
  observe({
    req(shared_island(), shared_metric())
    
    # Get backend metric name
    selected_metric <- isolate(metric_map[[shared_metric()]])
    
    # Get GID2 from island_metadata
    selected_gid2 <- island_metadata %>%
      filter(Island == shared_island()) %>%
      pull(GID2_ID)
    
    # Choose the correct metadata source and filter by GID2
    if (selected_metric == "tcc") {
      available_years <- tcc_yrs_metadata %>%
        filter(gid2 == selected_gid2) %>%
        pull(year)
    } else {
      available_years <- si_yrs_metadata %>%
        filter(gid2 == selected_gid2) %>%
        pull(year)
    }
    
    # Update both year dropdowns
    updateSelectInput(session, "year_choice_left",
                      choices = c("", sort(unique(available_years))),
                      selected = "")
    
    updateSelectInput(session, "year_choice_right",
                      choices = c("", sort(unique(available_years))),
                      selected = "")
    
  })
  
  # DISPLAY island info ----
  output$island_info <- renderUI({
    req(shared_island())
    
    chosen_isl_dat <- island_metadata %>%
      filter(Island == shared_island()) 
    
    selected_gid2 <- chosen_isl_dat$GID2_ID
    
    chosen_isl_erads <- intervention_metadata %>%
      filter(GID2 == selected_gid2) %>%
      mutate(Species = paste(Genus, Species, sep = " "),
             Type = RemTyp,
             Year = IntYr) %>%
      select(Type, Species, Year)
    
    tagList(
      p(strong("Island:"), str_replace(chosen_isl_dat$Isl, "_", " ")),
      p(strong("Country:"), chosen_isl_dat$COUNTRY),
      p(strong("Area:"), paste0(round(chosen_isl_dat$AreaKm2, 2), " km²")),
      
      # Eradications header
      tags$p(strong("Modeled Eradications:")),
      
      # Styled eradications table
      HTML(
        chosen_isl_erads %>%
          knitr::kable(format = "html", escape = FALSE, col.names = c("Type", "Species", "Year")) %>%
          kableExtra::kable_styling(
            bootstrap_options = c("hover", "condensed", "responsive"),
            full_width = TRUE
          ) %>%
          kableExtra::row_spec(0, bold = TRUE, background = light_neutral) %>%
          {
            for (i in seq_len(nrow(chosen_isl_erads))) {
              color <- ifelse(i %% 2 == 0, light_neutral, "white")
              . <-            . <- kableExtra::row_spec(., i, background = color)
            }
            .
          }
      )
    )
    
  })
  
  # READ data ----
  observe({
    
    # require the following inputs:
    req(island_data(), metric_selection(), year_choice_left(), year_choice_right())
    
    # convert user-friendly metric name to metadata column name
    selected_metric <- metric_map[metric_selection()]
    metric_col <- paste0(selected_metric, "_fp") 
    
    # extract the raster stack path from the island metadata
    predicted_rast_path <-paste0(island_data()[[metric_col]], "/m1_", selected_metric, "_", island_data()[["Res"]], "m_allcells_posterior.mean_annual_prediction.tif")
    predicted_df_path <- paste0(island_data()[[metric_col]], "/m1_", selected_metric, "_", island_data()[["Res"]], "m_cell_predictions.rds")
    observed_df_path <- paste0(island_data()[[metric_col]], "/", selected_metric, "_", island_data()[["Res"]], "m_observed_timeseries.rds")
    
    # check if the file exists
    if (length(predicted_rast_path) > 0 && file.exists(predicted_rast_path)) {
      
      # load the raster stack
      predicted_stack <- rast(predicted_rast_path)
      
      # grab the minimum year of data as a number
      min_year <- as.numeric(names(predicted_stack[[2]]))
      max_year <- as.numeric(names(predicted_stack[[nlyr(predicted_stack) - 2]]))
      
      # PREP data ----
      # read in the associated predicted data rds
      predicted_df <- read_rds(predicted_df_path)
      observed_df <- read_rds(observed_df_path)
      
      # select the correct layer based on year
      selected_layer_left <- names(predicted_stack) == as.character(year_choice_left())
      selected_layer_right <- names(predicted_stack) == as.character(year_choice_right())
      
      # add the cell_id information to a new band in each selected layer
      cell_id_layer <- names(predicted_stack) == "cell_id"
      
      # prep the layers to display in leaflet
      if (any(selected_layer_left)) {
        
        predicted_sp_left <- prep_tif_for_leaflet(r_stack = predicted_stack, 
                                                  selected_layer = selected_layer_left,
                                                  metric_selection = metric_selection()) 
        predicted_sp_right <- prep_tif_for_leaflet(r_stack = predicted_stack, 
                                                   selected_layer = selected_layer_right,
                                                   metric_selection = metric_selection())
        cell_id_sp <- prep_tif_for_leaflet(r_stack = predicted_stack, 
                                           selected_layer = cell_id_layer,
                                           metric_selection = metric_selection()) %>% 
          rename(cell_id = value)
        
        predicted_sp_react <- reactiveValues(sp = cell_id_sp, 
                                             clicked_cell = NULL)
        
        # set mapping aesthetics according to map type (tree cover vs spectral indices)
        if (metric_selection() == "NDMI") {
          
          value_range <- c(-1, 1) 
          pal <- colorNumeric(palette = "viridis", domain = value_range)
          
        } else if (metric_selection() == "Tree Cover") {
          
          value_range <- c(0, 100)
          pal <- colorNumeric(palette = "viridis", domain = value_range)
          
        } else {
          
          value_range <- c(0, 1)
          pal <- colorNumeric(palette = "viridis", domain = value_range)
          
        }
        
        # PLOT leaflet ----
        output$annual_predicted_splitmap <- renderLeaflet({
          
          # create the leaflet splitmap
          create_leaflet_splitmap(sp_left = predicted_sp_left, 
                                  sp_right = predicted_sp_right,
                                  title_left = as.character(year_choice_left()),
                                  title_right = as.character(year_choice_right()),
                                  pal = pal,
                                  value_range = value_range)
          
        })
        
        # ACTIVATE expand map ----
        observeEvent(input$expand_predictions_map, {
          showModal(modalDialog(
            leafletOutput("full_predictions_map", height = "600px"),
            easyClose = TRUE,
            size = "l",
            footer = modalButton("Exit")
          ))
        })
        
        output$full_predictions_map <- renderLeaflet({
          
          # create the leaflet splitmap
          create_leaflet_splitmap(sp_left = predicted_sp_left, 
                                  sp_right = predicted_sp_right,
                                  title_left = as.character(year_choice_left()),
                                  title_right = as.character(year_choice_right()),
                                  pal = pal,
                                  value_range = value_range)
          
        })
        
        # ACTIVATE map clicks ----
        observeEvent(input$annual_predicted_splitmap_shape_click, {
          
          # set click events
          click <- input$annual_predicted_splitmap_shape_click
          lat <- round(click$lat, 6)
          lng <- round(click$lng, 6)
          id <- click$id
          
          # update reactive value
          predicted_sp_react$clicked_cell <- list(X = lat, 
                                                  Y = lng, 
                                                  id = id)
          
        })
        
        
        output$pixel_summary_text <- renderUI({
          if (is.null(predicted_sp_react$clicked_cell)) return(NULL)
          
          cell_id_filter <- predicted_sp_react$clicked_cell$id
          lat <- predicted_sp_react$clicked_cell$X
          lng <- predicted_sp_react$clicked_cell$Y
          
          min_year <- as.numeric(names(predicted_stack[[2]]))
          
          # Pull values for this pixel from your data
          value_left <- predicted_df %>%
            mutate(year = time + min_year) %>%
            filter(cell_id == cell_id_filter, year == year_choice_left()) %>%
            pull(posterior.mean) %>%
            dplyr::first()
          
          value_right <- predicted_df %>%
            mutate(year = time + min_year) %>%
            filter(cell_id == cell_id_filter, year == year_choice_right()) %>%
            pull(posterior.mean) %>%
            dplyr::first()
          
          # Replace NA with "not available"
          value_left <- ifelse(is.na(value_left), "not available", round(value_left, 2))
          value_right <- ifelse(is.na(value_right), "not available", round(value_right, 2))
          
          tags$p(
            paste0(
              "You have selected a pixel at (",
              round(lng, 4), ", ", round(lat, 4), "). ",
              "The modeled ", metric_selection(), " at this pixel is ", round(value_left, 2),
              " in ", year_choice_left(), " and ", round(value_right, 2),
              " in ", year_choice_right(), ". Below are two plots that show the value of this cell through time. ",
              "On the left is a smoothed version of the model, and on the right is a model that includes annual variation."
            ),
            style = "font-size:16px; margin-bottom:15px;"
          )
          
        })
        
        # PLOT pixel trends ----
        pixel_trend_plots <- reactive({
          
          # plot if a cell is clicked
          if (is.null(predicted_sp_react$clicked_cell)) {
            
            return(NULL)
            
          } else {
            
            # identify clicked cell to filter on
            cell_id_filter <- predicted_sp_react$clicked_cell$id
            #showNotification(paste("Displaying timeseries for cell number:", cell_id_filter), type = "message")
            
          }
          
          # plot the filtered data
          create_pixel_trend_plot(intervention_data = intervention_data(),
                                  selected_metric = selected_metric,
                                  min_year = min_year,
                                  max_year = max_year,
                                  cell_id_filter = cell_id_filter,
                                  predicted_df = predicted_df,
                                  observed_df = observed_df)
          
        })
        
        output$pixel_trend_plot_a <- renderGirafe({
          
          req(pixel_trend_plots())
          
          girafe(
            ggobj = pixel_trend_plots()$left,
            width_svg  = 6,
            height_svg = 5,
            options = list(
              opts_hover(css = "fill: var(--coral);"),
              #opts_tooltip(css = "background-color:white;"),
              opts_sizing(rescale = TRUE),
              opts_toolbar(hidden = c("lasso_select", "lasso_deselect")),
              opts_selection(type = "none")
            )
          )
        })
        
        output$pixel_trend_plot_b <- renderGirafe({
          
          req(pixel_trend_plots())
          
          girafe(
            ggobj = pixel_trend_plots()$right,
            width_svg  = 6,
            height_svg = 5,
            options = list(
              opts_hover(css = "fill: var(--coral);"),
              #opts_tooltip(css = "background-color:white;"),
              opts_sizing(rescale = TRUE),
              opts_toolbar(hidden = c("lasso_select", "lasso_deselect")),
              opts_selection(type = "none")
            )
          )
        })
        
        
        # output$pixel_trend_plot <- renderPlot({
        #   
        #   # plot if a cell is clicked
        #   if (is.null(predicted_sp_react$clicked_cell)) {
        #     
        #     return(NULL)
        #     
        #   } else {
        #     
        #     # identify clicked cell to filter on
        #     cell_id_filter <- predicted_sp_react$clicked_cell$id
        #     showNotification(paste("Displaying timeseries for cell number:", cell_id_filter), type = "message")
        #     
        #   }
        #   
        #   # plot the filtered data
        #   create_pixel_trend_plot(intervention_data = intervention_data(),
        #                           selected_metric = selected_metric,
        #                           min_year = min_year,
        #                           max_year = max_year,
        #                           cell_id_filter = cell_id_filter,
        #                           predicted_df = predicted_df,
        #                           observed_df = observed_df)
        #   
        # })
        
        # output$pixel_trend_plot <- renderGirafe({
        #   
        #   # plot only if a cell is clicked
        #   req(predicted_sp_react$clicked_cell)
        #   
        #   cell_id_filter <- predicted_sp_react$clicked_cell$id
        #   showNotification(paste("Displaying timeseries for cell number:", cell_id_filter), type = "message")
        #   
        #   # create your ggiraph plots
        #   plots <- create_pixel_trend_plot(
        #     intervention_data = intervention_data(),
        #     selected_metric   = selected_metric,
        #     min_year          = min_year,
        #     max_year          = max_year,
        #     cell_id_filter    = cell_id_filter,
        #     predicted_df      = predicted_df,
        #     observed_df       = observed_df
        #   )
        #   
        #   # combine the plots side by side using htmltools::tagList
        #   girafe(tagList(
        #     plots$p.a,
        #     plots$p.b
        #   ))
        #   
        # })
        
        
      } else {
        
        # DISPLAY warning messages ----
        
        #showNotification("INVALID YEAR: Due to limits in data, the year you chose is not available.", type = "error")
        
      } 
      
      # DOWNLOAD data ----
      output$download_annual_predicted_rasters <- downloadHandler(
        
        filename = function() {
          
          # clean up the island name and create a valid filename
          island <- gsub("[()]", "", island_data()$Island)
          island <- gsub(" ", "_", island)
          metric <- gsub(" ", "", metric_selection())
          paste0(island, "_", metric, "_Stack.tif")
          
        },
        
        content = function(file) {
          
          # save the raster stack to the specified file
          writeRaster(predicted_stack, file, filetype = "GTiff", overwrite = TRUE)
          
        }
        
      )
      
      # DOWLOAD maps ----
      output$download_annual_predicted_maps <- downloadHandler(
        
        filename = function() {
          island <- gsub("[()]", "", island_data()$Island)
          island <- gsub(" ", "_", island)
          metric <- gsub(" ", "", metric_selection())
          paste0(island, "_", metric, "_Maps.zip")
        },
        
        content = function(file) {
          
          
          show_modal_spinner(text = "Generating maps, please wait. This could take a minute or two.", spin = "fading-circle")
          on.exit(remove_modal_spinner())
          
          temp_dir <- tempdir()
          image_files <- c()
          
          for (i in 1:nlyr(predicted_stack)) {
            layer_name <- names(predicted_stack)[i]
            
            if (layer_name == "cell_id") next
            
            layer <- predicted_stack[[i]]
            image_path <- file.path(temp_dir, paste0(metric_selection(), "_", layer_name, ".jpg"))
            
            jpeg(image_path, width = 8, height = 6, units = "in", res = 300)
            map <- generate_static_annual_map(raster_layer = layer,
                                              island_data = island_data(),
                                              metric = metric_selection())
            print(map)  # Render the map to the JPEG device
            dev.off()
            
            image_files <- c(image_files, image_path)
          }
          
          zip::zip(zipfile = file, files = image_files, mode = "cherry-pick")
        }
      )
      
      # DOWNLOAD timeseries ----
      output$download_pixel_plot1 <- downloadHandler(
        
        filename = function() {
          
          # clean up the island name and create a valid filename
          island <- gsub("[()]", "", island_data()$Island)
          island <- gsub(" ", "_", island)
          metric <- gsub(" ", "", metric_selection())
          pixel <- predicted_sp_react$clicked_cell$id
          paste0(island, "_", metric, "_", pixel, "_Timeseries.jpg")
          
        },
        
        content = function(file) {
          
          pixel_plot1 <- create_pixel_trend_plot(
            intervention_data = intervention_data(),
            selected_metric = selected_metric,
            min_year = min_year,
            max_year = max_year,
            cell_id_filter = predicted_sp_react$clicked_cell$id,
            predicted_df = predicted_df,
            observed_df = observed_df
          )
          
          # Save the plot
          ggsave(file, plot = pixel_plot1, bg = "white", width = 8, height = 4, dpi = 300)
          
        }
        
      )
      
    } else {
      
      #showNotification("INVALIDE METRIC: Due to limited resources, this metric has not been modeled for this island.", type = "error")
    }
    
  })
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                                                            ~~
  ##                            CHANGE MAPS MODULE                            ----
  ##                                                                            ~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # DISPLAY help button ----
  observeEvent(input$help_btn_change, {
    
    showModal(modalDialog(
      title = "Help: Modeled Vegetation Change",
      tagList(includeMarkdown("text/m3-per-pixel-change.md")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    
  })
  
  # ACTIVATE timeperiod type ----
  shared_timeperiod <- reactiveVal("")
  
  observeEvent(input$timeperiod_choice1, {
    if (input$timeperiod_choice1 != shared_timeperiod()) {
      shared_timeperiod(input$timeperiod_choice1)
    }
  })
  
  # observeEvent(input$timeperiod_choice2, {
  #   if (input$timeperiod_choice2 != shared_timeperiod()) {
  #     shared_timeperiod(input$timeperiod_choice2)
  #   }
  # })
  
  observeEvent(input$timeperiod_choice3, {
    if (input$timeperiod_choice3 != shared_timeperiod()) {
      shared_timeperiod(input$timeperiod_choice3)
    }
  })
  
  observe({
    updateSelectInput(session, "timeperiod_choice1", selected = shared_timeperiod())
    updateSelectInput(session, "timeperiod_choice2", selected = shared_timeperiod())
    updateSelectInput(session, "timeperiod_choice3", selected = shared_timeperiod())
  })
  
  
  timeperiod_selection <- reactive({
    
    req(shared_timeperiod())
    
    shared_timeperiod()
    
  })
  
  # UPDATE time period choices based on selected island
  observe({
    req(shared_island())
    req(shared_metric())
    
    # Get GID2 from island_metadata
    selected_gid2 <- island_metadata %>%
      filter(Island == shared_island()) %>%
      pull(GID2_ID)
    
    # Select based on metric
    if(shared_metric() == "Tree Cover") {
      
      # Filter time_periods_metadata for this GID2
      available_timeperiods <- time_periods_tcc_metadata %>%
        filter(gid2 == selected_gid2) %>%
        pull(time_period) %>% 
        unique() %>%
        sort()
      
    } else {
      
      # Filter time_periods_metadata for this GID2
      available_timeperiods <- time_periods_metadata %>%
        filter(gid2 == selected_gid2) %>%
        pull(time_period) %>%
        unique() %>%
        sort()
      
    }
    
    # Map backend timeperiods to display names using accumulated_map
    display_timeperiods <- names(accumulated_map)[accumulated_map %in% available_timeperiods]
    
    # Update all timeperiod dropdowns
    updateSelectInput(session, "timeperiod_choice1",
                      choices = c("", display_timeperiods),
                      selected = "")
    
    updateSelectInput(session, "timeperiod_choice2",
                      choices = c("", display_timeperiods),
                      selected = "")
    
    updateSelectInput(session, "timeperiod_choice3",
                      choices = c("", display_timeperiods),
                      selected = "")
  })
  
  # ACTIVATE probability type ----
  shared_probability <- reactiveVal("")
  
  observeEvent(input$probability_choice1, {
    if (input$probability_choice1 != shared_probability()) {
      shared_probability(input$probability_choice1)
    }
  })
  
  observeEvent(input$probability_choice2, {
    if (input$probability_choice2 != shared_probability()) {
      shared_probability(input$probability_choice2)
    }
  })
  
  observeEvent(input$probability_choice3, {
    if (input$probability_choice3 != shared_probability()) {
      shared_probability(input$probability_choice3)
    }
  })
  
  observe({
    updateSelectInput(session, "probability_choice1", selected = shared_probability())
    updateSelectInput(session, "probability_choice2", selected = shared_probability())
    updateSelectInput(session, "probability_choice3", selected = shared_probability())
  })
  
  
  probability_selection <- reactive({
    
    req(shared_probability())
    
    as.numeric(gsub("%", "", shared_probability())) / 100
    
  })
  
  # ACTIVATE change type ----
  shared_change_type <- reactiveVal("")
  
  observeEvent(input$change_choice1, {
    if (input$change_choice1 != shared_change_type()) {
      shared_change_type(input$change_choice1)
    }
  })
  
  observeEvent(input$change_choice2, {
    if (input$change_choice2 != shared_change_type()) {
      shared_change_type(input$change_choice2)
    }
  })
  
  observe({
    updateSelectInput(session, "change_choice1", selected = shared_change_type())
    updateSelectInput(session, "change_choice2", selected = shared_change_type())
  })
  
  change_type_selection <- reactive({
    
    req(shared_change_type())
    
    shared_change_type()
    
  })
  
  # DISPLAY island info ----
  output$island_info2 <- renderUI({
    req(shared_island())
    
    chosen_isl_dat <- island_metadata %>%
      filter(Island == shared_island()) 
    
    selected_gid2 <- chosen_isl_dat$GID2_ID
    
    chosen_isl_erads <- intervention_metadata %>%
      filter(GID2 == selected_gid2) %>%
      mutate(Species = paste(Genus, Species, sep = " "),
             Type = RemTyp,
             Year = IntYr) %>%
      select(Type, Species, Year)
    
    tagList(
      p(strong("Island:"), str_replace(chosen_isl_dat$Isl, "_", " ")),
      p(strong("Country:"), chosen_isl_dat$COUNTRY),
      p(strong("Area:"), paste0(round(chosen_isl_dat$AreaKm2, 2), " km²")),
      
      # Eradications header
      tags$p(strong("Modeled Eradications:")),
      
      # Styled eradications table
      HTML(
        chosen_isl_erads %>%
          knitr::kable(format = "html", escape = FALSE, col.names = c("Type", "Species", "Year")) %>%
          kableExtra::kable_styling(
            bootstrap_options = c("hover", "condensed", "responsive"),
            full_width = TRUE
          ) %>%
          kableExtra::row_spec(0, bold = TRUE, background = light_neutral) %>%
          {
            for (i in seq_len(nrow(chosen_isl_erads))) {
              color <- ifelse(i %% 2 == 0, light_neutral, "white")
              . <-            . <- kableExtra::row_spec(., i, background = color)
            }
            .
          }
      )
    )
    
  })
  
  # READ data ----
  observe({
    
    # require the following inputs:
    req(island_data(), metric_selection(), timeperiod_selection(), 
        probability_selection(), change_type_selection())
    
    # convert user-friendly names to in-data names
    selected_metric <- metric_map[metric_selection()]
    metric_col <- paste0(selected_metric, "_fp") 
    selected_time <- change_map[timeperiod_selection()]
    selected_prob <- as.numeric(probability_selection())
    change_type <- tolower(change_type_selection())
    
    # extract the raster stack path from the island metadata
    predicted_rast_path <-paste0(island_data()[[metric_col]], "/m1_", selected_metric, "_", island_data()[["Res"]], "m_allcells_posterior.mean_annual_prediction.tif")
    change_rast_path <-paste0(island_data()[[metric_col]], "/m1_", selected_metric, "_", island_data()[["Res"]], "m_", change_type, ".tif")
    predicted_df_path <- paste0(island_data()[[metric_col]], "/m1_", selected_metric, "_", island_data()[["Res"]], "m_cell_predictions.rds")
    observed_df_path <- paste0(island_data()[[metric_col]], "/", selected_metric, "_", island_data()[["Res"]], "m_observed_timeseries.rds")
    
    # check if the file exists
    if (length(change_rast_path) > 0 && file.exists(change_rast_path)) {
      
      # load the raster stack
      change_stack <- rast(change_rast_path)
      predicted_stack <- rast(predicted_rast_path)
      min_year <- as.numeric(names(predicted_stack[[2]]))
      max_year <- as.numeric(names(predicted_stack[[nlyr(predicted_stack) - 2]]))
      
      # PREP data ----
      # read in the associated predicted data rds
      predicted_df <- read_rds(predicted_df_path)
      observed_df <- read_rds(observed_df_path)
      
      # select the correct layers based on inputs
      selected_change_layer_left <- names(change_stack) == paste0("posterior.mean_", selected_time)
      selected_change_layer_right <- names(change_stack) == paste0("posterior.mean_hpdi", selected_prob*100,"_", selected_time)
      
      # add the cell_id information to a new band in each selected layer
      cell_id_change_layer <- names(change_stack) == paste0("cell_id_", selected_time)
      
      # prep the layers to display in leaflet
      if (any(selected_change_layer_left)) {
        
        change_sp_left <- prep_change_tif_for_leaflet(r_stack = change_stack,
                                                      selected_layer = selected_change_layer_left,
                                                      selected_time = selected_time,
                                                      metric_selection = metric_selection())
        change_left_title <- paste0(toupper(change_type), " ", metric_selection(), "; All Modeled Pixels")
        
        change_sp_right <- prep_change_tif_for_leaflet(r_stack = change_stack, 
                                                       selected_layer = selected_change_layer_right,
                                                       selected_time = selected_time,
                                                       metric_selection = metric_selection()) 
        
        change_right_title <- paste0("Pixels with Probability > ", selected_prob)
        
        cell_id_sp <- prep_change_tif_for_leaflet(r_stack = change_stack, 
                                                  selected_layer = cell_id_change_layer,
                                                  metric_selection = metric_selection()) %>% 
          rename(cell_id = value)
        
        change_sp_react <- reactiveValues(sp = cell_id_sp, 
                                          clicked_cell = NULL)
        
        
        # Set the color palette
        value_range <- get_palette_range(change_sp_left, col = "value")
        
        pal <- colorNumeric(palette = div_pal, domain = value_range)
        
        # PLOT leaflets ----
        output$change_sync_map <- renderLeaflet({
          
          # create the leaflet splitmap
          create_leaflet_splitmap_change(sp_left = change_sp_left, 
                                         sp_right = change_sp_right, 
                                         title_left = change_left_title,
                                         title_right = change_right_title,
                                         pal = pal, 
                                         value_range = value_range)
          
        })
        
        # ACTIVATE expand map ----
        observeEvent(input$expand_change_map, {
          showModal(modalDialog(
            leafletOutput("full_change_map", height = "600px"),
            easyClose = TRUE,
            size = "l",
            footer = modalButton("Exit")
          ))
        })
        
        # output$full_change_map <- renderLeaflet({
        #   
        #   # create the leaflet splitmap
        #   create_leaflet_splitmap_change(sp_left = change_sp_left, 
        #                                  sp_right = change_sp_right, 
        #                                  title_left = change_left_title,
        #                                  title_right = change_right_title,
        #                                  pal = pal, 
        #                                  value_range = value_range)
        #   
        # })
        
        # ACTIVATE map clicks ----
        observeEvent(input$change_sync_map_shape_click, {
          
          # set click events
          click <- input$change_sync_map_shape_click
          lat <- round(click$lat, 6)
          lng <- round(click$lng, 6)
          id <- click$id
          value <- click$value
          
          # update reactive value
          change_sp_react$clicked_cell <- list(X = lat, 
                                               Y = lng,
                                               id = id,
                                               value = value)
          
        })
        
        output$pixel_summary_text2 <- renderUI({
          if (is.null(change_sp_react$clicked_cell)) return(NULL)
          
          cell_id_filter <- change_sp_react$clicked_cell$id
          lat <- change_sp_react$clicked_cell$X
          lng <- change_sp_react$clicked_cell$Y
          
          min_year <- as.numeric(names(predicted_stack[[2]]))
          
          # Pull values for this pixel from your data
          value <- change_sp_left %>% 
            filter(cell_id == cell_id_filter) %>%
            pull(value) %>%
            dplyr::first()
          
          tags$p(
            paste0(
              "You have selected a pixel at (",
              round(lng, 4), ", ", round(lat, 4), "). ",
              "The modeled '", shared_change_type(), " ", shared_timeperiod(), " ", metric_selection(), " Change' at this pixel is ", round(value, 2),
              ". Below are two plots that show the value and trends of this cell through time. ",
              "On the left is a smoothed version of the model, and on the right is a model that includes annual variation."
            ),
            style = "font-size:16px; margin-bottom:15px;"
          )
          
        })
        
        # PLOT pixel trends ----
        pixel_trend_plots_change <- reactive({
          
          # plot if a cell is clicked
          if (is.null(change_sp_react$clicked_cell)) {
            
            return(NULL)
            
          } else {
            
            # identify clicked cell to filter on
            cell_id_filter <- change_sp_react$clicked_cell$id
            #showNotification(paste("Displaying timeseries for cell number:", cell_id_filter), type = "message")
            
          }
          
          # plot the filtered data
          create_pixel_trend_plot(intervention_data = intervention_data(),
                                  selected_metric = selected_metric,
                                  min_year = min_year,
                                  max_year = max_year,
                                  cell_id_filter = cell_id_filter,
                                  predicted_df = predicted_df,
                                  observed_df = observed_df)
          
        })
        
        output$pixel_trend_plot_a_change <- renderGirafe({
          
          req(pixel_trend_plots_change())
          
          girafe(
            ggobj = pixel_trend_plots_change()$left,
            width_svg  = 6,
            height_svg = 5,
            options = list(
              opts_hover(css = "fill: var(--coral);"),
              opts_sizing(rescale = TRUE),
              opts_toolbar(hidden = c("lasso_select", "lasso_deselect")),
              opts_selection(type = "none")
            )
          )
        })
        
        output$pixel_trend_plot_b_change <- renderGirafe({
          
          req(pixel_trend_plots_change())
          
          girafe(
            ggobj = pixel_trend_plots_change()$right,
            width_svg  = 6,
            height_svg = 5,
            options = list(
              opts_hover(css = "fill: var(--coral);"),
              opts_sizing(rescale = TRUE),
              opts_toolbar(hidden = c("lasso_select", "lasso_deselect")),
              opts_selection(type = "none")
            )
          )
        })
        
        # # PLOT pixel change ----
        # output$pixel_trend_plot_change <- renderPlot({
        #   
        #   # plot if a cell is clicked
        #   if (is.null(change_sp_react$clicked_cell)) {
        #     
        #     return(NULL)
        #     
        #   } else {
        #     
        #     # identify clicked cell to filter on
        #     cell_id_filter <- change_sp_react$clicked_cell$id
        #     showNotification(paste("Displaying timeseries for cell number:", cell_id_filter), type = "message")
        #     
        #   }
        #   
        #   # plot the filtered data
        #   create_pixel_trend_plot(intervention_data = intervention_data(),
        #                           selected_metric = selected_metric,
        #                           min_year = min_year,
        #                           max_year = max_year,
        #                           cell_id_filter = cell_id_filter,
        #                           predicted_df = predicted_df,
        #                           observed_df = observed_df)
        #   
        # })
        
      } else {
        
        # DISPLAY warning messages ----
        
        #showNotification("INVALID TYPE: The time period for change type you chose is not available.", type = "error")
        
      } 
      
      # DOWNLOAD data ----
      output$download_change_rasters <- downloadHandler(
        
        filename = function() {
          
          # clean up the island name and create a valid filename
          island <- gsub("[()]", "", island_data()$Island)
          island <- gsub(" ", "_", island)
          metric <- gsub(" ", "", metric_selection())
          paste0(island, "_", metric, toupper(change_type), "_Change_Stack.tif")
          
        },
        
        content = function(file) {
          
          # save the raster stack to the specified file
          writeRaster(change_stack, file, filetype = "GTiff", overwrite = TRUE)
          
        }
        
      )
      
      # DOWNLOAD maps ----
      output$download_change_map <- downloadHandler(
        
        filename = function() {
          
          # clean up the island name and create a valid filename
          island <- gsub("[()]", "", island_data()$Island)
          island <- gsub(" ", "_", island)
          metric <- gsub(" ", "", metric_selection())
          prob <- probability_selection()
          time_type <- timeperiod_selection()
          paste0(island, "_", metric, "_", time_type, prob, "_", toupper(change_type), ".jpg")
          
        },
        
        content = function(file) {
          
          change_static_plot <- generate_static_change_map(
            raster_layer_top = change_stack[[selected_change_layer_left]],
            raster_layer_bottom = change_stack[[selected_change_layer_right]],
            title_top = change_left_title,
            title_bottom = change_right_title,
            island_data = island_data(),
            metric = metric_selection()
          )
          
          # Save the plot
          ggsave(file, plot = change_static_plot, bg = "white", width = 8, height = 4, dpi = 300)
          
        }
        
      )
      
      # DOWNLOAD timeseries ----
      output$download_pixel_plot2 <- downloadHandler(
        
        filename = function() {
          
          # clean up the island name and create a valid filename
          island <- gsub("[()]", "", island_data()$Island)
          island <- gsub(" ", "_", island)
          metric <- gsub(" ", "", metric_selection())
          pixel <- change_sp_react$clicked_cell$id
          paste0(island, "_", metric, "_", pixel, "_Timeseries.jpg")
          
        },
        
        content = function(file) {
          
          pixel_plot2 <- create_pixel_change_plot(
            intervention_data = intervention_data(),
            selected_metric = selected_metric,
            min_year = min_year,
            max_year = max_year,
            cell_id_filter = change_sp_react$clicked_cell$id,
            predicted_df = predicted_df,
            observed_df = observed_df
          )
          
          # Save the plot
          ggsave(file, plot = pixel_plot2, bg = "white", width = 8, height = 4, dpi = 300)
          
        }
        
      )
      
    } else {
      
      #showNotification("INVALIDE METRIC: Due to limited resources, this metric has not been modeled for this island.", type = "error")
    }
    
  })
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                                                            ~~
  ##                          ACCUMULATED MAPS MODULE                         ----
  ##                                                                            ~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # # DISPLAY help button ----
  # observeEvent(input$help_btn_accumulated, {
  #   
  #   showModal(modalDialog(
  #     title = "Help: Modeled Vegetation Change",
  #     tagList(includeMarkdown("text/m4-per-pixel-change.md")),
  #     easyClose = TRUE,
  #     footer = modalButton("Close")
  #   ))
  #   
  # })
  # 
  # # READ data ----
  # observe({
  #   
  #   # require the following inputs:
  #   req(island_data(), metric_selection(), timeperiod_selection(), probability_selection())
  #   
  #   # convert user-friendly names to in-data names
  #   selected_metric <- metric_map[metric_selection()]
  #   metric_col <- paste0(selected_metric, "_fp") 
  #   selected_accumulated <- accumulated_map[timeperiod_selection()]
  #   selected_prob <- as.numeric(probability_selection())
  #   
  #   # extract the raster stack path from the island metadata
  #   predicted_rast_path <-paste0(island_data()[[metric_col]], "/m1_", selected_metric, "_", island_data()[["Res"]], "m_allcells_posterior.mean_annual_prediction.tif")
  #   accumulated_rast_path <-paste0(island_data()[[metric_col]], "/m1_", selected_metric, "_", island_data()[["Res"]], "m_accumulated.tif")
  #   predicted_df_path <- paste0(island_data()[[metric_col]], "/m1_", selected_metric, "_", island_data()[["Res"]], "m_cell_predictions.rds")
  #   observed_df_path <- paste0(island_data()[[metric_col]], "/", selected_metric, "_", island_data()[["Res"]], "m_observed_timeseries.rds")
  #   
  #   # check if the file exists
  #   if (length(accumulated_rast_path) > 0 && file.exists(accumulated_rast_path)) {
  #     
  #     # load the raster stack
  #     accumulated_stack <- rast(accumulated_rast_path)
  #     predicted_stack <- rast(predicted_rast_path)
  #     min_year <- as.numeric(names(predicted_stack[[2]]))
  #     max_year <- as.numeric(names(predicted_stack[[nlyr(predicted_stack) - 2]]))
  #     
  #     # PREP data ----
  #     # read in the associated predicted data rds
  #     predicted_df <- read_rds(predicted_df_path)
  #     observed_df <- read_rds(observed_df_path)
  #     
  #     # select the correct layers based on inputs
  #     selected_accumulated_layer_left <- names(accumulated_stack) == paste0("posterior.mean_", selected_accumulated)
  #     selected_accumulated_layer_right <- names(accumulated_stack) == paste0("posterior.mean_hpdi", selected_prob*100,"_", selected_accumulated)
  #     
  #     # add the cell_id information to a new band in each selected layer
  #     cell_id_accumulated_layer <- names(accumulated_stack) == paste0("cell_id_", selected_accumulated)
  #     
  #     # prep the layers to display in leaflet
  #     if (any(selected_accumulated_layer_left)) {
  #       
  #       accumulated_sp_left <- prep_accumulated_change_tif_for_leaflet(r_stack = accumulated_stack,
  #                                                                   selected_layer = selected_accumulated_layer_left,
  #                                                                   selected_accumulated = selected_accumulated,
  #                                                                   metric_selection = metric_selection())
  #       accumulated_left_title <- paste0("Accumlated ", metric_selection(), ";<br>All Modeled Pixels")
  #       
  #       accumulated_sp_right <- prep_accumulated_change_tif_for_leaflet(r_stack = accumulated_stack, 
  #                                                                    selected_layer = selected_accumulated_layer_right,
  #                                                                    selected_accumulated = selected_accumulated,
  #                                                                    metric_selection = metric_selection()) 
  #       
  #       accumulated_right_title <- paste0("Pixels with Probability > ", selected_prob)
  #       
  #       cell_id_sp <- prep_accumulated_change_tif_for_leaflet(r_stack = accumulated_stack, 
  #                                                           selected_layer = cell_id_accumulated_layer,
  #                                                           metric_selection = metric_selection()) %>% 
  #         rename(cell_id = value)
  #       
  #       accumulated_sp_react <- reactiveValues(sp = cell_id_sp, 
  #                                             clicked_cell = NULL)
  #       
  #       # Set the color palette
  #       value_range <- range(accumulated_sp_left$value, na.rm = TRUE)
  #       
  #       pal <- colorNumeric(palette = "viridis", domain = value_range)
  #       
  #       # PLOT leaflets ----
  #       output$accumulated_sync_map <- renderLeaflet({
  #         
  #         # create the leaflet splitmap
  #         create_leaflet_splitmap_accumulated(sp_left = accumulated_sp_left, 
  #                                            sp_right = accumulated_sp_right, 
  #                                            title_left = accumulated_left_title,
  #                                            title_right = accumulated_right_title,
  #                                            pal = pal, 
  #                                            value_range = value_range)
  #         
  #       })
  #       
  #       # ACTIVATE expand map ----
  #       observeEvent(input$expand_accumulated_map, {
  #         showModal(modalDialog(
  #           leafletOutput("full_accumulated_map", height = "600px"),
  #           easyClose = TRUE,
  #           size = "l",
  #           footer = modalButton("Exit")
  #         ))
  #       })
  #       
  #       output$full_accumulated_map <- renderLeaflet({
  #         
  #         # create the leaflet splitmap
  #         create_leaflet_splitmap_accumulated(sp_left = accumulated_sp_left, 
  #                                            sp_right = accumulated_sp_right, 
  #                                            title_left = accumulated_left_title,
  #                                            title_right = accumulated_right_title,
  #                                            pal = pal, 
  #                                            value_range = value_range)
  #         
  #       })
  #       
  #       # ACTIVATE map clicks ----
  #       observeEvent(input$accumulated_sync_map_shape_click, {
  #         
  #         # set click events
  #         click <- input$accumulated_sync_map_shape_click
  #         lat <- round(click$lat, 6)
  #         lng <- round(click$lng, 6)
  #         id <- click$id
  #         
  #         # update reactive value
  #         accumulated_sp_react$clicked_cell <- list(X = lat, 
  #                                                  Y = lng,
  #                                                  id = id)
  #         
  #       })
  #       
  #       # PLOT pixel accumulated ----
  #       output$pixel_trend_plot_accumulated <- renderPlot({
  #         
  #         # plot if a cell is clicked
  #         if (is.null(accumulated_sp_react$clicked_cell)) {
  #           
  #           return(NULL)
  #           
  #         } else {
  #           
  #           # identify clicked cell to filter on
  #           cell_id_filter <- accumulated_sp_react$clicked_cell$id
  #           showNotification(paste("Displaying timeseries for cell number:", cell_id_filter), type = "message")
  #           
  #         }
  #         
  #         # plot the filtered data
  #         create_pixel_trend_plot(intervention_data = intervention_data(),
  #                                 selected_metric = selected_metric,
  #                                 min_year = min_year,
  #                                 max_year = max_year,
  #                                 cell_id_filter = cell_id_filter,
  #                                 predicted_df = predicted_df,
  #                                 observed_df = observed_df)
  #         
  #       })
  #       
  #     } else {
  #       
  #       # DISPLAY warning messages ----
  #       
  #       showNotification("INVALID TYPE: The time period for accumulated change type you chose is not available.", type = "error")
  #       
  #     } 
  #     
  #     # DOWNLOAD data ----
  #     output$download_accumulated_rasters <- downloadHandler(
  #       
  #       filename = function() {
  #         
  #         # clean up the island name and create a valid filename
  #         island <- gsub("[()]", "", island_data()$Island)
  #         island <- gsub(" ", "_", island)
  #         metric <- gsub(" ", "", metric_selection())
  #         paste0(island, "_", metric, "_Accumulated_Stack.tif")
  #         
  #       },
  #       
  #       content = function(file) {
  #         
  #         # save the raster stack to the specified file
  #         writeRaster(accumulated_stack, file, filetype = "GTiff", overwrite = TRUE)
  #         
  #       }
  #       
  #     )
  #     
  #     # DOWNLOAD maps ----
  #     output$download_accumulated_map <- downloadHandler(
  #       
  #       filename = function() {
  #         
  #         # clean up the island name and create a valid filename
  #         island <- gsub("[()]", "", island_data()$Island)
  #         island <- gsub(" ", "_", island)
  #         metric <- gsub(" ", "", metric_selection())
  #         prob <- probability_selection()
  #         accumulated_type <- timeperiod_selection()
  #         paste0(island, "_", metric, "_", accumulated_type, prob, "_Accumulated.jpg")
  #         
  #       },
  #       
  #       content = function(file) {
  #         
  #         accumulated_static_plot <- generate_static_accumulated_change_map(
  #           raster_layer_top = accumulated_stack[[selected_accumulated_layer_left]],
  #           raster_layer_bottom = accumulated_stack[[selected_accumulated_layer_right]],
  #           title_top = accumulated_left_title,
  #           title_bottom = accumulated_right_title,
  #           island_data = island_data(),
  #           metric = metric_selection()
  #         )
  #         
  #         # Save the plot
  #         ggsave(file, plot = accumulated_static_plot, bg = "white", width = 8, height = 4, dpi = 300)
  #         
  #       }
  #       
  #     )
  #     
  #     # DOWNLOAD timeseries ----
  #     output$download_pixel_plot2 <- downloadHandler(
  #       
  #       filename = function() {
  #         
  #         # clean up the island name and create a valid filename
  #         island <- gsub("[()]", "", island_data()$Island)
  #         island <- gsub(" ", "_", island)
  #         metric <- gsub(" ", "", metric_selection())
  #         pixel <- accumulated_sp_react$clicked_cell$id
  #         paste0(island, "_", metric, "_", pixel, "_Timeseries.jpg")
  #         
  #       },
  #       
  #       content = function(file) {
  #         
  #         pixel_plot2 <- create_pixel_accumulated_plot(
  #           intervention_data = intervention_data(),
  #           selected_metric = selected_metric,
  #           min_year = min_year,
  #           max_year = max_year,
  #           cell_id_filter = accumulated_sp_react$clicked_cell$id,
  #           predicted_df = predicted_df,
  #           observed_df = observed_df
  #         )
  #         
  #         # Save the plot
  #         ggsave(file, plot = pixel_plot2, bg = "white", width = 8, height = 4, dpi = 300)
  #         
  #       }
  #       
  #     )
  #     
  #   } else {
  #     
  #     showNotification("INVALIDE METRIC: Due to limited resources, this metric has not been modeled for this island.", type = "error")
  #   }
  #   
  # })
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                                                            ~~
  ##                       RESTRORATION ACTION MAP MODULE                     ----
  ##                                                                            ~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  #   # DISPLAY help button ----
  #   observeEvent(input$help_btn_ram, {
  # 
  #     showModal(modalDialog(
  #       title = "Help: Restoration Action Map",
  #       tagList(includeMarkdown("text/m5-restoration-action-map.md")),
  #       easyClose = TRUE,
  #       footer = modalButton("Close")
  #     ))
  # 
  #   })
  # 
  #   # CONTROl metric inputs ----
  #   observeEvent(input$ram_tc_show, {
  #     if (input$ram_tc_show) {
  #       updateCheckboxInput(session, "ram_ndmi_show", value = FALSE)
  #       updateCheckboxInput(session, "ram_prod_show", value = FALSE)
  #     }
  #   })
  # 
  #   observeEvent(input$ram_ndmi_show, {
  #     if (input$ram_ndmi_show) {
  #       updateCheckboxInput(session, "ram_tc_show", value = FALSE)
  #       updateCheckboxInput(session, "ram_prod_show", value = FALSE)
  #     }
  #   })
  # 
  #   observeEvent(input$ram_prod_show, {
  #     if (input$ram_prod_show) {
  #       updateCheckboxInput(session, "ram_tc_show", value = FALSE)
  #       updateCheckboxInput(session, "ram_ndmi_show", value = FALSE)
  #     }
  #   })
  # 
  #   # TOGGLE inputs ----
  #   shinyjs::onclick("toggle_tc_ram", shinyjs::toggle(id = "tc_ram", anim = TRUE))
  #   shinyjs::onclick("toggle_ndmi_ram", shinyjs::toggle(id = "ndmi_ram", anim = TRUE))
  #   shinyjs::onclick("toggle_prod_ram", shinyjs::toggle(id = "prod_ram", anim = TRUE))
  #   shinyjs::onclick("toggle_ter_ram", shinyjs::toggle(id = "ter_ram", anim = TRUE))
  # 
  #   # ACTIVATE year ----
  #   year_choice_ram <- reactive({
  # 
  #     req(input$ram_year)
  # 
  #     as.numeric(format(input$ram_year, "%Y"))
  # 
  #   })
  # 
  #   # ACTIVATE probability levels ----
  #   probability_level_ram_tc <- reactive({
  # 
  #     req(input$probability_ram_tc)
  # 
  #     input$probability_ram_tc
  # 
  #   })
  #   probability_level_ram_ndmi <- reactive({
  # 
  #     req(input$probability_ram_ndmi)
  # 
  #     input$probability_ram_ndmi
  # 
  #   })
  #   probability_level_ram_prod <- reactive({
  # 
  #     req(input$probability_ram_prod)
  # 
  #     input$probability_ram_prod
  # 
  #   })
  # 
  #   # ACTIVATE tc range ----
  #   tc_min_ram <- reactive({
  # 
  #     req(input$ram_tc_range)
  # 
  #     input$ram_tc_range[1]
  # 
  #   })
  # 
  #   tc_max_ram <- reactive({
  # 
  #     req(input$ram_tc_range)
  # 
  #     input$ram_tc_range[2]
  # 
  #   })
  # 
  #   # ACTIVATE annualized tc range ----
  #   tc_ann_min_ram <- reactive({
  # 
  #     req(input$ram_tc_annualized_range)
  # 
  #     input$ram_tc_annualized_range[1]
  # 
  #   })
  # 
  #   tc_ann_max_ram <- reactive({
  # 
  #     req(input$ram_tc_annualized_range)
  # 
  #     input$ram_tc_annualized_range[2]
  # 
  #   })
  # 
  #   # ACTIVATE accumulated tc range ----
  #   tc_acc_min_ram <- reactive({
  # 
  #     req(input$ram_tc_accumulated_range)
  # 
  #     input$ram_tc_accumulated_range[1]
  # 
  #   })
  # 
  #   tc_acc_max_ram <- reactive({
  # 
  #     req(input$ram_tc_accumulated_range)
  # 
  #     input$ram_tc_accumulated_range[2]
  # 
  #   })
  # 
  #   # ACTIVATE ndmi range ----
  #   ndmi_min_ram <- reactive({
  # 
  #     req(input$ram_ndmi_range)
  # 
  #     input$ram_ndmi_range[1]
  # 
  #   })
  # 
  #   ndmi_max_ram <- reactive({
  # 
  #     req(input$ram_ndmi_range)
  # 
  #     input$ram_ndmi_range[2]
  # 
  #   })
  # 
  #   # ACTIVATE annualized ndmi range ----
  #   ndmi_ann_min_ram <- reactive({
  # 
  #     req(input$ram_ndmi_annualized_range)
  # 
  #     input$ram_ndmi_annualized_range[1]
  # 
  #   })
  # 
  #   ndmi_ann_max_ram <- reactive({
  # 
  #     req(input$ram_ndmi_annualized_range)
  # 
  #     input$ram_ndmi_annualized_range[2]
  # 
  #   })
  # 
  #   # ACTIVATE accumulated ndmi range ----
  #   ndmi_acc_min_ram <- reactive({
  # 
  #     req(input$ram_ndmi_accumulated_range)
  # 
  #     input$ram_ndmi_accumulated_range[1]
  # 
  #   })
  # 
  #   ndmi_acc_max_ram <- reactive({
  # 
  #     req(input$ram_ndmi_accumulated_range)
  # 
  #     input$ram_ndmi_accumulated_range[2]
  # 
  #   })
  # 
  #   # ACTIVATE prod range ----
  #   prod_min_ram <- reactive({
  # 
  #     req(input$ram_prod_range)
  # 
  #     input$ram_prod_range[1]
  # 
  #   })
  # 
  #   prod_max_ram <- reactive({
  # 
  #     req(input$ram_prod_range)
  # 
  #     input$ram_prod_range[2]
  # 
  #   })
  # 
  #   # ACTIVATE annualized prod range ----
  #   prod_ann_min_ram <- reactive({
  # 
  #     req(input$ram_prod_annualized_range)
  # 
  #     input$ram_prod_annualized_range[1]
  # 
  #   })
  # 
  #   prod_ann_max_ram <- reactive({
  # 
  #     req(input$ram_prod_annualized_range)
  # 
  #     input$ram_prod_annualized_range[2]
  # 
  #   })
  # 
  #   # ACTIVATE accumulated prod range ----
  #   prod_acc_min_ram <- reactive({
  # 
  #     req(input$ram_prod_accumulated_range)
  # 
  #     input$ram_prod_accumulated_range[1]
  # 
  #   })
  # 
  #   prod_acc_max_ram <- reactive({
  # 
  #     req(input$ram_prod_accumulated_range)
  # 
  #     input$ram_prod_accumulated_range[2]
  # 
  #   })
  # 
  #   # ACTIVATE slope range ----
  #   slope_min_ram <- reactive({
  # 
  #     req(input$ram_slope_range)
  # 
  #     input$ram_slope_range[1]
  # 
  #   })
  # 
  #   slope_max_ram <- reactive({
  # 
  #     req(input$ram_slope_range)
  # 
  #     input$ram_slope_range[2]
  # 
  #   })
  # 
  #   # ACTIVATE elevation range ----
  #   elevation_min_ram <- reactive({
  # 
  #     req(input$ram_elevation_range)
  # 
  #     input$ram_elevation_range[1]
  # 
  #   })
  # 
  #   elevation_max_ram <- reactive({
  # 
  #     req(input$ram_elevation_range)
  # 
  #     input$ram_elevation_range[2]
  # 
  #   })
  # 
  #   # ACTIVATE twi range ----
  #   twi_min_ram <- reactive({
  # 
  #     req(input$ram_twi_range)
  # 
  #     input$ram_twi_range[1]
  # 
  #   })
  # 
  #   twi_max_ram <- reactive({
  # 
  #     req(input$ram_twi_range)
  # 
  #     input$ram_twi_range[2]
  # 
  #   })
  # 
  #   # ACTIVATE aspect  ----
  #   ram_aspect_codes <- reactive({
  # 
  #     req(input$ram_aspect_type)
  # 
  #     aspect_map[input$ram_aspect_type]
  # 
  #   })
  # 
  #   # ACTIVATE ecosystem  ----
  #   ram_ecosystem_codes <- reactive({
  # 
  #     req(input$ram_eco_type)
  # 
  #     ecosystem_map[input$ram_eco_type]
  # 
  #   })
  # 
  #   # ACTIVATE expand map ----
  #   observeEvent(input$expand_action_map, {
  #     showModal(modalDialog(
  #       leafletOutput("full_action_map", height = "600px"),
  #       easyClose = TRUE,
  #       size = "l",
  #       footer = modalButton("Exit")
  #     ))
  #   })
  # 
  #   # ram_data <- reactiveVal()
  #   #
  #   # observeEvent(input$update_restoration_action_map, {
  #   #
  #   #   action_map_data <- create_action_map(
  #   #     dir = data_dir,
  #   #     island_data = island_data(),
  #   #     ram_year = year_choice_ram(),
  #   #     time_period = timeperiod_selection(),
  #   #     tc_min = tc_min_ram(),
  #   #     tc_max = tc_max_ram(),
  #   #     probability_level_tc = probability_level_ram_tc(),
  #   #     tc_ann_min = tc_ann_min_ram(),
  #   #     tc_ann_max = tc_ann_max_ram(),
  #   #     tc_acc_min = tc_acc_min_ram(),
  #   #     tc_acc_max = tc_acc_max_ram(),
  #   #     ndmi_min = ndmi_min_ram(),
  #   #     ndmi_max = ndmi_max_ram(),
  #   #     probability_level_ndmi = probability_level_ram_ndmi(),
  #   #     ndmi_ann_min = ndmi_ann_min_ram(),
  #   #     ndmi_ann_max = ndmi_ann_max_ram(),
  #   #     ndmi_acc_min = ndmi_acc_min_ram(),
  #   #     ndmi_acc_max = ndmi_acc_max_ram(),
  #   #     prod_min = prod_min_ram(),
  #   #     prod_max = prod_max_ram(),
  #   #     probability_level_prod = probability_level_ram_prod(),
  #   #     prod_ann_min = prod_ann_min_ram(),
  #   #     prod_ann_max = prod_ann_max_ram(),
  #   #     prod_acc_min = prod_acc_min_ram(),
  #   #     prod_acc_max = prod_acc_max_ram(),
  #   #     slope_min = slope_min_ram(),
  #   #     slope_max = slope_max_ram(),
  #   #     elev_min = elevation_min_ram(),
  #   #     elev_max = elevation_max_ram(),
  #   #     aspect_num = ram_aspect_codes(),
  #   #     twi_min = twi_min_ram(),
  #   #     twi_max = twi_max_ram(),
  #   #     eco_num = ram_ecosystem_codes()
  #   #   )
  #   #
  #   #   ram_data(action_map_data)
  #   #   output$restoration_action_map <- renderLeaflet({
  #   #     action_map_data$ram
  #   #   })
  #   #
  #   #
  #   # })
  # 
  #   # THIS ONE ALMOST WORKS
  #   # output$restoration_action_map <- renderLeaflet({
  #   #
  #   #   input$update_restoration_action_map
  #   #   isolate({
  #   #
  #   #     create_action_map(
  #   #             dir = data_dir,
  #   #             island_data = island_data(),
  #   #             ram_year = year_choice_ram(),
  #   #             time_period = timeperiod_selection(),
  #   #             tc_min = tc_min_ram(),
  #   #             tc_max = tc_max_ram(),
  #   #             probability_level_tc = probability_level_ram_tc(),
  #   #             tc_ann_min = tc_ann_min_ram(),
  #   #             tc_ann_max = tc_ann_max_ram(),
  #   #             tc_acc_min = tc_acc_min_ram(),
  #   #             tc_acc_max = tc_acc_max_ram(),
  #   #             ndmi_min = ndmi_min_ram(),
  #   #             ndmi_max = ndmi_max_ram(),
  #   #             probability_level_ndmi = probability_level_ram_ndmi(),
  #   #             ndmi_ann_min = ndmi_ann_min_ram(),
  #   #             ndmi_ann_max = ndmi_ann_max_ram(),
  #   #             ndmi_acc_min = ndmi_acc_min_ram(),
  #   #             ndmi_acc_max = ndmi_acc_max_ram(),
  #   #             prod_min = prod_min_ram(),
  #   #             prod_max = prod_max_ram(),
  #   #             probability_level_prod = probability_level_ram_prod(),
  #   #             prod_ann_min = prod_ann_min_ram(),
  #   #             prod_ann_max = prod_ann_max_ram(),
  #   #             prod_acc_min = prod_acc_min_ram(),
  #   #             prod_acc_max = prod_acc_max_ram(),
  #   #             slope_min = slope_min_ram(),
  #   #             slope_max = slope_max_ram(),
  #   #             elev_min = elevation_min_ram(),
  #   #             elev_max = elevation_max_ram(),
  #   #             aspect_num = ram_aspect_codes(),
  #   #             twi_min = twi_min_ram(),
  #   #             twi_max = twi_max_ram(),
  #   #             eco_num = ram_ecosystem_codes()
  #   #           )
  #   #
  #   #   })
  #   #
  #   # })
  #   ram_data <- reactive({
  # 
  #     input$update_restoration_action_map
  # 
  #     isolate({
  # 
  #           create_action_map(
  #                   dir = data_dir,
  #                   island_data = island_data(),
  #                   ram_year = year_choice_ram(),
  #                   time_period = timeperiod_selection(),
  #                   tc_min = tc_min_ram(),
  #                   tc_max = tc_max_ram(),
  #                   probability_level_tc = probability_level_ram_tc(),
  #                   tc_ann_min = tc_ann_min_ram(),
  #                   tc_ann_max = tc_ann_max_ram(),
  #                   tc_acc_min = tc_acc_min_ram(),
  #                   tc_acc_max = tc_acc_max_ram(),
  #                   ndmi_min = ndmi_min_ram(),
  #                   ndmi_max = ndmi_max_ram(),
  #                   probability_level_ndmi = probability_level_ram_ndmi(),
  #                   ndmi_ann_min = ndmi_ann_min_ram(),
  #                   ndmi_ann_max = ndmi_ann_max_ram(),
  #                   ndmi_acc_min = ndmi_acc_min_ram(),
  #                   ndmi_acc_max = ndmi_acc_max_ram(),
  #                   prod_min = prod_min_ram(),
  #                   prod_max = prod_max_ram(),
  #                   probability_level_prod = probability_level_ram_prod(),
  #                   prod_ann_min = prod_ann_min_ram(),
  #                   prod_ann_max = prod_ann_max_ram(),
  #                   prod_acc_min = prod_acc_min_ram(),
  #                   prod_acc_max = prod_acc_max_ram(),
  #                   slope_min = slope_min_ram(),
  #                   slope_max = slope_max_ram(),
  #                   elev_min = elevation_min_ram(),
  #                   elev_max = elevation_max_ram(),
  #                   aspect_num = ram_aspect_codes(),
  #                   twi_min = twi_min_ram(),
  #                   twi_max = twi_max_ram(),
  #                   eco_num = ram_ecosystem_codes()
  #                 )
  # 
  #     })
  # 
  #   })
  # 
  #   output$restoration_action_map <- renderLeaflet({
  #     ram_data()$ram
  #   })
  # 
  #   observe({
  #     leafletProxy("restoration_action_map") %>% clearControls()
  # 
  #     if ("Filtered Predicted TC" %in% input$restoration_action_map_groups) {
  #       leafletProxy("restoration_action_map") %>%
  #         addLegend(pal = ram_data()$pal_tc_pred,
  #                   values = ram_data()$filtered_predicted_tc_sf[[1]],
  #                   title = "Predicted TC",
  #                   position = "bottomleft")
  #     }
  # 
  #     if ("Filtered Annualized TC Change" %in% input$restoration_action_map_groups) {
  #           leafletProxy("restoration_action_map") %>%
  #             addLegend(pal = ram_data()$pal_tc_ann,
  #                       values = ram_data()$filtered_annualized_tc_sf[[1]],
  #                       title = "Annualized TC Change",
  #                       position = "bottomleft")
  #         }
  # 
  #         if ("Filtered Accumulated TC Change" %in% input$restoration_action_map_groups) {
  #           leafletProxy("restoration_action_map") %>%
  #             addLegend(pal = ram_data()$pal_tc_acc,
  #                       values = ram_data()$filtered_accumulated_tc_sf[[1]],
  #                       title = "Accumulated TC Change",
  #                       position = "bottomleft")
  #         }
  # 
  #         if ("Filtered Predicted NDMI" %in% input$restoration_action_map_groups) {
  #           leafletProxy("restoration_action_map") %>%
  #             addLegend(pal = ram_data()$pal_ndmi_pred,
  #                       values = ram_data()$filtered_predicted_ndmi_sf[[1]],
  #                       title = "Predicted NDMI",
  #                       position = "bottomleft")
  #         }
  # 
  #         if ("Filtered Annualized NDMI Change" %in% input$restoration_action_map_groups) {
  #           leafletProxy("restoration_action_map") %>%
  #             addLegend(pal = ram_data()$pal_ndmi_ann,
  #                       values = ram_data()$filtered_annualized_ndmi_sf[[1]],
  #                       title = "Annualized NDMI Change",
  #                       position = "bottomleft")
  #         }
  # 
  #         if ("Filtered Accumulated NDMI Change" %in% input$restoration_action_map_groups) {
  #           leafletProxy("restoration_action_map") %>%
  #             addLegend(pal = ram_data()$pal_ndmi_acc,
  #                       values = ram_data()$filtered_accumulated_ndmi_sf[[1]],
  #                       title = "Accumulated NDMI Change",
  #                       position = "bottomleft")
  #         }
  # 
  #         if ("Filtered Predicted Productivity" %in% input$restoration_action_map_groups) {
  #           leafletProxy("restoration_action_map") %>%
  #             addLegend(pal = ram_data()$pal_prod_pred,
  #                       values = ram_data()$filtered_predicted_prod_sf[[1]],
  #                       title = "Predicted Productivity",
  #                       position = "bottomleft")
  #         }
  # 
  #         if ("Filtered Annualized Productivity Change" %in% input$restoration_action_map_groups) {
  #           leafletProxy("restoration_action_map") %>%
  #             addLegend(pal = ram_data()$pal_prod_ann,
  #                       values = ram_data()$filtered_annualized_prod_sf[[1]],
  #                       title = "Annualized Productivity Change",
  #                       position = "bottomleft")
  #         }
  # 
  #         if ("Filtered Accumulated Productivity Change" %in% input$restoration_action_map_groups) {
  #           leafletProxy("restoration_action_map") %>%
  #             addLegend(pal = ram_data()$pal_prod_acc,
  #                       values = ram_data()$filtered_accumulated_prod_sf[[1]],
  #                       title = "Accumulated Productivity Change",
  #                       position = "bottomleft")
  #         }
  # 
  #         if ("Filtered Slope" %in% input$restoration_action_map_groups) {
  #           leafletProxy("restoration_action_map") %>%
  #             addLegend(pal = ram_data()$pal_slope,
  #                       values = ram_data()$filtered_slope_sf[[1]],
  #                       title = "Slope",
  #                       position = "bottomleft")
  #         }
  # 
  #         if ("Filtered Elevation" %in% input$restoration_action_map_groups) {
  #           leafletProxy("restoration_action_map") %>%
  #             addLegend(pal = ram_data()$pal_dem,
  #                       values = ram_data()$filtered_dem_sf[[1]],
  #                       title = "Elevation",
  #                       position = "bottomleft")
  #         }
  # 
  #         if ("Filtered TWI" %in% input$restoration_action_map_groups) {
  #           leafletProxy("restoration_action_map") %>%
  #             addLegend(pal = ram_data()$pal_twi,
  #                       values = ram_data()$filtered_twi_sf[[1]],
  #                       title = "Topographic Wetness Index",
  #                       position = "bottomleft")
  #         }
  # 
  #         if ("Filtered Aspect" %in% input$restoration_action_map_groups) {
  #           leafletProxy("restoration_action_map") %>%
  #             addLegend(pal = ram_data()$pal_aspect,
  #                       values = ram_data()$filtered_aspect_sf[[1]],
  #                       title = "Aspect",
  #                       position = "bottomleft")
  #         }
  # 
  #         if ("Filtered Ecosystem" %in% input$restoration_action_map_groups) {
  #           leafletProxy("restoration_action_map") %>%
  #             addLegend(pal = ram_data()$pal_eco,
  #                       values = ram_data()$filtered_eco_sf[[1]],
  #                       title = "Ecosystem",
  #                       position = "bottomleft")
  #         }
  # 
  #   })
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                                                            ~~
  ##                                 ABOUT INFO                               ----
  ##                                                                            ~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # TOGGLE inputs ----
  shinyjs::onclick("toggle_metric_info", shinyjs::toggle(id = "metric_info", anim = TRUE))
  
}

# CALL server ----
server