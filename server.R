# server.R

# START server instructions ----
server <- function(input, output, session) {
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                                                            ~~
  ##                                  HELPERS                                 ----
  ##                                                                            ~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Keep a set of duplicate inputs (spread across tabs) synced to one shared
  # reactive value, so a change to any one of them updates all the others.
  sync_shared_choice <- function(input_ids, shared_val) {
    for (id in input_ids) {
      local({
        this_id <- id
        observeEvent(input[[this_id]], {
          if (!identical(input[[this_id]], shared_val())) {
            shared_val(input[[this_id]])
          }
        })
      })
    }
    observe({
      val <- shared_val()
      for (id in input_ids) {
        updateSelectInput(session, id, selected = val)
      }
    })
  }

  # Format a modeled value so it stays readable whatever the metric and change
  # type is. A fixed two decimal places rounded annualized rates straight to
  # zero -- they are per-year figures a few thousandths in size -- while
  # accumulated tree cover change runs past 17. Showing a set number of
  # significant figures instead suits both: 0.00382, and -17.78.
  format_modeled_value <- function(x, sig = 3) {

    if (length(x) == 0 || is.na(x) || !is.finite(x)) return("not available")

    decimals <- if (x == 0) 2 else max(2, min(8, sig - floor(log10(abs(x))) - 1))

    formatC(x, format = "f", digits = decimals)

  }

  # Repopulate a dropdown's choices while keeping whatever the user already
  # picked, as long as it is still on offer. These lists are rebuilt whenever a
  # parent input changes; clearing the selection every time threw away choices
  # that were still perfectly valid and left the map with nothing to draw.
  update_choices_keeping_selection <- function(input_id, choices) {
    current <- isolate(input[[input_id]])
    still_offered <- !is.null(current) &&
      nzchar(current) &&
      current %in% as.character(choices)

    updateSelectInput(
      session,
      input_id,
      choices = c("", choices),
      selected = if (still_offered) current else ""
    )
  }

  # One sentence describing the pair of pixel timeseries plots, so every tab
  # that shows them introduces them the same way.
  pixel_trend_blurb <- function() {
    paste0(
      "Below are two plots that show the value of this cell through time. ",
      "On the left is a smoothed version of the model, and on the right is a ",
      "model that includes annual variation."
    )
  }

  # Draw the pair of pixel timeseries plots from the inputs a view captured
  # when its map was drawn. Reading the live reactives instead would let a
  # clicked pixel be looked up in another island's data: the selection
  # survives until the map is reloaded, but the data behind it would switch
  # the moment a dropdown moved.
  pixel_trend_plots_for <- function(trend, cell_id) {
    create_pixel_trend_plot(
      intervention_data = trend$intervention,
      selected_metric = trend$metric,
      min_year = trend$years$min,
      max_year = trend$years$max,
      cell_id_filter = cell_id,
      predicted_df = trend$predicted,
      observed_df = trend$observed
    )
  }

  # Every tab draws its pixel timeseries at the same size and with the same
  # interactions, so the girafe() options live here rather than in each tab.
  girafe_pixel_trend <- function(gg) {
    girafe(
      ggobj = gg,
      width_svg = 6,
      height_svg = 5,
      options = list(
        opts_hover(css = "fill: var(--coral);"),
        opts_sizing(rescale = TRUE),
        opts_toolbar(hidden = c("lasso_select", "lasso_deselect")),
        opts_selection(type = "none")
      )
    )
  }

  # Shared island info + modeled-eradications table markup, used by both the
  # Per-Pixel Annual tab and the Change Maps tab.
  render_island_info <- function(island) {
    req(island)

    chosen_isl_dat <- island_metadata %>%
      filter(Island == island)

    selected_gid2 <- chosen_isl_dat$GID2_ID

    chosen_isl_erads <- intervention_metadata %>%
      filter(GID2 == selected_gid2) %>%
      mutate(
        Species = paste(Genus, Species, sep = " "),
        Type = RemTyp,
        Year = IntYr
      ) %>%
      select(Type, Species, Year)

    erads_table <- chosen_isl_erads %>%
      knitr::kable(
        format = "html",
        escape = FALSE,
        col.names = c("Type", "Species", "Year")
      ) %>%
      kableExtra::kable_styling(
        bootstrap_options = c("hover", "condensed", "responsive"),
        full_width = TRUE
      ) %>%
      kableExtra::row_spec(0, bold = TRUE, background = light_neutral)

    for (i in seq_len(nrow(chosen_isl_erads))) {
      color <- ifelse(i %% 2 == 0, light_neutral, "white")
      erads_table <- kableExtra::row_spec(erads_table, i, background = color)
    }

    tagList(
      p(strong("Island:"), str_replace(chosen_isl_dat$Isl, "_", " ")),
      p(strong("Country:"), chosen_isl_dat$COUNTRY),
      p(strong("Area:"), paste0(round(chosen_isl_dat$AreaKm2, 2), " km²")),
      tags$p(strong("Modeled Eradications:")),
      HTML(erads_table)
    )
  }

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                                                            ~~
  ##                           OVERVIEW ISLAND CATALOGUE                      ----
  ##                                                                            ~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The map and the table are two views of one filtered set, so both read the
  # same reactive. island_overview itself is built once in global.R.

  # FILTER the catalogue ----
  overview_filtered <- reactive({
    out <- island_overview

    search <- if (is.null(input$ov_search)) "" else trimws(input$ov_search)
    if (nzchar(search)) {
      out <- out[grepl(search, out$island, ignore.case = TRUE), ]
    }

    if (length(input$ov_country)) {
      out <- out[out$country %in% input$ov_country, ]
    }

    # any of the chosen metrics, not all: picking two metrics is a question
    # about either of them being available, not about islands that have both
    if (length(input$ov_metric)) {
      pattern <- paste0(
        "\\b", gsub(" ", "\\\\s", input$ov_metric), "\\b", collapse = "|"
      )
      out <- out[grepl(pattern, out$metrics_available), ]
    }

    if (length(input$ov_intervention)) {
      keep <- vapply(
        out$intervention_types,
        function(types) {
          any(input$ov_intervention %in% trimws(strsplit(types, ",")[[1]]))
        },
        logical(1)
      )
      out <- out[keep, ]
    }

    out
  })

  observeEvent(input$ov_reset, {
    updateTextInput(session, "ov_search", value = "")
    updateSelectInput(session, "ov_country", selected = character(0))
    updateSelectInput(session, "ov_metric", selected = character(0))
    updateSelectInput(session, "ov_intervention", selected = character(0))
  })

  # DISPLAY how much of the catalogue is left ----
  output$ov_count <- renderUI({
    n <- nrow(overview_filtered())

    tags$p(
      class = "filter-count",
      tags$strong(n),
      if (n == 1) " island" else " islands",
      " of ", nrow(island_overview)
    )
  })

  # DISPLAY island map ----
  # Rendered once; the markers are swapped by the proxy below so that filtering
  # does not throw away the pan and zoom the reader has set.
  output$overview_map <- renderLeaflet({
    create_overview_map()
  })

  observe({
    draw_overview_markers(
      leafletProxy("overview_map"),
      overview_filtered()
    )
  })

  # OPEN a module ----
  # Sent by two things in www/overview-map.js: the buttons inside a map popup,
  # which name an island, and the shortcut cards above the catalogue, which do
  # not. Both switch tab; only the first changes the selection.
  observeEvent(input$ov_open_module, {
    choice <- input$ov_open_module
    island <- choice$island
    tab <- choice$tab

    # both are built from the app's own lists, so this should always hold;
    # refusing anything else keeps a stale page from a previous session from
    # putting an unknown value into the island selectors
    req(tab %in% overview_module_tabs)

    # The island goes into every module's dropdown, not only the one being
    # opened. sync_shared_choice keeps them in step anyway, and a reader who
    # follows one module with another should not have to pick twice.
    if (!is.null(island) && island %in% island_overview$island) {
      for (input_id in c(
        "island_choice1",
        "island_choice2",
        "island_choice3",
        "island_choice4",
        "island_choice5"
      )) {
        updateSelectInput(session, input_id, selected = island)
      }
    }

    updateTabsetPanel(session, "module_tabs", selected = tab)
  })

  # DISPLAY island table ----
  output$overview_table <- renderUI({
    islands <- overview_filtered()

    if (nrow(islands) == 0) {
      return(tags$p(
        class = "empty-table-note",
        "No island matches these filters. Try clearing one of them."
      ))
    }

    shown <- islands %>%
      transmute(
        Island = short_name,
        Country = country,
        `Area (km²)` = round(area_km2, 1),
        `Pixel size (m)` = resolution_m,
        `Metrics available` = metrics_available,
        `Intervention modeled` = intervention_types
      )

    # row.names = FALSE explicitly: filtering with `[` keeps the original row
    # names, so without this kable prints them as an unlabelled first column
    # of row numbers that jump around as the filters change.
    tbl <- shown %>%
      knitr::kable(format = "html", escape = FALSE, row.names = FALSE) %>%
      kableExtra::kable_styling(
        bootstrap_options = c("hover", "condensed", "responsive"),
        full_width = TRUE
      ) %>%
      kableExtra::row_spec(0, bold = TRUE, background = light_neutral)

    for (i in seq_len(nrow(shown))) {
      color <- ifelse(i %% 2 == 0, light_neutral, "white")
      tbl <- kableExtra::row_spec(tbl, i, background = color)
    }

    tagList(
      tags$h4(tags$strong("Island Details")),
      div(class = "overview-table-scroll", HTML(tbl))
    )
  })

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                                                            ~~
  ##                             ISLAND-WIDE MODULE                           ----
  ##                                                                            ~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # ACTIVATE island ----
  shared_island <- reactiveVal("")
  sync_shared_choice(
    c(
      "island_choice1",
      "island_choice2",
      "island_choice3",
      "island_choice4",
      "island_choice5"
    ),
    shared_island
  )

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
  sync_shared_choice(
    c("metric_choice1", "metric_choice2", "metric_choice3", "metric_choice4"),
    shared_metric
  )

  metric_selection <- reactive({
    island <- shared_island()
    req(island)

    # req() alone was not enough for an island with nothing modeled. It
    # cancels an output without clearing it, so the maps and plots below went
    # on showing the island the reader had been looking at before, next to an
    # info panel that had already switched to the new one. validate() puts the
    # reason there instead.
    validate(need(
      length(metrics_for_island(island)) > 0,
      paste0(
        "No vegetation metrics have been modeled for ", island, " yet, so ",
        "there is nothing to map here. The island catalogue on the Overview ",
        "tab lists the metrics each island has."
      )
    ))

    validate(need(
      nzchar(shared_metric()),
      "Select a metric to draw the map."
    ))

    shared_metric()
  })

  update_metric_choices <- function(island_input_id, metric_input_id) {
    observeEvent(input[[island_input_id]], {
      req(input[[island_input_id]])

      display_metrics <- metrics_for_island(input[[island_input_id]])

      if (length(display_metrics) == 0) {
        # c("", character(0)) is just "", so an island with nothing modeled
        # left this a blank box holding one nameless blank option -- it reads
        # as a broken control rather than as an answer. Naming that option
        # says what happened. Its value stays "", so every
        # req(shared_metric()) downstream still halts exactly as it did.
        updateSelectInput(
          session,
          metric_input_id,
          choices = c("No metrics modeled for this island yet" = ""),
          selected = ""
        )

        return()
      }

      # Update the corresponding metric input, keeping the current metric if
      # the newly selected island also has it
      update_choices_keeping_selection(metric_input_id, display_metrics)
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
  ##         SHARED PREDICTED (ANNUAL) DATA -- used by both modules below     ----
  ##                                                                            ~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The Per-Pixel Annual tab and the Change Maps tab both need the annual
  # predicted raster stack + its associated cell-level data frames for the
  # currently selected island/metric. Loading them here -- once per
  # island/metric combination -- means switching years, time periods,
  # probability levels, or change types (or just switching tabs) never
  # re-reads these files from disk unless the island or metric actually changed.

  metric_paths <- reactive({
    req(island_data(), metric_selection())

    selected_metric <- metric_map[[metric_selection()]]
    metric_col <- paste0(selected_metric, "_fp")

    list(
      metric = selected_metric,
      base = island_data()[[metric_col]],
      res = island_data()[["Res"]],
      predicted_rast = paste0(
        island_data()[[metric_col]],
        "/m1_",
        selected_metric,
        "_",
        island_data()[["Res"]],
        "m_allcells_posterior.mean_annual_prediction.tif"
      ),
      predicted_df = paste0(
        island_data()[[metric_col]],
        "/m1_",
        selected_metric,
        "_",
        island_data()[["Res"]],
        "m_cell_predictions.rds"
      ),
      observed_df = paste0(
        island_data()[[metric_col]],
        "/",
        selected_metric,
        "_",
        island_data()[["Res"]],
        "m_observed_timeseries.rds"
      )
    )
  })

  predicted_stack <- reactive({
    paths <- metric_paths()

    validate(need(
      length(paths$predicted_rast) > 0 && file.exists(paths$predicted_rast),
      "This metric has not yet been modeled for this island."
    ))

    rast(paths$predicted_rast)
  })

  predicted_df <- reactive({
    read_rds(metric_paths()$predicted_df)
  })

  observed_df <- reactive({
    read_rds(metric_paths()$observed_df)
  })

  # The year layers run from the second layer (the first is cell_id) through
  # the last one. Stopping two layers short of the end clipped the two most
  # recent years off every pixel trend plot's x axis.
  predicted_year_range <- reactive({
    stack <- predicted_stack()

    list(
      min = as.numeric(names(stack[[2]])),
      max = as.numeric(names(stack[[nlyr(stack)]]))
    )
  })

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
      title = "How to read this map: Per-Pixel Annual Vegetation",
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

    # Update both year dropdowns, keeping the chosen years where the new
    # island/metric still offers them
    year_choices <- sort(unique(available_years))

    update_choices_keeping_selection("year_choice_left", year_choices)
    update_choices_keeping_selection("year_choice_right", year_choices)
  })

  # DISPLAY island info ----
  output$island_info <- renderUI({
    render_island_info(shared_island())
  })

  # PREP layers ----
  # Each of these only recomputes when its own dependency changes -- e.g.
  # changing year_choice_right no longer re-reads the raster stack from disk
  # or re-preps the left-hand layer.

  annual_layer_left <- reactive({
    stack <- predicted_stack()
    selected_layer <- names(stack) == as.character(year_choice_left())

    validate(need(
      any(selected_layer),
      "The selected year is not available for this metric."
    ))

    prep_tif_for_leaflet(
      r_stack = stack,
      selected_layer = selected_layer,
      metric_selection = metric_selection()
    )
  })

  annual_layer_right <- reactive({
    stack <- predicted_stack()
    selected_layer <- names(stack) == as.character(year_choice_right())

    validate(need(
      any(selected_layer),
      "The selected year is not available for this metric."
    ))

    prep_tif_for_leaflet(
      r_stack = stack,
      selected_layer = selected_layer,
      metric_selection = metric_selection()
    )
  })

  # SET mapping aesthetics according to map type (tree cover vs spectral indices)
  annual_palette <- reactive({
    value_range <- if (metric_selection() == "NDMI") {
      c(-1, 1)
    } else if (metric_selection() == "Tree Cover") {
      c(0, 100)
    } else {
      c(0, 1)
    }

    list(
      range = value_range,
      pal = colorNumeric(palette = "viridis", domain = value_range)
    )
  })

  # ACTIVATE the update button ----
  # Same deferred-apply pattern as the Change Maps tab: the map redraws only
  # when Update is pressed, so the last applied view stays on screen while
  # several selections are changed together.
  annual_ready <- reactive({
    all(nzchar(c(shared_island(), shared_metric()))) &&
      isTruthy(input$year_choice_left) &&
      isTruthy(input$year_choice_right)
  })

  annual_selection_key <- reactive({
    paste(
      shared_island(),
      shared_metric(),
      input$year_choice_left,
      input$year_choice_right,
      sep = "|"
    )
  })

  annual_applied_key <- reactiveVal(NULL)

  observeEvent(input$apply_annual, {
    annual_applied_key(annual_selection_key())
  })

  annual_pending <- reactive({
    !identical(annual_selection_key(), annual_applied_key())
  })

  observe({
    shinyjs::toggleState(
      "apply_annual",
      condition = annual_ready() && annual_pending()
    )

    updateActionButton(
      session,
      "apply_annual",
      label = if (annual_pending()) "Load map" else "Map is up to date"
    )
  })

  annual_view <- eventReactive(input$apply_annual, {
    # Give both sides the same popup, so clicking a pixel on either side
    # reports its value in both of the selected years.
    paired <- add_paired_popups(
      sp_left = annual_layer_left(),
      sp_right = annual_layer_right(),
      label_left = year_choice_left(),
      label_right = year_choice_right(),
      metric_selection = metric_selection()
    )

    list(
      island = island_data(),
      metric = metric_selection(),
      trend = list(
        intervention = intervention_data(),
        metric = metric_paths()$metric,
        years = predicted_year_range(),
        predicted = predicted_df(),
        observed = observed_df()
      ),
      stack = predicted_stack(),
      left = paired$left,
      right = paired$right,
      year_left = year_choice_left(),
      year_right = year_choice_right(),
      palette = annual_palette()
    )
  })

  # PLOT leaflet ----
  output$annual_predicted_splitmap <- renderLeaflet({
    v <- annual_view()

    create_leaflet_splitmap(
      sp_left = v$left,
      sp_right = v$right,
      title_left = as.character(v$year_left),
      title_right = as.character(v$year_right),
      pal = v$palette$pal,
      value_range = v$palette$range,
      legend_title = v$metric
    )
  })

  # ACTIVATE expand map ----
  observeEvent(input$expand_predictions_map, {
    # name the map, since the dialog covers the controls that describe it
    modal_title <- tryCatch(
      {
        v <- annual_view()
        paste0(v$metric, ": ", v$year_left, " and ", v$year_right)
      },
      error = function(e) "Annual vegetation"
    )

    showModal(modalDialog(
      title = modal_title,
      leafletOutput("full_predictions_map", height = "600px"),
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Exit")
    ))
  })

  output$full_predictions_map <- renderLeaflet({
    v <- annual_view()

    create_leaflet_splitmap(
      sp_left = v$left,
      sp_right = v$right,
      title_left = as.character(v$year_left),
      title_right = as.character(v$year_right),
      pal = v$palette$pal,
      value_range = v$palette$range,
      legend_title = v$metric
    )
  })

  # ACTIVATE map clicks ----
  predicted_sp_react <- reactiveValues(clicked_cell = NULL)

  # Clear the selected pixel when the map is redrawn, and therefore when the
  # grid of cell ids actually changes under it. Watching the raster stack
  # instead dropped the selection as soon as a dropdown moved, while the map
  # it was made on was still on screen.
  observeEvent(
    annual_view(),
    {
      predicted_sp_react$clicked_cell <- NULL
    },
    ignoreInit = TRUE
  )

  observeEvent(input$annual_predicted_splitmap_shape_click, {
    click <- input$annual_predicted_splitmap_shape_click

    predicted_sp_react$clicked_cell <- list(
      X = round(click$lat, 6),
      Y = round(click$lng, 6),
      id = click$id
    )
  })

  output$pixel_summary_text <- renderUI({
    req(predicted_sp_react$clicked_cell)

    cell_id_filter <- predicted_sp_react$clicked_cell$id
    lat <- predicted_sp_react$clicked_cell$X
    lng <- predicted_sp_react$clicked_cell$Y

    # Read from the view the map is showing, so the numbers quoted here always
    # belong to the layers that were clicked. Reading the cell predictions
    # instead quoted the smoothed series while the map drew the annual one, so
    # the sentence and the pixel's own popup disagreed.
    v <- annual_view()

    pixel_value <- function(sp) {
      value <- sp$value[match(cell_id_filter, sp$cell_id)]

      if (length(value) == 0 || is.na(value)) {
        "not available"
      } else {
        round(value, 2)
      }
    }

    value_left <- pixel_value(v$left)
    value_right <- pixel_value(v$right)

    tags$p(
      paste0(
        "You have selected a pixel at (",
        round(lng, 4),
        ", ",
        round(lat, 4),
        "). ",
        "The modeled ",
        v$metric,
        " at this pixel is ",
        value_left,
        " in ",
        v$year_left,
        " and ",
        value_right,
        " in ",
        v$year_right,
        ". Below are two plots that show the value of this cell through time. ",
        "On the left is a smoothed version of the model, and on the right is a model that includes annual variation."
      ),
      style = "font-size:16px; margin-bottom:15px;"
    )
  })

  # PLOT pixel trends ----
  pixel_trend_plots <- reactive({
    req(predicted_sp_react$clicked_cell)

    pixel_trend_plots_for(annual_view()$trend, predicted_sp_react$clicked_cell$id)
  })

  output$pixel_trend_plot_a <- renderGirafe({
    req(pixel_trend_plots())

    girafe_pixel_trend(pixel_trend_plots()$left)
  })

  output$pixel_trend_plot_b <- renderGirafe({
    req(pixel_trend_plots())

    girafe_pixel_trend(pixel_trend_plots()$right)
  })

  # DOWNLOAD data ----
  annual_download_name <- reactive({
    v <- annual_view()
    island <- gsub("[()]", "", v$island$Island)
    island <- gsub(" ", "_", island)

    paste0(island, "_", gsub(" ", "", v$metric), "_Stack")
  })

  output$download_annual_predicted_rasters <- downloadHandler(
    filename = function() {
      paste0(annual_download_name(), ".zip")
    },

    content = function(file) {
      v <- annual_view()
      metric_code <- v$trend$metric
      tif_name <- paste0(annual_download_name(), ".tif")

      stack <- rename_annual_bands(v$stack, metric_code)

      write_download_zip(
        file = file,
        layers = stack,
        tif_name = tif_name,
        readme_lines = readme_annual(
          tif_name = tif_name,
          island = v$island$Island,
          metric_label = v$metric,
          metric_code = metric_code,
          stack = stack,
          res_m = v$island[["Res"]]
        )
      )
    }
  )

  # DOWLOAD maps ----
  output$download_annual_predicted_maps <- downloadHandler(
    filename = function() {
      v <- annual_view()
      island <- gsub("[()]", "", v$island$Island)
      island <- gsub(" ", "_", island)
      metric <- gsub(" ", "", v$metric)
      paste0(island, "_", metric, "_Maps.zip")
    },

    content = function(file) {
      show_modal_spinner(
        text = "Generating maps, please wait. This could take a minute or two.",
        spin = "fading-circle"
      )
      on.exit(remove_modal_spinner())

      v <- annual_view()
      stack <- v$stack
      temp_dir <- tempdir()
      image_files <- c()

      # size the canvas to the island so the maps are not mostly whitespace
      plan <- static_map_layout(v$island, panels = 1)

      for (i in 1:nlyr(stack)) {
        layer_name <- names(stack)[i]

        if (layer_name == "cell_id") {
          next
        }

        layer <- stack[[i]]
        image_path <- file.path(
          temp_dir,
          paste0(v$metric, "_", layer_name, ".jpg")
        )

        jpeg(
          image_path,
          width = plan$width,
          height = plan$height,
          units = "in",
          res = 300
        )
        map <- generate_static_annual_map(
          raster_layer = layer,
          island_data = v$island,
          metric = v$metric
        )
        print(map) # Render the map to the JPEG device
        dev.off()

        image_files <- c(image_files, image_path)
      }

      zip::zip(zipfile = file, files = image_files, mode = "cherry-pick")
    }
  )

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
      title = "How to read this map: Modeled Vegetation Change",
      tagList(includeMarkdown("text/m3-per-pixel-change.md")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  # ACTIVATE timeperiod type ----
  shared_timeperiod <- reactiveVal("")
  sync_shared_choice(
    c("timeperiod_choice1", "timeperiod_choice2", "timeperiod_choice3"),
    shared_timeperiod
  )

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
    if (shared_metric() == "Tree Cover") {
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
    display_timeperiods <- names(accumulated_map)[
      accumulated_map %in% available_timeperiods
    ]

    # Update all timeperiod dropdowns, keeping the chosen period where the new
    # island/metric still offers it
    for (tp in c(
      "timeperiod_choice1",
      "timeperiod_choice2",
      "timeperiod_choice3"
    )) {
      update_choices_keeping_selection(tp, display_timeperiods)
    }
  })

  # ACTIVATE probability type ----
  shared_probability <- reactiveVal("")
  sync_shared_choice(
    c("probability_choice1", "probability_choice2", "probability_choice3"),
    shared_probability
  )

  probability_selection <- reactive({
    req(shared_probability())

    as.numeric(gsub("%", "", shared_probability())) / 100
  })

  # The same level as the integer that appears in the raster layer names
  # (posterior.mean_hpdi75_*, _hpdi85_*, _hpdi95_*), which is what the
  # classifying functions take as their `hpdi` argument.
  hpdi_level <- reactive({
    round(probability_selection() * 100)
  })

  # ACTIVATE change type ----
  shared_change_type <- reactiveVal("")
  sync_shared_choice(
    c("change_choice1", "change_choice2"),
    shared_change_type
  )

  change_type_selection <- reactive({
    req(shared_change_type())

    shared_change_type()
  })

  # DISPLAY island info ----
  output$island_info2 <- renderUI({
    render_island_info(shared_island())
  })

  # PREP data ----
  change_paths <- reactive({
    req(metric_paths(), timeperiod_selection(), change_type_selection())

    paths <- metric_paths()
    change_type <- tolower(change_type_selection())

    list(
      change_type = change_type,
      change_rast = paste0(
        paths$base,
        "/m1_",
        paths$metric,
        "_",
        paths$res,
        "m_",
        change_type,
        ".tif"
      )
    )
  })

  change_stack <- reactive({
    paths <- change_paths()

    validate(need(
      length(paths$change_rast) > 0 && file.exists(paths$change_rast),
      "This time period / change type combination has not been modeled for this island and metric."
    ))

    rast(paths$change_rast)
  })

  selected_time <- reactive({
    req(timeperiod_selection())

    change_map[[timeperiod_selection()]]
  })

  change_layer_left <- reactive({
    stack <- change_stack()
    selected_layer <- names(stack) == paste0("posterior.mean_", selected_time())

    validate(need(
      any(selected_layer),
      "The selected time period is not available for this metric."
    ))

    prep_change_tif_for_leaflet(
      r_stack = stack,
      selected_layer = selected_layer,
      selected_time = selected_time(),
      metric_selection = metric_selection()
    )
  })

  change_layer_right <- reactive({
    req(probability_selection())

    stack <- change_stack()
    selected_layer <- names(stack) ==
      paste0(
        "posterior.mean_hpdi",
        probability_selection() * 100,
        "_",
        selected_time()
      )

    validate(need(
      any(selected_layer),
      "The selected probability level is not available for this metric."
    ))

    prep_change_tif_for_leaflet(
      r_stack = stack,
      selected_layer = selected_layer,
      selected_time = selected_time(),
      metric_selection = metric_selection()
    )
  })

  change_titles <- reactive({
    list(
      left = paste0(
        toupper(change_paths()$change_type),
        " ",
        metric_selection(),
        "; All Modeled Pixels"
      ),
      right = paste0("Pixels with Probability > ", hpdi_level(), "%")
    )
  })

  # Set the color palette
  change_palette <- reactive({
    value_range <- get_palette_range(change_layer_left(), col = "value")

    list(
      range = value_range,
      pal = colorNumeric(palette = div_pal, domain = value_range)
    )
  })

  # ACTIVATE the update button ----
  # The map redraws only when Update is pressed. eventReactive isolates its
  # body, so changing a dropdown no longer tears the map down: the last applied
  # view stays on screen until the user asks for a new one, and several
  # selections can be changed together.
  change_ready <- reactive({
    all(nzchar(c(
      shared_island(),
      shared_metric(),
      shared_timeperiod(),
      shared_probability(),
      shared_change_type()
    )))
  })

  # what is currently selected, as one comparable string
  change_selection_key <- reactive({
    paste(
      shared_island(),
      shared_metric(),
      shared_timeperiod(),
      shared_probability(),
      shared_change_type(),
      sep = "|"
    )
  })

  # what the map is currently showing
  change_applied_key <- reactiveVal(NULL)

  observeEvent(input$apply_change, {
    change_applied_key(change_selection_key())
  })

  change_pending <- reactive({
    !identical(change_selection_key(), change_applied_key())
  })

  # the button is only live when there is a complete, unapplied selection, so
  # a half-finished combination can never reach the map
  observe({
    shinyjs::toggleState(
      "apply_change",
      condition = change_ready() && change_pending()
    )

    updateActionButton(
      session,
      "apply_change",
      label = if (change_pending()) "Load map" else "Map is up to date"
    )
  })

  # everything this tab draws or downloads, captured when Update is pressed
  change_view <- eventReactive(input$apply_change, {
    list(
      island = island_data(),
      metric = metric_selection(),
      trend = list(
        intervention = intervention_data(),
        metric = metric_paths()$metric,
        years = predicted_year_range(),
        predicted = predicted_df(),
        observed = observed_df()
      ),
      change_type = change_paths()$change_type,
      change_type_label = change_type_selection(),
      timeperiod = timeperiod_selection(),
      probability = probability_selection(),
      selected_time = selected_time(),
      stack = change_stack(),
      left = change_layer_left(),
      right = change_layer_right(),
      titles = change_titles(),
      palette = change_palette()
    )
  })

  # PLOT leaflets ----
  output$change_sync_map <- renderLeaflet({
    v <- change_view()

    create_leaflet_splitmap_change(
      sp_left = v$left,
      sp_right = v$right,
      title_left = v$titles$left,
      title_right = v$titles$right,
      pal = v$palette$pal,
      value_range = v$palette$range,
      legend_title = v$metric
    )
  })

  # ACTIVATE expand map ----
  observeEvent(input$expand_change_map, {
    # name the map, since the dialog covers the controls that describe it
    modal_title <- tryCatch(
      {
        v <- change_view()
        paste0(v$metric, " ", tolower(v$change_type), " change: ", v$timeperiod)
      },
      error = function(e) "Modeled change"
    )

    showModal(modalDialog(
      title = modal_title,
      leafletOutput("full_change_map", height = "600px"),
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Exit")
    ))
  })

  output$full_change_map <- renderLeaflet({
    v <- change_view()

    create_leaflet_splitmap_change(
      sp_left = v$left,
      sp_right = v$right,
      title_left = v$titles$left,
      title_right = v$titles$right,
      pal = v$palette$pal,
      value_range = v$palette$range,
      legend_title = v$metric
    )
  })

  # ACTIVATE map clicks ----
  change_sp_react <- reactiveValues(clicked_cell = NULL)

  # Clear the selected pixel when the map is redrawn, and therefore when the
  # grid of cell ids actually changes under it. Watching the raster stack
  # instead dropped the selection as soon as a dropdown moved, while the map
  # it was made on was still on screen.
  observeEvent(
    change_view(),
    {
      change_sp_react$clicked_cell <- NULL
    },
    ignoreInit = TRUE
  )

  observeEvent(input$change_sync_map_shape_click, {
    click <- input$change_sync_map_shape_click

    change_sp_react$clicked_cell <- list(
      X = round(click$lat, 6),
      Y = round(click$lng, 6),
      id = click$id,
      value = click$value
    )
  })

  output$pixel_summary_text2 <- renderUI({
    req(change_sp_react$clicked_cell)

    cell_id_filter <- change_sp_react$clicked_cell$id
    lat <- change_sp_react$clicked_cell$X
    lng <- change_sp_react$clicked_cell$Y

    # Read from the view the map is showing, so the number quoted here always
    # belongs to the layer that was clicked
    v <- change_view()

    value <- v$left %>%
      filter(cell_id == cell_id_filter) %>%
      pull(value) %>%
      dplyr::first()

    # an annualized figure is a rate, so say so
    rate_suffix <- if (identical(tolower(v$change_type), "annualized")) " per year" else ""

    tags$p(
      paste0(
        "You have selected a pixel at (",
        round(lng, 4),
        ", ",
        round(lat, 4),
        "). ",
        "The modeled '",
        v$change_type_label,
        " ",
        v$timeperiod,
        " ",
        v$metric,
        " Change' at this pixel is ",
        format_modeled_value(value),
        rate_suffix,
        ". Below are two plots that show the value and trends of this cell through time. ",
        "On the left is a smoothed version of the model, and on the right is a model that includes annual variation."
      ),
      style = "font-size:16px; margin-bottom:15px;"
    )
  })

  # PLOT pixel trends ----
  pixel_trend_plots_change <- reactive({
    req(change_sp_react$clicked_cell)

    pixel_trend_plots_for(change_view()$trend, change_sp_react$clicked_cell$id)
  })

  output$pixel_trend_plot_a_change <- renderGirafe({
    req(pixel_trend_plots_change())

    girafe_pixel_trend(pixel_trend_plots_change()$left)
  })

  output$pixel_trend_plot_b_change <- renderGirafe({
    req(pixel_trend_plots_change())

    girafe_pixel_trend(pixel_trend_plots_change()$right)
  })

  # DOWNLOAD data ----
  change_download_name <- reactive({
    v <- change_view()
    island <- gsub("[()]", "", v$island$Island)
    island <- gsub(" ", "_", island)

    paste0(
      island, "_", gsub(" ", "", v$metric), "_",
      toupper(v$change_type), "_Change_Stack"
    )
  })

  output$download_change_rasters <- downloadHandler(
    filename = function() {
      paste0(change_download_name(), ".zip")
    },

    content = function(file) {
      v <- change_view()
      metric_code <- v$trend$metric
      tif_name <- paste0(change_download_name(), ".tif")

      stack <- prep_change_bands(v$stack)

      write_download_zip(
        file = file,
        layers = stack,
        tif_name = tif_name,
        readme_lines = readme_change(
          tif_name = tif_name,
          island = v$island$Island,
          metric_label = v$metric,
          metric_code = metric_code,
          stack = stack,
          res_m = v$island[["Res"]],
          change_type = v$change_type_label,
          viewed_period = v$timeperiod,
          viewed_probability = paste0(round(v$probability * 100), "%")
        )
      )
    }
  )

  # DOWNLOAD maps ----
  output$download_change_map <- downloadHandler(
    filename = function() {
      # clean up the island name and create a valid filename
      v <- change_view()
      island <- gsub("[()]", "", v$island$Island)
      island <- gsub(" ", "_", island)
      metric <- gsub(" ", "", v$metric)
      paste0(
        island,
        "_",
        metric,
        "_",
        v$timeperiod,
        v$probability,
        "_",
        toupper(v$change_type),
        ".jpg"
      )
    },

    content = function(file) {
      v <- change_view()
      stack <- v$stack
      sel_left <- names(stack) == paste0("posterior.mean_", v$selected_time)
      sel_right <- names(stack) ==
        paste0("posterior.mean_hpdi", v$probability * 100, "_", v$selected_time)

      change_static_plot <- generate_static_change_map(
        raster_layer_top = stack[[sel_left]],
        raster_layer_bottom = stack[[sel_right]],
        title_top = v$titles$left,
        title_bottom = v$titles$right,
        island_data = v$island,
        metric = v$metric
      )

      # Save the plot at the size it worked out for this island's shape
      plot_size <- attr(change_static_plot, "download_size")

      ggsave(
        file,
        plot = change_static_plot,
        bg = "white",
        width = plot_size[1],
        height = plot_size[2],
        dpi = 300,
        limitsize = FALSE
      )
    }
  )

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                                                            ~~
  ##                        TRENDS & CONDITIONS MODULE                        ----
  ##                                                                            ~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # One question at a time: the user picks a question, the function behind it
  # runs on the modeled data for the shared island/metric, and the pixels that
  # answer it are mapped. tc_question_specs (global.R) says which function
  # answers each question, which stack it runs on, and how to draw the result.

  # DISPLAY help button ----
  observeEvent(input$help_btn_tc, {
    showModal(modalDialog(
      title = "How to read this map: Vegetation Trends & Conditions",
      tagList(includeMarkdown("text/m6-trends-conditions.md")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  # DISPLAY island info ----
  output$island_info3 <- renderUI({
    render_island_info(shared_island())
  })

  # ACTIVATE question ----
  tc_spec <- reactive({
    req(input$tc_question_choice)

    tc_question_specs[[input$tc_question_choice]]
  })

  # PREP data ----
  # The condition and status questions run on the annual predicted stack with
  # the cell_id layer dropped, so only the year layers feed into the median.
  tc_annual_years <- reactive({
    stack <- predicted_stack()

    stack[[names(stack) != "cell_id"]]
  })

  # The trend questions run on the accumulated stack. It is loaded here rather
  # than reused from the Change Maps tab so the two tabs never have to agree on
  # a time period or change type.
  tc_accumulated_stack <- reactive({
    paths <- metric_paths()
    accumulated_rast <- paste0(
      paths$base,
      "/m1_",
      paths$metric,
      "_",
      paths$res,
      "m_accumulated.tif"
    )

    validate(need(
      length(accumulated_rast) > 0 && file.exists(accumulated_rast),
      "Accumulated change has not been modeled for this island and metric."
    ))

    rast(accumulated_rast)
  })

  # RUN the selected function ----
  # Each function keeps its own defaults for prob and na.values; only years and
  # layer_name are passed in, so the layer name always matches the legend.
  tc_classified <- reactive({
    spec <- tc_spec()

    # only the trend questions read an HPDI layer, so only they wait on a
    # probability level being chosen
    if (isTRUE(spec$uses_hpdi)) {
      req(probability_selection())
    }

    switch(
      spec$fn,
      high_condition = high_condition(
        tc_annual_years(),
        layer_name = spec$layer_name
      ),
      low_condition = low_condition(
        tc_annual_years(),
        layer_name = spec$layer_name
      ),
      high_status = high_status(
        tc_annual_years(),
        years = tc_status_years,
        layer_name = spec$layer_name
      ),
      low_status = low_status(
        tc_annual_years(),
        years = tc_status_years,
        layer_name = spec$layer_name
      ),
      increasing_trend = increasing_trend(
        tc_accumulated_stack(),
        layer_name = spec$layer_name,
        hpdi = hpdi_level()
      ),
      decreasing_trend = decreasing_trend(
        tc_accumulated_stack(),
        layer_name = spec$layer_name,
        hpdi = hpdi_level()
      )
    )
  })

  # The cell ids that pair with whichever stack the question ran on, so the
  # mapped pixels stay clickable.
  tc_cell_ids <- reactive({
    spec <- tc_spec()

    if (spec$source == "annual") {
      predicted_stack()[["cell_id"]]
    } else {
      # the cell ids for the same period the trend functions will have used.
      # Going through accumulated_layer rather than repeating the choice here
      # is what keeps the two in step: this used to mirror the old
      # fulltime-or-pre rule by hand, and so broke on the post1-only islands
      # in its own way even once the functions themselves were fixed.
      accumulated_layer(tc_accumulated_stack(), "cell_id")
    }
  })

  # the level to show in labels -- NULL for questions that never read an HPDI
  tc_hpdi <- reactive({
    if (isTRUE(tc_spec()$uses_hpdi)) hpdi_level() else NULL
  })

  tc_layer <- reactive({
    # force these before they reach the prep helper so a validate() message
    # surfaces on its own rather than wrapped in a dispatch error
    classified <- tc_classified()
    cell_ids <- tc_cell_ids()

    sp <- prep_classified_tif_for_leaflet(
      classified_rast = classified,
      cell_id_rast = cell_ids,
      metric_selection = metric_selection(),
      category_labels = question_labels(tc_spec(), tc_hpdi())
    )

    validate(need(
      !is.null(sp),
      "No pixels on this island answer the selected question."
    ))

    sp
  })

  # DISPLAY a note when the probability level does not apply ----
  output$tc_probability_note <- renderUI({
    req(input$tc_question_choice)

    if (isTRUE(tc_spec()$uses_hpdi)) {
      return(NULL)
    }

    helpText(paste(
      "The probability level does not apply to this question.",
      "It compares median values across years and does not use a",
      "highest posterior density interval."
    ))
  })

  tc_title <- reactive({
    paste0(metric_selection(), ": ", input$tc_question_choice)
  })

  # ACTIVATE the update button ----
  # Same deferred-apply pattern as the Change Maps tab: the map redraws only
  # when Update is pressed, so the last applied view stays on screen while
  # several selections are changed together.
  tc_ready <- reactive({
    if (!all(nzchar(c(shared_island(), shared_metric())))) {
      return(FALSE)
    }

    question <- input$tc_question_choice
    if (!isTruthy(question)) {
      return(FALSE)
    }

    # only the trend questions need a probability level
    if (isTRUE(tc_question_specs[[question]]$uses_hpdi)) {
      nzchar(shared_probability())
    } else {
      TRUE
    }
  })

  tc_selection_key <- reactive({
    question <- input$tc_question_choice
    if (is.null(question)) {
      question <- ""
    }

    paste(
      shared_island(),
      shared_metric(),
      question,
      shared_probability(),
      sep = "|"
    )
  })

  tc_applied_key <- reactiveVal(NULL)

  observeEvent(input$apply_tc, {
    tc_applied_key(tc_selection_key())
  })

  tc_pending <- reactive({
    !identical(tc_selection_key(), tc_applied_key())
  })

  observe({
    shinyjs::toggleState("apply_tc", condition = tc_ready() && tc_pending())

    updateActionButton(
      session,
      "apply_tc",
      label = if (tc_pending()) "Load map" else "Map is up to date"
    )
  })

  tc_view <- eventReactive(input$apply_tc, {
    sp <- tc_layer()
    cell_ids <- tc_cell_ids()

    list(
      sp = sp,
      categories = question_categories(tc_spec(), tc_hpdi()),
      title = tc_title(),
      classified = tc_classified(),
      layer_name = tc_spec()$layer_name,
      question_fn = tc_spec()$fn,
      uses_hpdi = isTRUE(tc_spec()$uses_hpdi),
      hpdi = tc_hpdi(),
      question = input$tc_question_choice,
      island = island_data(),
      metric = metric_selection(),
      trend = list(
        intervention = intervention_data(),
        metric = metric_paths()$metric,
        years = predicted_year_range(),
        predicted = predicted_df(),
        observed = observed_df()
      ),
      modeled_pixels = unname(global(cell_ids, fun = function(x) {
        sum(!is.na(x))
      })[1, 1]),

      # measured here rather than in the summary output so it is computed once,
      # when Load map is pressed, like everything else this view captures
      area = classified_area(tc_classified(), cell_ids)
    )
  })

  # PLOT leaflet ----
  output$tc_map <- renderLeaflet({
    v <- tc_view()

    create_leaflet_classified(
      sp = v$sp,
      categories = v$categories,
      title = v$title,
      bounds = island_view_bounds(v$island, v$sp)
    )
  })

  # ACTIVATE expand map ----
  observeEvent(input$expand_tc_map, {
    # name the map, since the dialog covers the controls that describe it
    modal_title <- tryCatch(
      {
        v <- tc_view()
        v$title
      },
      error = function(e) "Trends and conditions"
    )

    showModal(modalDialog(
      title = modal_title,
      leafletOutput("full_tc_map", height = "600px"),
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Exit")
    ))
  })

  output$full_tc_map <- renderLeaflet({
    v <- tc_view()

    create_leaflet_classified(
      sp = v$sp,
      categories = v$categories,
      title = v$title,
      bounds = island_view_bounds(v$island, v$sp)
    )
  })

  # ACTIVATE map clicks ----
  tc_sp_react <- reactiveValues(clicked_cell = NULL)

  # Clear the selected pixel when the map is redrawn, and therefore when the
  # set of mapped pixels actually changes under it. Watching the classified
  # raster instead made it eager: an observer forces its event expression, so
  # every dropdown change re-ran the classification -- reading the rasters and
  # recomputing the medians and quantiles -- before the user had asked for a
  # new map.
  observeEvent(
    tc_view(),
    {
      tc_sp_react$clicked_cell <- NULL
    },
    ignoreInit = TRUE
  )

  observeEvent(input$tc_map_shape_click, {
    click <- input$tc_map_shape_click

    tc_sp_react$clicked_cell <- list(
      X = round(click$lat, 6),
      Y = round(click$lng, 6),
      id = click$id
    )
  })

  # DISPLAY summary ----
  output$tc_summary_text <- renderUI({
    v <- tc_view()
    sp <- v$sp
    matched_pixels <- nrow(sp)
    modeled_pixels <- v$modeled_pixels

    # every question on this tab maps a single category, so the areas sum to
    # the one figure the reader is after
    area <- v$area
    shown_km2 <- sum(area$by_value$area_km2)
    shown_percent <- sum(area$by_value$percent)

    clicked <- tc_sp_react$clicked_cell

    tagList(
      p(strong("Question:"), v$question),

      if (isTRUE(v$uses_hpdi)) {
        p(strong("Probability level:"), paste0(v$hpdi, "%"))
      },

      p(
        strong("Area shown:"),
        paste0(
          format_km2(shown_km2),
          " of the ",
          format_km2(area$modeled_km2),
          " modeled (",
          round(shown_percent, 1),
          "%)"
        )
      ),

      # the percentage lives on the area line above, so it is not repeated here
      p(
        strong("Pixels shown:"),
        paste0(
          format(matched_pixels, big.mark = ","),
          " of ",
          format(modeled_pixels, big.mark = ","),
          " modeled pixels"
        )
      ),

      if (is.null(clicked)) {
        p("Click on a pixel above to see its location and timeseries.")
      } else {
        tagList(
          p(strong("Selected pixel:"), clicked$X, "latitude,", clicked$Y, "longitude"),
          p(pixel_trend_blurb())
        )
      }
    )
  })

  # DISPLAY the caveat that belongs with the result ----
  # The condition and status questions rank pixels against their own island,
  # so their maps cannot be read across islands; the trend questions do not
  # rank at all, so a near-empty map is an answer rather than missing data.
  # Either way the reader needs this beside the map, not inside the modal.
  output$tc_result_note <- renderUI({
    v <- tc_view()

    if (isTRUE(v$uses_hpdi)) {
      # Only the trend questions read the accumulated stack, so only they
      # depend on which period an island was modeled over. See the matching
      # note on the Management Questions tab.
      window <- accumulated_period_phrase(
        accumulated_period(tc_accumulated_stack())
      )

      result_note(
        "Only pixels where the model is confident about the direction of change ",
        "at this probability level are drawn. A small number of pixels, or none ",
        "at all, is a real result for this island rather than missing data.",
        if (is.null(window)) {
          NULL
        } else {
          HTML(paste0(
            " The trend is modeled over <strong>", window,
            "</strong>, which is the only period modeled for this island."
          ))
        }
      )
    } else {
      result_note(HTML(
        "These are the top or bottom 25% <strong>for this island</strong>. A pixel
         qualifies by ranking against its neighbours, not by passing a fixed
         threshold, so these maps should not be compared between islands."
      ))
    }
  })

  # PLOT pixel trends ----
  # This module follows the shared island/metric, so the timeseries behind a
  # clicked pixel comes from the same predicted data the first two tabs use.
  pixel_trend_plots_tc <- reactive({
    req(tc_sp_react$clicked_cell)

    pixel_trend_plots_for(tc_view()$trend, tc_sp_react$clicked_cell$id)
  })

  output$pixel_trend_plot_a_tc <- renderGirafe({
    req(pixel_trend_plots_tc())

    girafe_pixel_trend(pixel_trend_plots_tc()$left)
  })

  output$pixel_trend_plot_b_tc <- renderGirafe({
    req(pixel_trend_plots_tc())

    girafe_pixel_trend(pixel_trend_plots_tc()$right)
  })

  # DOWNLOAD rasters ----
  tc_download_name <- reactive({
    v <- tc_view()
    island <- gsub("[()]", "", v$island$Island)
    island <- gsub(" ", "_", island)

    paste0(
      island, "_", gsub(" ", "", v$metric), "_",
      gsub("\\.", "_", v$layer_name)
    )
  })

  output$download_tc_raster <- downloadHandler(
    filename = function() {
      paste0(tc_download_name(), ".zip")
    },

    content = function(file) {
      v <- tc_view()
      metric_code <- v$trend$metric
      question_fn <- v$question_fn
      tif_name <- paste0(tc_download_name(), ".tif")

      classified <- rename_classified_band(
        v$classified,
        metric_code,
        question_fn
      )

      write_download_zip(
        file = file,
        layers = classified,
        tif_name = tif_name,
        readme_lines = readme_classified(
          tif_name = tif_name,
          island = v$island$Island,
          metric_label = v$metric,
          metric_code = metric_code,
          r = classified,
          res_m = v$island[["Res"]],
          question = v$question,
          question_fn = question_fn,
          categories = v$categories,
          probability = if (isTRUE(v$uses_hpdi)) paste0(v$hpdi, "%") else NULL
        )
      )
    }
  )

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                                                            ~~
  ##                       MANAGEMENT QUESTIONS MODULE                        ----
  ##                                                                            ~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Island and question only -- no metric picker. The productivity questions run
  # on whichever index island_metadata$Prod_Met names for the island, and the
  # forest question always runs on tree cover, so this module never touches the
  # shared metric that the other tabs sync.

  # DISPLAY help button ----
  observeEvent(input$help_btn_mq, {
    showModal(modalDialog(
      title = "How to read this map: Key Management Questions",
      tagList(includeMarkdown("text/m7-management-questions.md")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  # DISPLAY island info ----
  output$island_info4 <- renderUI({
    render_island_info(shared_island())
  })

  # ACTIVATE question ----
  mq_spec <- reactive({
    req(input$mq_question_choice)

    mq_question_specs[[input$mq_question_choice]]
  })

  # PREP data ----
  # Every path this module needs, derived from the island row rather than from
  # the metric the user picked on another tab.
  mq_paths <- reactive({
    req(island_data())

    isl <- island_data()
    productivity_metric <- as.character(isl$Prod_Met)
    res <- isl[["Res"]]

    productivity_base <- as.character(isl[[paste0(productivity_metric, "_fp")]])
    tcc_base <- as.character(isl[["tcc_fp"]])

    list(
      productivity_metric = productivity_metric,
      productivity_display = names(metric_map)[match(
        productivity_metric,
        metric_map
      )],
      productivity_annual = paste0(
        productivity_base,
        "/m1_",
        productivity_metric,
        "_",
        res,
        "m_allcells_posterior.mean_annual_prediction.tif"
      ),
      productivity_accumulated = paste0(
        productivity_base,
        "/m1_",
        productivity_metric,
        "_",
        res,
        "m_accumulated.tif"
      ),
      tcc_annual = paste0(
        tcc_base,
        "/m1_tcc_",
        res,
        "m_allcells_posterior.mean_annual_prediction.tif"
      ),
      tcc_accumulated = paste0(tcc_base, "/m1_tcc_", res, "m_accumulated.tif")
    )
  })

  # The metric that answers the selected question, for the map title and popups
  mq_metric_display <- reactive({
    if (mq_spec()$source == "forest") {
      "Tree Cover"
    } else {
      mq_paths()$productivity_display
    }
  })

  mq_productivity_annual <- reactive({
    path <- mq_paths()$productivity_annual

    validate(need(
      length(path) > 0 && file.exists(path),
      "Productivity has not been modeled for this island."
    ))

    rast(path)
  })

  mq_productivity_accumulated <- reactive({
    path <- mq_paths()$productivity_accumulated

    validate(need(
      length(path) > 0 && file.exists(path),
      "Accumulated productivity change has not been modeled for this island."
    ))

    rast(path)
  })

  mq_tcc_accumulated <- reactive({
    path <- mq_paths()$tcc_accumulated

    validate(need(
      length(path) > 0 && file.exists(path),
      "Tree canopy cover has not been modeled for this island, so the forest question cannot be answered here."
    ))

    rast(path)
  })

  # RUN the selected function ----
  # Each function keeps its own defaults for prob and na.values; only years and
  # layer_name are passed in. Note the different stacks each one wants: the
  # status functions take the year layers only, while least_change() takes the
  # whole annual stack and drops the cell_id layer itself.
  mq_classified <- reactive({
    spec <- mq_spec()

    # every management question reads an HPDI layer
    req(probability_selection())

    switch(
      spec$fn,

      status_high_increasing = {
        stack <- mq_productivity_annual()
        status_high_increasing(
          stack[[names(stack) != "cell_id"]],
          mq_productivity_accumulated(),
          years = mq_status_years,
          layer_name = spec$layer_name,
          hpdi = hpdi_level()
        )
      },

      status_low_decreasing = {
        stack <- mq_productivity_annual()
        status_low_decreasing(
          stack[[names(stack) != "cell_id"]],
          mq_productivity_accumulated(),
          years = mq_status_years,
          layer_name = spec$layer_name,
          hpdi = hpdi_level()
        )
      },

      greatest_change = greatest_change(
        mq_productivity_accumulated(),
        layer_name = spec$layer_name,
        hpdi = hpdi_level()
      ),

      least_change = least_change(
        mq_productivity_annual(),
        mq_productivity_accumulated(),
        layer_name = spec$layer_name,
        hpdi = hpdi_level()
      ),

      forest_change = forest_change(
        mq_tcc_accumulated(),
        layer_name = spec$layer_name,
        hpdi = hpdi_level()
      )
    )
  })

  # The cell ids that pair with whichever stack the question ran on, so the
  # mapped pixels stay clickable.
  mq_cell_ids <- reactive({
    spec <- mq_spec()

    if (spec$source == "productivity_status") {
      mq_productivity_annual()[["cell_id"]]
    } else {
      stack <- if (spec$source == "forest") {
        mq_tcc_accumulated()
      } else {
        mq_productivity_accumulated()
      }

      # the cell ids for the same period the question functions will have
      # used -- see the matching comment in tc_cell_ids
      accumulated_layer(stack, "cell_id")
    }
  })

  # Why a question came back with nothing, in the terms of the question that
  # was asked: each message spells out the criteria the pixels had to meet and
  # the probability level in force, so a reader can see which part of the test
  # nothing on the island passed rather than guess at it.
  #
  # The resist-change question needs the longest answer. It asks where a trend
  # is absent, so a pixel drops out of it when the model is confident the pixel
  # IS changing -- which means raising the probability level adds pixels here
  # rather than removing them, the reverse of every other question on this tab
  # and of what the control says it does. Eight of the sixty-seven modeled
  # islands, Mona among them, come back empty at both 75% and 85%, and four of
  # those are empty at 95% too.
  mq_empty_message <- function(spec, hpdi) {
    # "an 85%", but "a 75%" and "a 95%" -- the three levels the control offers
    level <- paste0(if (hpdi == 85) "an " else "a ", hpdi, "%")

    # The questions that test two things at once hang on the joining "and", so
    # it is underlined. Shiny prints a validate() message as plain text, tags
    # and all, so the underline has to travel in the string: U+0332 COMBINING
    # LOW LINE under each letter draws it without any markup.
    and <- "a̲n̲d̲"

    switch(
      spec$fn,

      status_high_increasing = paste0(
        "There are no pixels on this island where median productivity across ",
        "the last ", mq_status_years, " years is in the top 25% of values ",
        "for the island ", and, " where there is at least ", level,
        " probability of an increasing trend."
      ),

      status_low_decreasing = paste0(
        "There are no pixels on this island where median productivity across ",
        "the last ", mq_status_years, " years is in the bottom 25% of values ",
        "for the island ", and, " where there is at least ", level,
        " probability of a decreasing trend."
      ),

      greatest_change = paste0(
        "There are no pixels on this island where there is at least ", level,
        " probability of a directional change in productivity."
      ),

      least_change = paste0(
        "There are no pixels on this island that meet the criteria for this ",
        "management question. This question requires that a pixel has: (1) no ",
        "directional trend at the selected probability level ", and, " (2) ",
        "low variability in annual values, relative to the rest of the ",
        "island. It is likely that there is at least ", level,
        " probability that productivity is changing across the entire island, ",
        "meaning there are ",
        "no areas that appear to resist change."
      ),

      forest_change = paste0(
        "There are no pixels on this island where there is at least ", level,
        " probability that tree canopy cover is increasing or decreasing."
      ),

      "No pixels on this island answer the selected question."
    )
  }

  mq_layer <- reactive({
    # force these before they reach the prep helper so a validate() message
    # surfaces on its own rather than wrapped in a dispatch error
    classified <- mq_classified()
    cell_ids <- mq_cell_ids()

    sp <- prep_classified_tif_for_leaflet(
      classified_rast = classified,
      cell_id_rast = cell_ids,
      metric_selection = mq_metric_display(),
      category_labels = question_labels(mq_spec(), hpdi_level())
    )

    validate(need(
      !is.null(sp),
      mq_empty_message(mq_spec(), hpdi_level())
    ))

    sp
  })

  mq_title <- reactive({
    paste0(mq_metric_display(), ": ", input$mq_question_choice)
  })

  # DISPLAY which metric answers the question ----
  output$mq_metric_note <- renderUI({
    req(shared_island(), input$mq_question_choice)

    helpText(paste0(
      "Answered using ",
      mq_metric_display(),
      if (mq_spec()$source == "forest") {
        " (the forest question always uses tree canopy cover)."
      } else {
        " (the productivity index modeled for this island)."
      }
    ))
  })

  # ACTIVATE the update button ----
  # Same deferred-apply pattern as the Change Maps tab: the map redraws only
  # when Update is pressed, so the last applied view stays on screen while
  # several selections are changed together.
  mq_ready <- reactive({
    nzchar(shared_island()) &&
      isTruthy(input$mq_question_choice) &&
      nzchar(shared_probability())
  })

  mq_selection_key <- reactive({
    question <- input$mq_question_choice
    if (is.null(question)) {
      question <- ""
    }

    paste(shared_island(), question, shared_probability(), sep = "|")
  })

  mq_applied_key <- reactiveVal(NULL)

  observeEvent(input$apply_mq, {
    mq_applied_key(mq_selection_key())
  })

  mq_pending <- reactive({
    !identical(mq_selection_key(), mq_applied_key())
  })

  observe({
    shinyjs::toggleState("apply_mq", condition = mq_ready() && mq_pending())

    updateActionButton(
      session,
      "apply_mq",
      label = if (mq_pending()) "Load map" else "Map is up to date"
    )
  })

  mq_view <- eventReactive(input$apply_mq, {
    sp <- mq_layer()
    cell_ids <- mq_cell_ids()

    list(
      sp = sp,
      categories = question_categories(mq_spec(), hpdi_level()),
      title = mq_title(),
      classified = mq_classified(),
      layer_name = mq_spec()$layer_name,
      question_fn = mq_spec()$fn,
      hpdi = hpdi_level(),
      question = input$mq_question_choice,
      island = island_data(),
      metric = mq_metric_display(),
      trend = list(
        intervention = intervention_data(),
        metric = mq_trend_paths()$metric,
        years = mq_trend_year_range(),
        predicted = mq_predicted_df(),
        observed = mq_observed_df()
      ),
      modeled_pixels = unname(global(cell_ids, fun = function(x) {
        sum(!is.na(x))
      })[1, 1]),

      # measured here rather than in the summary output so it is computed once,
      # when Load map is pressed, like everything else this view captures
      area = classified_area(mq_classified(), cell_ids)
    )
  })

  # PLOT leaflet ----
  output$mq_map <- renderLeaflet({
    v <- mq_view()

    create_leaflet_classified(
      sp = v$sp,
      categories = v$categories,
      title = v$title,
      bounds = island_view_bounds(v$island, v$sp)
    )
  })

  # ACTIVATE expand map ----
  observeEvent(input$expand_mq_map, {
    # name the map, since the dialog covers the controls that describe it
    modal_title <- tryCatch(
      {
        v <- mq_view()
        v$title
      },
      error = function(e) "Management questions"
    )

    showModal(modalDialog(
      title = modal_title,
      leafletOutput("full_mq_map", height = "600px"),
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Exit")
    ))
  })

  output$full_mq_map <- renderLeaflet({
    v <- mq_view()

    create_leaflet_classified(
      sp = v$sp,
      categories = v$categories,
      title = v$title,
      bounds = island_view_bounds(v$island, v$sp)
    )
  })

  # ACTIVATE map clicks ----
  mq_sp_react <- reactiveValues(clicked_cell = NULL)

  # Clear the selected pixel when the map is redrawn, and therefore when the
  # set of mapped pixels actually changes under it. Watching the classified
  # raster instead made it eager: an observer forces its event expression, so
  # every dropdown change re-ran the classification -- reading the rasters and
  # recomputing the medians and quantiles -- before the user had asked for a
  # new map.
  observeEvent(
    mq_view(),
    {
      mq_sp_react$clicked_cell <- NULL
    },
    ignoreInit = TRUE
  )

  observeEvent(input$mq_map_shape_click, {
    click <- input$mq_map_shape_click

    mq_sp_react$clicked_cell <- list(
      X = round(click$lat, 6),
      Y = round(click$lng, 6),
      id = click$id
    )
  })

  # DISPLAY summary ----
  output$mq_summary_text <- renderUI({
    v <- mq_view()
    sp <- v$sp
    categories <- v$categories
    area <- v$area

    clicked <- mq_sp_react$clicked_cell
    clicked_category <- if (!is.null(clicked)) {
      sp$category[match(clicked$id, sp$cell_id)]
    } else {
      NULL
    }

    tagList(
      p(strong("Question:"), v$question),

      p(strong("Answered using:"), v$metric),

      p(strong("Probability level:"), paste0(v$hpdi, "%")),

      # one line per category that this island actually produced
      lapply(seq_len(nrow(categories)), function(i) {
        n <- sum(sp$value == categories$value[i])
        if (n == 0) {
          return(NULL)
        }
        # a category with pixels always has a row here, since both are read
        # from the same classified layer
        measured <- area$by_value[
          match(categories$value[i], area$by_value$value), ,
          drop = FALSE
        ]
        p(
          strong(paste0(categories$label[i], ":")),
          paste0(
            format_km2(measured$area_km2),
            " of the ",
            format_km2(area$modeled_km2),
            " modeled (",
            round(measured$percent, 1),
            "%), ",
            format(n, big.mark = ","),
            " pixels"
          )
        )
      }),

      if (is.null(clicked)) {
        p("Click on a pixel above to see its location and timeseries.")
      } else {
        tagList(
          p(strong("Selected pixel:"), clicked$X, "latitude,", clicked$Y, "longitude"),
          p(
            strong("Category:"),
            if (is.na(clicked_category)) {
              "not shown on this map"
            } else {
              clicked_category
            }
          ),
          p(pixel_trend_blurb())
        )
      }
    )
  })

  # DISPLAY the caveat that belongs with the result ----
  # The metric is picked for the reader here, and the productivity questions
  # run on whichever index was modeled for the island, so two islands' maps of
  # the same question can rest on different measurements.
  output$mq_result_note <- renderUI({
    v <- mq_view()

    is_forest <- identical(mq_spec()$source, "forest")

    # Not every island was modeled over its whole satellite record: 27 of the
    # 198 accumulated rasters cover the post-intervention period alone. The
    # trend behind the answer then means something narrower than a reader
    # would assume, so it is named. Nothing is said when the whole record was
    # used, which is the case a reader already expects.
    period <- accumulated_period(
      if (is_forest) mq_tcc_accumulated() else mq_productivity_accumulated()
    )

    window <- accumulated_period_phrase(period)

    window_note <- if (is.null(window)) {
      NULL
    } else {
      HTML(paste0(
        " The trend behind this answer was modeled over <strong>",
        window,
        "</strong>, which is the only period modeled for this island."
      ))
    }

    if (is_forest) {
      result_note(
        "Only pixels where the model is confident about the direction of change ",
        "at this probability level are drawn. A small number of pixels, or none ",
        "at all, is a real result for this island rather than missing data.",
        window_note
      )
    } else {
      # built as one string: separate tags would put a space before the comma
      result_note(
        HTML(paste0(
          "This question was answered using <strong>",
          htmltools::htmlEscape(v$metric),
          "</strong>, the productivity index modeled for this island. Another island
           may be mapped with a different index, so the same question's maps are not
           directly comparable between islands."
        )),
        window_note
      )
    }
  })

  # PLOT pixel trends ----
  # This module never touches the shared metric, so the timeseries behind a
  # clicked pixel is read from whichever metric answered the question: tree
  # cover for the forest question, the island's productivity index otherwise.
  mq_trend_paths <- reactive({
    req(island_data())

    isl <- island_data()
    metric <- if (mq_spec()$source == "forest") {
      "tcc"
    } else {
      mq_paths()$productivity_metric
    }
    base <- as.character(isl[[paste0(metric, "_fp")]])
    res <- isl[["Res"]]

    list(
      metric = metric,
      predicted_df = paste0(
        base,
        "/m1_",
        metric,
        "_",
        res,
        "m_cell_predictions.rds"
      ),
      observed_df = paste0(
        base,
        "/",
        metric,
        "_",
        res,
        "m_observed_timeseries.rds"
      )
    )
  })

  # Read once per island/question-metric rather than on every pixel click.
  mq_predicted_df <- reactive({
    path <- mq_trend_paths()$predicted_df

    validate(need(
      length(path) > 0 && file.exists(path),
      "The modeled timeseries for this metric is not available on this island."
    ))

    read_rds(path)
  })

  mq_observed_df <- reactive({
    path <- mq_trend_paths()$observed_df

    validate(need(
      length(path) > 0 && file.exists(path),
      "The observed timeseries for this metric is not available on this island."
    ))

    read_rds(path)
  })

  # The annual stack is opened only for its layer names, which carry the
  # modeled years the plots are drawn across.
  mq_trend_year_range <- reactive({
    stack <- if (mq_spec()$source == "forest") {
      path <- mq_paths()$tcc_annual

      validate(need(
        length(path) > 0 && file.exists(path),
        "Annual tree cover has not been modeled for this island."
      ))

      rast(path)
    } else {
      mq_productivity_annual()
    }

    list(
      min = as.numeric(names(stack[[2]])),
      max = as.numeric(names(stack[[nlyr(stack)]]))
    )
  })

  pixel_trend_plots_mq <- reactive({
    req(mq_sp_react$clicked_cell)

    pixel_trend_plots_for(mq_view()$trend, mq_sp_react$clicked_cell$id)
  })

  output$pixel_trend_plot_a_mq <- renderGirafe({
    req(pixel_trend_plots_mq())

    girafe_pixel_trend(pixel_trend_plots_mq()$left)
  })

  output$pixel_trend_plot_b_mq <- renderGirafe({
    req(pixel_trend_plots_mq())

    girafe_pixel_trend(pixel_trend_plots_mq()$right)
  })

  # DOWNLOAD rasters ----
  mq_download_name <- reactive({
    v <- mq_view()
    island <- gsub("[()]", "", v$island$Island)
    island <- gsub(" ", "_", island)

    paste0(
      island, "_", gsub(" ", "", v$metric), "_",
      gsub("\\.", "_", v$layer_name)
    )
  })

  output$download_mq_raster <- downloadHandler(
    filename = function() {
      paste0(mq_download_name(), ".zip")
    },

    content = function(file) {
      v <- mq_view()
      metric_code <- v$trend$metric
      question_fn <- v$question_fn
      tif_name <- paste0(mq_download_name(), ".tif")

      classified <- rename_classified_band(
        v$classified,
        metric_code,
        question_fn
      )

      write_download_zip(
        file = file,
        layers = classified,
        tif_name = tif_name,
        readme_lines = readme_classified(
          tif_name = tif_name,
          island = v$island$Island,
          metric_label = v$metric,
          metric_code = metric_code,
          r = classified,
          res_m = v$island[["Res"]],
          question = v$question,
          question_fn = question_fn,
          categories = v$categories,
          probability = paste0(v$hpdi, "%")
        )
      )
    }
  )

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                                                            ~~
  ##                                 ABOUT INFO                               ----
  ##                                                                            ~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # TOGGLE inputs ----
  shinyjs::onclick(
    "toggle_metric_info",
    shinyjs::toggle(id = "metric_info", anim = TRUE)
  )
}
