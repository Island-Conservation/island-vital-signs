# ui.R

# START dashboard page & ui ----
ui <- dashboardPage(
  # SET browser tab title ----
  # Given no title of its own, dashboardPage() reuses the header title for the
  # page <title>, and the header title is a span() carrying the font size. A
  # <title> holds text rather than markup, so the browser tab read
  # <span style="font-size: 18px;">Island Vital Signs</span> literally -- and
  # so did every pasted link, since Slack, Teams and the like take their link
  # text from it. These two titles are separate on purpose; keep them in step.
  title = "Island Vital Signs",

  # START dashboard header ----
  dashboardHeader(
    # set title
    title = span("Island Vital Signs", style = "font-size: 18px;")
  ), # END dashboard header

  # START dashboard sidebar ----
  dashboardSidebar(
    sidebarMenu(
      # "Explore" rather than "Modeling Results": the other three sidebar
      # items are reference pages, and this one is the only place anything
      # happens. "Modeling Results" named the output and read like a report,
      # which undersold a section you work in. tabName is left alone so no
      # bookmark or internal reference breaks.
      shinydashboard::menuItem(
        "Explore",
        tabName = "modeling_results",
        icon = icon("compass")
      ),
      shinydashboard::menuItem(
        "About",
        tabName = "about",
        icon = icon("info-circle")
      ),
      shinydashboard::menuItem(
        "News",
        tabName = "news",
        icon = icon("newspaper")
      ),
      shinydashboard::menuItem(
        "Resources",
        tabName = "resources",
        icon = icon("book")
      )
    )
  ), # END dashboard sidebar

  # START dashboard body ----
  dashboardBody(
    # shinyjs is used to enable/disable the per-module update buttons
    useShinyjs(),

    # Bring in custom css from stylesheet
    tags$head(
      # Stamped with the file's own modified time: browsers cache www/ assets
      # hard, so without this a stylesheet change can sit invisible behind a
      # cached copy until someone thinks to force-reload.
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = asset_url("custom.css")
      ),

      # lets the split maps hand the side-by-side control every basemap
      tags$script(src = asset_url("splitmap.js")),

      # toggles for the short help notes beside the controls
      tags$script(src = asset_url("inline-help.js")),

      # "open this island in a module" buttons in the Overview map popups
      tags$script(src = asset_url("overview-map.js"))
    ),

    # START tab items ----
    tabItems(
      # START modeling results tab ----
      tabItem(
        tabName = "modeling_results",

        # START module tabset paneling ----
        # The tabs are numbered to give the modules a reading order: the
        # Overview is where you start, and the four numbered tabs are roughly
        # the order you would work through a question in. The id is what lets
        # a click on the Overview map send the reader to a module.
        tabsetPanel(
          id = "module_tabs",

          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ##                                                                            ~~
          ##                            ***START OVERVIEW***                          ----
          ##                                                                            ~~
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          tabPanel(
            title = "Overview",

            fluidRow(
              column(
                width = 12, # Content fills space

                # DISPLAY text ----
                br(),

                # Capped and cropped rather than left at its own aspect ratio.
                # At full width the photo stood about 370px tall, which was
                # most of the first screen on a laptop and pushed the island
                # catalogue below the fold even after it was moved up.
                img(
                  src = 'cropped-island-photo.jpg',
                  class = "overview-banner"
                ),

                # The page is ordered by how often each part is wanted, not by
                # how a first-time reader would read it start to finish. What
                # the tool is, and what the four modules are, is orientation:
                # read once, needed straight away, so it is above. The island
                # catalogue is the working surface people come back for, so it
                # is next. The problem framing, the value of the tool and the
                # key terms are reference -- looked up rather than read
                # through -- so they sit below the catalogue. Putting the
                # glossary above it taxed every repeat visit to serve a first
                # visit that only happens once.
                div(
                  class = "markdown-text overview-lede",
                  includeMarkdown("text/m0-overview-lede.md")
                ),

                # DISPLAY module shortcuts ----
                # One card per numbered tab, each a button into that module.
                fluidRow(
                  class = "module-card-row",

                  lapply(module_summaries, function(module) {
                    column(
                      width = 3,

                      tags$button(
                        type = "button",
                        class = "module-card",
                        `data-tab` = module$tab,
                        tags$span(class = "module-card-title", module$tab),
                        tags$span(class = "module-card-blurb", module$blurb)
                      )
                    )
                  })
                ),

                # DISPLAY island catalogue ----
                h3(tags$strong("Islands in This Tool")),

                module_intro(
                  "Use the map and search criteria below to explore the islands with data available within the Island Vital Signs dataset."
                ),

                br(),

                fluidRow(
                  column(
                    width = 4,

                    wellPanel(
                      # FILTER islands ----
                      textInput(
                        "ov_search",
                        "Search island name:",
                        placeholder = "e.g. Anacapa"
                      ),

                      selectInput(
                        "ov_country",
                        "Country:",
                        choices = overview_countries,
                        multiple = TRUE
                      ),

                      help_label(
                        "ov_metric",
                        "Metric available:",
                        "ov_metric_help"
                      ),

                      selectInput(
                        "ov_metric",
                        label = NULL,
                        choices = overview_metric_choices,
                        multiple = TRUE
                      ),

                      help_note(
                        "ov_metric_help",
                        tags$p(
                          "Keeps islands modeled for any of the metrics you
                           pick, not only those modeled for all of them."
                        )
                      ),

                      selectInput(
                        "ov_intervention",
                        "Intervention modeled:",
                        choices = c(
                          overview_intervention_choices,
                          "None modeled"
                        ),
                        multiple = TRUE
                      ),

                      actionButton("ov_reset", "Reset filters"),

                      br(),
                      br(),

                      uiOutput("ov_count")
                    )
                  ),

                  column(
                    width = 8,

                    wellPanel(
                      # DISPLAY island map ----
                      leafletOutput(
                        "overview_map",
                        width = "100%",
                        height = "440px"
                      ) %>%
                        withSpinner(color = coral)
                    )
                  )
                ),

                # DISPLAY island table ----
                wellPanel(
                  class = "light-well",

                  uiOutput("overview_table")
                ),

                br(),

                h3(tags$strong("About Island Vital Signs")),

                wellPanel(
                  div(
                    class = "markdown-text",
                    includeMarkdown("text/m0-tool-overview-text.md")
                  ),

                  br(),

                  fluidRow(
                    column(
                      width = 12,

                      wellPanel(
                        class = "light-well",

                        # Make the title control the toggle of the well panel
                        actionLink(
                          "toggle_metric_info",
                          label = tagList(
                            icon("caret-right"),
                            tags$strong(
                              "Additional Notes on Vegetation Metrics"
                            )
                          )
                        ),

                        hidden(
                          div(
                            id = "metric_info",

                            br(),

                            div(
                              class = "markdown-text",
                              includeMarkdown("text/about-text2.md")
                            )
                          )
                        )
                      )
                    )
                  ),

                  br(),

                  div(
                    class = "markdown-text",
                    includeMarkdown("text/m0-tool-overview-text3.md")
                  ),

                  br()
                ),

                br()
              )
            )
          ), # END overview

          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ##                                                                            ~~
          ##                  ***START PER-PIXEL ANNUAL VEG MODULE***                 ----
          ##                                                                            ~~
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          tabPanel(
            title = "1. Annual Values",

            fluidRow(
              column(
                width = 12, # Content fills space

                # DISPLAY text ----
                br(),

                h3(tags$strong("Per-Pixel Modeled Annual Vegetation Values")),

                module_intro(
                  "Compare the model's yearly value for every pixel on an island between two ",
                  "years, then click a pixel to see its whole history. ",
                  actionButton(
                    inputId = "help_btn_annual",
                    label = "How to read this map",
                    class = "help-link"
                  )
                ),

                br(),

                fluidRow(
                  column(
                    width = 4,

                    wellPanel(
                      # SELECT island ----
                      selectInput(
                        "island_choice2",
                        "Select an Island:",
                        choices = c("", sort(island_metadata$Island)),
                        selected = NULL,
                        multiple = FALSE
                      ),

                      # SELECT metric ----
                      help_label(
                        "metric_choice2",
                        "Select a Metric:",
                        "annual_metric_help"
                      ),

                      selectInput(
                        "metric_choice2",
                        label = NULL,
                        choices = c(
                          "",
                          sort(c("EVI", "Tree Cover", "NDMI", "NDVI", "MSAVI2"))
                        )
                      ),

                      help_note(
                        "annual_metric_help",
                        tags$p(HTML(
                          "The vegetation measurement being mapped.
                           <strong>Tree Cover</strong> is canopy cover as a percentage.
                           <strong>NDMI</strong> tracks canopy moisture.
                           <strong>EVI</strong>, <strong>NDVI</strong> and
                           <strong>MSAVI2</strong> are greenness indices that stand
                           in for productivity."
                        )),
                        tags$p(
                          "Only metrics modeled for the island you picked are listed."
                        )
                      ),

                      # SELECT year (left) ----
                      help_label(
                        "year_choice_left",
                        "Select a Year (Left):",
                        "annual_years_help"
                      ),

                      selectInput(
                        "year_choice_left",
                        label = NULL,
                        choices = c("", 1984:2024)
                      ),

                      help_note(
                        "annual_years_help",
                        tags$p(
                          "The two years the map compares. The left year is drawn on ",
                          "the left of the divider, the right year on the right; drag ",
                          "the divider across to see either one in full."
                        ),
                        tags$p(
                          "Only years modeled for this island and metric are listed, ",
                          "so the list changes when you change either."
                        )
                      ),

                      #SELECT year (right) ----
                      selectInput(
                        "year_choice_right",
                        "Select a Year (Right):",
                        choices = c("", 1984:2024)
                      ),

                      # APPLY the selections ----
                      actionButton(
                        "apply_annual",
                        "Load map",
                        class = "apply-btn",
                        width = "100%"
                      ),
                    ),

                    wellPanel(
                      h4(tags$strong("Island Info")),
                      uiOutput("island_info")
                    ),

                    wellPanel(
                      h4(tags$strong("Download Data")),

                      downloadButton(
                        "download_annual_predicted_rasters",
                        label = "Download the Raster Stack"
                      ),

                      tags$div(style = "margin-top: 10px;"),

                      downloadButton(
                        "download_annual_predicted_maps",
                        label = "Download Annual Static Maps"
                      )
                    )
                  ),

                  column(
                    width = 8,

                    wellPanel(
                      # DISPLAY splitmap leaflet ----
                      leafletOutput(
                        "annual_predicted_splitmap",
                        width = "100%",
                        height = "500px"
                      ) %>%
                        withSpinner(color = coral),

                      br(),

                      # CLICK expand button ----
                      actionButton("expand_predictions_map", "Expand Map"),

                      br(),

                      h4(tags$strong("Selected Pixel:")),

                      wellPanel(
                        class = "light-well",

                        uiOutput("pixel_summary_text")
                      ),

                      # DISPLAY pixel trend plot ----
                      fluidRow(
                        column(
                          width = 6,
                          div(
                            class = "plot-card",
                            girafeOutput(
                              "pixel_trend_plot_a",
                              width = "100%",
                              height = "500px"
                            )
                          )
                        ),
                        column(
                          width = 6,
                          div(
                            class = "plot-card",
                            girafeOutput(
                              "pixel_trend_plot_b",
                              width = "100%",
                              height = "500px"
                            )
                          )
                        )
                      )
                    )
                  )
                ),

                br()
              )
            )
          ), # END per-pixel annual veg module

          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ##                                                                            ~~
          ##                        ***START CHANGE MAPS MODULE***                     ----
          ##                                                                            ~~
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          tabPanel(
            title = "2. Change Over Time",

            fluidRow(
              column(
                width = 12, # Content fills space

                # DISPLAY text ----
                br(),

                h3(tags$strong("Modeled Vegetation Change")),

                module_intro(
                  "Map how much each pixel changed over a time period, beside the ",
                  "pixels where the model is confident that change is real. ",
                  actionButton(
                    inputId = "help_btn_change",
                    label = "How to read this map",
                    class = "help-link"
                  )
                ),

                br(),

                fluidRow(
                  column(
                    width = 4,

                    wellPanel(
                      # SELECT island ----
                      selectInput(
                        "island_choice3",
                        "Select an Island:",
                        choices = c("", sort(island_metadata$Island)),
                        selected = NULL,
                        multiple = FALSE
                      ),

                      # SELECT metric ----
                      help_label(
                        "metric_choice3",
                        "Select a Metric:",
                        "change_metric_help"
                      ),

                      selectInput(
                        "metric_choice3",
                        label = NULL,
                        choices = c(
                          "",
                          sort(c("EVI", "Tree Cover", "NDMI", "NDVI", "MSAVI2"))
                        )
                      ),

                      help_note(
                        "change_metric_help",
                        tags$p(HTML(
                          "The vegetation measurement whose change is mapped.
                           <strong>Tree Cover</strong> is canopy cover as a percentage.
                           <strong>NDMI</strong> tracks canopy moisture.
                           <strong>EVI</strong>, <strong>NDVI</strong> and
                           <strong>MSAVI2</strong> are greenness indices that stand
                           in for productivity."
                        )),
                        tags$p(
                          "Only metrics modeled for the island you picked are listed."
                        )
                      ),

                      # SELECT timeperiod ----
                      help_label(
                        "timeperiod_choice1",
                        "Select a Trend Type:",
                        "change_period_help"
                      ),

                      selectInput(
                        "timeperiod_choice1",
                        label = NULL,
                        choices = c(
                          "",
                          "Fulltime",
                          "Pre-Intervention",
                          "Pre/Post1 Delta",
                          "Post-Intervention 1",
                          "Post1/Post2 Delta",
                          "Post-Intervention 2"
                        )
                      ),

                      help_note(
                        "change_period_help",
                        tags$p(HTML(
                          "Which stretch of the time series the change is measured over.
                           <strong>Fulltime</strong> is the whole series.
                           <strong>Pre-Intervention</strong> and
                           <strong>Post-Intervention</strong> are the periods before and
                           after a modeled eradication."
                        )),
                        tags$p(HTML(
                          "A <strong>Delta</strong> is the difference between two of those
                           periods: how much the trend itself shifted, rather than how much
                           the vegetation changed."
                        )),
                        tags$p(
                          "Only periods modeled for this island are listed, so an island ",
                          "with no eradication offers fewer of them."
                        )
                      ),

                      # SELECT probability level ----
                      help_label(
                        "probability_choice1",
                        "Advanced: Select a Probability Level (85% recommended):",
                        "change_probability_help"
                      ),

                      selectInput(
                        "probability_choice1",
                        label = NULL,
                        choices = c("75%", "85%", "95%"),
                        selected = "85%"
                      ),

                      help_note(
                        "change_probability_help",
                        tags$p(
                          "How sure the model has to be that a pixel's change is real ",
                          "rather than noise. At 75% the bar is low and more pixels ",
                          "qualify; at 95% only the clearest signals do."
                        ),
                        tags$p(HTML(
                          "85% is the default choice we recommend &mdash; a
                           moderate probability."
                        )),
                        tags$p(
                          "It decides which pixels appear on the right-hand map. The ",
                          "left-hand map always shows every modeled pixel."
                        )
                      ),

                      # SELECT change type ----
                      help_label(
                        "change_choice1",
                        "Select a Change Type:",
                        "change_type_help"
                      ),

                      selectInput(
                        "change_choice1",
                        label = NULL,
                        choices = c("", "Annualized", "Accumulated"),
                        selected = "Annualized"
                      ),

                      help_note(
                        "change_type_help",
                        tags$p(HTML(
                          "<strong>Annualized</strong> is the average change per year, so
                           its numbers are small rates. <strong>Accumulated</strong> is the
                           total change across the whole period, so its numbers are larger."
                        )),
                        tags$p(
                          "They are different units and should not be compared with ",
                          "one another."
                        )
                      ),

                      # APPLY the selections ----
                      actionButton(
                        "apply_change",
                        "Load map",
                        class = "apply-btn",
                        width = "100%"
                      ),
                    ),

                    wellPanel(
                      h4(tags$strong("Island Info")),
                      uiOutput("island_info2")
                    ),

                    wellPanel(
                      h4(tags$strong("Download Data")),

                      downloadButton(
                        "download_change_rasters",
                        label = "Download the Raster Stack"
                      ),

                      tags$div(style = "margin-top: 10px;"),

                      downloadButton(
                        "download_change_map",
                        label = "Download Static Change Map"
                      )
                    )
                  ),

                  column(
                    width = 8,

                    wellPanel(
                      # DISPLAY leaflet ----
                      leafletOutput(
                        "change_sync_map",
                        width = "100%",
                        height = "500px"
                      ) %>%
                        withSpinner(color = coral),

                      br(),

                      # CLICK expand button ----
                      actionButton("expand_change_map", "Expand Map"),

                      br(),

                      h4(tags$strong("Selected Pixel:")),

                      wellPanel(
                        class = "light-well",

                        uiOutput("pixel_summary_text2")
                      ),

                      br(),

                      # DISPLAY pixel trend plot ----
                      fluidRow(
                        column(
                          width = 6,
                          div(
                            class = "plot-card",
                            girafeOutput(
                              "pixel_trend_plot_a_change",
                              width = "100%",
                              height = "500px"
                            )
                          )
                        ),
                        column(
                          width = 6,
                          div(
                            class = "plot-card",
                            girafeOutput(
                              "pixel_trend_plot_b_change",
                              width = "100%",
                              height = "500px"
                            )
                          )
                        )
                      )
                    )
                  )
                ),

                br()
              )
            )
          ), # END change maps module

          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ##                                                                            ~~
          ##                  ***START TRENDS & CONDITIONS MODULE***                  ----
          ##                                                                            ~~
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          tabPanel(
            title = "3. Classified Change",

            fluidRow(
              column(
                width = 12, # Content fills space

                # DISPLAY text ----
                br(),

                h3(tags$strong("Vegetation Trends & Conditions")),

                module_intro(
                  "Ask one question of the modeled results for an island and ",
                  "see only the pixels that answer it. ",
                  actionButton(
                    inputId = "help_btn_tc",
                    label = "How to read this map",
                    class = "help-link"
                  )
                ),

                br(),

                fluidRow(
                  column(
                    width = 4,

                    wellPanel(
                      # SELECT island ----
                      selectInput(
                        "island_choice4",
                        "Select an Island:",
                        choices = c("", sort(island_metadata$Island)),
                        selected = NULL,
                        multiple = FALSE
                      ),

                      # SELECT metric ----
                      help_label(
                        "metric_choice4",
                        "Select a Metric:",
                        "tc_metric_help"
                      ),

                      selectInput(
                        "metric_choice4",
                        label = NULL,
                        choices = c(
                          "",
                          sort(c("EVI", "Tree Cover", "NDMI", "NDVI", "MSAVI2"))
                        )
                      ),

                      help_note(
                        "tc_metric_help",
                        tags$p(HTML(
                          "The vegetation measurement the question is asked of.
                           <strong>Tree Cover</strong> is canopy cover as a percentage.
                           <strong>NDMI</strong> tracks canopy moisture.
                           <strong>EVI</strong>, <strong>NDVI</strong> and
                           <strong>MSAVI2</strong> are greenness indices that stand
                           in for productivity."
                        )),
                        tags$p(
                          "Only metrics modeled for the island you picked are listed."
                        )
                      ),

                      # SELECT question ----
                      help_label(
                        "tc_question_choice",
                        "Select a Question:",
                        "tc_question_help"
                      ),

                      selectInput(
                        "tc_question_choice",
                        label = NULL,
                        choices = c("", tc_question_choices)
                      ),

                      help_note(
                        "tc_question_help",
                        tags$p(HTML(
                          "<strong>High / low</strong> compares each pixel's median
                           across the whole time series against the rest of the island,
                           and maps the top or bottom 25%."
                        )),
                        tags$p(HTML(
                          "<strong>High / low in the last 5 years</strong> does the same
                           on the last five years only, so it can differ from the
                           long-run picture."
                        )),
                        tags$p(HTML(
                          "<strong>Increasing / decreasing</strong> uses the accumulated
                           change instead, and maps only pixels whose trend is clear at
                           the probability level you choose."
                        ))
                      ),

                      # SELECT probability level ----
                      help_label(
                        "probability_choice2",
                        "Advanced: Select a Probability Level (85% recommended):",
                        "tc_probability_help"
                      ),

                      selectInput(
                        "probability_choice2",
                        label = NULL,
                        choices = c("75%", "85%", "95%"),
                        selected = "85%"
                      ),

                      help_note(
                        "tc_probability_help",
                        tags$p(
                          "How sure the model has to be before a pixel counts as changing. ",
                          "At 75% the bar is low and more pixels qualify; at 95% only the ",
                          "clearest signals do."
                        ),
                        tags$p(HTML(
                          "85% is the default choice we recommend &mdash; a
                           moderate probability."
                        )),
                        tags$p(
                          "This only affects the increasing and decreasing questions. The ",
                          "high and low questions compare medians and never use it."
                        )
                      ),

                      # DISPLAY a note when the level does not apply ----
                      uiOutput("tc_probability_note"),

                      # APPLY the selections ----
                      actionButton(
                        "apply_tc",
                        "Load map",
                        class = "apply-btn",
                        width = "100%"
                      )
                    ),

                    wellPanel(
                      h4(tags$strong("Island Info")),
                      uiOutput("island_info3")
                    ),

                    wellPanel(
                      h4(tags$strong("Download Data")),

                      downloadButton(
                        "download_tc_raster",
                        label = "Download the Classified Raster"
                      )
                    )
                  ),

                  column(
                    width = 8,

                    wellPanel(
                      # DISPLAY leaflet ----
                      leafletOutput(
                        "tc_map",
                        width = "100%",
                        height = "500px"
                      ) %>%
                        withSpinner(color = coral),

                      br(),

                      # CLICK expand button ----
                      actionButton("expand_tc_map", "Expand Map"),

                      br(),

                      h4(tags$strong("Pixels Shown:")),

                      wellPanel(
                        class = "light-well",

                        uiOutput("tc_summary_text")
                      ),

                      # the one thing a reader could carry away having misread
                      uiOutput("tc_result_note"),

                      # DISPLAY pixel trend plot ----
                      fluidRow(
                        column(
                          width = 6,
                          div(
                            class = "plot-card",
                            girafeOutput(
                              "pixel_trend_plot_a_tc",
                              width = "100%",
                              height = "500px"
                            )
                          )
                        ),
                        column(
                          width = 6,
                          div(
                            class = "plot-card",
                            girafeOutput(
                              "pixel_trend_plot_b_tc",
                              width = "100%",
                              height = "500px"
                            )
                          )
                        )
                      )
                    )
                  )
                ),

                br()
              )
            )
          ), # END trends & conditions module

          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ##                                                                            ~~
          ##                 ***START MANAGEMENT QUESTIONS MODULE***                  ----
          ##                                                                            ~~
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          tabPanel(
            title = "4. Management Questions",

            fluidRow(
              column(
                width = 12, # Content fills space

                # DISPLAY text ----
                br(),

                h3(tags$strong("Key Management Questions")),

                module_intro(
                  "Ask a management question of an island. There is no metric to ",
                  "pick: each question is answered by the measurement that suits it. ",
                  actionButton(
                    inputId = "help_btn_mq",
                    label = "How to read this map",
                    class = "help-link"
                  )
                ),

                br(),

                fluidRow(
                  column(
                    width = 4,

                    wellPanel(
                      # SELECT island ----
                      selectInput(
                        "island_choice5",
                        "Select an Island:",
                        choices = c("", sort(island_metadata$Island)),
                        selected = NULL,
                        multiple = FALSE
                      ),

                      # SELECT question ----
                      help_label(
                        "mq_question_choice",
                        "Select a Question:",
                        "mq_question_help"
                      ),

                      selectInput(
                        "mq_question_choice",
                        label = NULL,
                        choices = c("", mq_question_choices)
                      ),

                      help_note(
                        "mq_question_help",
                        tags$p(
                          "Each question combines the modeled results in its own way, ",
                          "and is answered by the measurement that suits it."
                        ),
                        tags$p(HTML(
                          "The four productivity questions run on whichever greenness
                           index was modeled for this island, so the index can differ
                           from island to island. The forest question always runs on
                           tree cover. The app names the one it used under
                           <strong>Answered using</strong>."
                        ))
                      ),

                      # SELECT probability level ----
                      help_label(
                        "probability_choice3",
                        "Advanced: Select a Probability Level (85% recommended):",
                        "mq_probability_help"
                      ),

                      selectInput(
                        "probability_choice3",
                        label = NULL,
                        choices = c("75%", "85%", "95%"),
                        selected = "85%"
                      ),

                      help_note(
                        "mq_probability_help",
                        tags$p(
                          "How sure the model has to be that a pixel's change is real ",
                          "rather than noise. At 75% the bar is low and more pixels ",
                          "qualify; at 95% only the clearest signals do."
                        ),
                        tags$p(HTML(
                          "85% is the default choice we recommend &mdash; a
                           moderate probability."
                        )),
                        tags$p(
                          "Every question here reads it, so changing it changes every ",
                          "map on this tab."
                        ),
                        tags$p(HTML(
                          "One exception: <strong>Where does vegetation appear to
                           resist change?</strong> asks where a trend is
                           <em>absent</em>, so the level reads the other way round
                           there. A wider interval is easier to overlap, so 95%
                           classifies more pixels than 75% does, not fewer."
                        ))
                      ),

                      # DISPLAY which metric answers the question ----
                      uiOutput("mq_metric_note"),

                      # APPLY the selections ----
                      actionButton(
                        "apply_mq",
                        "Load map",
                        class = "apply-btn",
                        width = "100%"
                      )
                    ),

                    wellPanel(
                      h4(tags$strong("Island Info")),
                      uiOutput("island_info4")
                    ),

                    wellPanel(
                      h4(tags$strong("Download Data")),

                      downloadButton(
                        "download_mq_raster",
                        label = "Download the Classified Raster"
                      )
                    )
                  ),

                  column(
                    width = 8,

                    wellPanel(
                      # DISPLAY leaflet ----
                      leafletOutput(
                        "mq_map",
                        width = "100%",
                        height = "500px"
                      ) %>%
                        withSpinner(color = coral),

                      br(),

                      # CLICK expand button ----
                      actionButton("expand_mq_map", "Expand Map"),

                      br(),

                      h4(tags$strong("Pixels Shown:")),

                      wellPanel(
                        class = "light-well",

                        uiOutput("mq_summary_text")
                      ),

                      # the one thing a reader could carry away having misread
                      uiOutput("mq_result_note"),

                      # DISPLAY pixel trend plot ----
                      fluidRow(
                        column(
                          width = 6,
                          div(
                            class = "plot-card",
                            girafeOutput(
                              "pixel_trend_plot_a_mq",
                              width = "100%",
                              height = "500px"
                            )
                          )
                        ),
                        column(
                          width = 6,
                          div(
                            class = "plot-card",
                            girafeOutput(
                              "pixel_trend_plot_b_mq",
                              width = "100%",
                              height = "500px"
                            )
                          )
                        )
                      )
                    )
                  )
                ),

                br()
              )
            )
          ), # END management questions module
        ) # END tabset paneling
      ), #END modeling results tab

      # START about tab ----
      tabItem(
        tabName = "about",

        fluidPage(
          useShinyjs(),

          # DISPLAY about content ----
          h3(tags$strong("About the Project")),

          div(class = "markdown-text", includeMarkdown("text/about-text.md")),

          div(class = "markdown-text", includeMarkdown("text/about-text3.md")),

          wellPanel(
            img(src = 'all-logos.png', width = '100%')
          ),

          br()
        )
      ), # END about tab

      # START news tab ----
      tabItem(
        tabName = "news",

        fluidPage(
          # DISPLAY news content ----
          div(class = "markdown-text", includeMarkdown("text/news-text.md"))
        )
      ), # END news tab

      # START resources tab ----
      tabItem(
        tabName = "resources",

        fluidPage(
          # DISPLAY resources content ----
          div(
            class = "markdown-text",
            includeMarkdown("text/resources-text.md")
          )
        )
      ) # END resources tab
    ) # END tab items
  ) # END dashboard body
) # END dashboard page
