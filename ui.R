# ui.R

# START dashboard page & ui ----
ui <- dashboardPage(
  
  # START dashboard header ----
  dashboardHeader(
    
    # set title
    title = span("Island Vital Signs", 
                 style = "font-size: 18px;")
    
  ), # END dashboard header
  
  # START dashboard sidebar ----
  dashboardSidebar(
    
    sidebarMenu(
      
      shinydashboard::menuItem("Modeling Results", tabName = "modeling_results", icon = icon("magnifying-glass-chart")),
      shinydashboard::menuItem("About", tabName = "about", icon = icon("info-circle")),
      shinydashboard::menuItem("News", tabName = "news", icon = icon("newspaper")),
      shinydashboard::menuItem("Resources", tabName = "resources", icon = icon("book"))
    )
    
  ), # END dashboard sidebar
  
  # START dashboard body ----
  dashboardBody(
    
    # Bring in custom css from stylesheet
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    # START tab items ----
    tabItems(
      
      # START modeling results tab ----
      tabItem(tabName = "modeling_results",
              
              # START module tabset paneling ----
              tabsetPanel(
                
                ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                ##                                                                            ~~
                ##                            ***START OVERVIEW***                          ----
                ##                                                                            ~~
                ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                
                tabPanel(title = "Overview",
                         
                         fluidRow(
                           
                           column(width = 12, # Content fills space
                                  
                                  # DISPLAY text ----
                                  br(),
                                  
                                  img(src='cropped-island-photo.jpg',
                                      width = '100%',
                                      style = "border-radius: 12px;"),
                                  
                                  h3(tags$strong("Island Vital Signs Modules Overview")),
                                  
                                  wellPanel(
                                    
                                    div(class = "markdown-text", includeMarkdown("text/m0-tool-overview-text.md")),
                                    
                                    br(),
                                    
                                    fluidRow(
                                      
                                      column(width = 12,
                                             
                                             wellPanel(class = "light-well",
                                                       
                                                       # Make the title control the toggle of the well panel
                                                       actionLink("toggle_metric_info", 
                                                                  label = tagList(
                                                                    icon("caret-right"), 
                                                                    tags$strong("Additional Notes on Vegetation Metrics")
                                                                  )),
                                                       
                                                       hidden(
                                                         div(id = "metric_info",
                                                             
                                                             br(),
                                                             
                                                             div(class = "markdown-text", includeMarkdown("text/about-text2.md")))
                                                         
                                                       ))
                                             
                                      )),
                                    
                                    br(),
                                    
                                    div(class = "markdown-text", includeMarkdown("text/m0-tool-overview-text3.md")),
                                    
                                    br())
                                  
                           )
                           
                         )
                         
                ), # END overview
                
                ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                ##                                                                            ~~
                ##                       ***START ISLAND-WIDE MODULE***                     ----
                ##                                                                            ~~
                ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # 
                #                 tabPanel(title = "Island-Wide",
                #                          
                #                          fluidRow(
                #                            
                #                            column(width = 12,
                #                                   
                #                                   # DISPLAY text ----
                #                                   br(),
                #                                   
                #                                   div(class = "markdown-text", includeMarkdown("text/m1-islandwide-text.md")),
                #                                   
                #                                   br(),
                #                                   
                #                                   img(src='under-construction.png',
                #                                       width = '50%'),
                #                                   
                #                                   br(),
                #                                   
                #                                   fluidRow(
                #                                     
                #                                     column(width = 4,
                #                                            
                #                                            # wellPanel(
                #                                            # 
                #                                            #   # SELECT island ----
                #                                            #   selectInput("island_choice1", "Select an Island:",
                #                                            #               choices = c("", sort(island_metadata$Island)),
                #                                            #               selected = NULL,
                #                                            #               multiple = FALSE),
                #                                            #   
                #                                            #   # SELECT metric ----
                #                                            #   selectInput("metric_choice1", "Select a Metric:",
                #                                            #               choices = c("", sort(c("EVI", "Tree Cover", "NDMI", "NDVI", "MSAVI2")))),
                #                                            #   
                #                                            #   # SELECT results type ----
                #                                            #   selectInput("results_choice", "Select a Result Type:",
                #                                            #               choices = c("", "Overall Trend", "Accumulated Change", "Annualized Change"))
                #                                            # 
                #                                            # )
                #                                            
                #                                     ),
                #                                     
                #                                     column(width = 8,
                #                                            
                #                                            # plots
                #                                            
                #                                     )
                #                                     
                #                                   ),
                #                                   
                #                                   br()
                #                                   
                #                                   ))), # END island-wide module
                
                ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                ##                                                                            ~~
                ##                  ***START PER-PIXEL ANNUAL VEG MODULE***                 ----
                ##                                                                            ~~
                ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                
                tabPanel(title = "Per-Pixel Annual Vegetation",
                         
                         fluidRow(
                           
                           column(width = 12, # Content fills space
                                  
                                  # DISPLAY text ----
                                  br(),
                                  
                                  div(
                                    style = "display: flex; align-items: center;",
                                    
                                    h3(tags$strong("Per-Pixel Modeled Annual Vegetation Values")),
                                    
                                    actionButton(
                                      inputId = "help_btn_annual",
                                      label = NULL,
                                      icon = icon("question-circle"),
                                      class = "help-btn"
                                    )
                                    
                                  ),
                                  
                                  br(),
                                  
                                  fluidRow(
                                    
                                    column(width = 4,
                                           
                                           wellPanel(
                                             
                                             # SELECT island ----
                                             selectInput("island_choice2", "Select an Island:",
                                                         choices = c("", sort(island_metadata$Island)),
                                                         selected = NULL,
                                                         multiple = FALSE),
                                             
                                             # SELECT metric ----
                                             selectInput("metric_choice2", "Select a Metric:",
                                                         choices = c("", sort(c("EVI", "Tree Cover", "NDMI", "NDVI", "MSAVI2")))),
                                             
                                             # SELECT year (left) ----
                                             selectInput
                                             ("year_choice_left", "Select a Year (Left):",
                                               choices = c("", 1984:2024)),
                                             
                                             #SELECT year (right) ----
                                             selectInput
                                             ("year_choice_right", "Select a Year (Right):",
                                               choices = c("", 1984:2024)),
                                             
                                           ),
                                           
                                           wellPanel(
                                             
                                             h4(tags$strong("Island Info")),
                                             uiOutput("island_info")
                                             
                                           ),
                                           
                                           wellPanel(
                                             
                                             h4(tags$strong("Download Data")),
                                             
                                             downloadButton("download_annual_predicted_rasters", label = "Download the Raster Stack"),
                                             
                                             tags$div(style = "margin-top: 10px;"),
                                             
                                             downloadButton("download_annual_predicted_maps", label = "Download Annual Static Maps"),
                                             
                                             # tags$div(style = "margin-top: 10px;"),
                                             # 
                                             # downloadButton("download_pixel_plot1", label = HTML("Download This Pixel's<br>Timeseries Plot"))
                                             # 
                                           )
                                           
                                    ),
                                    
                                    column(width = 8,
                                           
                                           wellPanel(
                                             
                                             # DISPLAY splitmap leaflet ----
                                             leafletOutput("annual_predicted_splitmap", 
                                                           width = "100%", 
                                                           height = "500px") %>% 
                                               withSpinner(color = coral),
                                             
                                             br(),
                                             
                                             # CLICK expand button ----
                                             actionButton("expand_predictions_map", "Expand Map"),
                                             
                                             br(),
                                             
                                             h4(tags$strong("Selected Pixel:")),
                                             p("Click on a pixel above to see more information"),
                                             
                                             wellPanel(class = "light-well",
                                                       
                                                       uiOutput("pixel_summary_text")
                                                       
                                             ),
                                             
                                             # DISPLAY pixel trend plot ----
                                             fluidRow(
                                               column(
                                                 width = 6,
                                                 girafeOutput("pixel_trend_plot_a", width = "100%", height = "500px")
                                               ),
                                               column(
                                                 width = 6,
                                                 girafeOutput("pixel_trend_plot_b", width = "100%", height = "500px")
                                               )
                                             )
                                             # girafeOutput("pixel_trend_plot", 
                                             #              width = "100%", 
                                             #              height = "300px"),
                                             
                                             # plotOutput("pixel_trend_plot", 
                                             #            width = "100%", 
                                             #            height = "300px"),
                                             
                                           )
                                           
                                    )
                                    
                                  ),
                                  
                                  br()
                                  
                           ))), # END per-pixel annual veg module
                
                ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                ##                                                                            ~~
                ##                        ***START CHANGE MAPS MODULE***                     ----
                ##                                                                            ~~
                ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                
                tabPanel(title = "Change Maps",
                         
                         fluidRow(
                           
                           column(width = 12, # Content fills space
                                  
                                  # DISPLAY text ----
                                  br(),
                                  
                                  div(
                                    style = "display: flex; align-items: center;",
                                    
                                    h3(tags$strong("Modeled Vegetation Change")),
                                    
                                    actionButton(
                                      inputId = "help_btn_change",
                                      label = NULL,
                                      icon = icon("question-circle"),
                                      class = "help-btn"
                                    )
                                    
                                  ),
                                  
                                  br(),
                                  
                                  fluidRow(
                                    
                                    column(width = 4,
                                           
                                           wellPanel(
                                             
                                             # SELECT island ----
                                             selectInput("island_choice3", "Select an Island:",
                                                         choices = c("", sort(island_metadata$Island)),
                                                         selected = NULL,
                                                         multiple = FALSE),
                                             
                                             # SELECT metric ----
                                             selectInput("metric_choice3", "Select a Metric:",
                                                         choices = c("", sort(c("EVI", "Tree Cover", "NDMI", "NDVI", "MSAVI2")))),
                                             
                                             # SELECT timeperiod ----
                                             selectInput("timeperiod_choice1", "Select a Trend Type:",
                                                         choices = c("", "Fulltime", "Pre-Intervention", "Pre/Post1 Delta", "Post-Intervention 1", "Post1/Post2 Delta", "Post-Intervention 2")),
                                             
                                             # SELECT probability level ----
                                             selectInput("probability_choice1", "Select a Probability Level (likelihood the change is real):",
                                                         choices = c("", "75%", "85%", "95%")),
                                             
                                             # SELECT change type ----
                                             selectInput("change_choice1", "Select a Change Type:",
                                                         choices = c("", "Annualized", "Accumulated"),
                                                         selected = "Annualized"),
                                             
                                           ),
                                           
                                           wellPanel(
                                             
                                             h4(tags$strong("Island Info")),
                                             uiOutput("island_info2") 
                                             
                                           ),
                                           
                                           wellPanel(
                                             
                                             h4(tags$strong("Download Data")),
                                             
                                             downloadButton("download_change_rasters", label = "Download the Raster Stack"),
                                             
                                             tags$div(style = "margin-top: 10px;"),
                                             
                                             downloadButton("download_change_map", label = "Download Static Change Map"),
                                             
                                             # tags$div(style = "margin-top: 10px;"),
                                             # 
                                             # downloadButton("download_pixel_plot2", label = HTML("Download This Pixel's<br>Timeseries Plot"))
                                             # 
                                           )
                                           
                                    ),
                                    
                                    column(width = 8,
                                           
                                           wellPanel(
                                             
                                             # DISPLAY leaflet ----
                                             leafletOutput("change_sync_map", 
                                                           width = "100%",
                                                           height = "500px") %>% 
                                               withSpinner(color = coral),
                                             
                                             br(),
                                             
                                             # CLICK expand button ----
                                             actionButton("expand_change_map", "Expand Map"),
                                             
                                             br(),
                                             
                                             h4(tags$strong("Selected Pixel:")),
                                             p("Click on a pixel above to see more information"),
                                             
                                             wellPanel(class = "light-well",
                                                       
                                                       uiOutput("pixel_summary_text2")
                                                       
                                             ),
                                             
                                             br(),
                                             
                                             # DISPLAY pixel trend plot ----
                                             fluidRow(
                                               column(
                                                 width = 6,
                                                 girafeOutput("pixel_trend_plot_a_change", width = "100%", height = "500px")
                                               ),
                                               column(
                                                 width = 6,
                                                 girafeOutput("pixel_trend_plot_b_change", width = "100%", height = "500px")
                                               )
                                             )
                                             
                                           )
                                           
                                    )
                                    
                                  ),
                                  
                                  br()
                                  
                           ))), # END change maps module
                
                ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                ##                                                                            ~~
                ##                 ***START RESTORATION ACTION MAP MODULE***                ----
                ##                                                                            ~~
                ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                
                # tabPanel(title = "Restoration Action",
                # 
                #          fluidRow(
                # 
                #            # Initiate allowing Shiny Java Script
                #            useShinyjs(),
                # 
                #            column(width = 12, # Content fills space
                # 
                #                   # DISPLAY text ----
                #                   br(),
                # 
                #                   div(
                #                     style = "display: flex; align-items: center;",
                # 
                #                     h3(tags$strong("Restoration Action Map")),
                # 
                #                     actionButton(
                #                       inputId = "help_btn_ram",
                #                       label = NULL,
                #                       icon = icon("question-circle"),
                #                       class = "help-btn"
                #                     )
                # 
                #                   ),
                # 
                #                   br(),
                # 
                #                   h4("1. Select an island, a year of data to filter and evaluate (default is the most recent year), and your preferred probability level. Select 'All Pixels' if you do not want to filter on 'significance' of change. Select an 'inverse' option if you want pixels that are NOT shown to have a significant trend at that level."),
                # 
                #                   wellPanel(
                # 
                #                     fluidRow(
                # 
                #                       column(width = 4,
                # 
                #                              # SELECT island ----
                #                              selectInput("island_choice5", "Select an Island:",
                #                                          choices = c("", sort(island_metadata$Island)),
                #                                          selected = NULL,
                #                                          multiple = FALSE),
                # 
                #                              # # SELECT probability level ----
                #                              # selectInput("probability_ram", "Select Your Accepted Trend/Change Probability Level:",
                #                              #             choices = c("", "All Pixels", ".75", ".85", ".95", ".75 Inverse", ".85 Inverse", ".95 Inverse"))
                # 
                #                       ),
                # 
                #                       column(width = 4,
                # 
                #                              # SELECT year ----
                #                              sliderInput
                #                              ("ram_year", "Select a Year to Evaluate:",
                #                                min = as.Date("1984-01-01"),
                #                                max = as.Date("2024-01-01"),
                #                                value = as.Date("2023-01-01"),
                #                                timeFormat = "%Y"),
                # 
                #                              # # SELECT change type ----
                #                              # selectInput("change_choice2", "Select a Change Type:",
                #                              #             choices = c("", "Annualized (Change / Year)", "Accumulated (Total Change)"),
                #                              #             selected = "Annualized")
                # 
                #                       ),
                # 
                #                       column(width = 4,
                # 
                #                              # SELECT timeperiod ----
                #                              selectInput("timeperiod_choice3", "Select a Change Period:",
                #                                          choices = c("",
                #                                                      "Fulltime",
                #                                                      "Pre-Intervention",
                #                                                      "Pre/Post1 Delta",
                #                                                      "Post-Intervention 1",
                #                                                      "Post1/Post2 Delta",
                #                                                      "Post-Intervention 2")),
                # 
                #                       ))),
                # 
                #                   h4("2. Filter on ranges and change of each metric. Either filter on annualized change or accumulated change. We recommend only focusing on one metric at a time. The map will filter based on the ranges you have selected, once submitted. Leave the default ranges of anything not filtered on. Filtering too many things at once may result in null resultes (no pixels matching all criteria)."),
                # 
                #                   fluidRow(
                # 
                #                     column(width = 4,
                # 
                #                            wellPanel(
                # 
                #                              # Control which metric is selected
                #                              #checkboxInput("ram_tc_show", "Filter on Woody Cover", value = FALSE),
                # 
                #                              # Make the title control the toggle of the well panel
                #                              actionLink("toggle_tc_ram",
                #                                         label = tagList(
                #                                           icon("caret-right"),
                #                                           tags$strong("Woody Structure (Tree Cover)")
                #                                         )),
                # 
                #                              hidden(
                #                                div(id = "tc_ram",
                # 
                #                                    br(),
                # 
                #                                    # SELECT tc range ----
                #                                    sliderInput("ram_tc_range", "Tree Cover Range (year selected above):",
                #                                                min = 0,
                #                                                max = 100,
                #                                                value = c(0, 100),
                #                                                step = 10),
                # 
                #                                    # SELECT probability level ----
                #                                    selectInput("probability_ram_tc", "Select Your Accepted Change Probability Level:",
                #                                                choices = c("", "All Pixels", "75%", "85%", "95%", "75% Inverse", "85% Inverse", "95% Inverse"),
                #                                                selected = "All Pixels"),
                # 
                #                                    # SELECT annualized range ----
                #                                    sliderInput("ram_tc_annualized_range", "Tree Cover Annualized Change Range (time period selected above):",
                #                                                min = -10,
                #                                                max = 10,
                #                                                value = c(-10, 10),
                #                                                step = 2),
                # 
                #                                    h4(tags$strong("OR")),
                # 
                #                                    # SELECT accumulated range ----
                #                                    sliderInput("ram_tc_accumulated_range", "Tree Cover Accumulated Change Range (time period selected above):",
                #                                                min = -100,
                #                                                max = 100,
                #                                                value = c(-100, 100),
                #                                                step = 10),
                #                                ))
                # 
                #                            )
                #                     ),
                # 
                #                     column(width = 4,
                # 
                #                            wellPanel(
                # 
                #                              # Control which metric is selected
                #                              #checkboxInput("ram_ndmi_show", "Filter on Canopy Moisture", value = FALSE),
                # 
                #                              # Make the title control the toggle of the well panel
                #                              actionLink("toggle_ndmi_ram",
                #                                         label = tagList(
                #                                           icon("caret-right"),
                #                                           tags$strong("Canopy Moisture (NDMI)")
                #                                         )),
                # 
                #                              hidden(
                #                                div(id = "ndmi_ram",
                # 
                #                                    br(),
                # 
                #                                    # SELECT ndmi range ----
                #                                    sliderInput("ram_ndmi_range", "NDMI Range (year selected above):",
                #                                                min = -1,
                #                                                max = 1,
                #                                                value = c(-1, 1),
                #                                                step = 0.1),
                # 
                #                                    # SELECT probability level ----
                #                                    selectInput("probability_ram_ndmi", "Select Your Accepted Change Probability Level:",
                #                                                choices = c("", "All Pixels", "75%", "85%", "95%", "75% Inverse", "85% Inverse", "95% Inverse"),
                #                                                selected = "All Pixels"),
                # 
                #                                    # SELECT annualized range ----
                #                                    sliderInput("ram_ndmi_annualized_range", "NDMI Annualized Change Range (time period selected above):",
                #                                                min = -.1,
                #                                                max = .1,
                #                                                value = c(-.1, .1),
                #                                                step = 0.02),
                # 
                #                                    h4(tags$strong("OR")),
                # 
                #                                    # SELECT annualized range ----
                #                                    sliderInput("ram_ndmi_accumulated_range", "NDMI Accumulated Change Range (time period selected above):",
                #                                                min = -2,
                #                                                max = 2,
                #                                                value = c(-2, 2),
                #                                                step = 0.2),
                # 
                #                                ))
                # 
                #                            )
                #                     ),
                # 
                #                     column(width = 4,
                # 
                #                            wellPanel(
                # 
                #                              # Control which metric is selected
                #                              #checkboxInput("ram_prod_show", "Filter on Productivity", value = FALSE),
                # 
                #                              # Make the title control the toggle of the well panel
                #                              actionLink("toggle_prod_ram",
                #                                         label = tagList(
                #                                           icon("caret-right"),
                #                                           tags$strong("Productivity (NDVI/EVI/MSAVI2)")
                #                                         )),
                # 
                #                              hidden(
                #                                div(id = "prod_ram",
                # 
                #                                    br(),
                # 
                #                                    # SELECT productivity range ----
                #                                    sliderInput("ram_prod_range", "NDVI/EVI/MSAVI2 Range (year selected above):",
                #                                                min = 0,
                #                                                max = 1,
                #                                                value = c(0, 1),
                #                                                step = 0.01),
                # 
                #                                    # SELECT probability level ----
                #                                    selectInput("probability_ram_prod", "Select Your Accepted Change Probability Level:",
                #                                                choices = c("", "All Pixels", "75%", "85%", "95%", "75% Inverse", "85% Inverse", "95% Inverse"),
                #                                                selected = "All Pixels"),
                # 
                #                                    # SELECT annualized range ----
                #                                    sliderInput("ram_prod_annualized_range", "NDVI/EVI/MSAVI2 Annualized Change Range (time period selected above):",
                #                                                min = -.1,
                #                                                max = .1,
                #                                                value = c(-.1, .1),
                #                                                step = 0.02),
                # 
                #                                    h4(tags$strong("OR")),
                # 
                #                                    # SELECT annualized range ----
                #                                    sliderInput("ram_prod_accumulated_range", "NDVI/EVI/MSAVI2 Accumulated Change Range (time period selected above):",
                #                                                min = -1,
                #                                                max = 1,
                #                                                value = c(-1, 1),
                #                                                step = 0.2),
                # 
                #                                ))
                # 
                #                            )
                #                     )
                # 
                #                   ),
                # 
                #                   fluidRow(
                # 
                #                     column(width = 12,
                # 
                #                            wellPanel(
                # 
                #                              # Make the title control the toggle of the well panel
                #                              actionLink("toggle_ter_ram",
                #                                         label = tagList(
                #                                           icon("caret-right"),
                #                                           tags$strong("Terrain")
                #                                         )),
                # 
                #                              hidden(
                #                                div(id = "ter_ram",
                # 
                #                                    fluidRow(
                # 
                #                                      column(width = 4,
                # 
                #                                             br(),
                # 
                #                                             # SELECT slope range ----
                #                                             sliderInput("ram_slope_range", "Slope (°):",
                #                                                         min = 0,
                #                                                         max = 90,
                #                                                         value = c(0, 90)),
                # 
                #                                             # SELECT twi range ----
                #                                             sliderInput("ram_twi_range", "Topographic Wetness Index:",
                #                                                         min = 0,
                #                                                         max = 35,
                #                                                         value = c(0, 35)),
                # 
                #                                      ),
                # 
                #                                      column(width = 4,
                # 
                #                                             br(),
                # 
                #                                             # SELECT elevation range ----
                #                                             sliderInput("ram_elevation_range", "Elevation (m):",
                #                                                         min = 0,
                #                                                         max = 1000,
                #                                                         value = c(0, 1000)),
                # 
                #                                             # SELECT ecosystem types ----
                #                                             selectizeInput("ram_aspect_type",
                #                                                            "Aspect:",
                #                                                            choices = c("North (315° - 45°)",
                #                                                                        "East (45° - 135°),",
                #                                                                        "South (135° - 225°)",
                #                                                                        "West (225° - 315°)"),
                #                                                            selected = c("North (315° - 45°)",
                #                                                                         "East (45° - 135°),",
                #                                                                         "South (135° - 225°)",
                #                                                                         "West (225° - 315°)"),
                #                                                            multiple = TRUE,
                #                                                            options = list(plugins = list("remove_button")))
                # 
                #                                      ),
                # 
                #                                      column(width = 4,
                # 
                #                                             br(),
                # 
                #                                             # SELECT ecosystem types ----
                #                                             selectizeInput("ram_eco_type",
                #                                                            "Ecosystem Type:",
                #                                                            choices = c("Tree",
                #                                                                        "Shrubland",
                #                                                                        "Grassland",
                #                                                                        "Cropland",
                #                                                                        "Urban",
                #                                                                        "Bare",
                #                                                                        "Snow",
                #                                                                        "Water",
                #                                                                        "Wetland",
                #                                                                        "Mangrove",
                #                                                                        "Moss",
                #                                                                        "Undefined"),
                #                                                            selected = c("Tree",
                #                                                                         "Shrubland",
                #                                                                         "Grassland",
                #                                                                         "Cropland",
                #                                                                         "Urban",
                #                                                                         "Bare",
                #                                                                         "Snow",
                #                                                                         "Water",
                #                                                                         "Wetland",
                #                                                                         "Mangrove",
                #                                                                         "Moss",
                #                                                                         "Undefined"),
                #                                                            multiple = TRUE,
                #                                                            options = list(plugins = list("remove_button")))
                # 
                #                                      ))
                # 
                #                                ))
                # 
                #                            ),
                # 
                #                            h4("3. Select 'Update Map' and wait for the map to render. View the map below. Toggle on and off vaious layers. Each layer has been filtered to the desired ranges. The 'Mask' layer indicates which pixels accomodate all user selections above."),
                # 
                #                            actionButton("update_restoration_action_map",
                #                                         label = "Update Plot",
                #                                         class = "custom-submit"),
                # 
                #                            br(),
                # 
                # 
                #                            # CLICK expand button ----
                #                            #actionButton("expand_action_map", "Expand Map"),
                # 
                #                            br(),
                # 
                #                            #verbatimTextOutput("check"),
                # 
                #                     )),
                # 
                #                   fluidRow(
                # 
                #                     column(width = 12,
                # 
                #                            wellPanel(
                # 
                #                              # DISPLAY leaflet ----
                #                              leafletOutput("restoration_action_map",
                #                                            width = "100%",
                #                                            height = "500px") %>%
                #                              withSpinner(color = coral),
                # 
                #                              br()
                # 
                #                            )))
                # 
                #                   # # DISPLAY pixel trend plot ----
                #                   # plotOutput("pixel_trend_plot_ram",
                #                   #            width = "100%",
                #                   #            height = "300px")
                # 
                # 
                # 
                # 
                # 
                #            ))) # END restoration action maps module
                
              ) # END tabset paneling
              
      ), #END modeling results tab
      
      # START about tab ----
      tabItem(tabName = "about",
              
              fluidPage(
                
                useShinyjs(),
                
                # DISPLAY about content ----
                h3(tags$strong("About the Project")),
                
                div(class = "markdown-text", includeMarkdown("text/about-text.md")),
                
                div(class = "markdown-text", includeMarkdown("text/about-text3.md")),
                
                wellPanel(
                  
                  img(src='all-logos.png',
                      width = '100%')
                  
                ),
                
                br()
                
              )), # END about tab
      
      # START news tab ----
      tabItem(tabName = "news",
              
              fluidPage(
                
                # DISPLAY news content ----
                div(class = "markdown-text", includeMarkdown("text/news-text.md"))
                
              )), # END news tab
      
      # START resources tab ----
      tabItem(tabName = "resources",
              
              fluidPage(
                
                # DISPLAY resources content ----
                div(class = "markdown-text", includeMarkdown("text/resources-text.md"))
                
              )) # END resources tab
      
    ) # END tab items
    
  ) # END dashboard body
  
) # END dashboard page
