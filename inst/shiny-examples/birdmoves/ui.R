library(shiny)
library(shinyjs)
library(shinyBS)
library(leaflet)
library(feedr)
#library(ggvis)

shinyUI(fluidPage(

  shinyjs::useShinyjs(),

  ## CSS Styling

  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        padding: 1em;
        font-weight: bold;
        color: green;
      }
      .shiny-progress .progress-text {
        background-color: orange;
      }
      .shiny-progress .progress-text .progress-message {
        font-size: 100%;
        color: #03F;
      }
     img {
        border: 1;
        max-width: 100%;
     }
     element.style {
        width: 33.33%;
     }
    "))
  ),

  navbarPage(title = HTML("bird<strong>moves</strong>"),

             #################
             ## SETUP
             #################
             tabPanel("Select Data",
                      fluidRow(
                        column(4,
                               div(img(src = "logo.jpg", width = 400), style="text-align: left;"),
                               h2("Data Selection"),
                               h3("Select Data"),
                               uiOutput("UI_data_site_name"),
                               bsTooltip("data_site_name", "Site to grab data from",
                                         "right", options = list(container = "body")),
                               uiOutput("UI_data_species"),
                               bsTooltip("UI_data_species", "Species(s) to include/exclude",
                                         "right", options = list(container = "body")),
                               uiOutput("UI_data_date"),
                               radioButtons("data_weather", label = "Include Weather?",
                                            choices = c("Yes", "No"), selected = "Yes"),
                               bsTooltip("data_weather", "Should weather data be downloaded from Environment Canada and merged in?",
                                         "right", options = list(container = "body")),

                               bsButton("map_update", "Update map", style = "primary"),
                               p(),
                               bsButton("data_reset", "Reset inputs", style = "danger"),
                               bsButton("data_get", "Get Data"),
                               hr(),
                               strong("Data Access: "), textOutput("data_access", inline = TRUE),
                               tableOutput("data_selection"),
                               bsTooltip("data_selection", "Number of visits per species given the options selected.",
                                         "right", options = list(container = "body")),
                               hr(),
                               h3("Advanced Options"),
                               #bsButton("data_pause", "pause", style = "primary"),
                               actionButton("data_showadv", "Show Advanced Options"),
                               hidden(div(id = "advanced",
                                          uiOutput("UI_data_bird_id"),
                                          uiOutput("UI_data_feeder_id")))
                        ),
                        column(8,
                               leafletOutput("map_data", height = 600),
                               bsPopover("map_data", "Sampling", "Circle area depicts the amount of visits recorded per site or feeder given the options selected", placement = "top", trigger = "hover"),
                               plotOutput("plot_data_ggplot",
                                          brush = brushOpts(
                                            id = "plot_data_brush",
                                            direction = "x",
                                            resetOnNew = TRUE),height = "100%"),
                               div(strong("Drag and select a date range to further refine the data selection"), style = "text-align: center;")
                               #ggvisOutput("plot_data_ggvis")
                        )
                      )
              ),
            # tabPanel("Current Data",
            #          h1("Current Data Selection"),
            #          htmlOutput("data_current"),
            #          div("DATA TABLE")
            # ),

            #################
            ## Watch Now
            #################
            tabPanel("Current Activity",
                     bsButton("current_update", "Update Now"),
                     leafletOutput("map_current", height = 600),
                     htmlOutput('current_time')

            ),

            #################
            ## Visualization
            #################
            tabPanel("Visualizations",
                     mod_UI_map_animate("anim")
            ),

            # tabPanel("Paths (Static)",
            #          column(3,
            #                 h1("Data"),
            #                 uiOutput("UI_static_bird_id"),
            #                 h3("To Do:"),
            #                 div("Based on feedr package visualizations"),
            #                 div("Change feedr functions to have map and layers separate, so doesn't refresh entire map for each bird")
            #                 #sliderInput("speed", "Speed",
            #                 #            min = 0, max = 100,
            #                 #            post = "%",
            #                 #            value = 50),
            #                 #sliderInput("interval", "Interval",
            #                 #            min = 1,
            #                 #            max = 24,
            #                 #            value = 1,
            #                 #            post = " hour(s)")
            #          ),
            #          column(9,
            #                 fluidRow(leafletOutput("map_static", height = 600))
            #          ),
            #          fluidRow(
            #                   #DT::dataTableOutput("dt_paths")
            #                   )
            #
            # ),
            #################
            ## DATA
            #################
            tabPanel("Birds",
                     fluidRow(column(6,
                                     h3("Click on a row for more information"),
                                     div("Presumably, we could replace images these with the actual pictures that are stored in the database? Right now they all come from wikipedia")),
                              column(6, htmlOutput("img_birds"))),
                     fluidRow(DT::dataTableOutput("dt_birds"))
            ),
            tabPanel("Data Transformations",
                     column(2,
                            "This is where you can download your data",
                            br(),
                            bsButton("data_dl_all", "Download All", style = "success")
                            ),
                     column(10,
                            tabsetPanel(type = "tabs",
                                        tabPanel("Raw Data",
                                                 br(), downloadButton("data_dl_raw", "Download Raw Data"), br(),
                                                 DT::dataTableOutput("dt_raw")),
                                        tabPanel("Visits Data",
                                                 br(), downloadButton("data_dl_visits", "Download Visits Data"), br(),
                                                 DT::dataTableOutput("dt_v")),
                                        tabPanel("Feeding Data",
                                                 br(), downloadButton("data_dl_feeding", "Download Feeding Data"), br(),
                                                 DT::dataTableOutput("dt_f")),
                                        tabPanel("Movement Data",
                                                 br(), downloadButton("data_dl_move", "Download Movements Data"), br(),
                                                 DT::dataTableOutput("dt_m"))
                            )
                     )
            ),
            tabPanel("About",
                     h1("Project"),
                     "If you're interested in contruting functions and/or coding to this project, check out our project on ", a("github", href = "http://github.org/steffilazerte/feedr/"),

                     h1("People"),
                     "Stefanie LaZerte",
                     "Matt Reudink",
                     "David Hill",
                     "Etc."

            )
  )
))
