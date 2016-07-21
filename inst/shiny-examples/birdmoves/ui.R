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

     .bird-img {
        position:relative;
        width:100%;
     }
     .wiki-watermark {
        background: rgb(0, 0, 0); /* fallback color */
        background: rgba(0, 0, 0, 0.7);
        position:absolute;
        left:0px;
        bottom:0px;
        padding: 5px;
        color:white;
        font-weight:bold;
        font-size: 75%;
     }
     .wiki-watermark a:link, .wiki-watermark a:visited, .wiki-watermark a:hover, .wiki-watermark a:active, .wiki-watermark a:focus {
        color:white;
     }
    "))
  ),

  navbarPage(title = HTML("bird<strong>moves</strong>"),

             #################
             ## SETUP
             #################
             tabPanel("Select Data",
                      mod_UI_db_data("access")
              ),

            #################
            ## Watch Now
            #################
            tabPanel("Current Activity",
                     mod_UI_map_current("current")
                     # fluidRow(
                     #   column(2,
                     #          h3("Summary"),
                     #          bsButton("current_update", "Update Now"),
                     #          htmlOutput("summary_current")),
                     #   column(10,
                     #          leafletOutput("map_current", height = 600),
                     #          htmlOutput('current_time')
                     #   )
                     # )

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
            ## BIRDS
            #################
            tabPanel("Birds",
                     fluidRow(column(6,
                                     h3("Click on a row for more information")),
                              column(6, htmlOutput("img_birds"))),
                     fluidRow(DT::dataTableOutput("dt_birds"))
            ),
            #################
            ## DATA
            #################
            tabPanel("Data Transformations",
                     column(3,
                            h2("Downloads"),
                            p(shinyjs::disabled(downloadButton('data_dl', 'Download All'))),
                            p(shinyjs::disabled(downloadButton("data_dl_raw", "Raw"))),
                            p(shinyjs::disabled(downloadButton("data_dl_visits", "Visits"))),
                            p(shinyjs::disabled(downloadButton("data_dl_feeding", "Feeding"))),
                            p(shinyjs::disabled(downloadButton("data_dl_move", "Movements")))
                     ),
                     column(9,
                            tabsetPanel(type = "tabs",
                                        tabPanel("Raw Data", DT::dataTableOutput("dt_raw")),
                                        tabPanel("Visits Data", DT::dataTableOutput("dt_v")),
                                        tabPanel("Feeding Data", DT::dataTableOutput("dt_f")),
                                        tabPanel("Movement Data", DT::dataTableOutput("dt_m"))
                            )
                     )
            ),
            tabPanel("About",
                     h1("Project"),
                     p(HTML("The bird<strong>moves</strong> project is large collaborative effort to develop tools for the observation, visualization, and analysis of animal movements registered by RFID feeders or other static recording stations.")),

                     p("We hope that this project will be useful to scientists, citizen scientists, students and educators alike."),
                     p("Our goals are to provide a tool that is easy to use, yet powerful."),
                     p(),
                     "If you're interested in contruting functionality and/or coding to this project, check out our project on ", HTML(paste0("<a href = 'http://github.org/steffilazerte/feedr/' target = 'blank'>GitHub <img src = 'GitHub-Mark-32px.png'></a>")), "We welcome all involvement!",

                     h1("People"),
                     "Project Blurbs and Pictures",
                     "Stefanie LaZerte",
                     "Matt Reudink",
                     "David Hill",
                     "Etc."

            ),
            tabPanel("References",
                     h1("FAQ"),
                     h1("Caveats and Disclaimers"),
                     h1("Tutorials"),
                     h1("Glossary")
            )

  )
))
