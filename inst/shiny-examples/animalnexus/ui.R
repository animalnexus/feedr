library(shiny)
library(shinyjs)
library(shinyBS)
library(leaflet)
library(feedr)
#library(ggvis)

shinyUI(
  tagList(
    shinyjs::useShinyjs(),
    navbarPage(title = a(href = "http://animalnexus.ca", HTML("animal<strong>nexus</strong>")),
               id = "main",
               position = "fixed-top",
               collapsible = TRUE,
               windowTitle = "animalnexus",
               theme = "style.css",
               footer = column(12,
                               hr(),
                               div(class = "data-status", textOutput("data_info"))),

             #################
             ## Watch Now
             #################
             tabPanel("Home",
                      fluidRow(
                        div(style = "text-align:center", HTML("<h1>Welcome to animal<strong>nexus</strong></h1>")),
                        h4(style = "text-align:center", "This webapp is based on R shiny and uses the 'feedr' package to transform, summarize and visualize animal movement data collected from RFID stations."),
                        h4(style = "text-align:center", "To get started, ", actionLink("link_db", "select data from our data base"), "or", actionLink("link_import", "import your own"), "."),
                        #actionButton("pause", "Pause"),
                        hr(),
                        h4(style = "text-align:center", "Current activity at feeders on Thompson Rivers University Campus")),
                      fluidRow(
                        column(10, offset = 1,
                               div(style = "max-width: 800px; margin-left: auto; margin-right:auto;", feedr:::mod_UI_map_current("current"))
                        ))
             ),

             #################
             ## SETUP
             #################
             tabPanel(title = "Database", icon = icon("database"),
                      feedr:::mod_UI_data_db("access")),
             tabPanel(title = "Import", icon = icon("upload"),
                      feedr:::mod_UI_data_import("import")),

            #################
            ## Visualization
            #################
            tabPanel("Visualizations", icon = icon("eye"),
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
            tabPanel("Individuals",
                     fluidRow(column(6,
                                     h3("Click on a row for more information")),
                              column(6, htmlOutput("img_birds"))),
                     fluidRow(DT::dataTableOutput("dt_birds"))
            ),
            #################
            ## DATA
            #################
            tabPanel("Transformations", icon = icon("exchange"),
                     column(3,
                            htmlOutput("data_desc"),
                            hr(),
                            h3("Downloads"),
                            p(shinyjs::disabled(downloadButton('data_dl', 'All'))),
                            p(shinyjs::disabled(downloadButton("data_dl_raw", "Raw"))),
                            p(shinyjs::disabled(downloadButton("data_dl_visits", "Visits"))),
                            p(shinyjs::disabled(downloadButton("data_dl_feeding", "Feeding"))),
                            p(shinyjs::disabled(downloadButton("data_dl_move", "Movements")))
                     ),
                     column(9,
                            tabsetPanel(type = "tabs", id = "data_tabs",
                                        tabPanel("Raw Data", DT::dataTableOutput("dt_raw")),
                                        tabPanel("Visits Data", DT::dataTableOutput("dt_v")),
                                        tabPanel("Feeding Data", DT::dataTableOutput("dt_f")),
                                        tabPanel("Movement Data", DT::dataTableOutput("dt_m"))
                            )
                     )
            ),
            tabPanel("Help",
                     navlistPanel(widths = c(2, 10),
                                  HTML("About animal<strong>nexus</strong>"),
                                  tabPanel("About", includeMarkdown("about.md")),
                                  tabPanel("Contact us", includeMarkdown("contact.md")),
                                  tabPanel("References (in progress)"),
                                  HTML("Using animal<strong>nexus</strong>"),
                                  tabPanel("FAQ", includeMarkdown("faq.md")),
                                  tabPanel("Tutorials (in progress)"),
                                  tabPanel("Using the feedr package (in progress)")
                     )
            )

  )
))
