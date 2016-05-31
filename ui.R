
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)
library(shinyBS)
library(leaflet)
library(ggvis)
#library(shinyBS)

shinyUI(fluidPage(

  shinyjs::useShinyjs(),
  
  navbarPage(title = HTML("bird<strong>moves</strong>"),
             
             #################
             ## SETUP
             #################
             tabPanel("Select Data",
                      fluidRow(div(img(src = "logo.jpg", width = 600), style="text-align: center;")),
                      fluidRow(
                        column(4,
                               h2("Data Selection"),
                               h3("Select Data"),
                               uiOutput("UI_data"),
                               bsButton("data_update", "Update Selection", style = "primary"),
                               p(),
                               bsButton("data_reset", "Reset inputs", style = "danger"),
                               bsButton("data_get", "Get Data"),
                               hr(),
                               h3("Advanced Options"),
                               actionButton("data_showadv", "Show Advanced Options"),
                               hidden(div(id = "advanced",
                                          uiOutput("UI_data_adv")))
                        ),
                        column(8,
                               leafletOutput("map_data", height = 600),
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
            ## Visualization
            #################
            tabPanel("Visualizations",
                     column(4,
                            h1("Data"),
                            radioButtons("anim_type", "Show data by:",
                                         choices = c("Individual visits" = "visits",
                                                     "Total visits" = "t_visits",
                                                     "Avg. visits per bird" = "b_visits",
                                                     "Total birds" = "t_birds")),
                            h1("Animation"),
                            sliderInput("anim_speed", "Speed",
                                        min = 0, max = 100,
                                        post = "%",
                                        value = 50),
                            sliderInput("anim_interval", "Interval",
                                        min = 1,
                                        max = 24,
                                        value = 12,
                                        post = " hour(s)"),
                            h3("To Do:"),
                            div("Make the Date plot scale with the map?"),
                            hr(),
                            h1("Potential Options:"),
                            bsButton("data_dl_screenshot", "Download Screenshot", style = "success")
                     ),
                     column(8,
                            tabsetPanel(type = "tabs",
                                        tabPanel("Points",
                                                 fluidRow(leafletOutput("map_points", height = 600))),
                                        tabPanel("Paths",
                                                 fluidRow(leafletOutput("map_paths", height = 600)))),
                            fluidRow(column(8, offset = 2, uiOutput("UI_anim_time"))),
                            fluidRow(column(8, offset = 1, plotOutput("plot_anim")))
                     )
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
                                                 br(), bsButton("data_dl_raw", "Download Raw Data", style = "success"), br(),
                                                 DT::dataTableOutput("dt_data")),
                                        tabPanel("Visits Data",
                                                 br(), bsButton("data_dl_visits", "Download Visits Data", style = "success"), br(),
                                                 DT::dataTableOutput("dt_v")),
                                        tabPanel("Feeding Data",
                                                 br(), bsButton("data_dl_feeding", "Download Feeding Data", style = "success"), br(),
                                                 DT::dataTableOutput("dt_f")),
                                        tabPanel("Movement Data",
                                                 br(), bsButton("data_dl_movements", "Download Movements Data", style = "success"), br(),
                                                 DT::dataTableOutput("dt_m"))
                            )
                     )
            ),
            tabPanel("Home Range (testing)",
                     column(4,
                            h3("Homerange calculation"),
                            uiOutput("UI_hr"),
                            uiOutput("UI_hr_bandwidth"),
                            uiOutput("UI_hr_plot"),
                            bsButton("homerange", "Calculate and Display", style = "success"),
                            hr(),
                            fluidRow(div(strong(textOutput("hr_message")),  
                                         style="text-align:center; padding: 10px"),
                                     style = "border: 1px solid silver; margin-right: 7px")
                          
                     ),
                     column(8,
                            fluidRow(leafletOutput("map_hr", height = 600))
                            # fluidRow(column(8, offset = 2, uiOutput("UI_inat_time")))
                     )
                     
            ),
            tabPanel("iNaturalist (testing)",
                     column(4,
                            h1("Which Data?"),
                            radioButtons("inat_species", "Species:",
                                         choices = c("House Finch" = "Haemorhous mexicanus",
                                                     "Mountain Chickadee" = "Poecile gambeli",
                                                     "Dark-eyed Junco" = "Junco hyemalis")),
                            dateRangeInput("inat_date", label = "Observation Dates",
                                           start = as.Date("2010-01-01"),
                                           end = Sys.Date()),
                            bsButton("inat_data", "Get Data from iNaturalist", style = "success"),
                            hr(),
                            h1("Photos"),
                            htmlOutput("inat_photo")
                     ),
                     column(8,
                            fluidRow(leafletOutput("map_inat", height = 600))#,
                           # fluidRow(column(8, offset = 2, uiOutput("UI_inat_time")))
                     )
                  
            )
  )
))
