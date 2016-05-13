
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)
library(leaflet)
#library(shinyBS)

shinyUI(fluidPage(

  shinyjs::useShinyjs(),
  
  navbarPage(title = HTML("bird<strong>moves</strong>"),
             
             #################
             ## SETUP
             #################
             tabPanel("Home",
                      fluidRow(div(img(src = "logo.jpg", width = 600), style="text-align: center;")),
                      fluidRow(
                        column(4,
                               h2("Data Selection"),
                               h3("Select Data"),
                               uiOutput("UI_data_sitename"),
                               uiOutput("UI_data_species"),
                               uiOutput("UI_data_date"),
                               actionButton("data_get", "Get Data"),                               
                               h3("Advanced Options"),
                               actionButton("data_showadv", "Show Advanced Options"),
                               hidden(div(id = "advanced",
                                          uiOutput("UI_data_birdid"),
                                          uiOutput("UI_data_feederid"))),

                               h2("Current Data Selection"),
                               htmlOutput("data_current")
                        ),
                        column(8,
                               leafletOutput("map_data", height = 600),
                               plotOutput("plot_data")
                               #ggvisOutput("plot_data")
                        )
                      ),
                      fluidRow(
                      )
             ),
             #################
             ## Visualization
             #################
            tabPanel("Visualizations",
                     column(3,
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
                            div("Make the Date plot scale with the map?")
                            #hr(),
                            #h1("Potential Options:"),
                            #submitButton(text = "Download Screenshot"),
                            
                            #submitButton(text = "Download Data")
                     ),
                     column(9,
                            fluidRow(leafletOutput("map_time", height = 600)),
                            fluidRow(column(8, offset = 2, uiOutput("UI_anim_time"))),
                            fluidRow(column(8, offset = 1, plotOutput("plot_anim")))
                            #,
                            #fluidRow(
                            #         DT::dataTableOutput("dt_points")
                            #         )
                     )
                     
            ),
            
            # tabPanel("Paths (Static)",
            #          column(3,
            #                 h1("Data"), 
            #                 uiOutput("UI_static_birdid"),
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
            tabPanel("Raw Data",
                      DT::dataTableOutput("dt_data")
            ),
            
            tabPanel("Visits Data",
                     DT::dataTableOutput("dt_v")
            )
                   #fluidRow(
                   #         DT::dataTableOutput("dt_points")
                   #         )
          
  )
))
