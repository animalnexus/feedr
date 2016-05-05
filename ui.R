
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
library(lubridate)
#library(shinyBS)

shinyUI(fluidPage(

  navbarPage(title = "Song Extraction",
             
             #################
             ## SETUP
             #################
             
             #tabPanel("Selection",
            #          h2("Selection"),
            #          h3("Select Data")
            # ),

             #################
             ## DATA
             #################
            #tabPanel("Raw Data",
            #          DT::dataTableOutput("data")
            #),
            
            #tabPanel("Visits Data",
            #         DT::dataTableOutput("v")
            #),
             #################
             ## Visualization
             #################
            tabPanel("Visualizations",
                     column(3,
                            h1("Data"), 
                            radioButtons("birds", "Show data by:",
                                         choices = c("Individual visits" = "visits",
                                                     "Total visits" = "t_visits", 
                                                     "Avg. visits per bird" = "b_visits", 
                                                     "Total birds" = "t_birds")),
                            radioButtons("image", "Pictures:",
                                         choices = c("All birds", "Only birds with pictures")),
                            
                            textOutput("selection"),
                                        
                            h1("Animation"),
                            sliderInput("speed", "Speed",
                                        min = 0, max = 100,
                                        post = "%",
                                        value = 50),
                            sliderInput("interval", "Interval",
                                        min = 1,
                                        max = 24,
                                        value = 1,
                                        post = " hour(s)")
                     ),
                     column(9,
                            fluidRow(leafletOutput("map", height = 600)),
                            fluidRow(
                              shiny::column(8, offset = 2,
                                            uiOutput("time_UI")
                                            )
                              )#,
                            #fluidRow(
                            #         DT::dataTableOutput("points")
                            #         )
                     )
                     
            )
  )
))
