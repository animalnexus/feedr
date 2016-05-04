
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
                            uiOutput('speed_UI'),
                            sliderInput("speed", "Animation Speed",
                                        min = 0, max = 100,
                                        post = "%",
                                        value = 50)
                                        
                     ),
                     column(9,
                            fluidRow(leafletOutput("map", height = 600)),
                            fluidRow(
                              shiny::column(8, offset = 2,
                                            uiOutput("time_UI")
                                            )
                              )
                     #fluidRow(
                    #   DT::dataTableOutput("points")
                    # )
                     )
                     
            )
  )
))
