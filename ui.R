
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

  navbarPage(title = "BirdMoves",
             
             #################
             ## SETUP
             #################
             
             #tabPanel("Selection",
            #          h2("Selection"),
            #          h3("Select Data")
            # ),
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
                            radioButtons("image", "Pictures (placeholder):",
                                         choices = c("All birds", "Only birds with pictures")),
                            
                            #textOutput("selection"),
                                        
                            h1("Animation"),
                            sliderInput("speed", "Speed",
                                        min = 0, max = 100,
                                        post = "%",
                                        value = 50),
                            sliderInput("interval", "Interval",
                                        min = 1,
                                        max = 24,
                                        value = 12,
                                        post = " hour(s)")#,
                            #hr(),
                            #h1("Potential Options:"),
                            #submitButton(text = "Download Screenshot"),
                            
                            #submitButton(text = "Download Data")
                     ),
                     column(9,
                            fluidRow(leafletOutput("map_time", height = 600)),
                            fluidRow(
                              shiny::column(8, offset = 2,
                                            uiOutput("UI_time")
                                            )),
                            fluidRow(div("Click on the arrow to animate -->"))
                            #,
                            #fluidRow(
                            #         DT::dataTableOutput("points")
                            #         )
                     )
                     
            ),
            
            tabPanel("Paths (Static)",
                     column(3,
                            div("Based on feedr package visualizations"),
                            h1("Data"), 
                            uiOutput("UI_bird_id"),
                            h1("Animation")
                            #sliderInput("speed", "Speed",
                            #            min = 0, max = 100,
                            #            post = "%",
                            #            value = 50),
                            #sliderInput("interval", "Interval",
                            #            min = 1,
                            #            max = 24,
                            #            value = 1,
                            #            post = " hour(s)")
                     ),
                     column(9,
                            fluidRow(leafletOutput("map_static", height = 600))
                     ),
                     fluidRow(
                              #DT::dataTableOutput("paths")
                              )
                            
            ),
            #################
            ## DATA
            #################
            tabPanel("Birds",
                     fluidRow(column(6, h3("Click on a row for more information")),
                              column(6, htmlOutput("img_birds"))),
                     fluidRow(DT::dataTableOutput("birds"))
            ),
            tabPanel("Raw Data",
                      DT::dataTableOutput("data")
            ),
            
            tabPanel("Visits Data",
                     DT::dataTableOutput("v")
            )
                   #fluidRow(
                   #         DT::dataTableOutput("points")
                   #         )
          
  )
))
