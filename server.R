 
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(RPostgreSQL)
library(tidyr)
library(dplyr)
library(feedr)
library(leaflet)

dbname <- 'birdmv-dev'
dbuser <- 'tern'
dbpass <- '627_VBN'
dbport <- 5432
drv <- dbDriver("PostgreSQL")
dbhost <- 'geoteach.tru.ca'

shinyServer(function(input, output) {
  #dbListTables(con)
  #dbExistsTable(con, c("raw", "visits"))
  #dbExistsTable(con, "testtz")
  #data <- dbReadTable(con, c("testtz"))
  #feeders <- dbReadTable(con, c("feeders"))
  #dbGetQuery(con, statement = paste("SELECT *",
  #                                  "FROM feeders",
  #                                  "WHERE site_name LIKE 'Kamloops%'"))
  
  # Get interactive slider / selection bars populated based on data base information
  # Allow data to update depending on selection
  
  #Sys.setenv(TZ = "")
  con <- dbConnect(drv,host=dbhost,port=dbport,dbname=dbname,user=dbuser,password=dbpass)
  
  #feeders <- dbGetQuery(con, statement = paste0("SELECT * FROM feeders")) %>%
  #  mutate(loc = gsub("\\(|\\)", "", loc)) %>%
  #  separate(loc, into = c("lon", "lat"), ",", convert = TRUE)
  
  suppressWarnings(
  data <- dbGetQuery(con, statement = paste("SELECT bird_id, feeder_id, time",
                                            "FROM raw.visits",
                                            "WHERE time > CURRENT_TIMESTAMP - INTERVAL '2 month'")) %>%
    left_join(dbGetQuery(con, statement = paste0("SELECT * FROM feeders")), by = "feeder_id") %>%
    load.format(., tz = "")
  )
  
  dbDisconnect(con)
  
  feeders <- data %>%
    select(feeder_id, site_name, lon, lat) %>%
    unique(.)
  
  v <- visits(data)
  f <- v %>% group_by(bird_id) %>% do(feeding(.))
  m <- v %>% group_by(bird_id) %>% do(move(.)) %>%
    filter(bird_id == "0620000514")
  
  #output$data <- DT::renderDataTable(
  #  if(!is.null(data)) DT::datatable(data, options = list(pageLength = 10))
  #)
  
 # output$data <- DT::renderDataTable(
#    DT::datatable(data, rownames = FALSE)
#  )
 # output$v <- DT::renderDataTable(
  #  DT::datatable(v, rownames = FALSE)
  #)
  
  ### Interactive UI
  output$time_UI <- renderUI({
    req(v, input$speed)
    sliderInput("time", "Time",
                min = floor_date(min(v$start), unit = "hour"), 
                max = ceiling_date(max(v$start), unit = "hour"),
                value = floor_date(min(v$start), unit = "hour"),
                step = 3600, # 1 hour
                animate = animationOptions(interval = 1000 * (1 - input$speed/100), loop = TRUE),
                #animate = animationOptions(interval = 500, loop = TRUE),
                width = "100%")
  })
  
  ### Visualizations (look at points on leaflet map)
  # Get a point
  # Get a slider bar for looking at time and change over time
  
  points <- reactive({
    req(v, input$time)
    v[v$start >= input$time[1] & v$start < input$time[1] + 3600, ]
  })

  output$map <- renderLeaflet({
    suppressMessages(
      leaflet(feeders) %>%
        addProviderTiles("Esri.WorldImagery", group = "Satelite") %>%
        addMarkers(group = "Feeders")
    )
  })
  
  observe({
    ## Change markers depending on time
    if(nrow(points()) > 0){
      suppressMessages(
        leafletProxy("map", data = points()) %>%
          clearGroup(group = "Visits") %>%
          addCircleMarkers(group = "Visits", stroke = FALSE, fillOpacity = 1)#, clusterOptions = markerClusterOptions())
      )
    } else {
      leafletProxy("map") %>% clearGroup(group = "Visits")
    }
  })
  
})
