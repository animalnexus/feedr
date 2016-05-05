 
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

scale_area <- function(r, val.max, min = 5, max = 105){
  r <- ((r-1) / (val.max - 1)) * (max - min)
  r[r >= 0] <- r[r >= 0] + min
  r[r < 0] <- 0
  return(r)
}

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

  v <- visits(data) %>% 
    mutate(day = as.Date(start)) %>%
    group_by(day, feeder_id, lat, lon)
  
  t_visits <- v %>%
    summarize(n = length(start))
  
  b_visits <- v %>%
    group_by(bird_id, add = TRUE) %>%
    summarize(n = length(start)) %>%
    group_by(feeder_id, lat, lon) %>%
    summarize(n = max(n))
  
  t_birds <- v %>%
    summarize(n = length(unique(bird_id)))
  
  v <- v %>% group_by(feeder_id, lat, lon)
  
  # f <- v %>% group_by(bird_id) %>% do(feeding(.))
  # m <- v %>% group_by(bird_id) %>% do(move(.)) %>%
  #   filter(bird_id == "0620000514")
  # 
  # f %>% filter(feed_start >= v$start[1],
  #              feed_start < v$start[1] + 60*60*1) %>%
  #   group_by(bird_id) %>%
  #   do(activity(., res = 1, by_feeder = TRUE, keep_all = TRUE)) %>%
  #   filter(time < v$start[1] + 60*60*1) %>%
  #   summarize(a = sum(activity))
  

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
    req(v, input$speed, input$interval)
    sliderInput("time", "Time",
                min = floor_date(min(v$start), unit = "day"), 
                max = ceiling_date(max(v$start), unit = "day"),
                value = floor_date(min(v$start), unit = "day"),
                step = 60*60*input$interval,
                animate = animationOptions(interval = 1000 * (1 - input$speed/100), loop = TRUE),
                #animate = animationOptions(interval = 500, loop = TRUE),
                width = "100%")
  })
  
  ### Visualizations (look at points on leaflet map)
  # Get a point
  # Get a slider bar for looking at time and change over time
  
  v_info <- reactive({
    req(v, t_visits, b_visits, t_birds, input$birds)
    temp <- NULL
    if(input$birds == "visits") temp <- v %>% mutate(n = 100)
    if(input$birds == "t_visits") temp <- t_visits
    if(input$birds == "b_visits") temp <- b_visits
    if(input$birds == "t_birds") temp <- t_birds
    temp
  })
  
  v_points <- reactive({
    req(v, input$time, input$interval, input$birds)
    
    temp <- v %>% filter(start >= input$time[1], start < input$time[1] + 60 * 60 * input$interval)
    if(input$birds == "t_visits") temp <- temp %>% summarize(n = length(start))
    if(input$birds == "b_visits") {
      temp <- temp %>% 
        group_by(bird_id, add = TRUE) %>% 
        summarize(n = length(start)) %>%
        group_by(feeder_id, lat, lon) %>%
        summarize(n = mean(n))
    }
    if(input$birds == "t_birds") temp <- temp %>% summarize(n = length(unique(bird_id)))
    temp
    
  })

   output$points <- DT::renderDataTable(
      DT::datatable(v_points(), rownames = FALSE)
    )
   
   output$selection <- renderText({input$birds})
  
  # a_points <- reactive({
  #   req(f, input$time, input$interval)
  #   browser()
  #   f %>% filter(feed_start >= input$time[1],
  #                feed_start < input$time[1] + 60*60*interval) %>%
  #     group_by(bird_id) %>%
  #     do(activity(., res = 1, by_feeder = TRUE)) %>%
  #     filter(time < input$time[1] + 60*60*interval) %>%
  #     summarize(a = sum(activity))
  # })
  
  output$map <- renderLeaflet({
    pal <- colorNumeric(palette = colorRampPalette(c("blue", "green", "yellow","orange", "red"))(max(v_info()$n)), domain = 1:max(v_info()$n))
    #pal <- colorNumeric(palette = heat.colors(max(v_info()$n)), domain = 1:max(v_info()$n))
    #pal <- colorNumeric(palette = colorRampPalette(c("yellow","red"))(10), domain = 1:100)
    
    suppressMessages(
      m <- leaflet(feeders) %>%
        addMarkers(group = "Feeders") %>%
        addTiles(group = "Default (Open Street Map)") %>%
        addProviderTiles("Stamen.Toner", group = "Black and White") %>%
        addProviderTiles("Esri.WorldTopoMap", group = "Terrain") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satelite") %>%
        addLayersControl(overlayGroups = c("Feeders", "Visits"),
                         baseGroups = c("Default (Open Street Map)", 
                                        "Black and White", 
                                        "Satelite",
                                        "Terrain"),
                          options = layersControlOptions(collapsed = FALSE))
      )
    if(input$birds != "visits") {
      m <- m %>%
        addLegend(title = "Legend",
                  position = 'topright',
                  pal = pal,
                  values = 1:max(v_info()$n),
                  bins = 5,
                  opacity = 1)
    }
    m
  })
  
  observe({
    pal <- colorNumeric(palette = colorRampPalette(c("blue","green", "yellow","orange", "red"))(max(v_info()$n)), domain = 1:max(v_info()$n))
    ## Change markers depending on time
    if(nrow(v_points()) > 0){
      
      suppressMessages(
        leafletProxy("map") %>%
          clearGroup(group = "Visits")
        )
      
      if(input$birds == "visits") {
        suppressMessages(
          leafletProxy("map") %>%
          addCircleMarkers(data = v_points(), group = "Visits", stroke = FALSE, fillOpacity = 1)
        )
      }

       if(input$birds != "visits") {
         suppressMessages(
           leafletProxy("map") %>%
             addCircleMarkers(data = v_points(), group = "Visits",
                              stroke = FALSE,
                              fillOpacity = 1,
                              radius = ~scale_area(n, val.max = max(v_info()$n)),
                              fillColor = ~pal(n),
                              popup = ~htmltools::htmlEscape(as.character(round(n, 1))))
         )
       }
    } else {
      leafletProxy("map") %>% clearGroup(group = "Visits")
    }
  })
  
})
