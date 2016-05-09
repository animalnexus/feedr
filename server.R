 
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

imgs <- read.csv("./data/species.csv", colClasses = c("factor", "character"))

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
  
  birds <- dbGetQuery(con, statement = paste0("SELECT * FROM birds"))
  
  suppressWarnings(
  data <- dbGetQuery(con, statement = paste("SELECT bird_id, feeder_id, time",
                                            "FROM raw.visits",
                                            "WHERE time > CURRENT_TIMESTAMP - INTERVAL '2 month'")) %>%
    left_join(dbGetQuery(con, statement = "SELECT * FROM feeders"), by = "feeder_id") %>%
    left_join(dbGetQuery(con, statement = "SELECT * FROM birds"), by = c("bird_id", "site_name")) %>%
    load.format(., tz = "")
  )
  
  dbDisconnect(con)
  
  feeders <- data %>%
    select(feeder_id, site_name, lon, lat) %>%
    unique(.)

  birds <- data %>%
    select(bird_id, species, age, sex, tagged_on, site_name) %>%
    unique(.)
  
  # Data for animated maps
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
  
  # Data for static maps
  m <- v %>% 
    group_by(bird_id) %>% 
    do(move(.)) %>%
    group_by(bird_id, feeder_id, move_path) %>%
    summarize(path_use = length(move_path)) %>%
    arrange(bird_id, move_path)
                
  f <- v %>% 
    group_by(bird_id) %>% 
    do(feeding(.)) %>%
    group_by(bird_id, feeder_id) %>%
    summarize(feed_length = sum(feed_length))

  #   filter(bird_id == "0620000514")
  # 
  # f %>% filter(feed_start >= v$start[1],
  #              feed_start < v$start[1] + 60*60*1) %>%
  #   group_by(bird_id) %>%
  #   do(activity(., res = 1, by_feeder = TRUE, keep_all = TRUE)) %>%
  #   filter(time < v$start[1] + 60*60*1) %>%
  #   summarize(a = sum(activity))
  

  
  ## Load output UIs
  source("output_UI.R", local = TRUE)
                
  ## Load output data tables
  source("output_data.R", local = TRUE)
    
  ## Load reactive expressions
  source("reactive.R", local = TRUE)
  
  ### Visualizations
  source("map_animated.R", local = TRUE)
  source("map_static.R", local = TRUE)
  
  ## Look at birds
  output$img_birds <- renderText({
    
    if(is.null(input$birds_rows_selected) | is.null(birds)) {
      temp <-" https://upload.wikimedia.org/wikipedia/commons/thumb/b/b7/Bird01.svg/442px-Bird01.svg.png"
    } else {
      r <- input$birds_rows_selected
      
      if(birds$species[r] == "House Finch") {
      temp <- "https://upload.wikimedia.org/wikipedia/commons/thumb/8/88/House_Finch-27527-2.jpg/320px-House_Finch-27527-2.jpg"
      } else if(birds$species[r] == "Mountain Chickadee") {
      temp <- "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6e/Poecile_gambeli%2C_Walden%2C_Colorado_1.jpg/320px-Poecile_gambeli%2C_Walden%2C_Colorado_1.jpg"
      }
    }
    paste("<img src='",temp,"' height = 300>")
    })

  
})
