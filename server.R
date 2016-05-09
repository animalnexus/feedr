## shinyapps from github: devtools::install_github('rstudio/shinyapps')
## library(shinyapps); shinyapps::deployApp("~/Projects/BirdMoves")

## OR: 
#library(rsconnect)

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
  #data <- dbReadTable(con, c("ages"))
  #feeders <- dbReadTable(con, c("feeders"))
  #dbGetQuery(con, statement = paste("SELECT *",
  #                                  "FROM feeders",
  #                                  "WHERE site_name LIKE 'Kamloops%'"))
  
  # Get interactive slider / selection bars populated based on data base information
  # Allow data to update depending on selection
  
  #Sys.setenv(TZ = "")
  
  con <- dbConnect(drv,host=dbhost,port=dbport,dbname=dbname,user=dbuser,password=dbpass)
  #feeders <- dbGetQuery(con, statement = paste0("SELECT * FROM feeders"))
  birds_all <- dbGetQuery(con, statement = paste0("SELECT * FROM birds")) %>% load.format(tz = "")
  dates_all <- dbGetQuery(con, 
                          statement = paste(
                            "SELECT DISTINCT ON (raw.visits.time::date) 
                            raw.visits.time::date, feeders.site_name",
                            "FROM raw.visits, feeders",
                            "WHERE raw.visits.feeder_id = feeders.feeder_id")) %>%
    rename(date = time) %>%
    load.format(tz = "")
  dbDisconnect(con)
  
  
  
  
  ## Fix

  
  
  

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
  
  ## Load renderers
  source("output_render.R", local = TRUE)
                
  ## Load output data tables
  source("output_data.R", local = TRUE)
    
  ## Load reactive expressions
  source("reactive.R", local = TRUE)
  
  ### Visualizations
  source("map_animated.R", local = TRUE)
  source("map_static.R", local = TRUE)
  
  observeEvent(input$data_get, {
    req(input$data_birdid, input$data_species, input$data_sitename)
    values$current <- data.frame(sitename = input$data_sitename,
                                 species = input$data_species,
                                 bird_id = input$data_birdid)
  })
  
  ## Look at birds
  output$img_birds <- renderText({
    req(imgs)
    # Don't actually know what STRH stands for, assuming Sapphire-throated Hummingbird
    if(is.null(input$dt_birds_rows_selected) | is.null(birds_sub())) {
      temp <-imgs$url[imgs$species == "unknown"]
    } else {
      r <- input$dt_birds_rows_selected
      temp <- imgs$url[imgs$species == as.character(birds_sub()$species[r])]
      if(nchar(as.character(temp)) < 1) temp <-imgs$url[imgs$species == "unknown"]
    }
    paste("<img src='",temp,"' height = 300>")
    })

  
})
