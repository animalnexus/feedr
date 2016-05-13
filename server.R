## shinyapps from github: devtools::install_github('rstudio/shinyapps')
## library(shinyapps); shinyapps::deployApp("~/Projects/BirdMoves")


## devtools::install_github("steffilazerte/feedr")

## OR: 
#library(rsconnect)

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)
library(RPostgreSQL)
library(tidyr)
library(dplyr)
library(feedr)
library(leaflet)
library(ggplot2)
library(lubridate)
library(htmltools)
#library(ggvis)

dbname <- 'birdmv-dev'
dbuser <- 'tern'
dbpass <- '627_VBN'
dbport <- 5432
drv <- dbDriver("PostgreSQL")
dbhost <- 'geoteach.tru.ca'

imgs <- read.csv("./data/species.csv", colClasses = c("factor", "character"))

scale_area <- function(r, val.max = max(r, na.rm = TRUE), min = 5, max = 105){
  if(val.max == 1) val.max <- 5
  r <- ((r-1) / (val.max - 1)) * (max - min)
  r[r >= 0] <- r[r >= 0] + min
  r[r < 0] <- 0
  return(r)
}

qry <- function(x) paste0(x, collapse = "', '")


con <- dbConnect(drv,host=dbhost,port=dbport,dbname=dbname,user=dbuser,password=dbpass)

suppressWarnings(
  feeders_all <- dbGetQuery(con, statement = paste0("SELECT feeder_id, site_name, loc FROM feeders")) %>%
    load.format(tz = "") %>%
    mutate(site_name = factor(site_name))
)

sites_all <- feeders_all %>% 
  group_by(site_name) %>% 
  summarize(lon = mean(lon), lat = mean(lat)) %>%
  mutate(site_name = factor(site_name))

suppressWarnings(
  birds_all <- dbGetQuery(con, statement = paste0("SELECT DISTINCT raw.visits.bird_id FROM raw.visits")) %>%
    left_join(dbGetQuery(con, statement = paste0("SELECT bird_id, species, site_name, age, sex, tagged_on FROM birds")), by = "bird_id") %>% 
    load.format(tz = "") %>%
    mutate(species = factor(species),
           site_name = factor(site_name))
)

suppressWarnings(
  counts <- dbGetQuery(con, 
                     statement = paste0("SELECT raw.visits.bird_id, raw.visits.feeder_id, DATE(raw.visits.time), ",
                                        "COUNT(*) ",
                                        "FROM raw.visits ",
                                        "GROUP BY DATE(raw.visits.time), raw.visits.feeder_id, raw.visits.bird_id"#,
                     )) %>%
  load.format(tz = "") %>%
  left_join(birds_all[, c("site_name", "species", "bird_id")], by = "bird_id") %>%
  mutate(date = as.Date(date),
         count = as.numeric(count),
         species = factor(species, levels = sort(unique(birds_all$species))),
         site_name = factor(site_name, levels = sort(sites_all$site_name)),
         bird_id = factor(bird_id, levels = sort(unique(birds_all$bird_id))),
         feeder_id = factor(feeder_id, levels = sort(unique(feeders_all$feeder_id))))
)

dbDisconnect(con)



startup <- function(x) {
  #require that input objects were at least created (first pass)
  all(c("data_sitename", 
        "data_species", 
        "data_date"#,
        #"data_birdid", 
        #"data_feederid"
  ) %in% names(x))
}

shinyServer(function(input, output, session) {

    values <- reactiveValues(
      data_submit = TRUE,
      data = list(sitename = as.character(unique(counts$site_name)),
                  species = as.character(unique(counts$species)),
                  date = c(min(counts$date), max(counts$date)),
                  birdid = unique(counts$bird_id),
                  feederid = unique(counts$feeder_id)),
      current = NULL)
  
  ## Select Data
  source("select_data.R", local = TRUE)
  
  ## Load output data tables
  source("output_data.R", local = TRUE)
    
  ## Load reactive expressions
  source("reactive.R", local = TRUE)
  
  ### Visualizations
  source("map_data.R", local = TRUE)
  source("map_animated.R", local = TRUE)
  source("map_static.R", local = TRUE)
  
  # Get initial Data
  #con <- dbConnect(drv,host=dbhost,port=dbport,dbname=dbname,user=dbuser,password=dbpass)
  
  # dates_all <- reactive({
  #   req(values$map_data)
  #     con <- dbConnect(drv,host=dbhost,port=dbport,dbname=dbname,user=dbuser,password=dbpass)
  #     d <- dbGetQuery(con, 
  #                statement = paste(
  #                           "SELECT DISTINCT ON (raw.visits.time::date) 
  #                           raw.visits.time::date, feeders.site_name",
  #                           "FROM raw.visits, feeders",
  #                           "WHERE raw.visits.feeder_id = feeders.feeder_id")) %>%
  #       rename(date = time) %>%
  #       load.format(tz = "")
  #     dbDisconnect(con)
  #     d
  # })

  
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
