## shinyapps from github: devtools::install_github('rstudio/shinyapps')
## library(shinyapps); shinyapps::deployApp("~/Projects/BirdMoves/Scripts/birdMoves")


## devtools::install_github("steffilazerte/feedr")
## devtools::install_github("steffilazerte/envirocan")

## OR:
#library(rsconnect)

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

## Framework
library(shiny)
library(shinyjs)
library(htmltools)

## Visualizations
library(leaflet)
library(ggplot2)
##library(ggvis)

## Data manip
library(tidyr)
library(dplyr)
library(feedr)
library(lubridate)

## Data aquisition
library(rinat)
library(RPostgreSQL)

## Homerange
library(sp)
library(KernSmooth)

## Functions
source("functions.R", local = TRUE)

dbname <- 'birdmv-dev'
dbuser <- 'tern'
dbpass <- '627_VBN'
dbport <- 5432
drv <- dbDriver("PostgreSQL")
dbhost <- 'geoteach.tru.ca'

imgs <- read.csv("./data/species.csv", colClasses = c("factor", "character"))

qry <- function(x) paste0(x, collapse = "', '")

#withProgress(message = "Connecting to server...", {
suppressWarnings({
  cat("Connecting to server...\n")
  con <- dbConnect(drv,host=dbhost,port=dbport,dbname=dbname,user=dbuser,password=dbpass)

  cat("Getting feeder data...\n")
  #   incProgress(1/5)
  feeders_all <- dbGetQuery(con, statement = paste0("SELECT feeder_id, site_name, loc FROM feeders")) %>%
    load.format(tz = "") %>%
    mutate(site_name = factor(site_name))

  cat("Getting site data...\n")
  #    incProgress(2/5)
  sites_all <- feeders_all %>%
    group_by(site_name) %>%
    summarize(lon = mean(lon), lat = mean(lat)) %>%
    mutate(site_name = factor(site_name))

  cat("Getting bird data...\n")
  #  incProgress(3/5)
  birds_all <- dbGetQuery(con, statement = paste0("SELECT DISTINCT raw.visits.bird_id FROM raw.visits")) %>%
    left_join(dbGetQuery(con, statement = paste0("SELECT bird_id, species, site_name, age, sex, tagged_on FROM birds")), by = "bird_id") %>%
    load.format(tz = "") %>%
    mutate(species = factor(species),
           site_name = factor(site_name),
           bird_id = factor(bird_id))

  cat("Getting sample information...\n")
  #    incProgress(4/5)
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
  dbDisconnect(con)
})

#  incProgress(5/5)
  counts_sum <- bind_rows(
    get_counts(counts, summarize_by = "site_name"),
    get_counts(counts, summarize_by = "species"),
    get_counts(counts, summarize_by = "date"),
    get_counts(counts, summarize_by = "bird_id"),
    get_counts(counts, summarize_by = "feeder_id"))

#})

startup <- function(x) {
  #require that input objects were at least created (first pass)
  all(c("data_site_name",
        "data_species",
        "data_date"#,
        #"data_bird_id",
        #"data_feeder_id"
  ) %in% names(x))
}

shinyServer(function(input, output, session) {


  values <- reactiveValues(
    reset = FALSE,
    data = values_list(),
    data_initial = values_list(),
    data_previous = NULL,
    hr_message = "",
    map_hr = FALSE)

  ## Select Data
  source("select_data.R", local = TRUE)
  source("map_data.R", local = TRUE)

  ### Visualizations
  source("map_animated.R", local = TRUE)
  source("map_paths.R", local = TRUE)
  source("map_static.R", local = TRUE)

  ## Load reactive expressions
  source("reactive.R", local = TRUE)
  
  ## Load transformation data tables
  source("output_data.R", local = TRUE)
  
  ## Load inat
  source("inat.R", local = TRUE)
  source("homerange.R", local = TRUE)

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
