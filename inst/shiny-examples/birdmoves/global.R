
## Framework
library(shiny)
library(shinyjs)
library(htmltools)

## Visualizations
library(leaflet)
library(ggplot2)
##library(ggvis)

## Data manip
library(feedr)
library(tidyr)
library(dplyr)
library(lubridate)

## Data aquisition
#library(rinat)
library(RPostgreSQL)
library(envirocan)

## Homerange
#library(sp)
#library(KernSmooth)

## Functions
source("functions.R", local = TRUE)

imgs <- read.csv("./data/species.csv", colClasses = c("factor", "character"))

db_access <- FALSE
if(file.exists("/usr/local/share/feedr/db_full.R")) {
  db_access <- TRUE
  source("/usr/local/share/feedr/db_full.R")
  drv <- dbDriver("PostgreSQL")

  qry <- function(x) paste0(x, collapse = "', '")

  suppressWarnings({
    cat("Connecting to server...\n")
    con <- dbConnect(drv, host = dbhost, port = dbport, dbname = dbname, user = dbuser, password = dbpass)

    cat("Getting feeder data...\n")
    #   incProgress(1/5)
    feeders_all <- dbGetQuery(con, statement = paste("SELECT feeders.feeder_id, feeders.site_name, feeders.loc, fieldsites.dataaccess",
                                                     "FROM feeders, fieldsites",
                                                     "WHERE (fieldsites.site_name = feeders.site_name)")) %>%
      load_format(tz = "") %>%
      mutate(site_name = factor(site_name))

    cat("Getting site data...\n")
    #    incProgress(2/5)
    sites_all <- feeders_all %>%
      group_by(site_name) %>%
      summarize(lon = mean(lon), lat = mean(lat), dataaccess = unique(dataaccess)) %>%
      mutate(site_name = factor(site_name))

    cat("Getting bird data...\n")
    #  incProgress(3/5)
    birds_all <- dbGetQuery(con, statement = paste0("SELECT DISTINCT raw.visits.bird_id FROM raw.visits")) %>%
      left_join(dbGetQuery(con, statement = paste0("SELECT bird_id, species, site_name, age, sex, tagged_on FROM birds")), by = "bird_id") %>%
      load_format(tz = "") %>%
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
      load_format(tz = "UTC") %>%
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
  cat("Summarizing samples...\n")
  counts_sum <- bind_rows(
    get_counts(counts, summarize_by = "site_name"),
    get_counts(counts, summarize_by = "species"),
    get_counts(counts, summarize_by = "date"),
    get_counts(counts, summarize_by = "bird_id"),
    get_counts(counts, summarize_by = "feeder_id"))

  #})
}
