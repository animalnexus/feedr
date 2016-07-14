## Get current data

current <- reactive({
  req(db_access)
  invalidateLater(5 * 60 * 1000)
  input$current_update

    isolate({
    con <- dbConnect(drv, host = dbhost, port = dbport, dbname = dbname, user = dbuser, password = dbpass)

    #con <- src_postgres(dbname = dbname, host = dbhost, port = dbport, user = dbuser, password = dbpass)
    #now <- Sys.time() - 7 *
    #current <- tbl(con, sql("SELECT * FROM raw.visits, fieldsites")) %>%
    #filter(time >= as.POSIXct("2016-07-13 12:00:00"))

    values$current_time <- Sys.time()
    withProgress(message = "Updating...", {
      suppressWarnings({
        data <- dbGetQuery(con,
                           statement = paste0("SELECT raw.visits.bird_id, raw.visits.feeder_id, raw.visits.time, feeders.site_name, feeders.loc, birds.species, birds.age, birds.sex ",
                                              "FROM raw.visits, feeders, birds ",
                                              "WHERE (raw.visits.feeder_id = feeders.feeder_id) ",
                                              "AND (birds.bird_id = raw.visits.bird_id) ",
                                              "AND feeders.site_name IN ( 'Kamloops, BC' ) ",
                                              #"AND raw.visits.time::timestamp > (CURRENT_TIMESTAMP - INTERVAL '7 minutes') ",
                                              "AND raw.visits.time::timestamp > ('2016-05-25 15:23:30'::timestamp - INTERVAL '7 minutes') "))
      })
      dbDisconnect(con)

      if(nrow(data) > 0) {
        data <- data %>%
          load_format(., tz = "UTC", tz_disp = "America/Vancouver") %>%
          group_by(bird_id, feeder_id, species, age, sex, lon, lat) %>%
          summarize(n = length(bird_id),
                    time = as.numeric(max(time) - min(time))/60)
      } else data <- NULL
    })
    data
  })
})

output$current_time <- renderText(as.character(values$current_time))


## Map of current activity
output$map_current <- renderLeaflet({
  cat("Initializing map of current activity...\n")

  map_leaflet_base(locs = feeders_all %>% filter(site_name == "Kamloops, BC") %>% mutate(name = feeder_id), marker = "name", name = "Feeders") %>%
    addScaleBar(position = "bottomright")
})

## Add activity points
# Add circle markers for sample sizes
observeEvent(current(), {
  req(imgs)

  cat("Refreshing map of current activity...\n")
  if(nrow(current()) > 0) {
    leafletProxy("map_current") %>%
      clearGroup(group = "Activity") %>%
      addMarkers(data = current(),
                 popup = ~paste0("<strong>Bird ID:</strong> ", bird_id, "<br>",
                                 "<strong>No. RFID reads:</strong> ", n, "<br>",
                                 "<strong>Total time:</strong> ", time, "min <br>",
                                 get_image(current(), bird_id, 100, imgs)),
                 lng = ~jitter(lon, factor = 0.0001),
                 lat = ~jitter(lat, factor = 0.0001), group = "Activity")
  } else {
    leafletProxy("map_data") %>%
      clearGroup(group = "Activity")
  }
})


