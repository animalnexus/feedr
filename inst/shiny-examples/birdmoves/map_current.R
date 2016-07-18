## Get current data

current <- reactive({
  req(db_access, values$current_map)
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
                                              "AND raw.visits.time::timestamp > ('2016-03-25 15:23:30'::timestamp - INTERVAL '7 minutes') "))
      })
      dbDisconnect(con)

      if(nrow(data) > 0) {
        data <- data %>%
          load_format(., tz = "UTC", tz_disp = "America/Vancouver") %>%
          visits(.) %>%
          group_by(bird_id, feeder_id, species, age, sex, lon, lat) %>%
          summarize(n = length(bird_id),
                    time = round(sum(end - start)/60, 2)) %>%
          group_by(feeder_id) %>%
          do(circle(point = unique(.[, c("lat", "lon")]), data = ., radius = 0.01))
      } else data <- NULL
    })
    data
  })
})

output$current_time <- renderText(as.character(values$current_time))


## Map of current activity
output$map_current <- renderLeaflet({
  cat("Initializing map of current activity (", as.character(Sys.time()), ") ...\n")
  values$current_map <- TRUE
  map_leaflet_base(locs = feeders_all %>% filter(site_name == "Kamloops, BC") %>% mutate(name = feeder_id), marker = "name", name = "Feeders") %>%
    addScaleBar(position = "bottomright")
})

## Add activity points
# Add circle markers for sample sizes
observeEvent(current(), {
  req(imgs, values$current_map)

  sp_icons <- awesomeIconList(
    "Mountain Chickadee" = makeAwesomeIcon(icon = "star",
                                           marker = "green",
                                           iconColor = "white"),
    "House Finch" = makeAwesomeIcon(icon = "star",
                                    marker = "red",
                                    iconColor = "white"),
    "Dark-eyed Junco" = makeAwesomeIcon(icon = "star",
                                        marker = "darkpurple",
                                        iconColor = "white"))



  cat("Refreshing map of current activity (", as.character(Sys.time()), ") ...\n")
  if(nrow(current()) > 0) {
    leafletProxy("map_current") %>%
      clearGroup(group = "Activity") %>%
      #addMarkers(data = current(),
      addAwesomeMarkers(data = current(),
                        #icon = #makeAwesomeIcon(icon = "star",
                               #                markerColor = "orange",
                               #                iconColor = "blue"),
                        icon = ~sp_icons[species],
                        popup = ~paste0("<strong>Species:</strong> ", species, "<br>",
                                        "<strong>Bird ID:</strong> ", bird_id, "<br>",
                                        "<strong>No. RFID reads:</strong> ", n, "<br>",
                                        "<strong>Total time:</strong> ", time, "min <br>",
                                        get_image(current(), bird_id, 100, imgs)),
                        lng = ~lon, lat = ~lat, group = "Activity")
  } else {
    leafletProxy("map_data") %>%
      clearGroup(group = "Activity")
  }
})


output$summary_current <- renderText({
  req(current())
  paste0("<strong>Time:</strong> ", Sys.time(), "<br>",
         "<strong>Interval:</strong> 7 minutes", "<br>",
         "<strong>No. birds:</strong> ", length(unique(current()$bird_id)), "<br>",
         "<strong>No. feeders:</strong> ", length(unique(current()$feeder_id)), "<br>",
         "<strong>Total feeding time:</strong> ", sum(current()$time), " minutes", "<br>"
  )
})

