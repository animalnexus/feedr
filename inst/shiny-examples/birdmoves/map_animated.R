## Base Animations Map
map_animation <- reactive({
  map.leaflet.base(locs = feeders_all[feeders_all$site_name %in% data()$site_name,], marker = "feeder_id", name = "Feeders") %>%
    addScaleBar(position = "bottomright")
  # leaflet() %>%
  #   addMarkers(lng = ~lon, lat = ~lat, group = "Feeders") %>%
  #   addTiles(group = "Open Street Map") %>%
  #   addProviderTiles("Stamen.Toner", group = "Black and White") %>%
  #   addProviderTiles("Esri.WorldTopoMap", group = "Terrain") %>%
  #   addProviderTiles("Esri.WorldImagery", group = "Satelite") %>%
  #   addLayersControl(overlayGroups = c("Feeders", "Visits"),
  #                    baseGroups = c("Satelite",
  #                                   "Terrain",
  #                                   "Open Street Map",
  #                                   "Black and White"
  #                    ),
  #                    options = layersControlOptions(collapsed = FALSE))
})

## Render Animated Points Map
output$map_points <- renderLeaflet({
  withProgress(message = "Loading Map...",
               map_animation()
  )
})

# Time slider
output$UI_anim_time <- renderUI({
  req(input$anim_speed, input$anim_interval)
  sliderInput("anim_time", "Time",
              min = floor_date(min(v()$start), unit = "day"),
              max = ceiling_date(max(v()$start), unit = "day"),
              value = floor_date(min(v()$start), unit = "day"),
              step = 60*60*input$anim_interval,
              animate = animationOptions(interval = 1000 * (1 - input$anim_speed/100), loop = TRUE),
              width = "100%")
})


## Add Legends to Animated Map
observe({
  req(input$anim_type, v_info(), output$map_points)
  if(input$anim_type != "visits") {
    if(max(v_info()$n) == 1) vals <- 1:5 else vals <- 1:max(v_info()$n)
    pal <- colorNumeric(palette = colorRampPalette(c("blue", "green", "yellow","orange", "red"))(max(vals)),
                        domain = vals)

    leafletProxy("map_points") %>%
      addLegend(title = "Legend",
                position = 'topright',
                pal = pal,
                values = vals,
                bins = 5,
                opacity = 1,
                layerId = "legend")
  } else {
    leafletProxy("map_points") %>% removeControl(layerId = "legend")
  }
})

## Add visits to animated map
observe({
  req(v_points(), v_info(), input$anim_type == "visits")

  if(nrow(v_points()) > 0){
    leafletProxy("map_points") %>%
      clearGroup(group = "Visits") %>%
      addCircleMarkers(data = v_points(), lat = ~lat, lng = ~lon, group = "Visits",
                       stroke = FALSE, fillOpacity = 1)
  } else {
    leafletProxy("map_points") %>% clearGroup(group = "Visits")
  }
})

## Add summaries to animate map
observe({
  req(v_points(), v_info(), input$anim_type != "visits")

  if(max(v_info()$n) == 1) vals <- 1:5 else vals <- 1:max(v_info()$n)
  pal <- colorNumeric(palette = colorRampPalette(c("blue", "green", "yellow","orange", "red"))(max(vals)),
                      domain = vals)

  if(nrow(v_points()) > 0){
    #leafletProxy("map_points") %>%
    #  feeding.layer(
    leafletProxy("map_points") %>%
      clearGroup(group = "Visits") %>%
      addCircleMarkers(data = v_points(), lat = ~lat, lng = ~lon, group = "Visits",
                       stroke = FALSE,
                       fillOpacity = 1,
                       radius = ~feedr:::scale.area(n, val.max = max(v_info()$n)),
                       fillColor = ~pal(n),
                       popup = ~htmltools::htmlEscape(as.character(round(n, 1))))
  } else {
    leafletProxy("map_points") %>% clearGroup(group = "Visits")
  }
})

## Add sunrise sunset to animate map
observeEvent(input$anim_time, {
  s <- sun(loc = c(mean(data()$lon), mean(data()$lat)), date = substr(input$anim_time, 1, 10))
  lubridate::tz(s$rise) <- "UTC"
  lubridate::tz(s$set) <- "UTC"
  hour <- input$anim_time
  #s <- sun(loc = c(mean(data$lon), mean(data$lat)), date = as.POSIXct("2016-01-01"))
  #for(hour in seq(as.POSIXct("2016-01-01 00:00:00"), as.POSIXct("2016-01-02 01:00:00"), by = "hour")){
  if(hour < (s$rise - 60*60) | hour > (s$set + 60*60)) a <- "set"

  if(hour >= (s$rise - 60*60) & hour < (s$rise)) a <- "rising1"

  if(hour <= (s$set + 60*60) & hour > (s$set)) a <- "setting"

  if(hour >= (s$rise) & hour < (s$rise + 60*60)) a <- "rising2"
  if(hour <= (s$set) & hour > (s$set - 60*60)) a <- "setting2"

  if(hour > (s$rise + 60*60) & hour < (s$set - 60*60)) a <- "rise"
  #print(paste0(hour, " - ", a))
  #}

  coords <- matrix(c(c(min(data()$lon) - 0.25,
                       max(data()$lon) + 0.25,
                       max(data()$lon) + 0.25,
                       min(data()$lon) - 0.25),
                     c(min(data()$lat) - 0.25,
                       min(data()$lat) - 0.25,
                       max(data()$lat) + 0.25,
                       max(data()$lat) + 0.25)), ncol = 2)


  if(a == "rising1"){
    leafletProxy("map_points", data = coords) %>%
      removeShape("set1")
  }
  if(a == "rising2"){
    leafletProxy("map_points", data = coords) %>%
      removeShape(c("set1", "set2"))
  }
  if(a == "rise"){
    leafletProxy("map_points", data = coords) %>%
      removeShape(c("set1", "set2", "set3"))
  }
  if(a == "setting1"){
    leafletProxy("map_points", data = coords) %>%
      addPolygons(fillColor = "#000080",
                  fillOpacity = 0.05,
                  layerId = "set1",
                  group = "Daylight")
  }

  if(a == "setting2"){
    leafletProxy("map_points", data = coords) %>%
      addPolygons(fillColor = "#000080",
                  fillOpacity = 0.05,
                  layerId = "set1",
                  group = "Daylight") %>%
      addPolygons(fillColor = "#000080",
                  fillOpacity = 0.05,
                  layerId = "set2",
                  group = "Daylight")
  }
  if(a == "set"){
    leafletProxy("map_points", data = coords) %>%
      addPolygons(fillColor = "#000080",
                  fillOpacity = 0.05,
                  layerId = "set1",
                  group = "Daylight") %>%
      addPolygons(fillColor = "#000080",
                  fillOpacity = 0.05,
                  layerId = "set2",
                  group = "Daylight") %>%
      addPolygons(fillColor = "#000080",
                  fillOpacity = 0.05,
                  layerId = "set3",
                  group = "Daylight")
  }

})
