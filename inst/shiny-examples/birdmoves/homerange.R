output$hr_message <- renderText({
  if(class(try(data(), silent = TRUE)) == "try-error") {
    "No Data Selected" 
  } else values$hr_message
  })

output$UI_hr <- renderUI({
  req(data())
  ids <- data() %>%
    group_by(bird_id) %>% 
    summarize(n = length(unique(feeder_id))) %>%
    filter(n > 1)

  d <- data() %>% filter(bird_id %in% ids$bird_id)
  
  list(selectInput("hr_bird_id", label = "Select bird ID", choices = unique(as.character(d$bird_id)), selected = unique(as.character(d$bird_id))[1]),
       textInput("hr_grid_size", label = "No. Grid points", value = 750))
})

output$UI_hr_bandwidth <- renderUI({
  req(data(), input$hr_bird_id, input$hr_grid_size)
  d <- data()[data()$bird_id == input$hr_bird_id, ]
  suppressWarnings(est <- try(round(dpik(cbind(d$lon, d$lat), level = 3, gridsize = as.numeric(input$hr_grid_size)), 7), silent = TRUE))
  if(class(est) == "try-error" | est < 0.0005) est <- 0.0005
  textInput("hr_bandwidth", label = "Bandwidth (Override best guess)", 
            value = est)
})

output$UI_hr_plot <- renderUI({
  sliderInput("hr_contours", "Number of contours",
              min = 1,
              max = 50,
              value = 20)
})

output$map_hr <- renderLeaflet({
  cat("Initialize homerange map\n")
  isolate(values$map_hr <- TRUE)
  leaflet() %>%
    addTiles(group = "Open Street Map") %>%
    addProviderTiles("Stamen.Toner", group = "Black and White") %>%
    addProviderTiles("Esri.WorldTopoMap", group = "Terrain") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satelite") %>%
    addScaleBar(position = "bottomright") %>%
    addLayersControl(overlayGroups = c("Observations"),
                     baseGroups = c("Satelite",
                                    "Terrain",
                                    "Open Street Map",
                                    "Black and White"
                     ),
                     options = layersControlOptions(collapsed = FALSE))
})

observe({
  req(values$map_hr)
  cat("Updating homerange markers...\n")
  
  values$map_hr
  d <- data()
  
  isolate({
    if(nrow(d) > 0) {
      if(length(unique(d$site_name)) == 1) {
        if(unique(d$site_name) == "Kamloops, BC") zoom <- 17
        if(unique(d$site_name) == "Costa Rica") zoom <- 12
        d <- feeders_sub() %>% mutate(name = feeder_id)
      } else {
        zoom <- 2
        d <- sites_all %>% mutate(name = site_name)
      }
      leafletProxy("map_hr") %>%
        clearGroup(group = "Sites") %>%
        addMarkers(data = d, lng = ~lon, lat = ~lat, group = "Sites", popup = ~htmlEscape(name)) %>%
        setView(lat = mean(d$lat, na.rm = TRUE), lng = mean(d$lon, na.rm = TRUE), zoom = zoom, options = list('animate' = TRUE))
    } else {
      leafletProxy("map_hr") %>%
        clearGroup(group = "Sites") %>%
        clearGroup(group = "Contours") %>%
        setView(lng = -98.857903, lat = 21.363297, zoom = 2)
    }
  })
})

kernel <- eventReactive(input$homerange, {
  req(input$hr_bird_id, input$hr_grid_size, input$hr_bandwidth, data())
  cat("Start homerange kernel calculation...\n")
  
  values$hr_message <- ""
  d <- data()[data()$bird_id == input$hr_bird_id, ]

  withProgress(
    hr <- try(bkde2D(cbind(d$lon, d$lat), 
                     bandwidth = rep(as.numeric(input$hr_bandwidth), 2),
                     gridsize = rep(as.numeric(input$hr_grid_size), 2)), silent = TRUE),
    message = "Calculating kernel density estimates...")

  if(class(hr) == "try-error"){
    msg <- geterrmessage()
    if(grepl("cannot allocate vector of size", msg)) values$hr_message <- "Resolution too high, consider increasing bandwidth or reducing number of grid points"
    if(grepl("binning grid too coarse", msg)) values$hr_message <- "Grid too coarse considering bandwidth: Consider increasing number of grid points"
    d2d <- NULL
  } else {
    d2d <- bkde2D(cbind(d$lon, d$lat), 
                  bandwidth = rep(as.numeric(input$hr_bandwidth), 2),
                  gridsize = rep(as.numeric(input$hr_grid_size), 2))
  }
  d2d
})

poly <- reactive({
  req(kernel(), input$hr_contours)
  
  d2d <- kernel()
  ## Let's polygon object
  lines <- contourLines(x = d2d$x1, y = d2d$x2, z = d2d$fhat, nlevels = input$hr_contours)
  
  dd3 <- sapply(lines, function(x) Polygon(as.matrix(cbind(x$x, x$y))))  ## Turn each contour into polygon
  dd3 <- sapply(dd3, function(x) Polygons(list(x), ID = as.numeric(Sys.time()))) ##Turn all into Polygon list
  
  SpatialPolygonsDataFrame(Sr = SpatialPolygons(dd3), data = data.frame(Value = sapply(lines, function(x) x$level)), match.ID = FALSE)
})

observe({
  req(poly(), input$hr_contours)
  c <- poly()
  cat("Update homerange map\n")
  
  withProgress({
    pal <- colorNumeric(
      palette = colorRampPalette(c("yellow", "orange", "red"))(isolate(input$hr_contours)),
      domain = c$Value
    )
    if(nrow(c) > 0){
      leafletProxy("map_hr") %>% 
        clearGroup(group = "Contours") %>%
        addPolygons(data = c,
                    stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
                    color = ~pal(Value),
                    group = "Contours") %>%
        fitBounds(lng1 = c@bbox[1, 1], lat1 = c@bbox[2, 1],
                  lng2 = c@bbox[1, 2], lat2 = c@bbox[2, 2])
    } else {
      leafletProxy("map_hr") %>% 
        clearGroup(group = "Contours")
    }
  }, message = "Updating homerange map")
})