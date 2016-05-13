# Time slider
output$UI_anim_time <- renderUI({
  req(input$anim_speed, input$anim_interval)
  sliderInput("anim_time", "Time",
              min = floor_date(min(v()$start), unit = "day"), 
              max = ceiling_date(max(v()$start), unit = "day"),
              value = floor_date(min(v()$start), unit = "day"),
              step = 60*60*input$anim_interval,
              animate = animationOptions(interval = 1000 * (1 - input$anim_speed/100), loop = TRUE),
              #animate = animationOptions(interval = 500, loop = TRUE),
              width = "100%")
})


## Base Animated Map
output$map_time <- renderLeaflet({
  withProgress(message = "Loading Map...",
  leaflet(feeders_sub()) %>%
    addMarkers(lng = ~lon, lat = ~lat, group = "Feeders") %>%
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
})

## Add Legends to Animated Map
observe({
  if(input$anim_type != "visits") {
    
    if(max(v_info()$n) == 1) vals <- 1:5 else vals <- 1:max(v_info()$n)
    pal <- colorNumeric(palette = colorRampPalette(c("blue", "green", "yellow","orange", "red"))(max(vals)), 
                        domain = vals)

    leafletProxy("map_time") %>% 
      addLegend(title = "Legend",
                position = 'topright',
                pal = pal,
                values = vals,
                bins = 5,
                opacity = 1,
                layerId = "legend")
  } else {
    leafletProxy("map_time") %>% removeControl(layerId = "legend")
  }
})

## Add visits to animated map
observe({
  req(v_points(), v_info(), input$anim_type == "visits")
  
  if(nrow(v_points()) > 0){
    leafletProxy("map_time") %>% 
      clearGroup(group = "Visits") %>%
      addCircleMarkers(data = v_points(), lat = ~lat, lng = ~lon, group = "Visits", 
                       stroke = FALSE, fillOpacity = 1)
  } else {
    leafletProxy("map_time") %>% clearGroup(group = "Visits")
  }
})

## Add summaries to animate map
observe({
  req(v_points(), v_info(), input$anim_type != "visits")
  
  if(max(v_info()$n) == 1) vals <- 1:5 else vals <- 1:max(v_info()$n)
  pal <- colorNumeric(palette = colorRampPalette(c("blue", "green", "yellow","orange", "red"))(max(vals)), 
                      domain = vals)

  if(nrow(v_points()) > 0){
    leafletProxy("map_time") %>% 
      clearGroup(group = "Visits") %>%
      addCircleMarkers(data = v_points(), lat = ~lat, lng = ~lon, group = "Visits",
                       stroke = FALSE,
                       fillOpacity = 1,
                       radius = ~scale_area(n, val.max = max(v_info()$n)),
                       fillColor = ~pal(n),
                       popup = ~htmltools::htmlEscape(as.character(round(n, 1))))
  } else {
    leafletProxy("map_time") %>% clearGroup(group = "Visits")
  }
})