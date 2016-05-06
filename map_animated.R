
## Base Animated Map
output$map_time <- renderLeaflet({
  leaflet(feeders) %>%
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
})

## Add Legends to Animated Map
observe({
  pal <- colorNumeric(palette = colorRampPalette(c("blue", "green", "yellow","orange", "red"))(max(v_info()$n)), domain = 1:max(v_info()$n))
  if(input$birds != "visits") {
    leafletProxy("map_time") %>% 
      addLegend(title = "Legend",
                position = 'topright',
                pal = pal,
                values = 1:max(v_info()$n),
                bins = 5,
                opacity = 1,
                layerId = "legend")
  } else {
    leafletProxy("map_time") %>% removeControl(layerId = "legend")
  }
})

## Add points to animated map
observe({
  pal <- colorNumeric(palette = colorRampPalette(c("blue","green", "yellow","orange", "red"))(max(v_info()$n)), domain = 1:max(v_info()$n))
  if(nrow(v_points()) > 0){
    
    leafletProxy("map_time") %>% clearGroup(group = "Visits")
    
    if(input$birds == "visits") {
      suppressMessages(
        leafletProxy("map_time") %>%
          addCircleMarkers(data = v_points(), group = "Visits", stroke = FALSE, fillOpacity = 1)
      )
    }
    
    if(input$birds != "visits") {
      suppressMessages(
        leafletProxy("map_time") %>%
          addCircleMarkers(data = v_points(), group = "Visits",
                           stroke = FALSE,
                           fillOpacity = 1,
                           radius = ~scale_area(n, val.max = max(v_info()$n)),
                           fillColor = ~pal(n),
                           popup = ~htmltools::htmlEscape(as.character(round(n, 1))))
      )
    }
  } else {
    leafletProxy("map_time") %>% clearGroup(group = "Visits")
  }
})