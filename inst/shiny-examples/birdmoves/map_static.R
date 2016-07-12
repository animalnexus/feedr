

# Static
output$UI_static_bird_id <- renderUI({
  selectInput("static_bird_id", "Select bird",
              choices = as.character(unique(raw()$bird_id)))
})


# Data for static maps
m <- reactive({
  v <- v()
  if(length(unique(v$feeder_id)) > 1){
    m <- v() %>%
      group_by(bird_id) %>%
      do(move(.)) %>%
      group_by(bird_id, feeder_id, move_path) %>%
      summarize(path_use = length(move_path)) %>%
      arrange(bird_id, move_path)
  } else m <- NULL
  m
})

f <- reactive({
  f <- v() %>%
    group_by(bird_id) %>%
    do(feeding(.)) %>%
    group_by(bird_id, feeder_id) %>%
    summarize(feed_length = sum(feed_length))
  if(nrow(f[f$feed_length > 0,]) == 0) f <- NULL
  f
})

map_static <- reactive({
  req(input$static_bird_id)
  withProgress(message = "Calculating visits and movement paths...",
               map.leaflet(f = f()[f()$bird_id == input$static_bird_id, ], m = m()[m()$bird_id == input$static_bird_id,], locs = feeders_sub())
  )
})



output$map_static <- renderLeaflet({
  #pal <- colorNumeric(palette = colorRampPalette(c("blue", "green", "yellow","orange", "red"))(max(v_info()$n)), domain = 1:max(v_info()$n))
  map_static()
  # suppressMessages(
  #   leaflet(feeders) %>%
  #     addMarkers(group = "Feeders") %>%
  #     addTiles(group = "Default (Open Street Map)") %>%
  #     addProviderTiles("Stamen.Toner", group = "Black and White") %>%
  #     addProviderTiles("Esri.WorldTopoMap", group = "Terrain") %>%
  #     addProviderTiles("Esri.WorldImagery", group = "Satelite") %>%
  #     addLayersControl(overlayGroups = c("Feeders", "Visits"),
  #                      baseGroups = c("Default (Open Street Map)",
  #                                     "Black and White",
  #                                     "Satelite",
  #                                     "Terrain"),
  #                      options = layersControlOptions(collapsed = FALSE)) %>%
  #     addLegend(title = "Legend",
  #               position = 'topright',
  #               pal = pal,
  #               values = 1:max(v_info()$n),
  #               bins = 5,
  #               opacity = 1)
  # )
})

## Summarize data
# observe({
#   if(nrow(m_paths()) > 0){
#    leafletProxy("map_static") %>%
#      clearGroup(group = "Paths") %>%
#      addPolylines(data = m_paths(),lng = ~lon, lat = ~lat, opacity = 0.75, group = "Paths")
#   } else {
#    leafletProxy("map_static") %>% clearGroup(group = "Paths")
#   }
# })
