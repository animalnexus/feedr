## Render Animated Paths Map
output$map_paths <- renderLeaflet({
  withProgress(message = "Loading Map...",
               map_animation()
  )
})

## Add paths to animated map
observe({
  req(m_paths())
  
  if(nrow(m_paths()) > 0){
    leafletProxy("map_paths") %>% 
      clearGroup(group = "Paths") %>%
      addCircleMarkers(data = v_points(), lat = ~lat, lng = ~lon, group = "Visits", 
                       stroke = FALSE, fillOpacity = 1)
  } else {
    leafletProxy("map_points") %>% clearGroup(group = "Visits")
  }
})


