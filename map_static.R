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