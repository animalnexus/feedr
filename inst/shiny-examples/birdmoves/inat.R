

inat <- eventReactive(input$inat_data, {
  req(input$inat_species)
  withProgress(message = "Retrieving Data from iNaturalist...",
               get_inat_obs(taxon = input$inat_species,
                            quality = "research",
                            geo = TRUE, maxresults = 50)
  )
})

output$map_inat <- renderLeaflet({
  leaflet() %>%
    addTiles(group = "Open Street Map") %>%
    addProviderTiles("Stamen.Toner", group = "Black and White") %>%
    addProviderTiles("Esri.WorldTopoMap", group = "Terrain") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satelite") %>%
    addLayersControl(overlayGroups = c("Observations"),
                     baseGroups = c("Satelite",
                                    "Terrain",
                                    "Open Street Map",
                                    "Black and White"
                     ),
                     options = layersControlOptions(collapsed = FALSE))
})

## Add inat obs
observe({
  req(inat())
  obs <- inat()

  if(nrow(obs) > 0){
    leafletProxy("map_inat") %>% 
      clearGroup(group = "Observations") %>%
      addCircleMarkers(data = obs,
                       lat = ~latitude, lng = ~longitude, 
                       group = "Observations", 
                       stroke = FALSE, fillOpacity = 0.5, radius = 5, 
                       fillColor = "red")
  } else {
    leafletProxy("map_inat") %>% clearGroup(group = "Observations")
  }
})

# Time slider
output$UI_inat_time <- renderUI({
  #req(input$anim_speed, input$anim_interval)
  sliderInput("inat_time", "Time",
              min = min(as.Date(inat()$observed_on)), 
              max = max(as.Date(inat()$observed_on)),
              value = min(as.Date(inat()$observed_on)),
              step = 1,
              animate = animationOptions(interval = 500, loop = TRUE),
              #animate = animationOptions(interval = 500, loop = TRUE),
              width = "100%")
})

## Observation Photo
output$inat_photo <- renderText({
  #req(inat(), input$map_inat_marker_click)
  if(is.null(input$map_inat_marker_click) | class(try(inat(), silent = TRUE)) == "try-error") {
    temp <-"https://upload.wikimedia.org/wikipedia/commons/thumb/b/b7/Bird01.svg/442px-Bird01.svg.png"
  } else {
    temp <- inat()$image_url[inat()$latitude == input$map_inat_marker_click$lat & inat()$longitude == input$map_inat_marker_click$lng]
  }
  paste0("<img src = '",temp,"' height = 300>")
})
