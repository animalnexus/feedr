
## Map of data points
output$map_data <- renderLeaflet({
  leaflet() %>%
    setView(lng = -98.857903, lat = 21.363297, zoom = 2) %>%
    addTiles(group = "Default (Open Street Map)") %>%
    addProviderTiles("Stamen.Toner", group = "Black and White") %>%
    addProviderTiles("Esri.WorldTopoMap", group = "Terrain") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satelite") %>%
    addLayersControl(baseGroups = c("Satelite",
                                    "Terrain",
                                    "Open Street Map", 
                                    "Black and White"),
                     options = layersControlOptions(collapsed = FALSE))
})

# Add markers for Feeder sites  
observe({
  req(startup(input))
  d <- counts_sub()
  if(nrow(d) > 0) {
    if(length(unique(d$site_name)) == 1) {
      if(unique(d$site_name) == "Kamloops, BC") zoom <- 17
      if(unique(d$site_name) == "Costa Rica") zoom <- 12
      d <- d %>% 
        left_join(feeders_all, by = "feeder_id") %>%
        mutate(name = feeder_id)
    } else {
      zoom <- 2
      d <- sites_all %>% mutate(name = site_name)
    }
    leafletProxy("map_data") %>%
      clearGroup(group = "Sites") %>%
      addMarkers(data = d, lng = ~lon, lat = ~lat, group = "Sites", popup = ~htmlEscape(name)) %>%
      setView(lat = mean(d$lat, na.rm = TRUE), lng = mean(d$lon, na.rm = TRUE), zoom = zoom, options = list('animate' = TRUE))
  } else {
    leafletProxy("map_data") %>%
      clearGroup(group = "Sites") %>%
      clearGroup(group = "Points") %>%
      setView(lng = -98.857903, lat = 21.363297, zoom = 2)
  }
})
  

# Add circle markers for sample sizes
observe({
  req(startup(input))
  cat("Refreshing Map...\n")
  #cat(print(values$data))
  if(nrow(counts_sub()) > 0) {
    #Get counts summed across all dates
    if(length(unique(counts_sub()$site_name)) > 1) {
      s <- counts_sub() %>% 
        group_by(site_name) %>%
        summarize(n = sum(count)) %>%
        left_join(sites_all, by = "site_name")
    } else {
      s <- counts_sub() %>%
        group_by(feeder_id) %>%
        summarize(n = sum(count)) %>%
        left_join(feeders_all, by = c("feeder_id"))
    }
    leafletProxy("map_data") %>%
      clearGroup(group = "Points") %>%
      addCircleMarkers(data = s, lng = ~lon, lat = ~lat, group = "Points", 
                       radius = ~scale_area(n, max = 50),
                       fillOpacity = 0.7, 
                       fillColor = "orange")
    
  } else {
    leafletProxy("map_data") %>%
      clearGroup(group = "Points")
  }
})

## Plot of counts overtime

plot_data <- reactive({
  req(startup(input))
  cat("Refreshing Plot...\n")
  c <- counts_sub()
  if(nrow(c) > 0) {
    if(length(unique(c$site_name)) > 1) type <- "site_name" else type <- "feeder_id"
    g <- ggplot(data = c, aes_string(x = 'date', y = 'count', fill = type)) +
      geom_bar(stat = "identity", position = "dodge")
  } else {
    g <- ggplot(data = data.frame(date = isolate(values$data$date), count = 0), aes(x = date, y = count)) +
      geom_blank() + 
      scale_y_continuous(limits = c(0, 1))
  }
  
  g + 
    theme_classic() +
    theme(legend.position = "none") +
    labs(x = "Date", y = "Total visits")
})

## For data selection
output$plot_data <- renderPlot({
  plot_data() +
    scale_x_date(date_labels = "%Y %b %d")
  }, height = 150)

## For animation
output$plot_anim <- renderPlot({
  lim <- c(as.Date(floor_date(min(v()$start), unit = "day")), 
           as.Date(ceiling_date(max(v()$start), unit = "day")))
  plot_data() +
    scale_x_date(date_labels = "%Y %b %d", 
                 limits = lim,
                 breaks = seq(lim[1],lim[2], length.out = 5))
  }, height = 150, width = 550)


## Get points which should be mapped
# sample <- reactive({
#   # Because bird_ids() updates with species, we don't actually have to account for species here
#   req(values$data_sitename, values$data_feederid, data_birdid())
# 
#   con <- dbConnect(drv,host=dbhost,port=dbport,dbname=dbname,user=dbuser,password=dbpass)
#   if(length(values$data_sitename) > 1){
#     # If we're looking at more than one site, summarize by site_name
#     suppressWarnings(
#        data <- dbGetQuery(con,
#                           statement = paste0("SELECT DATE(raw.visits.time), birds.site_name, ",
#                                              "COUNT(*) ",
#                                              "FROM raw.visits, birds ",
#                                              "WHERE raw.visits.bird_id IN ( '", data_birdid(),"' ) ",
#                                              "AND raw.visits.bird_id=birds.bird_id ",
#                                              "GROUP BY DATE(raw.visits.time), birds.site_name"#,
#                                              #"AND raw.visits.time > CURRENT_TIMESTAMP - INTERVAL '2 month'"
#                                              ))
#     )
#        if(nrow(data) > 0) {
#          data <- data %>% 
#            load.format(tz = "") %>%
#            left_join(sites_all, by = "site_name")
#        }
#   } else {
#     # If we're looking at one site, summarize by feeder_id
#     suppressWarnings(
#       data <- dbGetQuery(con,
#                          statement = paste0("SELECT DATE(raw.visits.time), raw.visits.feeder_id, ",
#                                             "COUNT(*) ",
#                                             "FROM raw.visits ",
#                                             "WHERE raw.visits.feeder_id IN ( '", qry(values$data_feederid), "' ) ",
#                                             "AND raw.visits.bird_id IN ( '", data_birdid(),"' ) ",
#                                             "GROUP BY DATE(raw.visits.time), raw.visits.feeder_id"#,
#                                             #"AND raw.visits.time > CURRENT_TIMESTAMP - INTERVAL '2 month'"
#                          ))
#     )
#       if(nrow(data) > 0){
#         data <- data %>%
#           load.format(tz = "") %>%
#           left_join(feeders_all, by = c("feeder_id"))
#       }
#   }
#   dbDisconnect(con)
#   if(nrow(data) > 0) {
#     data <- data %>% 
#       mutate(count = as.numeric(count),
#              date = as.Date(date))
#   }
#   data
# })




# reactive({
#   counts_sub() %>%
#     ggvis(~date, ~count) %>%
#     layer_points()
# }) %>% bind_shiny("plot_data")


## Add points to data map
# observe({
#   if(nrow(v_points()) > 0){
#     
#     leafletProxy("map_time") %>% clearGroup(group = "Visits")
#     
#     if(input$birds == "visits") {
#       suppressMessages(
#         leafletProxy("map_time") %>%
#           addCircleMarkers(data = v_points(), group = "Visits", stroke = FALSE, fillOpacity = 1)
#       )
#     }
#     
#     if(input$birds != "visits") {
#       suppressMessages(
#         leafletProxy("map_time") %>%
#           addCircleMarkers(data = v_points(), group = "Visits",
#                            stroke = FALSE,
#                            fillOpacity = 1,
#                            radius = ~scale_area(n, val.max = max(v_info()$n)),
#                            fillColor = ~pal(n),
#                            popup = ~htmltools::htmlEscape(as.character(round(n, 1))))
#       )
#     }
#   } else {
#     leafletProxy("map_time") %>% clearGroup(group = "Visits")
#   }
# })