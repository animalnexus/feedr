## Render Map
output$map_data <- renderLeaflet({
  req(db_access)
  cat("Initializing data map...\n")

  #Get counts summed across all dates
  suppressWarnings(
    s <- get_counts(counts, summarize_by = "site_name") %>%
      left_join(sites_all, by = c("choices" = "site_name"))
  )

  map_leaflet_base(locs = sites_all %>% mutate(name = site_name), marker = "name", name = "Sites") %>%
    addScaleBar(position = "bottomright") %>%
    setView(lng = -98.857903, lat = 21.363297, zoom = 2) %>%
    addCircleMarkers(data = s, lng = ~lon, lat = ~lat, group = "Points",
                     radius = ~feedr:::scale_area(sum, val_min = 0),
                     fillOpacity = 0.7,
                     fillColor = "orange")
})

## Reset map on Reset Button
observeEvent(input$data_reset, {
  req(db_access)
  leafletProxy("map_data") %>%
    clearGroup(group = "Points") %>%
    clearGroup(group = "Sites") %>%
    setView(lng = -98.857903, lat = 21.363297, zoom = 2) %>%
    addMarkers(data = sites_all,
               lng = ~lon, lat = ~lat,
               popup  = htmltools::htmlEscape(sites_all$site_name),
               group = "Sites") %>%
    addCircleMarkers(data = suppressWarnings(get_counts(counts, summarize_by = "site_name") %>% left_join(sites_all, by = c("choices" = "site_name"))),
                     lng = ~lon, lat = ~lat,
                     group = "Points",
                     radius = ~feedr:::scale_area(sum, val_min = 0),
                     fillOpacity = 0.7,
                     fillColor = "orange")
})


# Update map feeder sites automatically on site selection
observeEvent(input$data_site_name, {
  req(db_access, input$data_site_name != "")
  cat("Updating markers...\n")
  f <- feeders_all[feeders_all$site_name == input$data_site_name, ]
  if(nrow(f) > 0) {
    if(unique(f$site_name) == "Kamloops, BC") zoom <- 17
    if(unique(f$site_name) == "Costa Rica") zoom <- 12
    leafletProxy("map_data") %>%
      clearGroup(group = "Sites") %>%
      addMarkers(data = f, lng = ~lon, lat = ~lat, group = "Sites", popup = ~htmlEscape(feeder_id)) %>%
      setView(lat = mean(f$lat, na.rm = TRUE), lng = mean(f$lon, na.rm = TRUE), zoom = zoom, options = list('animate' = TRUE))
  }
})

# Add circle markers for sample sizes
observe({
  req(startup(input), db_access, input$data_site_name != "")

  ## Watch for changes in either of these
  input$map_update
  input$data_site_name

  isolate({
    values$data_map <- values$keep  ## Keep track of current map values
    c <- values$keep

    cat("Refreshing Map...\n")
    if(nrow(c) > 0) {
      #Get counts summed across all dates
      if(length(unique(c$site_name)) > 1) {
        suppressWarnings({
          s <- get_counts(c, summarize_by = "site_name") %>%
            left_join(sites_all, by = c("choices" = "site_name"))
        })
      } else {
        suppressWarnings(
          s <- get_counts(c, summarize_by = "feeder_id") %>%
            left_join(feeders_all, by = c("choices" = "feeder_id"))
        )
      }
      s <- s[s$sum > 0, ]
      leafletProxy("map_data") %>%
        clearGroup(group = "Points") %>%
        addCircleMarkers(data = s, lng = ~lon, lat = ~lat, group = "Points",
                         radius = ~feedr:::scale_area(sum, val_min = 0),
                         fillOpacity = 0.7,
                         fillColor = "orange")
    } else {
      leafletProxy("map_data") %>%
        clearGroup(group = "Points")
    }
  })
})


## GGPLOT: Plot of counts overtime
plot_data_ggplot <- reactive({
  #req(startup(input), db_access, input$data_bird_id, input$data_feeder_id)
  req(startup(input), db_access)
  cat("Refreshing Time Plot...\n")

  i <- values$input
  #browser()

  total <- counts_site() %>%
    mutate(selected = factor("unselected", levels = c("unselected", "selected")),
           selected = replace(selected,
                              species %in% i$species &
                                date %within% interval(as.Date(i$date[1]), as.Date(i$date[2])) &
                                bird_id %in% i$bird_id &
                                feeder_id %in% i$feeder_id,
                              "selected")) %>%
    group_by(species, date, selected) %>%
    summarize(count = length(bird_id))

  if(nrow(total) > 0) {
    g <- ggplot(data = total, aes(x = date, y = count, fill = species, alpha = selected)) +
      geom_bar(stat = "identity") +
      scale_alpha_manual(values = c(0.1, 1), drop = FALSE, guide = FALSE)
  }# else {
  #  g <- ggplot(data = data.frame(date = values$input$date, count = 0), aes(x = date, y = count)) +
  #    geom_blank() +
  #    scale_y_continuous(limits = c(0, 1))
  #}

  g +
    theme_bw() +
    theme(legend.position = "top") +
    labs(x = "Date", y = "No. Individuals")
})

## For data selection
output$plot_data_ggplot <- renderPlot({
    plot_data_ggplot() +
      scale_x_date(date_labels = "%Y %b %d")
  }, height = 200)
