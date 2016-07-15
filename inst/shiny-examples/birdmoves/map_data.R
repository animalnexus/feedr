## Map of data points
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

# Update site/feeder markers on counts change
observeEvent(input$map_update, {
  req(startup(input), db_access)
  cat("Updating markers...\n")
  counts_site()
  d <- values$keep
  isolate({
    if(nrow(d) > 0) {
      if(length(unique(d$site_name)) == 1) {
        if(unique(d$site_name) == "Kamloops, BC") zoom <- 17
        if(unique(d$site_name) == "Costa Rica") zoom <- 12
        suppressWarnings(
          d <- d %>%
            left_join(feeders_all, by = "feeder_id") %>%
            mutate(name = feeder_id)
        )
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
})


# Add circle markers for sample sizes
observeEvent(input$map_update, {
  req(startup(input), db_access)
  c <- values$keep
  cat("Refreshing Map...\n")
  isolate({
    #cat(print(values$data))
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
