## Map of data points
output$map_data <- renderLeaflet({
  req(counts)
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

# observeEvent(input$data_reset, {
#   d <- counts_sum[counts_sum$variable == "site_name", c("sum", "choices", "name")] %>%
#     left_join(sites_all, by = c("choices" = "site_name"))
#
#   leafletProxy("map_data") %>%
#     clearGroup(group = "Sites") %>%
#     clearGroup(group = "Points") %>%
#     setView(lng = -98.857903, lat = 21.363297, zoom = 2) %>%
#     addMarkers(data = sites_all,
#                lng = ~lon, lat = ~lat,
#                group = "Sites", popup = ~htmlEscape(site_name)) %>%
#     addCircleMarkers(data = d,
#                      lng = ~lon, lat = ~lat, group = "Points",
#                      radius = ~feedr:::scale_area(sum, val_min = 0),
#                      fillOpacity = 0.7,
#                      fillColor = "orange")
# })



# Update site/feeder markers on counts change
observeEvent(values$map_update, {
  req(startup(input))
  cat("Updating markers...\n")
  counts_site()
  d <- counts_sub()
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
observeEvent(values$map_update, {
  req(startup(input))
  c <- counts_sub()
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
plot_data_ggplot <- eventReactive(values$map_update, {
  cat("Refreshing Time Plot...\n")

  c <- counts_sub()

  isolate({
    if(nrow(c) > 0) {
      if(length(unique(c$site_name)) > 1) type <- "site_name" else type <- "feeder_id"
      g <- ggplot(data = c, aes_string(x = 'date', y = 'count', fill = type)) +
        geom_bar(stat = "identity", position = "dodge")
    } else {
      g <- ggplot(data = data.frame(date = values$data$date, count = 0), aes(x = date, y = count)) +
        geom_blank() +
        scale_y_continuous(limits = c(0, 1))
    }

    g +
      theme_bw() +
      theme(legend.position = "none") +
      labs(x = "Date", y = "Total visits")
  })
})

## For data selection
output$plot_data_ggplot <- renderPlot({
    plot_data_ggplot() +
      scale_x_date(date_labels = "%Y %b %d")
  }, height = 150)

## For animation
output$plot_anim <- renderPlot({
  lim <- c(as.Date(floor_date(min(v()$start), unit = "day")),
           as.Date(ceiling_date(max(v()$start), unit = "day")))
  plot_data_ggplot() +
    scale_x_date(date_labels = "%Y %b %d",
                 limits = lim,
                 breaks = seq(lim[1],lim[2], length.out = 5))
  }, height = 150, width = 550)
