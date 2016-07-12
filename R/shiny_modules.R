## Shiny Modules

## Animated map - UI
#' @import shiny
#' @import magrittr
#' @export
mod_UI_map_animate <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    column(4,
           h1("Data"),
           radioButtons(ns("anim_type"), "Show data by:",
                        choices = c("Total no. visits" = "t_visits",
                                    "Avg. visits per bird" = "b_visits",
                                    "Total no. birds" = "t_birds")),
           h1("Animation"),
           sliderInput(ns("anim_speed"), "Speed",
                       min = 0, max = 100,
                       post = "%",
                       value = 50),
           sliderInput(ns("anim_interval"), "Interval",
                       min = 1,
                       max = 24,
                       value = 1,
                       post = " hour(s)")
    ),
    column(8,
           fluidRow(leafletOutput(ns("map"), height = 600)),
           fluidRow(column(8, uiOutput(ns("UI_anim_time")))),
           fluidRow(column(8, plotOutput(ns("plot_time"))))
    )
  )
}

# Module server function
#' @import shiny
#' @import magrittr
#' @export
mod_map_animate <- function(input, output, session, raw) {

  ns <- session$ns

  # Data

  # Fix time zone to LOOK like local non-DST, but to APPEAR as UTC (for timezone slider
  v <- raw %>%
    dplyr::mutate(time = lubridate::with_tz(time, tzone = tz_offset(attr(raw$time, "tzone"), tz_name = TRUE)))
  attr(v$time, "tzone") <- "UTC"

  v <- visits(v, allow_imp = TRUE) %>%
    dplyr::mutate(day = as.Date(start)) %>%
    dplyr::group_by(feeder_id, lat, lon)

  t_visits <- v %>%
    dplyr::group_by(day, add = TRUE) %>%
    dplyr::summarize(n = length(start))

  b_visits <- v %>%
    dplyr::group_by(bird_id, day, add = TRUE) %>%
    dplyr::summarize(n = length(start)) %>%
    dplyr::group_by(feeder_id, lat, lon) %>%
    dplyr::summarize(n = max(n))

  t_birds <- v %>%
    dplyr::group_by(day, add = TRUE) %>%
    dplyr::summarize(n = length(unique(bird_id)))

  v_info <- reactive({
    req(input$anim_type)
    temp <- NULL
    if(input$anim_type == "t_visits") temp <- t_visits
    if(input$anim_type == "b_visits") temp <- b_visits
    if(input$anim_type == "t_birds") temp <- t_birds
    temp
  })

  v_points <- reactive({
    req(input$anim_time, input$anim_interval, input$anim_type)

    temp <- v %>%
      dplyr::filter(start >= input$anim_time[1], start < input$anim_time[1] + 60 * 60 * input$anim_interval)
    if(input$anim_type == "t_visits") temp <- temp %>% dplyr::summarize(n = length(start))
    if(input$anim_type == "b_visits") {
      temp <- temp %>%
        dplyr::group_by(bird_id, add = TRUE) %>%
        dplyr::summarize(n = length(start)) %>%
        dplyr::group_by(feeder_id, lat, lon) %>%
        dplyr::summarize(n = mean(n))
    }
    if(input$anim_type == "t_birds") temp <- temp %>% dplyr::summarize(n = length(unique(bird_id)))
    temp
  })

  # Time slider
  output$UI_anim_time <- renderUI({
    req(input$anim_speed, input$anim_interval)
    tz <- tz_offset(attr(v$start, "tzone"))
    if(tz >=0) tz <- paste0("+", sprintf("%02d", abs(tz)), "00") else tz <- paste0("-", sprintf("%02d", abs(tz)), "00")
    sliderInput(ns("anim_time"), "Time",
                min = lubridate::floor_date(min(v$start), unit = "day"),
                max = lubridate::ceiling_date(max(v$start), unit = "day"),
                value = lubridate::floor_date(min(v$start), unit = "day"),
                step = 60 * 60 * input$anim_interval,
                timezone = tz,
                animate = animationOptions(interval = 500 * (1 - (input$anim_speed/100)) + 50, loop = TRUE),
                width = "550px")
  })

  ## Render Base Map
  output$map <- renderLeaflet({
    req(input$anim_type, v_info())

    if(max(v_info()$n) == 1) vals <- 1:5 else vals <- 1:max(v_info()$n)
    pal <- colorNumeric(palette = colorRampPalette(c("blue", "green", "yellow","orange", "red"))(max(vals)),
                        domain = vals)

    feedr::map_leaflet_base(locs = unique(raw[, c("feeder_id", "lat", "lon")]),
                            marker = "feeder_id",
                            name = "Feeders") %>%
      leaflet::addScaleBar(position = "bottomright") %>%
      addLegend(title = "Legend",
                position = 'topright',
                pal = pal,
                values = vals,
                bins = 5,
                opacity = 1,
                layerId = "legend")
  })

  ## Add Legends to Animated Map
  observe({
    req(input$anim_type, v_info())
    if(max(v_info()$n) == 1) vals <- 1:5 else vals <- 1:max(v_info()$n)
    pal <- colorNumeric(palette = colorRampPalette(c("blue", "green", "yellow","orange", "red"))(max(vals)),
                        domain = vals)

    leafletProxy(ns("map")) %>%
      addLegend(title = "Legend",
                position = 'topright',
                pal = pal,
                values = vals,
                bins = 5,
                opacity = 1,
                layerId = "legend")
  })

  ## Add points to animated map
  observe({
    req(v_points(), v_info())
    if(max(v_info()$n) == 1) vals <- 1:5 else vals <- 1:max(v_info()$n)
    pal <- colorNumeric(palette = colorRampPalette(c("blue", "green", "yellow","orange", "red"))(max(vals)),
                        domain = vals)

    if(nrow(v_points()) > 0){
      leafletProxy(ns("map")) %>%
        clearGroup(group = "Visits") %>%
        addCircleMarkers(data = v_points(), lat = ~lat, lng = ~lon, group = "Visits",
                         stroke = FALSE,
                         fillOpacity = 1,
                         radius = ~feedr:::scale_area(n),
                         fillColor = ~pal(n),
                         popup = ~htmltools::htmlEscape(as.character(round(n, 1))))
    } else {
      leafletProxy(ns("map")) %>% clearGroup(group = "Visits")
    }
  })

  ## Add sunrise sunset to animate map
  observeEvent(input$anim_time, {
    s <- sun(loc = c(mean(raw$lon), mean(raw$lat)), date = substr(input$anim_time, 1, 10))
    lubridate::tz(s$rise) <- "UTC"
    lubridate::tz(s$set) <- "UTC"
    hour <- input$anim_time

    if(hour < (s$rise - 60*60) | hour > (s$set + 60*60)) a <- "set"

    if(hour >= (s$rise - 60*60) & hour < (s$rise)) a <- "rising1"

    if(hour <= (s$set + 60*60) & hour > (s$set)) a <- "setting"

    if(hour >= (s$rise) & hour < (s$rise + 60*60)) a <- "rising2"
    if(hour <= (s$set) & hour > (s$set - 60*60)) a <- "setting2"

    if(hour > (s$rise + 60*60) & hour < (s$set - 60*60)) a <- "rise"
    #print(paste0(hour, " - ", a))
    #}

    coords <- matrix(c(c(min(raw$lon) - 0.25,
                         max(raw$lon) + 0.25,
                         max(raw$lon) + 0.25,
                         min(raw$lon) - 0.25),
                       c(min(raw$lat) - 0.25,
                         min(raw$lat) - 0.25,
                         max(raw$lat) + 0.25,
                         max(raw$lat) + 0.25)), ncol = 2)


    if(a == "rising1"){
      leafletProxy(ns("map"), data = coords) %>%
        removeShape("set1")
    }
    if(a == "rising2"){
      leafletProxy(ns("map"), data = coords) %>%
        removeShape(c("set1", "set2"))
    }
    if(a == "rise"){
      leafletProxy(ns("map"), data = coords) %>%
        removeShape(c("set1", "set2", "set3"))
    }
    if(a == "setting1"){
      leafletProxy(ns("map"), data = coords) %>%
        addPolygons(fillColor = "#000080",
                    fillOpacity = 0.05,
                    layerId = "set1",
                    group = "Daylight")
    }

    if(a == "setting2"){
      leafletProxy(ns("map"), data = coords) %>%
        addPolygons(fillColor = "#000080",
                    fillOpacity = 0.05,
                    layerId = "set1",
                    group = "Daylight") %>%
        addPolygons(fillColor = "#000080",
                    fillOpacity = 0.05,
                    layerId = "set2",
                    group = "Daylight")
    }
    if(a == "set"){
      leafletProxy(ns("map"), data = coords) %>%
        addPolygons(fillColor = "#000080",
                    fillOpacity = 0.05,
                    layerId = "set1",
                    group = "Daylight") %>%
        addPolygons(fillColor = "#000080",
                    fillOpacity = 0.05,
                    layerId = "set2",
                    group = "Daylight") %>%
        addPolygons(fillColor = "#000080",
                    fillOpacity = 0.05,
                    layerId = "set3",
                    group = "Daylight")
    }

  })



}

