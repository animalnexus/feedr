mod_maps <- function() {

  app <- shiny::shinyApp(ui = shiny::fluidPage(includeCSS(system.file("extra", "style.css", package = "feedr")),
                                               mod_UI_map_animate("standalone")),
                         server = function(input, output, session) {
                           shiny::callModule(mod_map_animate, "standalone",
                                             v = v)
                         }
  )
  shiny::runApp(app, display.mode = "normal")
}


mod_UI_maps_instructions <- function(id, specific) {
  ns <- NS(id)
  tagList(
    h3("Instructions:"),
    specific,
    p("Events can be animated over time by clicking on the", strong("small blue arrow"), "to the lower right of the 'Time' slider (right)."),
    p("The time interval (resolution) and the speed of the animation can be adjusted above."),
    h3("Tips:"),
    p("To prevent animations from lagging, you are limited in the resolution you can animate over. To animate at a smaller resolution, reduce the time range."),
    p("The time depicted in the slider bar to the left indicates the middle of the time interval over which you are animated. For example, if you pick a resolution of 1 hr and the time shows 2016-01-07 09:30:00, the data has been summarized over 1 hour from 9am until 10am.")
  )
}


## Animated map - UI
#' @import shiny
mod_UI_maps_controls <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  tagList(
    popify(
      uiOutput(ns("UI_time_range")),
      title = "Time range",
      content = "Select a particular time range to look at", options = list(container = "body")),
    hr(),
    popify(
      uiOutput(ns("UI_interval")),
      title = "Resolution",
      content = "Amount of time to advance in each frame.", options = list(container = "body")),
    popify(sliderInput(ns("anim_speed"), "Animation speed",
                       min = 0, max = 100,
                       post = "%",
                       value = 50),
           title = "Animation speed", content = "How fast should the animated steps advance.", options = list(container = "body"))
  )
}

# Module server function
#' @import shiny
#' @export
mod_maps_controls <- function(input, output, session, times) {

  ns <- session$ns

  start <- reactive({lubridate::floor_date(min(times()), unit = "hour")})
  end <- reactive({lubridate::ceiling_date(max(times()), unit = "hour")})

  tz_offset <- reactive({
    x <- feedr::tz_offset(tz = attr(times(), "tzone"))
    if(x >= 0) x <- paste0("+", sprintf("%02d", abs(x)), "00") else x <- paste0("-", sprintf("%02d", abs(x)), "00")
    return(x)
  })

  tz <- reactive({lubridate::tz(times())})

  # Time range - select subsection of data
  output$UI_time_range <- renderUI({
    sliderInput(ns("time_range"), "Time Range",
                min = start(),
                max = end(),
                value = c(start(), end()),
                step = 60 * 60,
                timezone = tz_offset())
  })

  ## UI Interval
  output$UI_interval <- renderUI({
    req(times())
    isolate(radioButtons2(ns("interval"), "Resolution", choices = data_limits()$i, selected = interval_selection(c(min(times()), max(times()))), inline = TRUE))
  })

  # Update Interval
  observe({
    req(input$interval, data_range())
    i <- data_limits()

    ## Toggle radio buttons
    lapply(1:(length(i$i)-1), function(a) shinyjs::toggleState(id = paste0("interval_", i$i[a]), condition = data_range() < i$n[a+1]))

    ## Adjust selection
    s <- interval_selection(data_range(), i)
    if(s > as.numeric(input$interval)) updateRadioButtons2(session, "interval", selected = s)
  })

  ## Floor/ceiling to nearest relevant hour. For larger time periods, start at 6am
  time_range <- reactive({
    req(input$time_range, interval())
    tr <- lubridate::with_tz(input$time_range, tz())
    if(interval() <= 60) {
      tr[1] <- lubridate::floor_date(tr[1], unit = "hour")
      tr[2] <- lubridate::ceiling_date(tr[2], unit = "hour")
    } else if(interval() == 180) {
      tr[1] <- lubridate::floor_date(tr[1], unit = "3 hours")
      tr[2] <- lubridate::ceiling_date(tr[2], unit = "3 hours")
    } else if(interval() == 360) {
      tr[1] <- lubridate::floor_date(tr[1], unit = "6 hours")
      tr[2] <- lubridate::ceiling_date(tr[2], unit = "6 hours")
    } else if(interval() == 720) {
      tr[1] <- round_6(tr[1])
      tr[2] <- round_6(tr[2])
    } else if(interval() == 1440) {
      tr[1] <- lubridate::floor_date(tr[1], unit = "24 hours")
      tr[2] <- lubridate::ceiling_date(tr[2], unit = "24 hours")
    }
    return(tr)
  })

  interval <- reactive({
    req(input$interval)
    as.numeric(input$interval)
  })

  instant_range <- reactive({
    req(interval())
    lubridate::seconds(interval()*60/2)
  })

  data_range <- reactive({
    req(time_range(), times())
    d <- times()[times() >= time_range()[1] & times() <= time_range()[2]]
    as.numeric(difftime(max(d), min(d), units = "min"))
  })

  anim_speed <- reactive({input$anim_speed})

  return(c(interval = interval, instant_range = instant_range, time_range = time_range, anim_speed = reactive({input$anim_speed}), tz = tz, tz_offset = tz_offset))
}


mod_UI_maps_time <- function(id, type = "the RFID logger (Feeder)") {

  ns <- NS(id)

  tagList(tags$style(HTML(paste0(
    "div#", ns("plot_time")," {
      text-align: center;
      }"))),
    tags$style(HTML(paste0(
      "div#", ns("plot_time")," img {
      max-width: 100%;
      }"))),
    tags$style(HTML(paste0(
      "div#", ns("UI_time")," {
      padding-left:30px;
      width: 550px;
      max-width: 100%;
      display: block;
      margin-left: auto;
      margin-right: auto;
      }"))),
    div(
           fluidRow(uiOutput(ns("UI_time")), style = "text-align: center;"),
           fluidRow(div(popify(plotOutput(ns("plot_time"), height = "100%"),
                               placement = "left", title = "Summary over time",
                               content = "Summarized data over time."),
                        style = "height: 200px")),
           fluidRow(div(
             strong("Note that times are in Local Standard Time (no DST)"), br(),
             strong("Colours represent ", type), style = "text-align: center;")),
           style = "text-align: center;")
  )
}

# Module server function
#' @import shiny
#' @export
mod_maps_time <- function(input, output, session, controls, events, lab = "Events") {

  ns <- session$ns

  output$UI_time <- renderUI({
    req(controls$anim_speed(), controls$interval(), controls$time_range())
    sliderInput(ns("instant"), "Time",
                min = controls$time_range()[1] + controls$instant_range(),
                max = controls$time_range()[2] + controls$instant_range(),
                value = controls$time_range()[1] + controls$instant_range(),
                step = 60 * controls$interval()[1],
                timezone = controls$tz_offset(),
                animate = animationOptions(interval = 500 * (1 - (controls$anim_speed()/100)) + 0.1, loop = FALSE),
                width = "520px")
  })

  ## Convert to proper tz
  instant <- reactive({
    req(input$instant, controls$tz())
    lubridate::with_tz(input$instant - controls$instant_range(), controls$tz())
  })

  # Time figure
  g_time <- eventReactive(events(), {
    req(events(), controls$instant_range(), controls$interval(), controls$time_range())
    if(length(unique(events()$type)) > 8) lp <- "none" else lp = "bottom"

    ## Shift start of block time to mid point of interval:
    d <- dplyr::mutate(events(), block = block + controls$instant_range())
    ggplot2::ggplot(data = d, ggplot2::aes(x = block, y = amount, fill = type)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = lp) +
      ggplot2::labs(x = "Time", y = lab, fill = "") +
      ggplot2::scale_y_continuous(expand = c(0,0)) +
      ggplot2::scale_x_datetime(labels = scales::date_format("%Y %b %d\n%H:%M", tz = controls$tz()),
                                limits = controls$time_range() + c(-controls$interval()*60, controls$interval()*60)) +
      ggplot2::geom_bar(stat = "identity", width = controls$interval()*60, colour = "black")
  })

  output$plot_time <- renderPlot({
    req(g_time())
    g_time()
  }, height = 200, width = 625)

  return(instant = instant)
}


mod_UI_maps_sunrise <- function(id) {
  ns <- NS(id)
  tagList(
    popify(radioButtons(ns("sunrise"), "Show sunrise/sunset?",
                        choices = list("Yes" = TRUE, "No" = FALSE), inline = TRUE),
           title = "Sunrise/Sunset", content = "Whether or not to include a shadow layer on the map demonstrating daytime and nighttime.", options = list(container = "body"))
  )
}

# Module server function
#' @import shiny
#' @export
mod_maps_sunrise <- function(input, output, session, instant, controls) {

  ns <- session$ns
  values <- reactiveValues()

  ## Add sunrise sunset
  observeEvent(instant(), {
    req(instant(), input$sunrise == TRUE)
    leafletProxy(ns("map")) %>%
      addTerminator(time = instant() + controls$instant_range(),
                    layerId = paste0("set-", instant()),
                    group = "Sunrise/Sunset") %>%
      removeShape(layerId = paste0("set-", values$time_prev))
    values$time_prev <- instant()
  }, priority = 100)

  # Get rid of sunrise if radio unselected
  observeEvent(input$sunrise, {
    req(input$sunrise == FALSE)
    leafletProxy(ns("map")) %>%
      clearGroup(group = "Sunrise/Sunset")
  })

  # Omit option to show sunrise if 24h selected
  observeEvent(controls$interval(), {
    shinyjs::toggleState(id = "sunrise", condition = controls$interval() < 1440) #enable below 24hr
    if(controls$interval() == 1440) updateRadioButtons(session, inputId = "sunrise", selected = FALSE)
  })
}

## Map_leaflet UI
mod_UI_maps_leaflet <- function(id) {
  ns <- NS(id)
  tagList(
    popify(fluidRow(leafletOutput(ns("map"), height = 600)),
           title = "Activity summaries",
           content = "Circles show the amount of activity at each feeder for the given time interval.",
           options = list(container = "body"))
  )
}

## Map_leaflet_palette server
map_palette <- function(pal, data, col = "amount", type = "value") {

  if(type == "value") vals <- 1:max(data[col])
  if(type == "n") vals <- 1:length(data[col])

  if(max(vals) == 1) vals <- 1:5

  leaflet::colorNumeric(palette = pal(max(vals)), domain = vals)
}

# Map_leaflet Server
#' @import shiny
#' @export
mod_maps_leaflet <- function(input, output, session, data, data_total, title, type = "visits", palette = NULL) {

  ns <- session$ns

  values <- reactiveValues()
  d <- reactiveValues()
  d_total <- reactiveValues()
  pal <- reactiveValues()

  ## Get data type
  observeEvent(data(), {
    req(data(), data_total())
    if(is.data.frame(data())) {
      d$visits <- data()
      d_total$visits <- data_total()
    } else if(!is.data.frame(data())) {
      d$feeding <- data()$feeding
      d$movements <- data()$movements
      d_total$feeding <- data_total()$feeding
      d_total$movements <- data_total()$movements
    }
  })

  observe({
    req(names(d_total))

    lapply(names(d_total), function(a) {
      if(!is.null(d_total[[a]])){
        if(is.null(palette)) palette <- colorRampPalette(c("yellow","orange", "red"))
        if(a == "visits") pal[[a]] <- map_palette(palette, d_total[[a]], col = "n", type = "value")
        if(a == "movements") pal[[a]] <- map_palette(palette, d_total[[a]], col = "path_use", type = "value")
        if(a == "feeding") pal[[a]] <- map_palette(palette, d_total[[a]], col = "n", type = "value")
      }
    })
  })


  ## Render Base Map
  output$map <- renderLeaflet({
    req(names(d_total))
    loggers <- do.call('rbind', lapply(reactiveValuesToList(d_total), function(a) unique(a[, c("feeder_id", "lat", "lon")])))
    groups <- stringr::str_to_title(names(d_total))

    map_leaflet_base(locs = loggers) %>%
      addScaleBar(position = "bottomright") %>%
      addLayersControl(baseGroups = c("Satellite", "Terrain", "Open Street Map", "Black and White"),
                       overlayGroups = c("Loggers", "Sunrise/Sunset", groups),
                       options = layersControlOptions(collapsed = FALSE))
  })

  ## Add points to animated map
  if(type == "visits"){
    observe({
      req(names(d_total), names(pal), title())
      leafletProxy(ns("map")) %>%
        addLegend(title = title(),
                  position = 'topright',
                  pal = pal$visits,
                  values = d_total$visits$n,
                  bins = 5,
                  opacity = 1,
                  layerId = "legend")
    })

    observe({
      req(data(), data_total(), values$pal)
      if(nrow(data()) > 0){
        leafletProxy(ns("map")) %>%
          clearGroup(group = "Visits") %>%
          use_markers(u = data(), u_scale = 1, u_pal = values$pal, u_title = "Visits", val_min = min(data_total()$amount), val_max = max(data_total()$amount))
      } else {
        leafletProxy(ns("map")) %>% clearGroup(group = "Visits")
      }
    })
  }

  if(type == "movements"){
    observe({
      req(names(d_total), names(pal), title())
      leafletProxy(ns("map")) %>%
        addLegend(title = title(),
                  position = 'topright',
                  pal = pal$feeding,
                  values = d_total$feeding$n,
                  opacity = 1,
                  layerId = "legend")
    })

    observe({
      req(d$feeding, d$movements, names(pal))

      ## Add movements
      if(!(identical(values$m_old, d$movements))){
        # Compare to last and see what's different
        if(!is.null(values$m_old) && nrow(values$m_old) > 0) mv <- d$movements[d$movements$n > max(values$m_old$n), ] else mv <- d$movements
        if(!is.null(mv) && nrow(mv) > 0) {
          for(n in sort(unique(mv$n), decreasing = TRUE)) {
            temp <- mv[mv$n == n, ] %>%
              dplyr::arrange(direction)
            leafletProxy(ns("map")) %>%
              path_lines(data = temp, p_pal = pal$movements,
                         p_scale = 1, p_title = "Movements",
                         val_min = min(d_total$movements$path_use),
                         val_max = max(d_total$movements$path_use),
                         layerId = paste0("move_", n))
          }
        } else if(!is.null(values$m_old)) {
          ## Which to remove?
          if(nrow(d$movements) == 0) old_layers <- 1:max(values$m_old$n) else old_layers <- (max(d$movements$n)+1):max(values$m_old$n)
          if(!is.null(values$m_old) && nrow(values$m_old) > 0) leafletProxy(ns("map")) %>% removeShape(layerId = paste0("move_", old_layers))
        }
        values$m_old <- d$movements
      }
      if(!is.null(d$feeding) && nrow(d$feeding) > 0){
        if(!(identical(values$f_old, d$feeding))){
          leafletProxy(ns("map")) %>% clearGroup(group = "Feeding")
          values$f_old <- d$feeding
          leafletProxy(ns("map")) %>%
            use_markers(data = d$feeding,
                        u_scale = 1, u_pal = pal[['feeding']],
                        u_title = "Feeding",
                        val_min = min(d_total$feeding$amount),
                        val_max = max(d_total$feeding$amount))



          # addCircleMarkers(data = d$feeding,
            #                  ~lon, ~lat,
            #                  weight = 2,
            #                  fillOpacity = 1,
            #                  radius = ~ n + 10,
            #                  group = "Feeding",
            #                  color = NULL,
            #                  opacity = 1,
            #                  fillColor = ~pal[['feeding']](n))
        }
      }
    })
  }

}


# Map_data Server
#' @import shiny
#' @export
mod_maps_data <- function(input, output, session, controls, instant, data) {
  data_instant <- reactive({
    req(instant(), controls$interval(), data())
    data() %>%
      dplyr::filter(block >= instant(), block < instant() + 60 * controls$interval())
  })
  return(data_instant)
}

