#' Animated map for individuals with leaflet
#'
#' Interactive shiny app to select and animate movements and feeder use of
#' different individuals over time. Also available through \code{animalnexus()}.
#'
#' @param v Data frame. Data frame. Visits data frame created with the
#'   \code{visits()} function.
#'
#' @examples
#'
#' \dontrun{
#' map_animate_indiv(visits(finches))
#' }
#'
#' @export
map_animate_indiv <- function(v) {

  # Check for correct formatting
  check_name(v, c("bird_id", "feeder_id", "start", "end"))
  check_time(v)
  check_format(v)

  app <- shiny::shinyApp(ui = shiny::fluidPage(includeCSS(system.file("extra", "style.css", package = "feedr")),
                                                          mod_UI_map_animate_indiv("standalone")),
                         server = function(input, output, session) {
                           shiny::callModule(mod_map_animate_indiv, "standalone",
                                             v = v)
                         }
  )
  shiny::runApp(app, display.mode = "normal")
}

## Animated map - UI
#' @import shiny
#' @import magrittr
#' @export
mod_UI_map_animate_indiv <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    tags$style(HTML(paste0(
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
    column(4,
           popify(uiOutput(ns("UI_bird_id")), title = "Individual", placement = "right",
                  content = "ID of the individual to select.<br/>(X mv; Y fd) represent total number of movements and feeding events respectively.",
                  options = list(container = "body")),
           popify(uiOutput(ns("UI_time_range")), title = "Time range",
                  content = "Select a particular time range to look at", options = list(container = "body")),
           hr(),
           popify(uiOutput(ns("UI_interval")),
                  title = "Resolution", content = "Amount of time to advance in each frame.", options = list(container = "body")),
           popify(sliderInput(ns("anim_speed"), "Animation speed",
                       min = 0, max = 100,
                       post = "%",
                       value = 50),
                  title = "Animation speed", content = "How fast should the animated steps advance.", options = list(container = "body")),
           popify(radioButtons(ns("sunset"), "Show sunset/sunrise?",
                        choices = list("Yes" = TRUE, "No" = FALSE), inline = TRUE),
                  title = "Sunset/Sunrise", content = "Whether or not to include a shadow layer on the map demonstrating daytime and nighttime.", options = list(container = "body")),
           h3("Instructions:"),
           p("Select which individual you wish to animate, the time range, and the specific time (right) to at which to start."),
           p("Paths and feeder use can be animated over time by clicking on the", strong("small blue arrow"), "to the lower right of the 'Time' slider (right)."),
           p("The time interval of each jump and the speed of the animation can be adjusted above."),
           h3("Tip:"),
           p("If you find your animations lagging, reduce the amount of data (adjust the time range).")#,
           #actionButton(ns("pause"), "Pause")
    ),
    column(8,
           popify(fluidRow(leafletOutput(ns("map"), height = 600)),
                  title = "Movements and feeder use",
                  content = "This map shows individual movements and use of different feeders over time.<br/><b>Colour:</b> Time (pale blue is early, dark blue is late)<br/><b>Line width:</b> Increases with number of movements<br/><b>Circle size:</b> Increases with number of feeding bouts (note, <em>number of bouts</em>, not total feeding time)", options = list(container = "body")),
           div(
             fluidRow(uiOutput(ns("UI_time")), style = "text-align: center;"),
             fluidRow(div(popify(plotOutput(ns("plot_time"), height = "100%"),
                                 placement = "left", title = "Events over time",
                                 content = "Number of movements or feeding bouts over time."),
                          style = "height: 200px")),
             fluidRow(div(strong("Note that times are in Local Standard Time (no DST)"), br())),
             style = "text-align: center;")
    )
  )
}

# Module server function
#' @import shiny
#' @import magrittr
#' @import leaflet
#' @export
mod_map_animate_indiv <- function(input, output, session, v = NULL, f = NULL, m = NULL) {

  ns <- session$ns
  values <- reactiveValues(m_old = NULL,
                           f_old = NULL)

  if(is.null(v) & is.null(f) & is.null(m)) stop("Require 'v', 'f' and/or 'm'.")

  # Fix time zone to LOOK like local non-DST, but assigned UTC (for timezone slider)
  if(is.null(f) & is.null(m)){
    tz <- tz_offset(attr(v$start, "tzone"))
    tz_name <- tz_offset(attr(v$start, "tzone"), tz_name = TRUE)

    v <- v %>%
      dplyr::mutate(start = lubridate::with_tz(start, tzone = tz_name),
                    end = lubridate::with_tz(end, tzone = tz_name),
                    day = as.Date(start))


    m <- move(v, all = TRUE)
    f <- feeding(v)

    if(nrow(m) == 0 || all(is.na(m$direction))) m <- NULL
    if(nrow(f) == 0 || all(is.na(f$feed_start))) f <- NULL

  } else {

    if(!is.null(m))  {
      tz <- tz_offset(attr(m$time, "tzone"))
      tz_name <- tz_offset(attr(m$time, "tzone"), tz_name = TRUE)
      m <- m %>%
        dplyr::mutate(time = lubridate::with_tz(time, tzone = tz_name),
                      day = as.Date(start))
    }

    if(!is.null(f)) {
      tz <- tz_offset(attr(f$feed_start, "tzone"))
      tz_name <- tz_offset(attr(f$feed_start, "tzone"), tz_name = TRUE)
      f <- f %>%
        dplyr::mutate(feed_start = lubridate::with_tz(feed_start, tzone = tz_name),
                      feed_end = lubridate::with_tz(feed_end, tzone = tz_name),
                      day = as.Date(feed_start))
    }
  }

  if(tz >=0) tz <- paste0("+", sprintf("%02d", abs(tz)), "00") else tz <- paste0("-", sprintf("%02d", abs(tz)), "00")

  start <- reactive({
    t <- vector()
    if(!is.null(m_id())) t <- as.character(m_id()$time)
    if(!is.null(f_id())) t <- c(t, as.character(f_id()$feed_start))
    lubridate::floor_date(min(lubridate::ymd_hms(t, tz = tz_name), na.rm = TRUE), unit = "hour")
  })

  end <- reactive({
    t <- vector()
    if(!is.null(m_id())) t <- as.character(m_id()$time)
    if(!is.null(f_id())) t <- c(t, as.character(f_id()$feed_end))
    lubridate::ceiling_date(max(lubridate::ymd_hms(t, tz = tz_name), na.rm = TRUE), unit = "hour")
  })


  ## Summarize movements and feeding
  if(!is.null(m)){
    n_move <- m %>%
      dplyr::group_by(bird_id) %>%
      dplyr::summarize(n = floor(length(direction) / 2))
  } else {
    n_move <- data.frame(bird_id = unique(f$bird_id), n = 0)
  }

  if(!is.null(f)){
    n_feeding <- f %>%
      dplyr::group_by(bird_id) %>%
      dplyr::summarize(t = length(feed_length))
  } else n_feeding <- data.frame(bird_id = unique(m$bird_id), n = 0)

  n_move <- merge(n_move, n_feeding, by = "bird_id")

  feeders <- unique(rbind(m[, c("feeder_id", "lon", "lat")],
                          f[, c("feeder_id", "lon", "lat")]))

  ## UIs

  # Bird ID selection
  output$UI_bird_id <- renderUI({
    labels <- as.character(n_move$bird_id)
    names(labels) <- paste0(n_move$bird_id, " (", n_move$n, " mv; ", n_move$t, " fd)")
    selectInput(ns("bird_id"), label = "Select Individual",
                choices = c("Choose" = "", labels))
  })


  # Time range - select subsection of data
   output$UI_time_range <- renderUI({
     req(start(), end())
     sliderInput(ns("time_range"), "Time Range",
                 min = start(),
                 max = end(),
                 value = c(start(), end()),
                 step = 60 * 60,
                 timezone = tz)
   })

   # Interval
   output$UI_interval <- renderUI({
     req(time_range()[1] != time_range()[2])
     x <- list("5 min" = 5, "15 min" = 15,
               "30 min" = 30, "1 hr" = 60,
               "3 hr" = 60*3, "6 hr" = 60*6)
     x <- x[x <= difftime(time_range()[2], time_range()[1], units = "min")]

     radioButtons(ns("interval"), "Resolution",
                  choices = x, inline = TRUE)
   })


   ## Convert to proper tz
   time_range <- reactive({
     req(input$time_range, start(), end())
     lubridate::with_tz(input$time_range, tz_name)
   })

   # Time slider
   output$UI_time <- renderUI({
     req(input$anim_speed, interval(), time_range())#, input$time)
     sliderInput(ns("time"), label = "Time",
                 min = time_range()[1],
                 max = time_range()[2],
                 value = time_range()[1],
                 step = 60 * interval(),
                 timezone = tz,
                 animate = animationOptions(interval = 500 * (1 - (input$anim_speed/100)) + 0.1, loop = FALSE),
                 width = "520px")
   })

   ## Convert to proper tz
   time <- reactive({
     req(input$time)
     lubridate::with_tz(input$time, tz_name)
   })

   interval <- reactive({
     req(input$interval)
     as.numeric(input$interval)
   })

  ## Subselections
  f_id <- reactive({
    req(input$bird_id)
    req(input$bird_id %in% unique(f$bird_id))
    if(!is.null(f)){
      validate(need(sum(names(f) %in% c("lat", "lon")) == 2, "Latitude and longitude ('lat' and 'lon', respectively) were not detected in the data. Can't determine movement paths without them"))
      d <- dplyr::filter(f, bird_id == input$bird_id)
    } else d <- NULL
    return(d)
  })

  f_range <- reactive({
    req(time_range())
    if(!is.null(f_id())){
      d <- f_id() %>%
        dplyr::filter(feed_start >= time_range()[1] & feed_end <= time_range()[2])
    } else d <- NULL
    return(d)
  })

  f_time <- reactive({
    req(time(), interval())
    if(!is.null(f_range())) {
      d <- f_range() %>%
        dplyr::filter(feed_start <= time() + interval() * 60)
    } else d <- NULL
    return(d)
  })

  m_id <- reactive({
    req(input$bird_id)
    req(input$bird_id %in% unique(m$bird_id))
    if(!is.null(m)){
      validate(need(sum(names(m) %in% c("lat", "lon")) == 2, "Latitude and longitude ('lat' and 'lon', respectively) were not detected in the data. Can't determine movement paths without them"))
      d <- m %>%
        dplyr::filter(bird_id == input$bird_id) %>%
        dplyr::arrange(time) %>%
        dplyr::group_by(direction) %>%
        dplyr::mutate(n = 1:length(direction)) %>%
        dplyr::group_by(direction, move_path) %>%
        dplyr::mutate(n_path = 1:length(move_path)) %>%
        dplyr::ungroup()
    } else d <- NULL
    return(d)
  })

  m_range <- reactive({
    req(time_range())
    if(!is.null(m_id())) {
      d <- m_id() %>%
        dplyr::filter(time >= time_range()[1] & time <= time_range()[2]) %>%
        dplyr::arrange(time)
      if(nrow(d) > 0 && d$direction[1] == "arrived") d <- d[-1, ]
    } else d <- NULL
    return(d)
  })

  m_time <- reactive({
    req(time(), interval())
    if(!is.null(m_range())){
      d <- m_range()[m_range()$n %in% m_range()$n[m_range()$direction == "arrived" &
                                                    #m_range()$time > time()[1] &
                                                    m_range()$time <= time() + interval() * 60],]
    } else d <- NULL
    return(d)
  })

  ## Palette
  pal <- reactive({
    req(any(nrow(m_range()) > 0, nrow(f_range()) > 0))
    t <- vector()
    if(!is.null(m_range())) t <- m_range()$time
    if(!is.null(f_range())) t <- c(t, f_range()$feed_start, f_range()$feed_end)

    colorNumeric(palette = "Blues", domain = as.numeric(c(min(t), max(t))))
  })

  # Render Base Map
  output$map <- renderLeaflet({
    leaflet(data = feeders[!is.na(feeders$lon),]) %>%
      addTiles(group = "Open Street Map") %>%
      addProviderTiles("Stamen.Toner", group = "Black and White") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Terrain") %>%
      addMarkers(~lon, ~lat,
                 popup  = htmltools::htmlEscape(feeders$feeder_id),
                 group = "Readers") %>%
      addScaleBar(position = "bottomright") %>%
      addLayersControl(baseGroups = c("Satellite", "Terrain", "Open Street Map", "Black and White"),
                       overlayGroups = c("Readers", "Sunset/Sunrise", "Paths", "Feeding"),
                       options = layersControlOptions(collapsed = TRUE))
  })

  max_radius <- eventReactive(input$bird_id, {
  req(f_id())
  (75 - 10) / nrow(f_id())
  })

  max_width <- eventReactive(input$bird_id, {
    req(f_id())
    (50 - 10) / nrow(f_id())
  })

  # Add paths and points to animated map
  observeEvent(time(), {
    #req(f_time(), m_time())
    p <- pal()
    if(!is.null(m_id())) {
      if(!(identical(values$m_old, m_time()))){
        leafletProxy(ns("map")) %>% clearGroup(group = "Paths")
        values$m_old <- m_time()
        if(nrow(m_time()) > 0){
          temp <- m_time() %>%
            dplyr::group_by(direction, move_path) %>%
            dplyr::mutate(n_path = length(move_path):1)
          for(n in unique(temp$n)) {
            leafletProxy(ns("map")) %>%
              addPolylines(data = temp[temp$n == n, ],
                           ~lon, ~lat,
                           opacity = 1,
                           weight = ~ n_path * max_width() + 10,
                           group = "Paths",
                           color = ~p(as.numeric(time)))
          }
        }
      }
    }
    if(!is.null(f_id())){
      if(!(identical(values$f_old, f_time()))){
        leafletProxy(ns("map")) %>% clearGroup(group = "Feeding")
        values$f_old <- f_time()
        if(nrow(f_time()) > 0) {
          f <- f_time() %>%
            dplyr::group_by(feeder_id) %>%
            dplyr::mutate(n_bout = length(feed_start):1)
          leafletProxy(ns("map")) %>%
            addCircleMarkers(data = f,
                             ~lon, ~lat,
                             weight = 2,
                             fillOpacity = 1,
                             radius = ~ n_bout * max_radius() + 10,
                             group = "Feeding",
                             color = NULL,
                             opacity = 1,
                             fillColor = ~p(as.numeric(feed_start)))
        }
      }
    }
  })

  # Add sunrise sunset
  observeEvent(time(), {
    req(time(), interval(), input$sunset == TRUE)
    leafletProxy(ns("map")) %>%
      addTerminator(time = time(),
                    layerId = paste0("set-", time()),
                    group = "Sunrise/Sunset") %>%
      removeShape(layerId = paste0("set-", values$time_prev))
    values$time_prev <- time()
  }, priority = 100)

  observeEvent(input$sunset, {
    req(input$sunset == FALSE)
    leafletProxy(ns("map")) %>%
      clearGroup(group = "Sunrise/Sunset")
  })

  ## Summary for time figure
  events <- reactive({
    req(start(), end(), interval(), time_range(), any(nrow(m_range()) > 0, nrow(f_range()) > 0))

    m_range()
    f_range()
    interval()

    isolate({
      int_start <- seq(start(), end() - interval() * 60, by = paste(interval(), "min"))
      int_end <- seq(start() + interval() * 60, end(), by = paste(interval(), "min"))

      ## Add to end if not even
      if(length(int_end) != end()) {
        int_start <- lubridate::with_tz(c(int_start, int_end[length(int_end)]), tzone = tz_name)
        int_end <- lubridate::with_tz(c(int_end, end()), tzone = tz_name)
      }

      d <- data.frame()

      if(!is.null(f_range()) && nrow(f_range()) > 0) {
        d <- dplyr::bind_cols(data.frame(block = sapply(f_range()$feed_start, FUN = function(x) which(x >= int_start & x < int_end)))) %>%
          dplyr::bind_cols(data.frame(time = int_start[.$block])) %>%
          dplyr::mutate(type = "Feeding")
      }

      if(!is.null(m_range()) && nrow(m_range()) > 0) {
        d <- dplyr::bind_rows(d,
                              dplyr::bind_cols(data.frame(block = sapply(m_range()$time[m_range()$direction == "arrived"], FUN = function(x) which(x >= int_start & x < int_end)))) %>%
                                dplyr::bind_cols(data.frame(time = int_start[.$block])) %>%
                                dplyr::mutate(type = "Movement"))
      }
    })

    d %>%
      dplyr::group_by(type, block, time) %>%
      dplyr::summarize(n = length(type))
  })

  ## Time figure
  g_time <- eventReactive(events(), {
    lab <- "No. events"
    ggplot2::ggplot(data = events(), ggplot2::aes(x = time, y = n, fill = type)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(x = "Time", y = lab) +
      ggplot2::scale_y_continuous(expand = c(0,0)) +
      ggplot2::scale_x_datetime(labels = scales::date_format("%Y %b %d\n%H:%M", tz = tz_name),
                                limits = time_range() + c(-interval()*60, interval()*60)) +
      ggplot2::geom_bar(stat = "identity", position = "dodge", width = interval()*60, colour = "black") +
      ggplot2::labs(fill = "Event Type")
  })

  output$plot_time <- renderPlot({
    g_time()
  }, height = 200, width = 625)


  observeEvent(input$pause, {
    browser()
  })
}
