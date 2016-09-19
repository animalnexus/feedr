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
           popify(uiOutput(ns("UI_bird_id")), title = "Individual",
                  content = "ID of the individual to select.<br/>(Numbers) represent total number of movements.",
                  options = list(container = "body")),
           popify(uiOutput(ns("UI_time_range")), title = "Time range",
                  content = "Select a particular time range to look at", options = list(container = "body")),
           hr(),
           popify(radioButtons(ns("interval"), "Resolution",
                        choices = list("5 min" = 5, "15 min" = 15, "30 min" = 30, "1 hr" = 60, "3 hr" = 60*3, "6 hr" = 60*6), inline = TRUE),
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

  if(is.null(v) & (is.null(f) | is.null(m))) stop("Require either 'v' or both 'f' and 'm'.")

  # Fix time zone to LOOK like local non-DST, but assigned UTC (for timezone slider)
  if(is.null(f) | is.null(m)){
    v <- v %>%
      dplyr::mutate(start = lubridate::with_tz(start, tzone = tz_offset(attr(v$start, "tzone"), tz_name = TRUE)),
                    end = lubridate::with_tz(end, tzone = tz_offset(attr(v$end, "tzone"), tz_name = TRUE)),
                    day = as.Date(start))
    tz <- tz_offset(attr(v$start, "tzone"))

    m <- move(v, all = TRUE)
    f <- feeding(v)
  } else {
    m <- m %>%
      dplyr::mutate(time = lubridate::with_tz(time, tzone = tz_offset(attr(m$time, "tzone"), tz_name = TRUE)), day = as.Date(start))
    f <- f %>%
      dplyr::mutate(feed_start = lubridate::with_tz(feed_start, tzone = tz_offset(attr(f$feed_start, "tzone"), tz_name = TRUE)),
                    feed_end = lubridate::with_tz(feed_end, tzone = tz_offset(attr(f$feed_end, "tzone"), tz_name = TRUE)),
                    day = as.Date(feed_start))
    tz <- tz_offset(attr(m$time, "tzone"))
  }

  if(tz >=0) tz <- paste0("+", sprintf("%02d", abs(tz)), "00") else tz <- paste0("-", sprintf("%02d", abs(tz)), "00")

  start <- reactive(lubridate::floor_date(min(c(m_id()$time, f_id()$feed_start), na.rm = TRUE), unit = "hour"))
  end <- reactive(lubridate::ceiling_date(max(c(m_id()$time, f_id()$feed_end), na.rm = TRUE), unit = "hour"))


  ## How many movements?
  n_move <- m %>%
    dplyr::group_by(bird_id) %>%
    dplyr::summarize(n = floor(length(direction) / 2))


  feeders <- unique(rbind(m[, c("feeder_id", "lon", "lat")],
                          f[, c("feeder_id", "lon", "lat")]))

  ## UIs

  # Bird ID selection
  output$UI_bird_id <- renderUI({
    labels <- as.character(n_move$bird_id)
    names(labels) <- paste0(n_move$bird_id, " (", n_move$n, ")")
    selectInput(ns("bird_id"), label = "Select Individual",
                choices = c("Choose" = "", labels))
  })

  output$UI_temp <- renderUI({
    radioButtons(ns("temp"), label = "Testing", choices = c("sucks", "to"))
  })

  # Time range - select subsection of data
   output$UI_time_range <- renderUI({
     req(m_id(), f_id())
     sliderInput(ns("time_range"), "Time Range",
                 min = start(),
                 max = end(),
                 value = c(start(), end()),
                 step = 60 * 60,
                 timezone = tz)
   })

   ## Convert to proper tz
   time_range <- reactive({
     req(input$time_range)
     lubridate::with_tz(input$time_range, lubridate::tz(m$time))
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
     lubridate::with_tz(input$time, lubridate::tz(m$time))
   })

   interval <- reactive({
     req(input$interval)
     as.numeric(input$interval)
   })

  ## Subselections
  f_id <- reactive({
    req(input$bird_id)
    validate(need(sum(names(f) %in% c("lat", "lon")) == 2, "Latitude and longitude ('lat' and 'lon', respectively) were not detected in the data. Can't determine movement paths without them"))
    dplyr::filter(f, bird_id == input$bird_id)
  })

  f_range <- reactive({
    req(f_id(), time_range())
    f_id() %>%
      dplyr::filter(feed_start >= time_range()[1] & feed_end <= time_range()[2])
  })

  f_time <- reactive({
    req(f_range(), time(), interval())
    f_range() %>%
      dplyr::filter(feed_end <= time() + interval() * 60)
  })

  m_id <- reactive({
    req(input$bird_id)
    validate(need(sum(names(m) %in% c("lat", "lon")) == 2, "Latitude and longitude ('lat' and 'lon', respectively) were not detected in the data. Can't determine movement paths without them"))
    m %>%
      dplyr::filter(bird_id == input$bird_id) %>%
      dplyr::arrange(time) %>%
      dplyr::group_by(direction) %>%
      dplyr::mutate(n = 1:length(direction)) %>%
      dplyr::group_by(direction, move_path) %>%
      dplyr::mutate(n_path = 1:length(move_path)) %>%
      dplyr::ungroup()
  })

  m_range <- reactive({
    req(m_id(), time_range())
    m_id() %>%
      dplyr::filter(time >= time_range()[1] & time <= time_range()[2])
  })

  m_time <- reactive({
    req(m_range(), time(), interval())
    m_range()[m_range()$n %in% m_range()$n[m_range()$direction == "arrived" &
                                           #m_range()$time > time()[1] &
                                           m_range()$time <= time() + interval() * 60],]
  })

  ## Palette
  pal <- reactive({
    colorNumeric(palette = "Blues",
                 domain = as.numeric(c(min(c(m_range()$time, f_range()$feed_start, f_range()$feed_end)),
                                       max(c(m_range()$time, f_range()$feed_start, f_range()$feed_end)))))
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
    req(f_time(), m_time())
    p <- pal()
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
    req(m_range(), f_range(), start(), end(), interval())

    int_start <- seq(start(), end() - interval() * 60, by = paste(interval(), "min"))
    int_end <- seq(start() + interval() * 60, end(), by = paste(interval(), "min"))

    ## Add to end if not even
    if(length(int_end) != end()) {
      int_start <- c(int_start, int_end[length(int_end)])
      int_end <- c(int_end, end())
    }
    dplyr::bind_rows(
      dplyr::bind_cols(data.frame(block = sapply(f_range()$feed_start, FUN = function(x) which(x >= int_start & x < int_end)))) %>%
        dplyr::bind_cols(data.frame(time = int_start[.$block])) %>%
        dplyr::mutate(type = "Feeding"),
      dplyr::bind_cols(data.frame(block = sapply(m_range()$time, FUN = function(x) which(x >= int_start & x < int_end)))) %>%
        dplyr::bind_cols(data.frame(time = int_start[.$block])) %>%
        dplyr::mutate(type = "Movement")) %>%
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
      ggplot2::scale_x_datetime(labels = scales::date_format("%Y %b %d\n%H:%M", tz = lubridate::tz(m$time)),
                                limits = time_range()) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      ggplot2::labs(fill = "Event Type")
  })

  output$plot_time <- renderPlot({
    g_time()
  }, height = 200, width = 550)


 observeEvent(input$pause, {
    browser()
  })
}
