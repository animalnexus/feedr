#' Create summary map with leaflet
#'
#' A shiny wrapper for \code{map_leaflet()}. Allows the user to dynamically
#' select the time range that should be summarized. For more options, see
#' \code{map_leaflet()}. Also available through \code{animalnexus()}.
#'
#'
#' @param v Data frame. Visits data frame created with the \code{visits()}
#'   function.
#'
#' @examples
#'
#' \dontrun{
#' map_summary(visits(finches))
#' }
#'
#' @export
map_summary <- function(v) {

  # Check for correct formatting
  check_name(v, c("bird_id", "feeder_id", "start", "end"))
  check_time(v)
  check_format(v)

  app <- shiny::shinyApp(ui = shiny::fluidPage(includeCSS(system.file("extra", "style.css", package = "feedr")),
                                                          mod_UI_map_summary("standalone")),
                         server = function(input, output, session) {
                           shiny::callModule(mod_map_summary, "standalone",
                                             v = v)
                         }
  )
  shiny::runApp(app, display.mode = "normal")
}

## Summary map - UI
#' @import shiny
#' @import magrittr
#' @export
mod_UI_map_summary <- function(id) {
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
           #popify(uiOutput(ns("UI_bird_id")), title = "Individual",
           #       content = "ID of the individual to select.<br/>(Numbers) represent total number of movements.",
           #       options = list(container = "body")),
           popify(uiOutput(ns("UI_time_range")), title = "Time range",
                  content = "Select a particular time range to look at", options = list(container = "body")),
           hr(),
           h3("Instructions:"),
           p("Select the time range overwhich to summarize."),
           actionButton(ns("pause"), "Pause")
    ),
    column(8,
           popify(fluidRow(leafletOutput(ns("map"), height = 600)),
                  title = "Summary of movements and feeder use",
                  content = "This map shows overall summaries of movements and use of different feeders over time.<br/><b>Colour:</b> Time (pale blue is early, dark blue is late)<br/><b>Line width:</b> Increases with number of movements<br/><b>Circle size:</b> Increases with number of feeding bouts (note, <em>number of bouts</em>, not total feeding time)", options = list(container = "body"))
    )
  )
}

# Module server function
#' @import shiny
#' @import magrittr
#' @import leaflet
#' @export
mod_map_summary <- function(input, output, session, v = NULL, f = NULL, m = NULL) {

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
                          f[, c("feeder_id", "lon", "lat")])) %>%
    dplyr::filter(!is.na(feeder_id))

  ## UIs

  # Bird ID selection
  output$UI_bird_id <- renderUI({
    labels <- as.character(n_move$bird_id)
    names(labels) <- paste0(n_move$bird_id, " (", n_move$n, ")")
    selectInput(ns("bird_id"), label = "Select Individual",
                choices = c("Choose" = "", labels))
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

  ## Subselections
  f_id <- reactive({
    #req(input$bird_id)
    validate(need(sum(names(f) %in% c("lat", "lon")) == 2, "Latitude and longitude ('lat' and 'lon', respectively) were not detected in the data. Can't determine movement paths without them"))
    #dplyr::filter(f, bird_id == input$bird_id)
    f
  })

  f_range <- reactive({
    req(f_id(), time_range())
    f_id() %>%
      dplyr::filter(feed_start >= time_range()[1] & feed_end <= time_range()[2]) %>%
      dplyr::group_by(feeder_id, lat, lon) %>%
      dplyr::summarize(amount = sum(as.numeric(feed_length))) %>%
      dplyr::ungroup()
  })

  m_id <- reactive({
    #req(input$bird_id)
    validate(need(sum(names(m) %in% c("lat", "lon")) == 2, "Latitude and longitude ('lat' and 'lon', respectively) were not detected in the data. Can't determine movement paths without them"))
    m %>%
      #dplyr::filter(bird_id == input$bird_id) %>%
      dplyr::arrange(time)
  })

  m_range <- reactive({
    req(m_id(), time_range())
    m %>%
      dplyr::filter(!is.na(feeder_id),
                    time >= time_range()[1],
                    time <= time_range()[2]) %>%
      dplyr::group_by(bird_id, move_id) %>%
      dplyr::filter(length(move_id) == 2) %>%
      dplyr::group_by(feeder_id, move_path, lat, lon) %>%
      dplyr::summarize(path_use = length(move_path)) %>%
      dplyr::ungroup()
  })

  ## Palette
  pal <- reactive({
    colorNumeric(palette = "Blues",
                 domain = as.numeric(c(min(c(m_range()$time, f_range()$feed_start, f_range()$feed_end)),
                                       max(c(m_range()$time, f_range()$feed_start, f_range()$feed_end)))))
  })

  # Render Base Map
  output$map <- renderLeaflet({

    if(nrow(f_range()) == 0 & nrow(m_range()) == 0) feedr::map_leaflet_base(locs = feeders) else feedr::map_leaflet(u = f_range(), p = m_range(), locs = feeders)
    # leaflet(data = feeders[!is.na(feeders$lon),]) %>%
    #   addTiles(group = "Open Street Map") %>%
    #   addProviderTiles("Stamen.Toner", group = "Black and White") %>%
    #   addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    #   addProviderTiles("Esri.WorldTopoMap", group = "Terrain") %>%
    #   addMarkers(~lon, ~lat,
    #              popup  = htmltools::htmlEscape(feeders$feeder_id),
    #              group = "Readers") %>%
    #   addScaleBar(position = "bottomright") %>%
    #   addLayersControl(baseGroups = c("Satellite", "Terrain", "Open Street Map", "Black and White"),
    #                    overlayGroups = c("Readers", "Sunset/Sunrise", "Paths", "Feeding"),
    #                    options = layersControlOptions(collapsed = TRUE))
  })

  max_radius <- eventReactive(input$bird_id, {
  req(f_id())
  (75 - 10) / nrow(f_id())
  })

  max_width <- eventReactive(input$bird_id, {
    req(f_id())
    (50 - 10) / nrow(f_id())
  })

 observeEvent(input$pause, {
    browser()
  })
}
