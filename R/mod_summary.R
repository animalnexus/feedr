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
           p("Select the time range overwhich to summarize.")#,
           #actionButton(ns("pause"), "Pause")
    ),
    column(8,
           popify(fluidRow(leafletOutput(ns("map"), height = 600)),
                  title = "Summary of movements and feeder use",
                  content = "This map shows overall summaries of movements and use of different feeders over time.<br/><b>Colour:</b> Time (pale blue is early, dark blue is late)<br/><b>Line width:</b> Increases with number of movements<br/><b>Circle size:</b> Increases with number of feeding bouts (note, <em>number of bouts</em>, not total feeding time)", options = list(container = "body")),
           div(
             fluidRow(div(popify(plotOutput(ns("plot_time"), height = "100%"),
                                 placement = "left", title = "Summarized events over time",
                                 content = "Number of movements or feeding bouts over time which are summarized in the above map."),
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

    m <- move(v)
    f <- feeding(v)
  } else {
    m <- m %>%
      dplyr::mutate(time = lubridate::with_tz(time, tzone = tz_offset(attr(m$time, "tzone"), tz_name = TRUE)), day = as.Date(start))
    f <- f %>%
      dplyr::mutate(feed_start = lubridate::with_tz(feed_start, tzone = tz_offset(attr(f$feed_start, "tzone"), tz_name = TRUE)),
                    feed_end = lubridate::with_tz(feed_end, tzone = tz_offset(attr(f$feed_end, "tzone"), tz_name = TRUE)),
                    day = as.Date(feed_start))
  }
  tz <- tz_offset(attr(f$feed_start, "tzone"))
  tz_name <- tz_offset(attr(f$feed_start, "tzone"), tz_name = TRUE)

  if(tz >=0) tz <- paste0("+", sprintf("%02d", abs(tz)), "00") else tz <- paste0("-", sprintf("%02d", abs(tz)), "00")

  start <- lubridate::floor_date(min(c(m$time, f$feed_start), na.rm = TRUE), unit = "hour")
  end <- lubridate::ceiling_date(max(c(m$time, f$feed_end), na.rm = TRUE), unit = "hour")


  ## How many movements?
  n_move <- m %>%
    dplyr::group_by(bird_id) %>%
    dplyr::summarize(n = floor(length(direction) / 2))


  feeders <- unique(rbind(m[, c("feeder_id", "lon", "lat")],
                          f[, c("feeder_id", "lon", "lat")])) %>%
    dplyr::filter(!is.na(feeder_id))

  ## UIs

  # Time range - select subsection of data
   output$UI_time_range <- renderUI({
     req(m, f, start, end, tz)
     sliderInput(ns("time_range"), "Time Range",
                 min = start,
                 max = end,
                 value = c(start, end),
                 step = 60 * 60,
                 timezone = tz)
   })

   ## Convert to proper tz
   time_range <- reactive({
     req(input$time_range, start, end)
     lubridate::with_tz(input$time_range, lubridate::tz(m$time))
   })

  ## Subselections
  f_range <- reactive({
    req(f, time_range())
    validate(need(sum(names(f) %in% c("lat", "lon")) == 2, "Latitude and longitude ('lat' and 'lon', respectively) were not detected in the data. Can't determine movement paths without them"))
    f %>%
      dplyr::filter(feed_start >= time_range()[1] & feed_end <= time_range()[2])
  })

  f_avg <- reactive({
    req(f_range())
    f_range() %>%
      dplyr::group_by(feeder_id, lat, lon) %>%
      dplyr::summarize(amount = sum(as.numeric(feed_length))) %>%
      dplyr::ungroup()
  })

  m_range <- reactive({
    req(m, time_range())
    validate(need(sum(names(m) %in% c("lat", "lon")) == 2, "Latitude and longitude ('lat' and 'lon', respectively) were not detected in the data. Can't determine movement paths without them"))
    m %>%
      dplyr::arrange(time) %>%
      dplyr::filter(!is.na(feeder_id),
                    time >= time_range()[1],
                    time <= time_range()[2]) %>%
      dplyr::group_by(bird_id, move_id) %>%
      dplyr::filter(length(move_id) == 2)
  })

  m_avg <- reactive({
    req(m_range())
    m_range() %>%
      dplyr::group_by(feeder_id, move_path, lat, lon) %>%
      dplyr::summarize(path_use = length(move_path)) %>%
      dplyr::ungroup()
  })

  ## Palette
  pal <- reactive({
    colorNumeric(palette = "Blues",
                 domain = as.numeric(c(min(c(m_avg()$time, f_avg()$feed_start, f_avg()$feed_end)),
                                       max(c(m_avg()$time, f_avg()$feed_start, f_avg()$feed_end)))))
  })

  # Render Base Map
  output$map <- renderLeaflet({
    if(nrow(f_avg()) == 0 & nrow(m_avg()) == 0) {
      feedr::map_leaflet_base(locs = feeders)
    } else {
      feedr::map_leaflet(u = f_avg(), p = m_avg(), locs = feeders)
    }
  })


  ## Summary for time figure
  events <- reactive({
    req(start, end, time_range(), any(nrow(m_range()) > 0, nrow(f_range()) > 0))

    m_range()
    f_range()

    isolate({
      int_start <- seq(start, end - 5 *60, by = paste(5, "min"))
      int_end <- seq(start + 5 *60, end, by = paste(5, "min"))

      ## Add to end if not even
      if(length(int_end) != end) {
        int_start <- lubridate::with_tz(c(int_start, int_end[length(int_end)]), tzone = tz_name)
        int_end <- lubridate::with_tz(c(int_end, end), tzone = tz_name)
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
    lab <- "No. events summarized"
    ggplot2::ggplot(data = events(), ggplot2::aes(x = time, y = n, fill = type)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(x = "Time", y = lab) +
      ggplot2::scale_y_continuous(expand = c(0,0)) +
      ggplot2::scale_x_datetime(labels = scales::date_format("%Y %b %d\n%H:%M", tz = tz_name),
                                limits = time_range()) +
      ggplot2::geom_bar(stat = "identity", position = "dodge", width = 5*60, colour = "black") +
      ggplot2::labs(fill = "Event Type")
  })

  output$plot_time <- renderPlot({
    g_time()
  }, height = 200, width = 625)


 observeEvent(input$pause, {
    browser()
  })
}
