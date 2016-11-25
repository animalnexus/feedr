#' Animated map for individuals with leaflet
#'
#' Interactive shiny app to select and animate movements and feeder use of
#' different individuals over time. Also available through \code{animalnexus()}.
#'
#' @param v Data frame. Data frame. Visits data frame created with the
#'   \code{visits()} function.
#' @parm verbose Logical. Print to console log events.
#'
#' @examples
#'
#' \dontrun{
#' map_animate_indiv(visits(finches))
#' }
#'
#' @export
map_animate_indiv <- function(v, verbose = FALSE) {

  # Check for correct formatting
  check_name(v, c("bird_id", "feeder_id", "start", "end"))
  check_time(v)
  check_format(v)

  app <- shiny::shinyApp(ui = shiny::fluidPage(shinyjs::useShinyjs(),
                                               includeCSS(system.file("extra", "style.css", package = "feedr")),
                                               mod_UI_stop("stp"),
                                               mod_UI_map_animate_indiv("standalone")),
                         server = function(input, output, session) {
                           shiny::callModule(mod_map_animate_indiv, "standalone", v = v, verbose = verbose)
                           shiny::callModule(mod_stop, "stp")
                         }
  )
  shiny::runApp(app, launch.browser = TRUE)
}

## Animated map - UI
#' @import shiny
#' @import magrittr
#' @export
mod_UI_map_animate_indiv <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    column(4,
           feedr:::mod_UI_maps_instructions(ns("details"),
                                            specific = tagList(p("Select which data you wish to summarize and how you wish to summarize it. Hover the mouse over options for more details."))),
           feedr:::mod_UI_maps_advanced(ns("adv")),
           feedr:::mod_UI_maps_controls(ns("setup")),

           feedr:::mod_UI_maps_sunrise(ns("map")),
           feedr:::mod_UI_maps_tips(ns("tips"))
    ),
    column(8,
           # popify(fluidRow(leafletOutput(ns("map"), height = 600)),
           #        title = "Movements and feeder use",
           #        content = "This map shows individual movements and use of different feeders over time.<br/><b>Colour:</b> Time (pale blue is early, dark blue is late)<br/><b>Line width:</b> Increases with number of movements<br/><b>Circle size:</b> Increases with number of feeding bouts (note, <em>number of bouts</em>, not total feeding time)", options = list(container = "body")),
           feedr:::mod_UI_maps_leaflet(ns("map")),
           feedr:::mod_UI_maps_time(ns("setup_time"), type = "Feeding/Movement events")
    )
  )
}

# Module server function
#' @import shiny
#' @import magrittr
#' @import leaflet
#' @export
mod_map_animate_indiv <- function(input, output, session, v = NULL, f = NULL, m = NULL, verbose = FALSE) {

  ns <- session$ns

  if(is.null(v) & is.null(f) & is.null(m)) stop("Require 'v' or 'f' and/or 'm'.")

  ## Controls
  controls <- callModule(mod_maps_controls, "setup", times = t_id, verbose = verbose)
  summary <- callModule(mod_maps_advanced, "adv", samples = reactive({samples}), verbose = verbose)
  instant <- callModule(mod_maps_time, "setup_time", controls = controls, events = events, verbose = verbose)

  ## Maps
  callModule(mod_maps_sunrise, "map", instant = instant, controls = controls, verbose = verbose)
  callModule(mod_maps_leaflet, "map",
             summary = summary$summary,
             data = reactive({list(feeding = f_instant(), movements = m_instant())}),
             data_total = reactive({list(feeding = f_data(), movements = m_data())}), verbose = verbose)

  ## Data
  # Fix time zone to local non-DST
  if(!is.null(v)){
    v <- data_tz(v)
    m <- move(v, all = TRUE)
    f <- feeding(v)
    if(nrow(m) == 0 || all(is.na(m$direction))) m <- NULL
  } else {
    f <- data_tz(f)
    if(!is.null(m)) m <- data_tz(m)
  }

  ## Fix one visit feeding bouts (technically length == 0)
  f$feed_length[f$feed_length == 0] <- 3/60

  ## Summarize movements and feeding
  samples <- f %>%
    dplyr::group_by(bird_id) %>%
    dplyr::summarize(feed = length(feed_length))

  if(!is.null(m)){
      samples <- dplyr::full_join(samples,
                                  m %>%
                                    dplyr::group_by(bird_id) %>%
                                    dplyr::summarize(move = floor(length(direction) / 2)),
                                  by = "bird_id") %>%
        dplyr::mutate(move = replace(move, is.na(move), 0))
  } else {
    samples$move <- 0
  }

  ## Subselections - Get bird ID
  f_id <- reactive({
    req(f, summary$bird_id())
    validate(need(sum(names(f) %in% c("lat", "lon")) == 2, "Latitude and longitude ('lat' and 'lon', respectively) were not detected in the data. Can't determine movement paths without them"))
    if(verbose) cat("Feeder ID: \n")

    if(summary$bird_id() == "all") {
      if(verbose) cat("  all\n")
      f_id <- f
    } else {
      req(summary$bird_id() %in% samples$bird_id)
      if(verbose) cat("  indiv\n")
      f_id <- f %>% dplyr::filter(bird_id == summary$bird_id())
    }
    return(f_id)
  })

  m_id <- reactive({
    req(m, summary$bird_id())
    validate(need(sum(names(m) %in% c("lat", "lon")) == 2, "Latitude and longitude ('lat' and 'lon', respectively) were not detected in the data. Can't determine movement paths without them"))
    if(verbose) cat("Movements ID: \n")

    if(summary$bird_id() == "all"){
      if(verbose) cat("  all\n")
      m_id <- m
    } else {
      req(summary$bird_id() %in% samples$bird_id)
      if(verbose) cat("  indiv\n")
      m_id <- m %>% dplyr::filter(bird_id == summary$bird_id())
    }
    m_id <- dplyr::filter(m_id, !is.na(direction))
    return(m_id)
  })

  ## Get ranges (don't depend on f_id/m_id, as those are updated, breaks() will be updated, so best to THEN activate
  f_avg <- reactive({
    req(controls$breaks(), summary$summary())
    #req(summary$bird_id() %in% unique(f$bird_id))
    isolate({
      req(controls$tz(), f_id())
      if(verbose) cat("Feed avg: ")
      withProgress(message = "Updating intervals", detail = "Feeding bouts", {
        f_avg <- f_id() %>%
          dplyr::filter(feed_start >= min(controls$breaks()) & feed_end <= max(controls$breaks())) %>%
          dplyr::mutate(block = as.POSIXct(cut(feed_start, breaks = controls$breaks(), include.lowest = TRUE), tz = controls$tz())) %>%
          dplyr::group_by(block, feeder_id, lat, lon)

        if(summary$summary() == "sum") {
          if(verbose) cat("sum\n")
          f_avg <- dplyr::summarize(f_avg,
                                    amount = sum(feed_length))
        } else if(summary$summary() == "sum_indiv") {
          if(verbose) cat("sum_indiv\n")
          f_avg <- dplyr::summarize(f_avg,
                                    amount = sum(feed_length) / length(unique(bird_id)))
        } #else if(summary$summary() == "total_indiv"){
          #if(verbose) cat("total_indiv\n")
          #f_avg <- dplyr::summarize(f_avg,
          #                          amount = length(unique(bird_id)))
        #}
        f_avg <- dplyr::ungroup(f_avg)
      })
      return(f_avg)
    })
  })

  m_avg <- reactive({
    req(controls$breaks(), summary$summary())
    isolate({
      req(controls$tz(), m_id())
      if(verbose) cat("Movement avg: ")
      withProgress(message = "Updating intervals", detail = "Movements", {
        #req(summary$bird_id() %in% unique(m$bird_id))
        m_avg <- m_id() %>%
          dplyr::mutate(move_id = paste0(bird_id, "_", move_id)) %>%
          dplyr::group_by(move_id) %>%
          dplyr::mutate(block = as.POSIXct(cut(time[direction == "arrived"], breaks = controls$breaks(), include.lowest = TRUE), tz = controls$tz())) %>%
          dplyr::ungroup() %>%
          dplyr::filter(!is.na(block)) %>%
          dplyr::group_by(block, feeder_id, lat, lon, move_path)
        if(summary$summary() == "sum") {
          if(verbose) cat("sum\n")
          m_avg <- dplyr::summarize(m_avg, path_use = length(move_path))
        } else if(summary$summary() == "sum_indiv") {
          if(verbose) cat("sum_indiv\n")
          m_avg <- dplyr::summarize(m_avg, path_use = length(move_path) / length(unique(bird_id)))
        } #else if(summary$summary() == "total_indiv"){
         # if(verbose) cat("total_indiv\n")
         # m_avg <- dplyr::summarize(m_avg, path_use = length(unique(bird_id)))
       # }
        m_avg <- dplyr::ungroup(m_avg)
      })
      return(m_avg)
    })
  })

  ## Get multiple data sets corresponding to all instants
  f_data <- reactive({
    req(f_avg(), summary$type()) #cumulative vs. instant
    if(verbose) cat("Feed data\n")
    withProgress(message = "Updating intervals", detail = "Feeding intervals", {
      f_data <- tibble::tibble(block = isolate(controls$breaks())) %>%
        dplyr::group_by(block) %>%
        dplyr::do(sub = prep_feeding(x = f_avg(), y = ., type = summary$type())) %>%
        dplyr::ungroup()
    })
    return(f_data)
  })

  m_data <- reactive({
    req(m_avg(), summary$type()) #cumulative vs. instant
    if(verbose) cat("Movement data\n")
    withProgress(message = "Updating intervals", detail = "Movement intervals", {
      m_data <- tibble::tibble(block = isolate(controls$breaks())) %>%
        dplyr::group_by(block) %>%
        dplyr::do(sub = prep_movements(x = m_avg(), y = ., type = summary$type()))
    })
    return(m_data)
  })

  ## Get data corresponding to specific instant
  f_instant <- reactive({
    req(f_data(), instant())
    isolate({
      req(instant() %in% controls$breaks())
      if(verbose) cat("Feed instant\n")
      f_data()$sub[[which(f_data()$block == instant())]]
    })
  })

  m_instant <- reactive({
    req(m_data(),  instant())
    isolate({
      req(instant() %in% controls$breaks())
      if(verbose) cat("Movement instant\n")
      m_data()$sub[[which(m_data()$block == instant())]]
    })
  })

  ## Total time range
  t_id <- reactive({
    req(m_id(), f_id())
    if(verbose) cat("Times ID\n")
    sort(lubridate::with_tz(c(m_id()$time, f_id()$feed_start, f_id()$feed_end), tz = lubridate::tz(m_id()$time)))
  })

  ## Summary for time figure
  events <- reactive({
    req(f_avg(), m_avg(), any(nrow(m_avg()) > 0, nrow(f_avg()) > 0))

    if(verbose) cat("Events\n")
    m_events <- m_avg()
    f_events <- f_avg()

    events <- f_events %>%
      dplyr::select(block, n = amount) %>%
      dplyr::mutate(type = "Feeding",
                    n = n) %>%
      dplyr::bind_rows(m_events %>%
                         dplyr::select(block, move_path, path_use) %>%
                         dplyr::group_by(block, move_path) %>%
                         dplyr::summarize(type = "Movements",
                                          n = unique(path_use)) %>%
                         dplyr::ungroup()) %>%
      dplyr::group_by(block, type) %>%
      dplyr::summarize(n = sum(n)) %>%
      dplyr::group_by(type) %>%
      dplyr::mutate(n = n / sum(n)) %>%
      dplyr::ungroup()
    return(events)
  })

  observeEvent(input$pause, {
    browser()
  })
}
