# Map UI function -------------------------------------------------------
#' Animated map for individuals with leaflet
#'
#' Interactive shiny app to select and animate presence at and movements between
#' RFID loggers over time. Also available online at \url{http://animalnexus.ca} or
#' by launching the local animalnexus app through \code{\link{animalnexus}}.
#'
#' @param v Data frame. Visits data frame created with the \code{\link{visits}}
#'   function.
#' @param verbose Logical. Print log events to console.
#' @param diagnostic Logical. Display pause button for debugging
#'
#' @examples
#'
#' \dontrun{
#' map_animate_indiv(visits(finches))
#' }
#'
#' @export

ui_animate <-  function(v, verbose = FALSE, diagnostic = FALSE) {
  # Check for correct formatting
  check_name(v, c("animal_id", "logger_id", "start", "end"))
  check_time(v)
  check_format(v)

  ui_app(name = "map_animate", visits = reactive({v}), verbose = verbose, diagnostic = diagnostic)
}



# Complete UI for animations -------------------------------------------------------
#' @import shiny
#' @import magrittr
mod_UI_map_animate <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    column(4,
           mod_UI_maps_instructions(ns("details")),
           mod_UI_maps_advanced(ns("adv")),
           mod_UI_maps_controls(ns("setup")),

           mod_UI_maps_sunrise(ns("map")),
           mod_UI_maps_tips(ns("tips"))
    ),
    column(8,
           mod_UI_maps_leaflet(ns("map")),
           mod_UI_maps_time(ns("setup_time"), type = "Presence/Movement events")
    )
  )
}

# Complete server for animations -------------------------------------------------------
#' @import shiny
#' @import magrittr
#' @import leaflet
mod_map_animate <- function(input, output, session, visits, verbose = FALSE) {

  ns <- session$ns

  # Instructions-------------------------------
  callModule(mod_maps_instructions, "details")

  # Controls ----------------------------------
  controls <- callModule(mod_maps_controls, "setup", times = t_id, verbose = verbose)
  summary <- callModule(mod_maps_advanced, "adv", samples = samples, verbose = verbose)
  instant <- callModule(mod_maps_time, "setup_time", controls = controls, events = events, verbose = verbose)

  # Maps---------------------------------------
  callModule(mod_maps_sunrise, "map", instant = instant, controls = controls, verbose = verbose)
  callModule(mod_maps_leaflet, "map",
             summary = summary$summary,
             data_instant = data_instant,
             data_total = data_total, verbose = verbose)

  # Data ---------------------------------------
  # Fix time zone to local non-DST

  data_instant <- reactive({
    req(p_instant())
    if(nrow(m_avg()) == 0) d <- list(presence = p_instant()) else d <- list(presence = p_instant(), movements = m_instant())
    return(d)
  })

  data_total <- reactive({
    req(p_data())
    if(nrow(m_avg()) == 0) d <- list(presence = p_data()) else d <- list(presence = p_data(), movements = m_data())
    return(d)
  })

  v <- reactive({
    req(visits())
    data_tz(visits())
  })

  m <- reactive({
    req(v())
    move(v())
  })

  p <- reactive({
    req(v())
    p <- presence(v())
    ## Fix one visit presence bouts (technically length == 0)
    p$length[p$length == 0] <- 3/60
    return(p)
  })

  # Summarize movements and presence -------------------------------------------
  samples <- reactive({
    req(p(), m())
    p() %>%
      dplyr::group_by(animal_id) %>%
      dplyr::summarize(presence = length(length)) %>%
      dplyr::full_join(m() %>%
                         dplyr::group_by(animal_id) %>%
                         dplyr::summarize(move = floor(length(direction) / 2)),
                       by = "animal_id") %>%
      dplyr::mutate(move = replace(move, is.na(move), 0))
  })

  # Subselections - Get animal ID -------------------------------------------------
  p_id <- reactive({
    req(p(), summary$animal_id())
    validate(need(sum(names(p()) %in% c("lat", "lon")) == 2, "Latitude and longitude ('lat' and 'lon', respectively) were not detected in the data. Can't determine movement paths without them"))
    if(verbose) cat("Logger ID: \n")

    if(summary$animal_id() == "all") {
      if(verbose) cat("  all\n")
      p_id <- p()
    } else {
      req(summary$animal_id() %in% samples()$animal_id)
      if(verbose) cat("  indiv\n")
      p_id <- p() %>% dplyr::filter(animal_id == summary$animal_id())
    }
    return(p_id)
  })

  m_id <- reactive({
    req(m(), summary$animal_id())
    if(nrow(m()) > 0) validate(need(sum(names(m()) %in% c("lat", "lon")) == 2, "Latitude and longitude ('lat' and 'lon', respectively) were not detected in the data. Can't determine movement paths without them"))
    if(verbose) cat("Movements ID: \n")

    if(summary$animal_id() == "all"){
      if(verbose) cat("  all\n")
      m_id <- m()
    } else {
      req(summary$animal_id() %in% samples()$animal_id)
      if(verbose) cat("  indiv\n")
      m_id <- m() %>% dplyr::filter(animal_id == summary$animal_id())
    }
    return(m_id)
  })

  # Get data ranges -------------------------------------------------------------------------
  # Don't react to p_id/m_id, as p_id/m_id are updated, breaks() will be updated, so best to just respond to breaks
  p_avg <- reactive({
    req(controls$breaks(), summary$summary())
    #req(summary$animal_id() %in% unique(p$animal_id))
    isolate({
      req(controls$tz(), p_id())
      if(verbose) cat("Presence avg: ")
      withProgress(message = "Updating intervals", detail = "Presence", {
        p_avg <- p_id() %>%
          dplyr::filter(start >= min(controls$breaks()) & end <= max(controls$breaks())) %>%
          dplyr::mutate(block = as.POSIXct(cut(start, breaks = controls$breaks(), include.lowest = TRUE), tz = controls$tz())) %>%
          dplyr::group_by(block, logger_id, lat, lon)

        if(summary$summary() == "sum") {
          if(verbose) cat("sum\n")
          p_avg <- dplyr::summarize(p_avg,
                                    amount = sum(length),
                                    n = length(length))
        } else if(summary$summary() == "sum_indiv") {
          if(verbose) cat("sum_indiv\n")
          p_avg <- dplyr::summarize(p_avg,
                                    amount = sum(length) / length(unique(animal_id)),
                                    n = length(length))
        } #else if(summary$summary() == "total_indiv"){
          #if(verbose) cat("total_indiv\n")
          #p_avg <- dplyr::summarize(p_avg,
          #                          amount = length(unique(animal_id)))
        #}
        p_avg <- dplyr::ungroup(p_avg)
      })
      return(p_avg)
    })
  })

  m_avg <- reactive({
    req(controls$breaks(), summary$summary())
    isolate({
      req(controls$tz(), m_id())
      if(verbose) cat("Movement avg: ")
      withProgress(message = "Updating intervals", detail = "Movements", {
        #req(summary$animal_id() %in% unique(m$animal_id))
        m_avg <- m_id()
        if(nrow(m_avg) > 0) {
          m_avg <- m_avg %>%
            dplyr::mutate(move_id = paste0(animal_id, "_", move_id)) %>%
            dplyr::group_by(move_id) %>%
            dplyr::mutate(block = as.POSIXct(cut(time[direction == "arrived"], breaks = controls$breaks(), include.lowest = TRUE), tz = controls$tz())) %>%
            dplyr::ungroup() %>%
            dplyr::filter(!is.na(block)) %>%
            dplyr::group_by(block, logger_id, lat, lon, move_path)
          if(summary$summary() == "sum") {
            if(verbose) cat("sum\n")
            m_avg <- dplyr::summarize(m_avg, path_use = length(move_path))
          } else if(summary$summary() == "sum_indiv") {
            if(verbose) cat("sum_indiv\n")
            m_avg <- dplyr::summarize(m_avg, path_use = length(move_path) / length(unique(animal_id)))
          }
          m_avg <- dplyr::ungroup(m_avg)
        } else {
          if(verbose) cat("none\n")
        }
      })
      return(m_avg)
    })
  })

  # Get multiple data sets corresponding to all instants ---------------------------
  p_data <- reactive({
    req(p_avg(), summary$type()) #cumulative vs. instant
    if(verbose) cat("Presence data\n")
    withProgress(message = "Updating intervals", detail = "Presence intervals", {
      p_data <- tibble::tibble(block = isolate(controls$breaks())) %>%
        dplyr::group_by(block) %>%
        dplyr::do(sub = prep_presence(x = p_avg(), y = ., type = summary$type())) %>%
        dplyr::ungroup()
    })
    return(p_data)
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

  # Get data corresponding to specific instant --------------------------------------
  p_instant <- reactive({
    req(p_data(), instant())
    isolate({
      req(as.numeric(instant()) %in% controls$breaks())
      req(as.numeric(instant()) %in% p_data()$block)
      if(verbose) cat("Presence instant\n")
      p_data()$sub[[which(p_data()$block == instant())]]
    })
  })

  m_instant <- reactive({
    req(m_data(),  instant())
    isolate({
      req(as.numeric(instant()) %in% controls$breaks())
      req(as.numeric(instant()) %in% m_data()$block)
      if(verbose) cat("Movement instant\n")
      m_data()$sub[[which(m_data()$block == instant())]]
    })
  })

  # Total time range ----------------------------------------------------------------------
  t_id <- reactive({
    req(m_id(), p_id())
    if(verbose) cat("Times ID\n")
    sort(lubridate::with_tz(c(p_id()$start, p_id()$end), tz = lubridate::tz(p_id()$start)))
  })

  # Summary for time figure ---------------------------------------------------------------
  events <- reactive({
    req(p_avg(), m_avg(), any(nrow(m_avg()) > 0, nrow(p_avg()) > 0))
    req(controls$instant_range())

    if(verbose) cat("Events\n")

    if(nrow(m_avg()) == 0) {
      m_events <- p_avg() %>%
        dplyr::select(block) %>%
        dplyr::mutate(type = "Movements",
                      n = 0)
    } else {
      m_events <- m_avg() %>%
        dplyr::select(block, move_path, path_use) %>%
        dplyr::group_by(block, move_path) %>%
        dplyr::summarize(type = "Movements",
                         n = unique(path_use)) %>%
        dplyr::ungroup()
    }

    p_avg() %>%
      dplyr::select(block, n) %>%
      dplyr::mutate(type = "Presence") %>%
      dplyr::bind_rows(m_events) %>%
      dplyr::group_by(block, type) %>%
      dplyr::summarize(n = sum(n)) %>%
      dplyr::ungroup()
  })
}
