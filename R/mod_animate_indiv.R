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
    column(4,
           popify(uiOutput(ns("UI_bird_id")), title = "Individual", placement = "right",
                  content = "ID of the individual to select.<br/>(X mv; Y fd) represent total number of movements and feeding events respectively.",
                  options = list(container = "body")),

           feedr:::mod_UI_maps_controls(ns("setup")),
           feedr:::mod_UI_maps_sunrise(ns("map")),
           feedr:::mod_UI_maps_instructions(ns("details"),
                                            specific = tagList(p("Select which individual you wish to animate, the time range, and the specific time (right) to at which to start.")))
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
mod_map_animate_indiv <- function(input, output, session, v = NULL, f = NULL, m = NULL) {

  ns <- session$ns
  values <- reactiveValues(m_old = NULL,
                           f_old = NULL)

  if(is.null(v) & is.null(f) & is.null(m)) stop("Require 'v' or 'f' and/or 'm'.")

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

  feeders <- unique(rbind(m[, c("feeder_id", "lon", "lat")],
                          f[, c("feeder_id", "lon", "lat")]))

  ## UIs

  # Bird ID selection
  output$UI_bird_id <- renderUI({
    labels <- as.character(samples$bird_id)
    names(labels) <- paste0(samples$bird_id, " (", samples$move, " mv; ", samples$feed, " fd)")
    selectInput(ns("bird_id"), label = "Select Individual",
                choices = c("Choose" = "", labels))
  })

  ## Subselections
  f_id <- reactive({
    req(input$bird_id, f)
    validate(need(sum(names(f) %in% c("lat", "lon")) == 2, "Latitude and longitude ('lat' and 'lon', respectively) were not detected in the data. Can't determine movement paths without them"))
    f %>% dplyr::filter(bird_id == input$bird_id)
  })

  f_avg <- reactive({
    req(f_id(), controls$time_range(), breaks)
    #req(input$bird_id %in% unique(f$bird_id))
    f_id() %>%
      dplyr::filter(feed_start >= controls$time_range()[1] & feed_end <= controls$time_range()[2]) %>%
      dplyr::mutate(block = as.POSIXct(cut(feed_start, breaks = breaks(), include.lowest = TRUE), tz = controls$tz())) %>%
      dplyr::group_by(feeder_id, block, lat, lon) %>%
      dplyr::summarize(amount = length(feed_start)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(n = 1:length(amount))
  })

  f_instant <- reactive({
    req(f_avg(), instant(), controls$interval())
    f_avg() %>%
        dplyr::filter(block <= instant() + controls$interval() * 60)
  })

  m_id <- reactive({
    req(input$bird_id, m)
    validate(need(sum(names(m) %in% c("lat", "lon")) == 2, "Latitude and longitude ('lat' and 'lon', respectively) were not detected in the data. Can't determine movement paths without them"))
    m %>% dplyr::filter(bird_id == input$bird_id)
  })

  m_avg <- reactive({
    req(m_id(), controls$time_range(), breaks)
    #req(input$bird_id %in% unique(m$bird_id))
    m_avg <- m_id() %>%
      dplyr::rename(n = move_id) %>%
      dplyr::filter(time >= controls$time_range()[1] & time <= controls$time_range()[2]) %>%
      dplyr::mutate(block = as.POSIXct(cut(time, breaks = breaks(), include.lowest = TRUE), tz = controls$tz())) %>%
      dplyr::arrange(time, move_path) %>%
      dplyr::group_by(feeder_id, move_path) %>%
      dplyr::mutate(path_use = length(move_path),
                    path_use = cumsum(path_use))
    if(nrow(m_avg) > 0 && m_avg$direction[1] == "arrived") m_avg <- m_avg[-1, ]
    prep_move <- function(x, y) {
      x <- x[x$block <= y$block[1], ]
      if(nrow(x) > 0 && x$direction[nrow(x)] == "left") x <- x[-nrow(x), ]
      return(x)
    }
    m_avg <- tibble::tibble(block = breaks()) %>%
      dplyr::group_by(block) %>%
      dplyr::do(sub = prep_move(x = m_avg, y = .))
    return(m_avg)
  })

  m_instant <- reactive({
    req(m_avg(), instant() %in% breaks())
    #m_avg()[m_avg()$n %in% m_avg()$n[m_avg()$direction == "arrived" & m_avg()$block <= instant()],]
    return(m_avg()$sub[[which(m_avg()$block == instant())]])
  })

  t_id <- reactive({
    req(m_id(), f_id())
    sort(lubridate::with_tz(c(m_id()$time, f_id()$feed_start, f_id()$feed_end), tz = lubridate::tz(m_id()$time)))
  })

  ## Summary for time figure
  events <- reactive({
    req(f_avg(), m_avg(), any(nrow(m_avg()) > 0, nrow(f_avg()) > 0))

    m_events <- m_avg()$sub[[nrow(m_avg())]]
    f_events <- f_avg()

    f_events %>%
      dplyr::group_by(block) %>%
      dplyr::summarize(type = "Feeding",
                    amount = sum(amount)) %>%
      dplyr::ungroup() %>%
      dplyr::bind_rows(m_events %>%
                         dplyr::group_by(block) %>%
                         dplyr::filter(direction == "arrived") %>%
                         dplyr::summarize(type = "Movements",
                                          amount = length(move_path)) %>%
                         dplyr::ungroup())
  })

  breaks <- reactive({
    req(controls$time_range, controls$interval())
    seq(controls$time_range()[1], controls$time_range()[2], by = paste(controls$interval(), "min"))
  })


  max_radius <- eventReactive(input$bird_id, {
    req(f_avg())
    (75 - 10) / nrow(f_avg())
  })

  max_width <- eventReactive(input$bird_id, {
    req(m_avg())
    (50 - 10) / nrow(m_avg()$sub[[nrow(m_avg())]])
  })

  controls <- callModule(mod_maps_controls, "setup", times = t_id)
  instant <- callModule(mod_maps_time, "setup_time", controls = controls, events = events)
  callModule(mod_maps_sunrise, "map", instant = instant, controls = controls)

  callModule(mod_maps_leaflet, "map", type = "movements", title = reactive({"Time"}),
             data = reactive({list(feeding = f_instant(), movements = m_instant())}),
             data_total = reactive({list(feeding = f_avg(), movements = m_avg()$sub[[nrow(m_avg())]])}))

  ## Palette
  # pal <- reactive({
  #   req(any(nrow(m_avg()) > 0, nrow(f_avg()) > 0))
  #   t <- vector()
  #   if(!is.null(m_avg())) t <- m_avg()$time
  #   if(!is.null(f_avg())) t <- c(t, f_avg()$feed_start, f_avg()$feed_end)

  #    pal <- colorNumeric(palette = "Blues", domain = as.numeric(c(min(t), max(t))))
  # })

  # Render Base Map
  # output$map <- renderLeaflet({
  #   leaflet(data = feeders[!is.na(feeders$lon),]) %>%
  #     addTiles(group = "Open Street Map") %>%
  #     addProviderTiles("Stamen.Toner", group = "Black and White") %>%
  #     addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  #     addProviderTiles("Esri.WorldTopoMap", group = "Terrain") %>%
  #     addMarkers(~lon, ~lat,
  #                popup  = htmltools::htmlEscape(feeders$feeder_id),
  #                group = "Readers") %>%
  #     addScaleBar(position = "bottomright") %>%
  #     addLayersControl(baseGroups = c("Satellite", "Terrain", "Open Street Map", "Black and White"),
  #                      overlayGroups = c("Readers", "Sunset/Sunrise", "Paths", "Feeding"),
  #                      options = layersControlOptions(collapsed = TRUE))
  # })


  # Add paths and points to animated map
  # observeEvent(instant(), {
  #   p <- pal()
  #   if(!is.null(m_avg())) {
  #     if(!(identical(values$m_old, m_instant()))){
  #       leafletProxy(ns("map")) %>% clearGroup(group = "Paths")
  #       values$m_old <- m_instant()
  #       if(nrow(m_instant()) > 0){
  #         temp <- m_instant() %>%
  #           dplyr::group_by(direction, move_path) %>%
  #           dplyr::mutate(n_path = length(move_path):1)
  #         for(n in unique(temp$n)) {
  #           leafletProxy(ns("map")) %>%
  #             addPolylines(data = temp[temp$n == n, ],
  #                          ~lon, ~lat,
  #                          opacity = 1,
  #                          weight = ~ n_path * max_width() + 10,
  #                          group = "Paths",
  #                          color = ~p(as.numeric(time)))
  #         }
  #       }
  #     }
  #   }
  #   if(!is.null(f_avg())){
  #     if(!(identical(values$f_old, f_instant()))){
  #       leafletProxy(ns("map")) %>% clearGroup(group = "Feeding")
  #       values$f_old <- f_instant()
  #       if(nrow(f_instant()) > 0) {
  #         f <- f_instant() %>%
  #           dplyr::group_by(feeder_id) %>%
  #           dplyr::mutate(n_bout = length(feed_start):1)
  #         leafletProxy(ns("map")) %>%
  #           addCircleMarkers(data = f,
  #                            ~lon, ~lat,
  #                            weight = 2,
  #                            fillOpacity = 1,
  #                            radius = ~ n_bout * max_radius() + 10,
  #                            group = "Feeding",
  #                            color = NULL,
  #                            opacity = 1,
  #                            fillColor = ~p(as.numeric(feed_start)))
  #       }
  #     }
  #   }
  # })



  observeEvent(input$pause, {
    browser()
  })
}
