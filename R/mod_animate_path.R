#' Animate map with leaflet
#'
#' @export
map_animate_path <- function(v) {

  # Check for correct formatting
  check_name(v, c("bird_id", "feeder_id", "start", "end"))
  check_time(v)
  check_format(v)

  app <- shiny::shinyApp(ui = shiny::fluidPage(mod_UI_map_animate_path("standalone")),
                         server = function(input, output, session) {
                           shiny::callModule(mod_map_animate_path, "standalone",
                                             v = v)
                         }
  )
  shiny::runApp(app, display.mode = "normal")
}

## Animated map - UI
#' @import shiny
#' @import magrittr
#' @export
mod_UI_map_animate_path <- function(id) {
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
      "div#", ns("UI_anim_time")," {
      padding-left:30px;
      width: 550px;
      max-width: 100%;
      display: block;
      margin-left: auto;
      margin-right: auto;
      }"))),
    column(4,
           uiOutput(ns("UI_bird_id")),
           hr(),
           sliderInput(ns("anim_speed"), "Animation speed",
                       min = 0, max = 100,
                       post = "%",
                       value = 50),
           h3("Instructions:"),
           p("Select which individual you wish to animate, and the specific 'Time' (right) to at which to start."),
           p("Paths can be animated over time by clicking on the", strong("small blue arrow"), "to the lower right of the 'Time' slider (right)."),
           p("The speed of this animation can be adjusted with the 'Speed' slider bar (above)."),
           actionButton(ns("pause"), "Pause")
  ),
  column(8,
         fluidRow(leafletOutput(ns("map"), height = 600)),
         div(
           fluidRow(uiOutput(ns("UI_anim_time")), style = "text-align: center;"),
           fluidRow(div(plotOutput(ns("plot_time"), height = "100%"), style = "height: 150px")),
           fluidRow(div(
             strong("Note that times are in Local Standard Time (no DST)"), br(),
             strong("Colours represent 'Reader'"), style = "text-align: center;")),
           style = "text-align: center;")
  )
    )
  }

# Module server function
#' @import shiny
#' @import magrittr
#' @import leaflet
#' @export
mod_map_animate_path <- function(input, output, session, v) {

  ns <- session$ns
  values <- reactiveValues(m_sub_time_old = NULL)

  ## Palette
  pal <- colorRampPalette(c("cyan","blue"))

  # Fix time zone to LOOK like local non-DST, but assigned UTC (for timezone slider)
  v <- v %>%
    dplyr::mutate(start = lubridate::with_tz(start, tzone = tz_offset(attr(v$start, "tzone"), tz_name = TRUE)),
                  end = lubridate::with_tz(end, tzone = tz_offset(attr(v$end, "tzone"), tz_name = TRUE)),
                  day = as.Date(start))
  tz <- tz_offset(attr(v$start, "tzone"))
  if(tz >=0) tz <- paste0("+", sprintf("%02d", abs(tz)), "00") else tz <- paste0("-", sprintf("%02d", abs(tz)), "00")

  ## Calculate movements
  m <- feedr::move(v, all = TRUE)

  #lubridate::tz(v$start) <- "UTC"
  #lubridate::tz(v$end) <- "UTC"

  ## UIs
  # Bird ID selection
  output$UI_bird_id <- renderUI({
    selectInput(ns("bird_id"), label = "Select Individual", choices = c("Choose" = "", unique(as.character(m$bird_id))))
  })

  # Time slider - minimum interval,
  output$UI_anim_time <- renderUI({
    req(input$anim_speed, input$bird_id, m_sub_ID(), interval())
    validate(need(sum(!is.na(m_sub_ID()$direction)) > 0, "Individual made no movements"))

    sliderInput(ns("anim_time"), "Time",
                min = lubridate::floor_date(min(m_sub_ID()$time), unit = "hour"),
                max = lubridate::ceiling_date(max(m_sub_ID()$time), unit = "hour"),
                value = lubridate::floor_date(min(m_sub_ID()$time), unit = "hour"),
                step = 60 * interval(),
                timezone = tz,
                animate = animationOptions(interval = 500 * (1 - (input$anim_speed/100)) + 50, loop = FALSE),
                width = "520px")
  })


  ## Convert to proper tz
  anim_time <- reactive({
    req(input$anim_time)
    lubridate::with_tz(input$anim_time, lubridate::tz(v$start))
  })

  ## Subselections
  m_sub_ID <- reactive({
    req(input$bird_id)
    validate(need(sum(names(m) %in% c("lat", "lon")) == 2, "Latitude and longitude ('lat' and 'lon', respectively) were not detected in the data. Can't determine movement paths without them"))
    m %>%
      dplyr::filter(bird_id == input$bird_id)
  })

  m_sub_time <- reactive({
    req(input$anim_time, interval(), m_sub_ID())
    m_sub_ID() %>%
      dplyr::filter(time <= anim_time() + interval() * 60,
                    direction == "arrived")
  })

  #Time interval
  interval <- reactive({
    req(m_sub_ID())
    validate(need(sum(!is.na(m_sub_ID()$direction)) > 0, "Individual made no movements"))
    d <- min(difftime(m_sub_ID()$time[m_sub_ID()$direction == "arrived"], m_sub_ID()$time[m_sub_ID()$direction == "left"], units = "mins"), na.rm = TRUE)
    if(d == Inf) d <- 30
    return(as.numeric(d))
  })

  ## Render Base Map
  output$map <- renderLeaflet({
    req(m_sub_ID())

   map_leaflet_base(locs = unique(v[, c("feeder_id", "lat", "lon")])) %>%
      addScaleBar(position = "bottomright") %>%
      addLayersControl(baseGroups = c("Satellite", "Terrain", "Open Street Map", "Black and White"),
                       overlayGroups = c("Readers", "Sunset/Sunrise", "Paths"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
     clearGroup(group = "Paths")
  })

  ## Add points to animated map
  observeEvent(m_sub_time(), {
    req(m_sub_time())

    #if(is.null(values$m_sub_time_old) || m_sub_time() != values$m_sub_time_old){
      if(nrow(m_sub_time()) > 0){
        leafletProxy(ns("map")) %>%
          clearGroup(group = "Paths") %>%
          addPolylines(data = m_sub_time(),
                       ~lon, ~lat,
                       opacity = 0.45,
                       weight = 10,
                       group = "Paths",
                       color = 'blue')
      } else {
        leafletProxy(ns("map")) %>% clearGroup(group = "Paths")
      }
      #values$m_sub_time_old <- m_sub_time()
    #}
  }, priority = 50)


  ## Add sunrise sunset
  observeEvent(anim_time(), {
    req(anim_time(), interval())
    leafletProxy(ns("map")) %>%
      addTerminator(time = anim_time(),
                    layerId = paste0("set-", anim_time()),
                    group = "Sunrise/Sunset") %>%
      removeShape(layerId = paste0("set-", values$time_prev))
    values$time_prev <- anim_time()
  }, priority = 100)

  ## Time figure
  # g_time <- eventReactive(m_sub_ID(), {
  #   lab <- "Total movements"
  #   #lim <- c(start, ifelse(max(p_total()$block_time) + interval() * 60 * 60 > end, max(p_total()$block_time) + interval() * 60 * 60, end))
  #   g_time <- ggplot2::ggplot(data = m_sub_ID()) +
  #     ggplot2::theme_bw() +
  #     ggplot2::theme(legend.position = "none") +
  #     ggplot2::labs(x = "Time", y = lab) +
  #     ggplot2::scale_y_continuous(expand = c(0,0)) +
  #     ggplot2::scale_x_datetime(labels = scales::date_format("%Y %b %d\n%H:%M", tz = lubridate::tz(v$start)))
  #   g_time <- g_time + ggplot2::geom_bar(ggplot2::aes(x = day, y = direction))
  #   g_time
  # })


 observeEvent(input$pause, {
    browser()
  })

  # output$plot_time <- renderPlot({
  #   g_time()# + annotate("rect", xmin = anim_time()[1], xmax = anim_time()[1] + 60 * 60 * input$anim_interval, ymin = -Inf, ymax = +Inf, alpha = 0.5)
  # }, height = 150, width = 550)
}
