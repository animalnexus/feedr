#' Animated map with leaflet
#'
#' Interactive shiny app to select and animate feeder use of multiple
#' individuals over time. Also available through \code{animalnexus()}.
#'
#' @param v Data frame. Data frame. Visits data frame created with the
#'   \code{visits()} function.
#'
#' @examples
#' \dontrun{
#' map_animate(visits(finches))
#' }
#'
#' @export
map_animate <- function(v) {

  # Check for correct formatting
  check_name(v, c("bird_id", "feeder_id", "start", "end"))
  check_time(v)
  check_format(v)

  app <- shiny::shinyApp(ui = shiny::fluidPage(mod_UI_map_animate("standalone")),
                         server = function(input, output, session) {
                           shiny::callModule(mod_map_animate, "standalone",
                                             v = v)
                         }
  )
  shiny::runApp(app, display.mode = "normal")
}


## Animated map - UI
#' @import shiny
#' @import magrittr
#' @export
mod_UI_map_animate <- function(id) {
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
           #actionButton(ns("animate"), "Create Animation"),
           #actionButton(ns("pause"), "Pause"),
           #hr(),
           popify(radioButtons(ns("anim_type"), "Summary type:",
                        choices = c("Total no. visits" = "t_visits",
                                    "Avg. visits per individual" = "b_visits",
                                    "Total no. individuals" = "t_birds")),
                  title = "Summaries", content = "How the data should be summarized for each time interval at each reader:<br/>Count the total number of visits<br/>Calculate the average number of visits per individual<br/>Count the total number of individuals.",
                  options = list(container = "body")),
           popify(uiOutput(ns("UI_time_range")), title = "Time range",
                  content = "Select a particular time range to look at", options = list(container = "body")),
           hr(),
           popify(radioButtons(ns("interval"), "Resolution",
                        choices = list("5 min" = 5, "15 min" = 15, "30 min" = 30, "1 hr" = 60, "3 hr" = 60*3, "6 hr" = 60*6, "12 hr" = 60 * 12, "24 hr" = 60 * 24), inline = TRUE),
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
           p("Select your summary type, and time range (above), and the specific 'Time' (right) to show a summary of visits to each feeder for that interval of time on the map."),
           p("Summaries can be animated over time by clicking on the", strong("small blue arrow"), "to the lower right of the 'Time' slider (right)."),
           p("The time interval of each jump and the speed of the animation can be adjusted above."),
           h3("Tip:"),
           p("If you find your animations lagging, reduce the amount of data (reduce the time range or increase the interval length).")
           #p("Circles on the map representing a summary of visits to each feeder (Total no. visits, Avg. visits per bird, or Total no. birds) for the interval choosen at the time indicated on the time slider."),
           #p("For example, with 'Avg. visits per bird' and an interval of '4 hours', the colour of each circle represents the average number of visits per bird made at a particular feeder over that 4 hour interval."),
           #p("The specific time can be changed with the slider bar and can even be automatically advanced by clicking on the", strong("small blue arrow"), "to the lower right of the Time slider")
    ),
    column(8,
           popify(fluidRow(leafletOutput(ns("map"), height = 600)),
                    title = "Activity summaries",
                    content = "Circles show the amount of activity at each feeder for the given time interval.",
                    options = list(container = "body")),
           div(
             fluidRow(uiOutput(ns("UI_time")), style = "text-align: center;"),
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
mod_map_animate <- function(input, output, session, v) {

  ns <- session$ns
  values <- reactiveValues()

  ## Palette
  pal <- colorRampPalette(c("yellow","orange", "red"))

  # Fix time zone to LOOK like local non-DST, but assigned UTC (for timezone slider)
  v <- v %>%
    dplyr::mutate(start = lubridate::with_tz(start, tzone = tz_offset(attr(v$start, "tzone"), tz_name = TRUE)),
                  end = lubridate::with_tz(end, tzone = tz_offset(attr(v$end, "tzone"), tz_name = TRUE)),
                  day = as.Date(start))

  tz <- tz_offset(attr(v$start, "tzone"))
  if(tz >=0) tz <- paste0("+", sprintf("%02d", abs(tz)), "00") else tz <- paste0("-", sprintf("%02d", abs(tz)), "00")

  #lubridate::tz(v$start) <- "UTC"
  #lubridate::tz(v$end) <- "UTC"

  start <- lubridate::floor_date(min(v$start), unit = "hour")
  end <- lubridate::ceiling_date(max(v$start), unit = "hour")

  interval <- reactive({
    req(input$interval)
    as.numeric(input$interval)
  })

  # Time range - select subsection of data
  output$UI_time_range <- renderUI({
    sliderInput(ns("time_range"), "Time Range",
                min = start,
                max = end,
                value = c(start, end),
                step = 60 * 60,
                timezone = tz)
  })

  time_range <- reactive({
    req(input$time_range)
    lubridate::with_tz(input$time_range, lubridate::tz(v$start))
  })

  v_range <- reactive({
    req(time_range())
    dplyr::filter(v, start >= time_range()[1]  & end <= time_range()[2])
  })

  # Time slider
  output$UI_time <- renderUI({
    req(input$anim_speed, interval(), time_range())
    sliderInput(ns("time"), "Time",
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
    lubridate::with_tz(input$time, lubridate::tz(v$start))
  })


  ## Break visits into blocks of time depending on animation interval
  v_block <- reactive({
    req(interval(), v_range())

    validate(need(sum(names(v_range()) %in% c("lat", "lon")) == 2, "Latitude and longitude ('lat' and 'lon', respectively) were not detected in the data. Can't determine feeder locations without them"))

    int_start <- seq(start, end - interval() * 60, by = paste(interval(), "min"))
    int_end <- seq(start + interval() * 60, end, by = paste(interval(), "min"))
    ## Add to end if not even
    if(length(int_end) != end) {
      int_start <- c(int_start, int_end[length(int_end)])
      int_end <- c(int_end, end)
    }
    v_block <- v_range() %>%
      dplyr::bind_cols(data.frame(block = sapply(v_range()$start, FUN = function(x) which(x >= int_start & x < int_end)))) %>%
      dplyr::bind_cols(data.frame(block_time = int_start[.$block]))

  })

  ## Get total data sets depending on options
  p_total <- reactive({
    req(input$anim_type)

    validate(need(sum(names(v_range()) %in% c("lat", "lon")) == 2, "Latitude and longitude ('lat' and 'lon', respectively) were not detected in the data. Can't determine feeder locations without them"))

    withProgress({
      if(input$anim_type == "t_visits") {
        #Total number of visits
        p_total <- v_block() %>%
          dplyr::group_by(feeder_id, lat, lon, block, block_time, add = TRUE) %>%
          dplyr::summarize(n = length(start))
      } else if(input$anim_type == "b_visits") {
        #Average number of visits per bird
        p_total <- v_block() %>%
          dplyr::group_by(feeder_id, lat, lon, bird_id, block, block_time, add = TRUE) %>%
          dplyr::summarize(n = length(start)) %>%
          dplyr::group_by(feeder_id, lat, lon, block, block_time) %>%
          dplyr::summarize(n = mean(n))
      } else if(input$anim_type == "t_birds") {
        #Total number of birds
        p_total <- v_block() %>%
          dplyr::group_by(feeder_id, lat, lon, block, block_time, add = TRUE) %>%
          dplyr::summarize(n = length(unique(bird_id)))
      }
    }, message = "Calculating intervals")
    p_total
  })

  ## Filter points by time
  p <- reactive({
    req(time(), interval())
    p_total() %>% dplyr::filter(block_time >= time(), block_time < time() + 60 * interval())
  })

  ## Render Base Map
  output$map <- renderLeaflet({
    req(input$anim_type, p_total())

    if(max(p_total()$n) == 1) vals <- 1:5 else vals <- 1:max(p_total()$n)
    pal <- colorNumeric(palette = pal(max(vals)),
                        domain = vals)

    map_leaflet_base(locs = unique(v[, c("feeder_id", "lat", "lon")])) %>%
      addScaleBar(position = "bottomright") %>%
      addLayersControl(baseGroups = c("Satellite", "Terrain", "Open Street Map", "Black and White"),
                       overlayGroups = c("Readers", "Sunset/Sunrise", "Visits"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
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
    req(input$anim_type, p(), p_total())
    if(max(p_total()$n) == 1) vals <- 1:5 else vals <- 1:max(p_total()$n)
    pal <- colorNumeric(palette = pal(max(vals)),
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
    req(p(), p_total())

    if(max(p_total()$n) == 1) vals <- 1:5 else vals <- 1:max(p_total()$n)
    pal <- colorNumeric(palette = pal(max(vals)),
                        domain = vals)

    if(nrow(p()) > 0){
      leafletProxy(ns("map")) %>%
        clearGroup(group = "Visits") %>%
        addCircleMarkers(data = p(), lat = ~lat, lng = ~lon, group = "Visits",
                                  stroke = FALSE,
                                  fillOpacity = 1,
                                  radius = 50,
                                  fillColor = ~pal(n),
                                  popup = ~htmltools::htmlEscape(as.character(round(n, 1))))
    } else {
      leafletProxy(ns("map")) %>% clearGroup(group = "Visits")
    }
  }, priority = 50)


  ## Add sunrise sunset
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




  ## Time figure
  g_time <- eventReactive(p_total(), {
    lab <- ifelse(input$anim_type == "t_visits", "Total no. visists", ifelse(input$anim_type == "b_visits", "Avg. visits per bird", "Total no. birds"))
    lim <- c(start, ifelse(max(p_total()$block_time) + interval() * 60 > end, max(p_total()$block_time) + interval() * 60, end))
    g_time <- ggplot2::ggplot(data = p_total()) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(x = "Time", y = lab) +
      ggplot2::scale_y_continuous(expand = c(0,0)) +
      ggplot2::scale_x_datetime(labels = scales::date_format("%Y %b %d\n%H:%M", tz = lubridate::tz(v$start)),
                                limits = time_range())
                                #breaks = seq(start, end, length.out = 5),
                                #expand = c(0,0))

      g_time <- g_time + ggplot2::geom_bar(stat = "identity", ggplot2::aes(x = block_time, y = n, group = feeder_id)) +
        ggplot2::labs(fill = "Reader")

    g_time
  })


  ## If video selected, create animated video instead
  # observeEvent(input$animate, {
  #   req(p_total())
  #   d <- p_total() %>%
  #     dplyr::rename(amount = n)
  #
  #   for(i in unique(d$block_time)){
  #     g <- map_ggmap(u = d[d$block_time == i, ])
  #     ggsave(filename = paste0("~/Desktop/temp",i,".png"), plot = g)
  #   }
  #
  #   browser()
  # })

  observeEvent(input$pause, {
    browser()
  })

  output$plot_time <- renderPlot({
    g_time()# + annotate("rect", xmin = time()[1], xmax = time()[1] + 60 * 60 * interval(), ymin = -Inf, ymax = +Inf, alpha = 0.5)
  }, height = 150, width = 550)
}
