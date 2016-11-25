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

  app <- shiny::shinyApp(ui = shiny::fluidPage(shinyjs::useShinyjs(),
                                               includeCSS(system.file("extra", "style.css", package = "feedr")),
                                               mod_UI_map_animate("standalone")),
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
mod_UI_map_animate <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    column(4,
           popify(radioButtons(ns("anim_type"), "Summary type:",
                        choices = c("Total no. visits" = "t_visits",
                                    "Avg. visits per individual" = "b_visits",
                                    "Total no. individuals" = "t_birds")),
                  title = "Summaries", content = "How the data should be summarized for each time interval at each reader:<br/>Count the total number of visits<br/>Calculate the average number of visits per individual<br/>Count the total number of individuals.",
                  options = list(container = "body")),
           feedr:::mod_UI_maps_controls(ns("setup")),
           feedr:::mod_UI_maps_sunrise(ns("map")),
           feedr:::mod_UI_maps_instructions(ns("details"),
                                            specific = tagList(
                                              p("Select your summary type, and time range (above), and the specific 'Time' (right) to show a summary of visits to each feeder for that interval of time on the map.")
                                            ))
    ),

    column(8,
           feedr:::mod_UI_maps_leaflet(ns("map")),
           feedr:::mod_UI_maps_time(ns("setup_time"))
    )
  )
}

# Module server function
#' @import shiny
#' @import magrittr
#' @import leaflet
#' @import lubridate
mod_map_animate <- function(input, output, session, v) {

  ns <- session$ns
  values <- reactiveValues()

  v <- data_tz(v)

  v_avg <- reactive({
    req(v, controls$time_range(), controls$interval(), input$anim_type)
    validate(need(sum(names(v) %in% c("lat", "lon")) == 2, "Latitude and longitude ('lat' and 'lon', respectively) were not detected in the data. Can't determine feeder locations without them"))

    breaks <- seq(controls$time_range()[1], controls$time_range()[2], by = paste(controls$interval(), "min"))
    v <- v %>%
      dplyr::filter(start >= controls$time_range()[1]  & end <= controls$time_range()[2]) %>%
      dplyr::mutate(block = as.POSIXct(cut(start, breaks = breaks, include.lowest = TRUE), tz = controls$tz()))
    withProgress({
      if(input$anim_type == "t_visits") {
        #Total number of visits
        v_avg <- v %>%
          dplyr::group_by(feeder_id, lat, lon, block, add = TRUE) %>%
          dplyr::summarize(amount = length(start))
      } else if(input$anim_type == "b_visits") {
        #Average number of visits per bird
        v_avg <- v %>%
          dplyr::group_by(feeder_id, lat, lon, bird_id, block, add = TRUE) %>%
          dplyr::summarize(amount = length(start)) %>%
          dplyr::group_by(feeder_id, lat, lon, block) %>%
          dplyr::summarize(amount = mean(amount))
      } else if(input$anim_type == "t_birds") {
        #Total number of birds
        v_avg <- v %>%
          dplyr::group_by(feeder_id, lat, lon, block, add = TRUE) %>%
          dplyr::summarize(amount = length(unique(bird_id)))
      }
    }, message = "Calculating intervals")
    v_avg
  })

  events <- reactive({
    req(v_avg())
    v_avg() %>%
      dplyr::rename(type = feeder_id)
  })


  type_title <- reactive({
    rep(input$anim_type)
    if(input$anim_type == "t_visits") return("Total no.<br>visits")
    if(input$anim_type == "b_visits") return("Avg. visits<br>per individual")
    if(input$anim_type == "t_birds") return("Total no.<br>individuals")
  })

  ## Call map modules
  controls <- callModule(mod_maps_controls, "setup", times = reactive({v$start}))
  instant <- callModule(mod_maps_time, "setup_time", controls = controls, events = events)

  v_instant <- callModule(mod_maps_data, "data", controls = controls, instant = instant, data = v_avg)

  callModule(mod_maps_sunrise, "map", instant = instant, controls = controls)
  callModule(mod_maps_leaflet, "map",
             data = v_instant, data_total = v_avg, title = type_title)

  observeEvent(input$pause, {
    browser()
  })
}
