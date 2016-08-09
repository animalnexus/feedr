#' Launch local animalnexus site
#' @export
animalnexus <- function() {
  appDir <- system.file("shiny-examples", "animalnexus", package = "feedr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `feedr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

#' Animate map with leaflet
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
