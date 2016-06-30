#' Launch local birdmoves site
#' @export
birdmoves <- function() {
  appDir <- system.file("shiny-examples", "birdmoves", package = "feedr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `feedr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

#' Animate map with leaflet
#'
#' @export
map_animate <- function(raw) {
  app <- shiny::shinyApp(ui = shiny::fluidPage(mod_UI_map_animate("standalone")),
                         server = function(input, output, session) {
                           shiny::callModule(mod_map_animate, "standalone",
                                             raw = raw)
                         }
  )
  shiny::runApp(app, display.mode = "normal")
}


