#' @export
birdmoves <- function() {
  appDir <- system.file("shiny-examples", "birdmoves", package = "feedr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `feedr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

#' @export
map_animate <- function(data) {
  app <- shiny::shinyApp(ui = shiny::fluidPage(UI_map_animated("standalone")),
                         server = function(input, output, session) {
                           shiny::callModule(map_animated, "standalone",
                                             data = data)
                         }
  )
  shiny::runApp(app, display.mode = "normal")
}


