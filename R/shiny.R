birdmoves <- function() {
  appDir <- system.file("shiny-examples", "birdmoves", package = "feedr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `feedr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
