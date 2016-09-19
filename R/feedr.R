#' Transforming Raw RFID Data
#'
#' Load, clean, transform, and visualize raw RFID data for looking at a variety
#' of biological questions.
#'
#' @references animalnexus \url{http://animalnexus.ca}.
#' @docType package
#' @name feedr-package
#' @aliases feedr feedr-package animalnexus animalnexus-package
NULL


#' Launch local animalnexus
#'
#' A local shiny app of the animalnexus site allowing users to import, transform and visualize data locally. Note that visualizations require an internet connection to download map tiles.
#'
#' @examples
#'
#' \dontrun{
#' animalnexus()
#' }
#'
#' @export
animalnexus <- function() {
  appDir <- system.file("shiny-examples", "animalnexus", package = "feedr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `feedr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

