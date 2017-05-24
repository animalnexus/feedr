#' Transforming Raw RFID Data
#'
#' Load, clean, transform, and visualize raw RFID data for looking at a variety
#' of biological questions.
#'
#' @references animalnexus \url{http://animalnexus.ca}.
#' @docType package
#' @name feedr-package
#' @aliases feedr feedr-package animalnexus-package
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
#' @aliases animalnexus animalnexus-site
#' @export
animalnexus <- function() {
  appDir <- system.file("shiny-examples", "animalnexus", package = "feedr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `feedr`.", call. = FALSE)
  }

  shiny::runApp(appDir, launch.browser = TRUE)
}


#' Dealing with CRAN Notes due to Non-standard evaluation
#'
.onLoad <- function(libname = find.package("feedr"), pkgname = "feedr"){
# CRAN Note avoidance
if(getRversion() >= "2.15.1")
  utils::globalVariables(
    # Vars used in Non-Standard Evaluations, declare here to avoid CRAN warnings
    c(# General
      "time", "logger_id", "animal_id", "left", "arrived", "species", "age", "sex", "v",

      # Loading
      "bird_id", "feeder_id",

      # Housekeeping
      "animal_ids", "animals_ids",

      # Visits
      "variable", "animal_n", "logger_n", "diff_imp", "diff_logger", "new", "diff_time", "diff_animal",

      # Movements
      "direction", "move_id", "strength", "left", "arrived", "direction", "value", "type", "start", "end", "path_use", "move_path",

      # Presence
      "amount",

      # Displacements/Dominance
      "role", "n", "displacer", "displacee", "desc", "p_win", "win", "loss",

      # Activity
      "time_c", "median", "rise", "set", "activity_c", "p_unknown",

      # Mapping
      "lat", "lon", "amount2", "combo", "lon_1", "lon_2", "lat_1", "lat_2", "path_use2", "block",

      # Shiny Modules
      "site_name", "dataaccess", "count", "start", "x", "fill", "facet", "end", "y", "counts_sum", "temp", "engl_name", "val", "id", "f", "arg", "title", "lab", "site_id",

      # Convert functions
      "item.number",

      # piping requires '.' at times
      ".")

  )
invisible()
}

