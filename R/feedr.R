#' Transforming Raw RFID Data
#'
#' Load, clean, transform, and visualize raw RFID data for looking at a variety
#' of biological questions.
#'
#' In-depth turorials available online:
#' <https://animalnexus.github.io/feedr/>
#'
#' @references animalnexus <http://animalnexus.ca>.
#' @docType package
#' @name feedr-package
#' @aliases feedr feedr-package animalnexus-package
#' @importFrom rlang .data
NULL


# Dealing with Non-standard evaluation
.onLoad <- function(libname = find.package("feedr"), pkgname = "feedr"){
# CRAN Note avoidance
if(getRversion() >= "2.15.1")
  utils::globalVariables(
    c(".") # piping requires '.' at times
  )
invisible()
}

