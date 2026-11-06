#' Transforming Raw RFID Data
#'
#' Load, clean, transform, and visualize raw RFID data for looking at a variety
#' of biological questions.
#'
#' @name feedr-package
#' @aliases feedr feedr-package
#' @importFrom rlang .data .env :=
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL


# Dealing with Non-standard evaluation
.onLoad <- function(libname = find.package("feedr"), pkgname = "feedr") {
  # CRAN Note avoidance
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(
      c(".") # piping requires '.' at times
    )
  }
  invisible()
}
