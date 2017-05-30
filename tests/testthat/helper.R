library(magrittr)

expect_equal_to_ggplot_reference <- function(object, file, info = NULL) {

  if (!file.exists(file)) {
    ggplot2::ggsave(file, object, width = 5, height = 5, dpi = 300)
    succeed()
  }
  else {
    ggplot2::ggsave("temp.png", object, width = 5, height = 5, dpi = 300)
    reference <- png::readPNG(file)
    object <- png::readPNG("temp.png")
    if(length(reference) == length(object)) {
      diff <- 100 * sum(object == reference) / length(reference)
    } else diff <- 0
    expect_gt(diff, 98, "Figures not equal")
    file.remove("temp.png")
  }
  invisible(object)
}

expect_equal_to_leaflet_reference <- function(object, file, info = NULL) {

  if (!file.exists(file)) {
    saveRDS(objct, file)
    testthat::succeed()
  }
  else {
    ref <- readRDS(file)

    ref$dependencies <- NA
    object$dependencies <- NA

    ref$x$calls[[1]]$args[[1]] <- NA
    object$x$calls[[1]]$args[[1]] <- NA

    ref$x$calls[[1]]$args[[4]]$attribution <- NA
    object$x$calls[[1]]$args[[4]]$attribution <- NA

    comp <- compare(object, ref)
    expect(comp$equal, sprintf("Object not equal to reference.\n%s",
                               comp$message), info = info)

  }
  invisible(object)
}
