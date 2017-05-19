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
    expect_gt(diff, 99, "Figures not equal")
    file.remove("temp.png")
  }
  invisible(object)
}
